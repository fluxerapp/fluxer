// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {UserID} from '@app/api/BrandedTypes';
import {JobCancelledError, type WorkerTaskHelpers, type WorkerTaskResult} from '@pkgs/worker/src/contracts/WorkerTask';

const MAX_RECORDED_FAILURES = 100;

interface AdminBulkFailure {
	id: string;
	error: string;
}

interface AdminBulkJobParams {
	helpers: WorkerTaskHelpers;
	ids: ReadonlyArray<string>;
	progressEvery: number;
	apply: (id: string) => Promise<void>;
	afterItems?: () => Promise<ReadonlyArray<[string, string]>>;
	summary: {
		auditService: AdminAuditService;
		adminUserId: UserID;
		action: string;
		auditLogReason: string | null;
		metadata: ReadonlyArray<[string, string]>;
		target?: {type: string; id: bigint};
	};
}

function describeError(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

export async function runAdminBulkJob({
	helpers,
	ids,
	progressEvery,
	apply,
	afterItems,
	summary,
}: AdminBulkJobParams): Promise<WorkerTaskResult> {
	const total = ids.length;
	const failed: Array<AdminBulkFailure> = [];
	let successfulCount = 0;
	let processed = 0;
	let cancelled = false;
	for (const id of ids) {
		if (await helpers.shouldCancel()) {
			cancelled = true;
			break;
		}
		try {
			await apply(id);
			successfulCount++;
		} catch (error) {
			failed.push({id, error: describeError(error)});
			helpers.logger.warn({id, err: error}, 'Admin bulk job item failed');
		}
		processed++;
		if (processed % progressEvery === 0) {
			await helpers.reportProgress(processed, total, null);
		}
	}
	const finalMetadata: Array<[string, string]> = [];
	let finalizeError: string | null = null;
	if (afterItems) {
		try {
			finalMetadata.push(...(await afterItems()));
		} catch (error) {
			finalizeError = describeError(error);
			helpers.logger.error({err: error}, 'Admin bulk job finalisation failed');
		}
	}
	await summary.auditService.createAuditLog({
		adminUserId: summary.adminUserId,
		targetType: summary.target?.type ?? 'bulk_job',
		targetId: summary.target?.id ?? helpers.jobId,
		action: summary.action,
		auditLogReason: summary.auditLogReason,
		metadata: new Map([
			...summary.metadata,
			['job_id', helpers.jobId.toString()],
			...finalMetadata,
			...(finalizeError !== null ? ([['finalize_error', finalizeError]] as Array<[string, string]>) : []),
			['processed', processed.toString()],
			['successful', successfulCount.toString()],
			['failed', failed.length.toString()],
			...(cancelled ? ([['cancelled', 'true']] as Array<[string, string]>) : []),
		]),
	});
	if (cancelled) {
		throw new JobCancelledError();
	}
	await helpers.reportProgress(total, total, `+${successfulCount} ok, ${failed.length} failed`);
	helpers.logger.info({successful: successfulCount, failed: failed.length}, 'Admin bulk job complete');
	return {
		successful_count: successfulCount,
		failed_count: failed.length,
		failed: failed.slice(0, MAX_RECORDED_FAILURES),
		...(finalizeError !== null ? {finalize_error: finalizeError} : {}),
	};
}
