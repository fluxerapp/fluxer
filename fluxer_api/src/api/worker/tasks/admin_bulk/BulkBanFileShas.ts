// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {createUserID} from '@app/api/BrandedTypes';
import {ContentBlocklistCategory, ContentBlocklistSeverity} from '@app/api/constants/ContentModeration';
import {fileShaCache} from '@app/api/middleware/FileShaCache';
import {getCacheService} from '@app/api/middleware/ServiceSingletons';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {JobCancelledError} from '@pkgs/worker/src/contracts/WorkerTask';

const BANNED_FILE_SHAS_REFRESH_CHANNEL = 'banned_file_shas:refresh';
const SHA256_RE = /^[0-9a-fA-F]{64}$/;

interface Payload {
	sha256_list: Array<string>;
	admin_user_id: string;
	audit_log_reason: string | null;
}

const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload: Payload = {
		sha256_list: rawPayload.sha256_list as Array<string>,
		admin_user_id: rawPayload.admin_user_id as string,
		audit_log_reason: (rawPayload.audit_log_reason as string | null) ?? null,
	};
	const deps = getWorkerDependencies();
	const cacheService = getCacheService();
	const auditService = new AdminAuditService(deps.adminRepository, deps.snowflakeService);
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const total = payload.sha256_list.length;
	const successful: Array<string> = [];
	const failed: Array<{
		id: string;
		error: string;
	}> = [];
	await helpers.setContextLink('/file-sha-bans');
	await helpers.reportProgress(0, total, `Banning ${total} file SHAs`);
	for (let i = 0; i < payload.sha256_list.length; i++) {
		if (await helpers.shouldCancel()) throw new JobCancelledError();
		const sha = payload.sha256_list[i]!.toLowerCase();
		if (!SHA256_RE.test(sha)) {
			failed.push({id: sha, error: 'invalid_sha256'});
			continue;
		}
		try {
			await deps.adminRepository.banFileSha({
				sha256_hex: sha,
				category: ContentBlocklistCategory.MANUAL,
				severity: ContentBlocklistSeverity.BLOCK,
				content_type: null,
				source_url: null,
				added_at: new Date(),
				added_by: adminUserId,
				notes: null,
			});
			fileShaCache.add(sha);
			successful.push(sha);
		} catch (err) {
			failed.push({id: sha, error: err instanceof Error ? err.message : String(err)});
		}
		if ((i + 1) % 50 === 0) {
			await helpers.reportProgress(i + 1, total, null);
		}
	}
	await cacheService.publish(BANNED_FILE_SHAS_REFRESH_CHANNEL, 'refresh');
	await auditService.createAuditLog({
		adminUserId,
		targetType: 'file_sha',
		targetId: BigInt(0),
		action: 'bulk_ban_file_shas',
		auditLogReason: payload.audit_log_reason,
		metadata: new Map([
			['count', total.toString()],
			['successful', successful.length.toString()],
			['failed', failed.length.toString()],
		]),
	});
	await helpers.reportProgress(total, total, `+${successful.length} ok, ${failed.length} failed`);
	helpers.logger.info({successful: successful.length, failed: failed.length}, 'bulkBanFileShas complete');
};

export default handler;
