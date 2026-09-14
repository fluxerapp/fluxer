// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {runAdminBulkJob} from '@app/api/worker/tasks/admin_bulk/AdminBulkJob';
import {createAdminBulkServices} from '@app/api/worker/tasks/admin_bulk/AdminBulkServices';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {ALL_SUSPICIOUS_ACTIVITY_FLAGS, SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

interface Payload {
	user_ids: Array<string>;
	add_flags: Array<string>;
	remove_flags: Array<string>;
	admin_user_id: string;
	audit_log_reason: string | null;
}

const PROGRESS_EVERY = 25;

function resolveFlagMask(flagNames: ReadonlyArray<string>): number {
	let mask = 0;
	const unknownNames: Array<string> = [];
	for (const flagName of flagNames) {
		const value = SuspiciousActivityFlags[flagName as keyof typeof SuspiciousActivityFlags];
		if (value === undefined) {
			unknownNames.push(flagName);
			continue;
		}
		mask |= value;
	}
	if (unknownNames.length > 0) {
		throw new Error(`Unknown suspicious activity flag names: ${unknownNames.join(', ')}`);
	}
	return mask;
}

const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload: Payload = {
		user_ids: rawPayload.user_ids as Array<string>,
		add_flags: rawPayload.add_flags as Array<string>,
		remove_flags: rawPayload.remove_flags as Array<string>,
		admin_user_id: rawPayload.admin_user_id as string,
		audit_log_reason: (rawPayload.audit_log_reason as string | null) ?? null,
	};
	const addMask = resolveFlagMask(payload.add_flags);
	const removeMask = resolveFlagMask(payload.remove_flags);
	const deps = getWorkerDependencies();
	const {auditService, userService} = createAdminBulkServices(deps);
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const acls = new Set<string>();
	const total = payload.user_ids.length;
	await helpers.setContextLink(`/users?ids=${payload.user_ids.slice(0, 50).join(',')}`);
	await helpers.reportProgress(0, total, `Updating suspicious flags on ${total} users`);
	return await runAdminBulkJob({
		helpers,
		ids: payload.user_ids,
		progressEvery: PROGRESS_EVERY,
		apply: async (id) => {
			const userIdBigInt = BigInt(id);
			const user = await deps.userRepository.findUnique(createUserID(userIdBigInt));
			if (!user) {
				throw new UnknownUserError();
			}
			const currentFlags = user.suspiciousActivityFlags ?? 0;
			const flags = (currentFlags | addMask) & ~removeMask & ALL_SUSPICIOUS_ACTIVITY_FLAGS;
			await userService.securityService.updateSuspiciousActivityFlags(
				{user_id: userIdBigInt, flags},
				adminUserId,
				payload.audit_log_reason,
				acls,
			);
		},
		summary: {
			auditService,
			adminUserId,
			action: 'bulk_update_suspicious_activity_flags',
			auditLogReason: payload.audit_log_reason,
			metadata: (
				[
					['user_count', total.toString()],
					['add_flags', payload.add_flags.join(',')],
					['remove_flags', payload.remove_flags.join(',')],
				] as Array<[string, string]>
			).filter(([, value]) => value.length > 0),
		},
	});
};

export default handler;
