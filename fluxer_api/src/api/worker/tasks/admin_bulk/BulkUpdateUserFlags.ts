// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {AdminUserUpdatePropagator} from '@app/api/admin/services/AdminUserUpdatePropagator';
import {createUserID} from '@app/api/BrandedTypes';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {JobCancelledError} from '@pkgs/worker/src/contracts/WorkerTask';

interface BulkUpdateUserFlagsPayload {
	user_ids: Array<string>;
	add_flags: Array<string>;
	remove_flags: Array<string>;
	admin_user_id: string;
	audit_log_reason: string | null;
}

const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload: BulkUpdateUserFlagsPayload = {
		user_ids: rawPayload.user_ids as Array<string>,
		add_flags: rawPayload.add_flags as Array<string>,
		remove_flags: rawPayload.remove_flags as Array<string>,
		admin_user_id: rawPayload.admin_user_id as string,
		audit_log_reason: (rawPayload.audit_log_reason as string | null) ?? null,
	};
	const deps = getWorkerDependencies();
	const auditService = new AdminAuditService(deps.adminRepository, deps.snowflakeService);
	const propagator = new AdminUserUpdatePropagator({
		userCacheService: deps.userCacheService,
		userRepository: deps.userRepository,
		guildRepository: deps.guildRepository,
		gatewayService: deps.gatewayService,
	});
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const userIds = payload.user_ids.map((id) => BigInt(id));
	const addFlags = payload.add_flags.map((f) => BigInt(f));
	const removeFlags = payload.remove_flags.map((f) => BigInt(f));
	const total = userIds.length;
	const successful: Array<string> = [];
	const failed: Array<{
		id: string;
		error: string;
	}> = [];
	await helpers.setContextLink(`/users?ids=${userIds.slice(0, 50).join(',')}`);
	await helpers.reportProgress(0, total, `Updating flags on ${total} users`);
	for (let i = 0; i < userIds.length; i++) {
		if (await helpers.shouldCancel()) throw new JobCancelledError();
		const userIdBigInt = userIds[i]!;
		const userId = createUserID(userIdBigInt);
		try {
			const user = await deps.userRepository.findUnique(userId);
			if (!user) throw new Error('user_not_found');
			let newFlags = user.flags;
			for (const f of addFlags) newFlags |= f;
			for (const f of removeFlags) newFlags &= ~f;
			const updatedUser = await deps.userRepository.patchUpsert(userId, {flags: newFlags}, user.toRow());
			await propagator.propagateUserUpdate({userId, oldUser: user, updatedUser});
			await auditService.createAuditLog({
				adminUserId,
				targetType: 'user',
				targetId: BigInt(userId),
				action: 'update_flags',
				auditLogReason: null,
				metadata: new Map(
					(
						[
							['add_flags', addFlags.map((f) => f.toString()).join(',')],
							['remove_flags', removeFlags.map((f) => f.toString()).join(',')],
							['new_flags', newFlags.toString()],
						] as Array<[string, string]>
					).filter(([_, v]) => v.length > 0),
				),
			});
			successful.push(userId.toString());
		} catch (err) {
			failed.push({id: userIdBigInt.toString(), error: err instanceof Error ? err.message : String(err)});
		}
		if ((i + 1) % 25 === 0) {
			await helpers.reportProgress(i + 1, total, null);
		}
	}
	await auditService.createAuditLog({
		adminUserId,
		targetType: 'user',
		targetId: BigInt(0),
		action: 'bulk_update_user_flags',
		auditLogReason: payload.audit_log_reason,
		metadata: new Map(
			(
				[
					['user_count', total.toString()],
					['add_flags', addFlags.map((f) => f.toString()).join(',')],
					['remove_flags', removeFlags.map((f) => f.toString()).join(',')],
					['successful', successful.length.toString()],
					['failed', failed.length.toString()],
				] as Array<[string, string]>
			).filter(([_, v]) => v.length > 0),
		),
	});
	await helpers.reportProgress(total, total, `+${successful.length} ok, ${failed.length} failed`);
	helpers.logger.info({successful: successful.length, failed: failed.length}, 'bulkUpdateUserFlags task complete');
};

export default handler;
