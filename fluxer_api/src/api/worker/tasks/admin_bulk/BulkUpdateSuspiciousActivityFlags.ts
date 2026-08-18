// SPDX-License-Identifier: AGPL-3.0-or-later

import {imposePhoneRequirements, SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {JobCancelledError} from '@pkgs/worker/src/contracts/WorkerTask';
import {AdminAuditService} from '../../../admin/services/AdminAuditService';
import {AdminUserUpdatePropagator} from '../../../admin/services/AdminUserUpdatePropagator';
import {createUserID} from '../../../BrandedTypes';
import {getWorkerDependencies} from '../../WorkerContext';

interface Payload {
	user_ids: Array<string>;
	add_flags: Array<string>;
	remove_flags: Array<string>;
	admin_user_id: string;
	audit_log_reason: string | null;
}

const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload: Payload = {
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
	const addMask = payload.add_flags.reduce((mask, name) => {
		const v = SuspiciousActivityFlags[name as keyof typeof SuspiciousActivityFlags];
		return v !== undefined ? mask | v : mask;
	}, 0);
	const removeMask = payload.remove_flags.reduce((mask, name) => {
		const v = SuspiciousActivityFlags[name as keyof typeof SuspiciousActivityFlags];
		return v !== undefined ? mask | v : mask;
	}, 0);
	const total = userIds.length;
	const successful: Array<string> = [];
	const failed: Array<{
		id: string;
		error: string;
	}> = [];
	await helpers.setContextLink(`/users?ids=${userIds.slice(0, 50).join(',')}`);
	await helpers.reportProgress(0, total, `Updating suspicious flags on ${total} users`);
	for (let i = 0; i < userIds.length; i++) {
		if (await helpers.shouldCancel()) throw new JobCancelledError();
		const userIdBigInt = userIds[i]!;
		const userId = createUserID(userIdBigInt);
		try {
			const user = await deps.userRepository.findUnique(userId);
			if (!user) throw new Error('user_not_found');
			const currentFlags = user.suspiciousActivityFlags ?? 0;
			const newFlags = imposePhoneRequirements(currentFlags, addMask) & ~removeMask;
			const updatedUser = await deps.userRepository.patchUpsert(
				userId,
				{suspicious_activity_flags: newFlags},
				user.toRow(),
			);
			await propagator.propagateUserUpdate({userId, oldUser: user, updatedUser});
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
		action: 'bulk_update_suspicious_activity_flags',
		auditLogReason: payload.audit_log_reason,
		metadata: new Map(
			(
				[
					['user_count', total.toString()],
					['add_flags', payload.add_flags.join(',')],
					['remove_flags', payload.remove_flags.join(',')],
					['successful', successful.length.toString()],
					['failed', failed.length.toString()],
				] as Array<[string, string]>
			).filter(([_, v]) => v.length > 0),
		),
	});
	await helpers.reportProgress(total, total, `+${successful.length} ok, ${failed.length} failed`);
	helpers.logger.info(
		{successful: successful.length, failed: failed.length},
		'bulkUpdateSuspiciousActivityFlags complete',
	);
};

export default handler;
