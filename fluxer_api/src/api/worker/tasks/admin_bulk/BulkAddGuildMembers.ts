// SPDX-License-Identifier: AGPL-3.0-or-later

import {JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {JobCancelledError} from '@pkgs/worker/src/contracts/WorkerTask';
import {AdminAuditService} from '../../../admin/services/AdminAuditService';
import {createGuildID, createUserID} from '../../../BrandedTypes';
import {createRequestCache} from '../../../middleware/RequestCacheMiddleware';
import {getWorkerDependencies} from '../../WorkerContext';

interface Payload {
	guild_id: string;
	user_ids: Array<string>;
	admin_user_id: string;
	audit_log_reason: string | null;
}

const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload: Payload = {
		guild_id: rawPayload.guild_id as string,
		user_ids: rawPayload.user_ids as Array<string>,
		admin_user_id: rawPayload.admin_user_id as string,
		audit_log_reason: (rawPayload.audit_log_reason as string | null) ?? null,
	};
	const deps = getWorkerDependencies();
	const auditService = new AdminAuditService(deps.adminRepository, deps.snowflakeService);
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const guildId = createGuildID(BigInt(payload.guild_id));
	const userIds = payload.user_ids.map((id) => BigInt(id));
	const total = userIds.length;
	const successful: Array<string> = [];
	const failed: Array<{
		id: string;
		error: string;
	}> = [];
	await helpers.setContextLink(`/guilds/${guildId}`);
	await helpers.reportProgress(0, total, `Adding ${total} members to guild ${guildId}`);
	for (let i = 0; i < userIds.length; i++) {
		if (await helpers.shouldCancel()) throw new JobCancelledError();
		const userIdBigInt = userIds[i]!;
		const userId = createUserID(userIdBigInt);
		try {
			await deps.guildService.members.addUserToGuild({
				skipRiskGate: true,
				userId,
				guildId,
				sendJoinMessage: false,
				skipBanCheck: true,
				joinSourceType: JoinSourceTypes.ADMIN_FORCE_ADD,
				requestCache: createRequestCache(),
				initiatorId: adminUserId,
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
		targetType: 'guild',
		targetId: BigInt(guildId),
		action: 'bulk_add_guild_members',
		auditLogReason: payload.audit_log_reason,
		metadata: new Map([
			['guild_id', guildId.toString()],
			['user_count', total.toString()],
			['successful', successful.length.toString()],
			['failed', failed.length.toString()],
		]),
	});
	await helpers.reportProgress(total, total, `+${successful.length} ok, ${failed.length} failed`);
	helpers.logger.info({successful: successful.length, failed: failed.length}, 'bulkAddGuildMembers complete');
};

export default handler;
