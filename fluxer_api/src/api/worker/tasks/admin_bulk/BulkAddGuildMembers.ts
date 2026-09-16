// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {runAdminBulkJob} from '@app/api/worker/tasks/admin_bulk/AdminBulkJob';
import {createAdminBulkServices} from '@app/api/worker/tasks/admin_bulk/AdminBulkServices';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

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
	const {auditService, guildService} = createAdminBulkServices(getWorkerDependencies());
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const guildId = BigInt(payload.guild_id);
	const total = payload.user_ids.length;
	await helpers.setContextLink(`/guilds/${guildId}`);
	await helpers.reportProgress(0, total, `Adding ${total} members to guild ${guildId}`);
	return await runAdminBulkJob({
		helpers,
		ids: payload.user_ids,
		progressEvery: 25,
		apply: async (id) => {
			await guildService.membershipService.forceAddUserToGuild({
				data: {guild_id: guildId, user_id: BigInt(id)},
				requestCache: createRequestCache(),
				adminUserId,
				auditLogReason: payload.audit_log_reason,
				sendJoinMessage: false,
			});
		},
		summary: {
			auditService,
			adminUserId,
			action: 'bulk_add_guild_members',
			auditLogReason: payload.audit_log_reason,
			target: {type: 'guild', id: guildId},
			metadata: [
				['guild_id', guildId.toString()],
				['user_count', total.toString()],
			],
		},
	});
};

export default handler;
