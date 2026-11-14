// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {runAdminBulkJob} from '@app/api/worker/tasks/admin_bulk/AdminBulkJob';
import {createAdminBulkServices} from '@app/api/worker/tasks/admin_bulk/AdminBulkServices';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

interface Payload {
	guild_ids: Array<string>;
	add_features: Array<string>;
	remove_features: Array<string>;
	admin_user_id: string;
	audit_log_reason: string | null;
}

const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload: Payload = {
		guild_ids: rawPayload.guild_ids as Array<string>,
		add_features: rawPayload.add_features as Array<string>,
		remove_features: rawPayload.remove_features as Array<string>,
		admin_user_id: rawPayload.admin_user_id as string,
		audit_log_reason: (rawPayload.audit_log_reason as string | null) ?? null,
	};
	const {auditService, guildService} = createAdminBulkServices(getWorkerDependencies());
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const total = payload.guild_ids.length;
	await helpers.setContextLink(`/guilds?ids=${payload.guild_ids.slice(0, 50).join(',')}`);
	await helpers.reportProgress(0, total, `Updating features on ${total} guilds`);
	return await runAdminBulkJob({
		helpers,
		ids: payload.guild_ids,
		progressEvery: 25,
		apply: async (id) => {
			await guildService.updateService.updateGuildFeatures({
				guildId: createGuildID(BigInt(id)),
				addFeatures: payload.add_features,
				removeFeatures: payload.remove_features,
				adminUserId,
				auditLogReason: payload.audit_log_reason,
			});
		},
		summary: {
			auditService,
			adminUserId,
			action: 'bulk_update_guild_features',
			auditLogReason: payload.audit_log_reason,
			metadata: [
				['guild_count', total.toString()],
				['add_features', payload.add_features.join(',')],
				['remove_features', payload.remove_features.join(',')],
			],
		},
	});
};

export default handler;
