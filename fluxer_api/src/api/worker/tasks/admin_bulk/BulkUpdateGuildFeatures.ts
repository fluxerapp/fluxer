// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {AdminGuildUpdatePropagator} from '@app/api/admin/services/guild/AdminGuildUpdatePropagator';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {getGuildDiscoveryRepository} from '@app/api/middleware/ServiceSingletons';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {JobCancelledError} from '@pkgs/worker/src/contracts/WorkerTask';

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
	const deps = getWorkerDependencies();
	const auditService = new AdminAuditService(deps.adminRepository, deps.snowflakeService);
	const propagator = new AdminGuildUpdatePropagator({
		gatewayService: deps.gatewayService,
		discoveryRepository: getGuildDiscoveryRepository(),
	});
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const guildIds = payload.guild_ids.map((id) => BigInt(id));
	const total = guildIds.length;
	const successful: Array<string> = [];
	const failed: Array<{
		id: string;
		error: string;
	}> = [];
	await helpers.setContextLink(`/guilds?ids=${guildIds.slice(0, 50).join(',')}`);
	await helpers.reportProgress(0, total, `Updating features on ${total} guilds`);
	for (let i = 0; i < guildIds.length; i++) {
		if (await helpers.shouldCancel()) throw new JobCancelledError();
		const guildIdBigInt = guildIds[i]!;
		const guildId = createGuildID(guildIdBigInt);
		try {
			const guild = await deps.guildRepository.findUnique(guildId);
			if (!guild) throw new Error('guild_not_found');
			const newFeatures = new Set(guild.features);
			for (const f of payload.add_features) newFeatures.add(f);
			for (const f of payload.remove_features) newFeatures.delete(f);
			const updatedGuild = await deps.guildRepository.upsertPartial(guildId, {features: newFeatures}, guild.toRow());
			await propagator.dispatchGuildUpdate(guildId, updatedGuild, {
				adminUserId,
				reconcileDiscoveryFeature: true,
			});
			await auditService.createAuditLog({
				adminUserId,
				targetType: 'guild',
				targetId: BigInt(guildId),
				action: 'update_features',
				auditLogReason: null,
				metadata: new Map([
					['add_features', payload.add_features.join(',')],
					['remove_features', payload.remove_features.join(',')],
					['new_features', Array.from(newFeatures).join(',')],
				]),
			});
			successful.push(guildId.toString());
		} catch (err) {
			failed.push({id: guildIdBigInt.toString(), error: err instanceof Error ? err.message : String(err)});
		}
		if ((i + 1) % 25 === 0) {
			await helpers.reportProgress(i + 1, total, null);
		}
	}
	await auditService.createAuditLog({
		adminUserId,
		targetType: 'guild',
		targetId: BigInt(0),
		action: 'bulk_update_guild_features',
		auditLogReason: payload.audit_log_reason,
		metadata: new Map([
			['guild_count', total.toString()],
			['add_features', payload.add_features.join(',')],
			['remove_features', payload.remove_features.join(',')],
			['successful', successful.length.toString()],
			['failed', failed.length.toString()],
		]),
	});
	await helpers.reportProgress(total, total, `+${successful.length} ok, ${failed.length} failed`);
	helpers.logger.info({successful: successful.length, failed: failed.length}, 'bulkUpdateGuildFeatures complete');
};

export default handler;
