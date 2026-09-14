// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {BulkCancelledError, type BulkProgressHelpers} from '@app/api/admin/services/BulkProgressHelpers';
import type {AdminGuildUpdateService} from '@app/api/admin/services/guild/AdminGuildUpdateService';
import {createGuildID, type UserID} from '@app/api/BrandedTypes';
import type {BulkUpdateGuildFeaturesRequest} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';

interface AdminGuildBulkServiceDeps {
	guildUpdateService: AdminGuildUpdateService;
	auditService: AdminAuditService;
}

export class AdminGuildBulkService {
	constructor(private readonly deps: AdminGuildBulkServiceDeps) {}

	async bulkUpdateGuildFeatures(
		data: BulkUpdateGuildFeaturesRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		helpers?: BulkProgressHelpers,
	) {
		const {guildUpdateService, auditService} = this.deps;
		const successful: Array<string> = [];
		const failed: Array<{
			id: string;
			error: string;
		}> = [];
		const total = data.guild_ids.length;
		await helpers?.reportProgress(0, total, `Updating features on ${total} guilds`);
		let processed = 0;
		for (const guildIdBigInt of data.guild_ids) {
			if (helpers && (await helpers.shouldCancel())) throw new BulkCancelledError();
			try {
				const guildId = createGuildID(guildIdBigInt);
				await guildUpdateService.updateGuildFeatures({
					guildId,
					addFeatures: data.add_features,
					removeFeatures: data.remove_features,
					adminUserId,
					auditLogReason: null,
				});
				successful.push(guildId.toString());
			} catch (error) {
				failed.push({
					id: guildIdBigInt.toString(),
					error: error instanceof Error ? error.message : 'Unknown error',
				});
			}
			processed++;
			if (helpers && processed % 25 === 0) {
				await helpers.reportProgress(processed, total, null);
			}
		}
		await helpers?.reportProgress(total, total, `+${successful.length} ok, ${failed.length} failed`);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild',
			targetId: BigInt(0),
			action: 'bulk_update_guild_features',
			auditLogReason,
			metadata: new Map([
				['guild_count', data.guild_ids.length.toString()],
				['add_features', data.add_features.join(',')],
				['remove_features', data.remove_features.join(',')],
			]),
		});
		return {
			successful,
			failed,
		};
	}
}
