// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapGuildToAdminResponse} from '@app/api/admin/models/GuildTypes';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {AdminGuildUpdatePropagator} from '@app/api/admin/services/guild/AdminGuildUpdatePropagator';
import {createGuildID, createUserID, type GuildID, type UserID} from '@app/api/BrandedTypes';
import type {GuildRow} from '@app/api/database/types/GuildTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {EntityAssetService, PreparedAssetUpload} from '@app/api/infrastructure/EntityAssetService';
import {Logger} from '@app/api/Logger';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {
	ClearGuildFieldsRequest,
	TransferGuildOwnershipRequest,
	UpdateGuildNameRequest,
	UpdateGuildSettingsRequest,
} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';

interface AdminGuildUpdateServiceDeps {
	guildRepository: IGuildRepositoryAggregate;
	entityAssetService: EntityAssetService;
	auditService: AdminAuditService;
	updatePropagator: AdminGuildUpdatePropagator;
}

export class AdminGuildUpdateService {
	constructor(private readonly deps: AdminGuildUpdateServiceDeps) {}

	async updateGuildFeatures({
		guildId,
		addFeatures,
		removeFeatures,
		adminUserId,
		auditLogReason,
	}: {
		guildId: GuildID;
		addFeatures: Array<string>;
		removeFeatures: Array<string>;
		adminUserId: UserID;
		auditLogReason: string | null;
	}) {
		const {guildRepository, auditService, updatePropagator} = this.deps;
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		const newFeatures = new Set(guild.features);
		for (const feature of addFeatures) {
			newFeatures.add(feature);
		}
		for (const feature of removeFeatures) {
			newFeatures.delete(feature);
		}
		const updatedGuild = await guildRepository.upsertPartial(guildId, {features: newFeatures}, guild.toRow());
		await updatePropagator.dispatchGuildUpdate(guildId, updatedGuild, {
			adminUserId,
			reconcileDiscoveryFeature: true,
		});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild',
			targetId: BigInt(guildId),
			action: 'update_features',
			auditLogReason,
			metadata: new Map([
				['add_features', addFeatures.join(',')],
				['remove_features', removeFeatures.join(',')],
				['new_features', Array.from(newFeatures).join(',')],
			]),
		});
		return {
			guild: mapGuildToAdminResponse(updatedGuild),
		};
	}

	async clearGuildFields(data: ClearGuildFieldsRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {guildRepository, entityAssetService, auditService, updatePropagator} = this.deps;
		const guildId = createGuildID(data.guild_id);
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		const patch: Partial<GuildRow> = {};
		const preparedAssets: Array<PreparedAssetUpload> = [];
		const imageFields = {
			icon: {previousHash: guild.iconHash, patchField: 'icon_hash'},
			banner: {previousHash: guild.bannerHash, patchField: 'banner_hash'},
			splash: {previousHash: guild.splashHash, patchField: 'splash_hash'},
			embed_splash: {previousHash: guild.embedSplashHash, patchField: 'embed_splash_hash'},
		} as const;
		for (const field of data.fields) {
			const {previousHash, patchField} = imageFields[field];
			const prepared = await entityAssetService.prepareAssetUpload({
				assetType: field,
				entityType: 'guild',
				entityId: guildId,
				previousHash,
				base64Image: null,
				errorPath: field,
			});
			preparedAssets.push(prepared);
			patch[patchField] = prepared.newHash;
		}
		const updatedGuild =
			Object.keys(patch).length === 0 ? guild : await guildRepository.upsertPartial(guildId, patch, guild.toRow());
		const commitResults = await Promise.allSettled(
			preparedAssets.map((prepared) => entityAssetService.commitAssetChange(prepared)),
		);
		for (const [index, result] of commitResults.entries()) {
			if (result.status === 'fulfilled') continue;
			Logger.error(
				{error: result.reason, guildId, previousS3Key: preparedAssets[index].previousS3Key},
				'Failed to queue cleared guild asset for deletion after successful DB update',
			);
		}
		await updatePropagator.dispatchGuildUpdate(guildId, updatedGuild);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild',
			targetId: BigInt(guildId),
			action: 'clear_fields',
			auditLogReason,
			metadata: new Map([['fields', data.fields.join(',')]]),
		});
	}

	async updateGuildName(data: UpdateGuildNameRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {guildRepository, auditService, updatePropagator} = this.deps;
		const guildId = createGuildID(data.guild_id);
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		const oldName = guild.name;
		const updatedGuild = await guildRepository.upsertPartial(guildId, {name: data.name}, guild.toRow());
		await updatePropagator.dispatchGuildUpdate(guildId, updatedGuild);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild',
			targetId: BigInt(guildId),
			action: 'update_name',
			auditLogReason,
			metadata: new Map([
				['old_name', oldName],
				['new_name', data.name],
			]),
		});
		return {
			guild: mapGuildToAdminResponse(updatedGuild),
		};
	}

	async updateGuildSettings(data: UpdateGuildSettingsRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {guildRepository, auditService, updatePropagator} = this.deps;
		const guildId = createGuildID(data.guild_id);
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		const patch: Partial<GuildRow> = {};
		const metadata = new Map<string, string>();
		if (data.verification_level !== undefined) {
			patch.verification_level = data.verification_level;
			metadata.set('verification_level', data.verification_level.toString());
		}
		if (data.mfa_level !== undefined) {
			patch.mfa_level = data.mfa_level;
			metadata.set('mfa_level', data.mfa_level.toString());
		}
		if (data.nsfw_level !== undefined) {
			patch.nsfw_level = data.nsfw_level;
			metadata.set('nsfw_level', data.nsfw_level.toString());
		}
		if (data.nsfw !== undefined) {
			patch.nsfw = data.nsfw;
			metadata.set('nsfw', data.nsfw.toString());
		}
		if (data.content_warning_level !== undefined) {
			patch.content_warning_level = data.content_warning_level;
			metadata.set('content_warning_level', data.content_warning_level.toString());
		}
		if (data.content_warning_text !== undefined) {
			patch.content_warning_text = data.content_warning_text;
			metadata.set('content_warning_text', data.content_warning_text ?? '');
		}
		if (data.explicit_content_filter !== undefined) {
			patch.explicit_content_filter = data.explicit_content_filter;
			metadata.set('explicit_content_filter', data.explicit_content_filter.toString());
		}
		if (data.default_message_notifications !== undefined) {
			patch.default_message_notifications = data.default_message_notifications;
			metadata.set('default_message_notifications', data.default_message_notifications.toString());
		}
		if (data.disabled_operations !== undefined) {
			patch.disabled_operations = data.disabled_operations;
			metadata.set('disabled_operations', data.disabled_operations.toString());
		}
		const updatedGuild =
			Object.keys(patch).length === 0 ? guild : await guildRepository.upsertPartial(guildId, patch, guild.toRow());
		await updatePropagator.dispatchGuildUpdate(guildId, updatedGuild);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild',
			targetId: BigInt(guildId),
			action: 'update_settings',
			auditLogReason,
			metadata,
		});
		return {
			guild: mapGuildToAdminResponse(updatedGuild),
		};
	}

	async transferGuildOwnership(
		data: TransferGuildOwnershipRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {guildRepository, auditService, updatePropagator} = this.deps;
		const guildId = createGuildID(data.guild_id);
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		const newOwnerId = createUserID(data.new_owner_id);
		const oldOwnerId = guild.ownerId;
		const updatedGuild = await guildRepository.upsertPartial(
			guildId,
			{owner_id: newOwnerId},
			guild.toRow(),
			oldOwnerId,
		);
		await updatePropagator.dispatchGuildUpdate(guildId, updatedGuild);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild',
			targetId: BigInt(guildId),
			action: 'transfer_ownership',
			auditLogReason,
			metadata: new Map([
				['old_owner_id', oldOwnerId.toString()],
				['new_owner_id', newOwnerId.toString()],
			]),
		});
		return {
			guild: mapGuildToAdminResponse(updatedGuild),
		};
	}
}
