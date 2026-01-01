/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createGuildID, createUserID, type GuildID, type UserID} from '~/BrandedTypes';
import {UnknownGuildError} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {EntityAssetService, PreparedAssetUpload} from '~/infrastructure/EntityAssetService';
import type {Guild} from '~/Models';
import type {
	ClearGuildFieldsRequest,
	TransferGuildOwnershipRequest,
	UpdateGuildNameRequest,
	UpdateGuildSettingsRequest,
} from '../../AdminModel';
import {mapGuildToAdminResponse} from '../../AdminModel';
import type {AdminAuditService} from '../AdminAuditService';
import type {AdminGuildUpdatePropagator} from './AdminGuildUpdatePropagator';

interface AdminGuildUpdateServiceDeps {
	guildRepository: IGuildRepository;
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

		const guildRow = guild.toRow();
		const updatedGuild = await guildRepository.upsert({
			...guildRow,
			features: newFeatures,
		});

		await updatePropagator.dispatchGuildUpdate(guildId, updatedGuild);

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

		const guildRow = guild.toRow();
		const updates: Partial<typeof guildRow> = {};
		const preparedAssets: Array<PreparedAssetUpload> = [];

		for (const field of data.fields) {
			if (field === 'icon') {
				const prepared = await entityAssetService.prepareAssetUpload({
					assetType: 'icon',
					entityType: 'guild',
					entityId: guildId,
					previousHash: guild.iconHash,
					base64Image: null,
					errorPath: 'icon',
				});
				preparedAssets.push(prepared);
				updates.icon_hash = prepared.newHash;
			} else if (field === 'banner') {
				const prepared = await entityAssetService.prepareAssetUpload({
					assetType: 'banner',
					entityType: 'guild',
					entityId: guildId,
					previousHash: guild.bannerHash,
					base64Image: null,
					errorPath: 'banner',
				});
				preparedAssets.push(prepared);
				updates.banner_hash = prepared.newHash;
			} else if (field === 'splash') {
				const prepared = await entityAssetService.prepareAssetUpload({
					assetType: 'splash',
					entityType: 'guild',
					entityId: guildId,
					previousHash: guild.splashHash,
					base64Image: null,
					errorPath: 'splash',
				});
				preparedAssets.push(prepared);
				updates.splash_hash = prepared.newHash;
			} else if (field === 'embed_splash') {
				const prepared = await entityAssetService.prepareAssetUpload({
					assetType: 'embed_splash',
					entityType: 'guild',
					entityId: guildId,
					previousHash: guild.embedSplashHash,
					base64Image: null,
					errorPath: 'embed_splash',
				});
				preparedAssets.push(prepared);
				updates.embed_splash_hash = prepared.newHash;
			}
		}

		let updatedGuild: Guild;
		try {
			updatedGuild = await guildRepository.upsert({
				...guildRow,
				...updates,
			});
		} catch (error) {
			await Promise.allSettled(preparedAssets.map((p) => entityAssetService.rollbackAssetUpload(p)));
			throw error;
		}

		await Promise.allSettled(preparedAssets.map((p) => entityAssetService.commitAssetChange({prepared: p})));

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
		const guildRow = guild.toRow();
		const updatedGuild = await guildRepository.upsert({
			...guildRow,
			name: data.name,
		});

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

		const guildRow = guild.toRow();
		const updates: Partial<typeof guildRow> = {};
		const metadata = new Map<string, string>();

		if (data.verification_level !== undefined) {
			updates.verification_level = data.verification_level;
			metadata.set('verification_level', data.verification_level.toString());
		}
		if (data.mfa_level !== undefined) {
			updates.mfa_level = data.mfa_level;
			metadata.set('mfa_level', data.mfa_level.toString());
		}
		if (data.nsfw_level !== undefined) {
			updates.nsfw_level = data.nsfw_level;
			metadata.set('nsfw_level', data.nsfw_level.toString());
		}
		if (data.explicit_content_filter !== undefined) {
			updates.explicit_content_filter = data.explicit_content_filter;
			metadata.set('explicit_content_filter', data.explicit_content_filter.toString());
		}
		if (data.default_message_notifications !== undefined) {
			updates.default_message_notifications = data.default_message_notifications;
			metadata.set('default_message_notifications', data.default_message_notifications.toString());
		}
		if (data.disabled_operations !== undefined) {
			updates.disabled_operations = data.disabled_operations;
			metadata.set('disabled_operations', data.disabled_operations.toString());
		}

		const updatedGuild = await guildRepository.upsert({
			...guildRow,
			...updates,
		});

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
		const guildRow = guild.toRow();
		const updatedGuild = await guildRepository.upsert({
			...guildRow,
			owner_id: newOwnerId,
		});

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
