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

import {createEmojiID, createStickerID, type GuildID, type UserID} from '~/BrandedTypes';
import {mapGuildEmojiToResponse, mapGuildStickerToResponse} from '~/guild/GuildModel';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import {ExpressionAssetPurger} from '~/guild/services/content/ExpressionAssetPurger';
import type {IAssetDeletionQueue} from '~/infrastructure/IAssetDeletionQueue';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {PurgeGuildAssetError, PurgeGuildAssetResult, PurgeGuildAssetsResponse} from '../models/AdminTypes';
import type {AdminAuditService} from './AdminAuditService';

interface AdminAssetPurgeServiceDeps {
	guildRepository: IGuildRepository;
	gatewayService: IGatewayService;
	assetDeletionQueue: IAssetDeletionQueue;
	auditService: AdminAuditService;
}

export class AdminAssetPurgeService {
	private readonly assetPurger: ExpressionAssetPurger;

	constructor(private readonly deps: AdminAssetPurgeServiceDeps) {
		this.assetPurger = new ExpressionAssetPurger(deps.assetDeletionQueue);
	}

	async purgeGuildAssets(args: {
		ids: Array<string>;
		adminUserId: UserID;
		auditLogReason: string | null;
	}): Promise<PurgeGuildAssetsResponse> {
		const {ids, adminUserId, auditLogReason} = args;
		const processed: Array<PurgeGuildAssetResult> = [];
		const errors: Array<PurgeGuildAssetError> = [];
		const seen = new Set<string>();

		for (const rawId of ids) {
			const trimmedId = rawId.trim();
			if (trimmedId === '' || seen.has(trimmedId)) {
				continue;
			}
			seen.add(trimmedId);

			let numericId: bigint;
			try {
				numericId = BigInt(trimmedId);
			} catch {
				errors.push({id: trimmedId, error: 'Invalid numeric ID'});
				continue;
			}

			try {
				const result = await this.processAssetId(numericId, trimmedId, adminUserId, auditLogReason);
				processed.push(result);
			} catch (error) {
				const message = error instanceof Error && error.message !== '' ? error.message : 'Failed to purge asset';
				errors.push({id: trimmedId, error: message});
			}
		}

		return {processed, errors};
	}

	private async processAssetId(
		numericId: bigint,
		idString: string,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<PurgeGuildAssetResult> {
		const {guildRepository} = this.deps;

		const emojiId = createEmojiID(numericId);
		const emoji = await guildRepository.getEmojiById(emojiId);
		if (emoji) {
			await guildRepository.deleteEmoji(emoji.guildId, emojiId);
			await this.dispatchGuildEmojisUpdate(emoji.guildId);
			await this.assetPurger.purgeEmoji(idString);
			await this.createAuditLog({
				adminUserId,
				targetType: 'guild_emoji',
				targetId: numericId,
				action: 'purge_guild_emoji_asset',
				auditLogReason,
				metadata: new Map([
					['asset_type', 'emoji'],
					['guild_id', emoji.guildId.toString()],
				]),
			});

			return {
				id: idString,
				asset_type: 'emoji',
				found_in_db: true,
				guild_id: emoji.guildId.toString(),
			};
		}

		const stickerId = createStickerID(numericId);
		const sticker = await guildRepository.getStickerById(stickerId);
		if (sticker) {
			await guildRepository.deleteSticker(sticker.guildId, stickerId);
			await this.dispatchGuildStickersUpdate(sticker.guildId);
			await this.assetPurger.purgeSticker(idString);
			await this.createAuditLog({
				adminUserId,
				targetType: 'guild_sticker',
				targetId: numericId,
				action: 'purge_guild_sticker_asset',
				auditLogReason,
				metadata: new Map([
					['asset_type', 'sticker'],
					['guild_id', sticker.guildId.toString()],
				]),
			});

			return {
				id: idString,
				asset_type: 'sticker',
				found_in_db: true,
				guild_id: sticker.guildId.toString(),
			};
		}

		await this.assetPurger.purgeEmoji(idString);
		await this.assetPurger.purgeSticker(idString);
		await this.createAuditLog({
			adminUserId,
			targetType: 'asset',
			targetId: numericId,
			action: 'purge_asset',
			auditLogReason,
			metadata: new Map([['asset_type', 'unknown']]),
		});

		return {
			id: idString,
			asset_type: 'unknown',
			found_in_db: false,
			guild_id: null,
		};
	}

	private async dispatchGuildEmojisUpdate(guildId: GuildID): Promise<void> {
		const {guildRepository, gatewayService} = this.deps;
		const emojis = await guildRepository.listEmojis(guildId);
		await gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_EMOJIS_UPDATE',
			data: {emojis: emojis.map(mapGuildEmojiToResponse)},
		});
	}

	private async dispatchGuildStickersUpdate(guildId: GuildID): Promise<void> {
		const {guildRepository, gatewayService} = this.deps;
		const stickers = await guildRepository.listStickers(guildId);
		await gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_STICKERS_UPDATE',
			data: {stickers: stickers.map(mapGuildStickerToResponse)},
		});
	}

	private async createAuditLog(params: {
		adminUserId: UserID;
		targetType: string;
		targetId: bigint;
		action: string;
		auditLogReason: string | null;
		metadata: Map<string, string>;
	}): Promise<void> {
		const {auditService} = this.deps;
		await auditService.createAuditLog({
			adminUserId: params.adminUserId,
			targetType: params.targetType,
			targetId: params.targetId,
			action: params.action,
			auditLogReason: params.auditLogReason,
			metadata: params.metadata,
		});
	}
}
