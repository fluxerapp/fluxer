// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildNSFWLevelValue} from '@fluxer/constants/src/GuildConstants';
import type {
	PurgeGuildAssetError,
	PurgeGuildAssetResult,
	PurgeGuildAssetsResponse,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {createEmojiID, createStickerID, type GuildID, type UserID} from '../../BrandedTypes';
import {mapGuildEmojiToResponse, mapGuildStickerToResponse} from '../../guild/GuildModel';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import {ExpressionAssetPurger} from '../../guild/services/content/ExpressionAssetPurger';
import type {IAssetDeletionQueue} from '../../infrastructure/IAssetDeletionQueue';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {AdminAuditService} from './AdminAuditService';

interface AdminAssetPurgeServiceDeps {
	guildRepository: IGuildRepositoryAggregate;
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
		guildId: GuildID;
		ids: Array<string>;
		adminUserId: UserID;
		auditLogReason: string | null;
	}): Promise<PurgeGuildAssetsResponse> {
		const {guildId, ids, adminUserId, auditLogReason} = args;
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
				const result = await this.processAssetId(guildId, numericId, trimmedId, adminUserId, auditLogReason);
				if (result === null) {
					errors.push({id: trimmedId, error: 'Asset belongs to another guild'});
					continue;
				}
				processed.push(result);
			} catch (error) {
				const message = error instanceof Error && error.message !== '' ? error.message : 'Failed to purge asset';
				errors.push({id: trimmedId, error: message});
			}
		}
		return {processed, errors};
	}

	private async processAssetId(
		guildId: GuildID,
		numericId: bigint,
		idString: string,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<PurgeGuildAssetResult | null> {
		const {guildRepository} = this.deps;
		const emojiId = createEmojiID(numericId);
		const emoji = await guildRepository.getEmojiById(emojiId);
		if (emoji) {
			if (emoji.guildId !== guildId) {
				return null;
			}
			await guildRepository.deleteEmoji(emoji.guildId, emojiId);
			await this.dispatchGuildEmojisUpdate(emoji.guildId);
			await this.assetPurger.purgeEmoji(idString);
			const guildNsfwLevel = await this.getGuildNsfwLevel(emoji.guildId);
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
				guild_nsfw_level: guildNsfwLevel,
			};
		}
		const stickerId = createStickerID(numericId);
		const sticker = await guildRepository.getStickerById(stickerId);
		if (sticker) {
			if (sticker.guildId !== guildId) {
				return null;
			}
			await guildRepository.deleteSticker(sticker.guildId, stickerId);
			await this.dispatchGuildStickersUpdate(sticker.guildId);
			await this.assetPurger.purgeSticker(idString);
			const guildNsfwLevel = await this.getGuildNsfwLevel(sticker.guildId);
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
				guild_nsfw_level: guildNsfwLevel,
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
			guild_nsfw_level: null,
		};
	}

	private async getGuildNsfwLevel(guildId: GuildID): Promise<GuildNSFWLevelValue | null> {
		const {guildRepository} = this.deps;
		const guild = await guildRepository.findUnique(guildId);
		return guild?.nsfwLevel ?? null;
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
