// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_GUILD_EXPRESSION_SLOTS_UNLIMITED, MAX_GUILD_STICKERS} from '@fluxer/constants/src/LimitConstants';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {MaxGuildStickersStaticError} from '@fluxer/errors/src/domains/guild/MaxGuildStickersStaticError';
import {UnknownGuildStickerError} from '@fluxer/errors/src/domains/guild/UnknownGuildStickerError';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {getErrorMessageUnsafe} from '@fluxer/errors/src/i18n/ErrorI18n';
import {resolveLimit} from '@fluxer/limits/src/LimitResolver';
import type {
	GuildStickerResponse,
	GuildStickerWithUserResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {createStickerID, type GuildID, type StickerID, type UserID} from '../../../BrandedTypes';
import {getContentMessage} from '../../../content_i18n/ContentI18n';
import type {AvatarService} from '../../../infrastructure/AvatarService';
import {contentModerationService} from '../../../infrastructure/ContentModerationService';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../../infrastructure/ISnowflakeService';
import type {UserCacheService} from '../../../infrastructure/UserCacheService';
import type {LimitConfigService} from '../../../limits/LimitConfigService';
import {createLimitMatchContext} from '../../../limits/LimitMatchContextBuilder';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import type {GuildSticker} from '../../../models/GuildSticker';
import type {User} from '../../../models/User';
import {getCachedUserPartialResponse} from '../../../user/UserCacheHelpers';
import {mapGuildStickersWithUsersToResponse, mapGuildStickerToResponse} from '../../GuildModel';
import type {IGuildRepositoryAggregate} from '../../repositories/IGuildRepositoryAggregate';
import type {ContentHelpers} from './ContentHelpers';
import type {ExpressionAssetPurger} from './ExpressionAssetPurger';

export class StickerService {
	constructor(
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly userCacheService: UserCacheService,
		private readonly gatewayService: IGatewayService,
		private readonly avatarService: AvatarService,
		private readonly snowflakeService: ISnowflakeService,
		private readonly contentHelpers: ContentHelpers,
		private readonly assetPurger: ExpressionAssetPurger,
		private readonly limitConfigService: LimitConfigService,
	) {}

	private resolveGuildLimit(key: LimitKey, fallback: number, guildFeatures: Iterable<string> | null): number {
		const ctx = createLimitMatchContext({user: null, guildFeatures});
		const resolved = resolveLimit(this.limitConfigService.getConfigSnapshot(), ctx, key, {
			evaluationContext: 'guild',
		});
		if (!Number.isFinite(resolved) || resolved < 0) {
			return fallback;
		}
		return Math.floor(resolved);
	}

	private getLocalizedBulkCreateError(error: unknown, locale: string | null | undefined): string {
		if (error instanceof FluxerError) {
			return getErrorMessageUnsafe(error.code, locale, error.messageVariables, error.message);
		}
		return getContentMessage('guild.bulk_create.unknown_error', locale);
	}

	async getStickers(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildStickerWithUserResponse>> {
		const {userId, guildId, requestCache} = params;
		await this.contentHelpers.getGuildData({userId, guildId});
		const stickers = await this.guildRepository.listStickers(guildId);
		return await mapGuildStickersWithUsersToResponse(stickers, this.userCacheService, requestCache);
	}

	async getStickerUser(params: {
		userId: UserID;
		guildId: GuildID;
		stickerId: StickerID;
		requestCache: RequestCache;
	}): Promise<UserPartialResponse> {
		const {userId, guildId, stickerId, requestCache} = params;
		await this.contentHelpers.getGuildData({userId, guildId});
		const sticker = await this.guildRepository.getSticker(stickerId, guildId);
		if (!sticker) throw new UnknownGuildStickerError();
		const userPartial = await getCachedUserPartialResponse({
			userId: sticker.creatorId,
			userCacheService: this.userCacheService,
			requestCache,
		});
		return userPartial;
	}

	async createSticker(
		params: {
			user: User;
			guildId: GuildID;
			name: string;
			description?: string | null;
			tags: Array<string>;
			image: string;
		},
		auditLogReason?: string | null,
	): Promise<GuildStickerResponse> {
		const {user, guildId, name, description, tags, image} = params;
		const stickerModCtx = {
			userId: user.id,
			guildId,
			channelId: null,
			messageId: null,
			surface: 'sticker' as const,
		};
		contentModerationService.scanText(name, stickerModCtx);
		contentModerationService.scanText(description, stickerModCtx);
		contentModerationService.scanText(tags.join(' '), stickerModCtx);
		const guildData = await this.contentHelpers.getGuildData({userId: user.id, guildId});
		await this.contentHelpers.checkCreateExpressionsPermission({userId: user.id, guildId});
		const allStickers = await this.guildRepository.listStickers(guildId);
		const stickerCount = allStickers.length;
		const guildFeatures = guildData.features;
		const hasUnlimitedStickers = guildFeatures.includes(GuildFeatures.UNLIMITED_STICKERS);
		const maxStickers = hasUnlimitedStickers
			? MAX_GUILD_EXPRESSION_SLOTS_UNLIMITED
			: this.resolveGuildLimit('max_guild_stickers', MAX_GUILD_STICKERS, guildFeatures);
		if (stickerCount >= maxStickers) {
			throw new MaxGuildStickersStaticError(maxStickers);
		}
		const {animated, imageBuffer} = await this.avatarService.processSticker({
			errorPath: 'image',
			base64Image: image,
			guildFeatures,
		});
		const stickerId = createStickerID(await this.snowflakeService.generate());
		await this.avatarService.uploadSticker({prefix: 'stickers', stickerId, imageBuffer});
		const sticker = await this.guildRepository.upsertSticker({
			guild_id: guildId,
			sticker_id: stickerId,
			name,
			description: description ?? null,
			animated,
			tags,
			creator_id: user.id,
			version: 1,
		});
		const updatedStickers = [...allStickers, sticker];
		await this.dispatchGuildStickersUpdate({guildId, stickers: updatedStickers});
		await this.contentHelpers.recordAuditLog({
			guildId,
			userId: user.id,
			action: AuditLogActionType.STICKER_CREATE,
			targetId: sticker.id,
			auditLogReason: auditLogReason ?? null,
			changes: this.contentHelpers.guildAuditLogService.computeChanges(
				null,
				this.contentHelpers.serializeStickerForAudit(sticker),
			),
		});
		return mapGuildStickerToResponse(sticker);
	}

	async cloneSticker(
		params: {
			user: User;
			guildId: GuildID;
			sourceStickerId: StickerID;
		},
		auditLogReason?: string | null,
	): Promise<GuildStickerResponse> {
		const {user, guildId, sourceStickerId} = params;
		const sourceSticker = await this.guildRepository.getStickerById(sourceStickerId);
		if (!sourceSticker) throw new UnknownGuildStickerError();
		const sourceGuild = await this.guildRepository.findUnique(sourceSticker.guildId);
		if (!sourceGuild || sourceGuild.features.has(GuildFeatures.CLONE_STICKER_DISABLED)) {
			throw new MissingAccessError();
		}
		const guildData = await this.contentHelpers.getGuildData({userId: user.id, guildId});
		await this.contentHelpers.checkCreateExpressionsPermission({userId: user.id, guildId});
		const allStickers = await this.guildRepository.listStickers(guildId);
		const stickerCount = allStickers.length;
		const guildFeatures = guildData.features;
		const hasUnlimitedStickers = guildFeatures.includes(GuildFeatures.UNLIMITED_STICKERS);
		const maxStickers = hasUnlimitedStickers
			? MAX_GUILD_EXPRESSION_SLOTS_UNLIMITED
			: this.resolveGuildLimit('max_guild_stickers', MAX_GUILD_STICKERS, guildFeatures);
		if (stickerCount >= maxStickers) {
			throw new MaxGuildStickersStaticError(maxStickers);
		}
		const stickerId = createStickerID(await this.snowflakeService.generate());
		await this.avatarService.cloneStickerImage({sourceStickerId, stickerId});
		const sticker = await this.guildRepository.upsertSticker({
			guild_id: guildId,
			sticker_id: stickerId,
			name: sourceSticker.name,
			description: sourceSticker.description,
			animated: sourceSticker.animated,
			tags: sourceSticker.tags,
			creator_id: user.id,
			version: 1,
		});
		const updatedStickers = [...allStickers, sticker];
		await this.dispatchGuildStickersUpdate({guildId, stickers: updatedStickers});
		await this.contentHelpers.recordAuditLog({
			guildId,
			userId: user.id,
			action: AuditLogActionType.STICKER_CREATE,
			targetId: sticker.id,
			auditLogReason: auditLogReason ?? null,
			changes: this.contentHelpers.guildAuditLogService.computeChanges(
				null,
				this.contentHelpers.serializeStickerForAudit(sticker),
			),
		});
		return mapGuildStickerToResponse(sticker);
	}

	async bulkCreateStickers(
		params: {
			user: User;
			guildId: GuildID;
			stickers: Array<{
				name: string;
				description?: string | null;
				tags: Array<string>;
				image: string;
			}>;
		},
		auditLogReason?: string | null,
	): Promise<{
		success: Array<GuildStickerResponse>;
		failed: Array<{
			name: string;
			error: string;
		}>;
	}> {
		const {user, guildId, stickers} = params;
		const guildData = await this.contentHelpers.getGuildData({userId: user.id, guildId});
		await this.contentHelpers.checkCreateExpressionsPermission({userId: user.id, guildId});
		const allStickers = await this.guildRepository.listStickers(guildId);
		const guildFeatures = guildData.features;
		const hasUnlimitedStickers = guildFeatures.includes(GuildFeatures.UNLIMITED_STICKERS);
		const maxStickers = hasUnlimitedStickers
			? MAX_GUILD_EXPRESSION_SLOTS_UNLIMITED
			: this.resolveGuildLimit('max_guild_stickers', MAX_GUILD_STICKERS, guildFeatures);
		let stickerCount = allStickers.length;
		const success: Array<GuildStickerResponse> = [];
		const failed: Array<{
			name: string;
			error: string;
		}> = [];
		const newStickers: Array<GuildSticker> = [];
		for (const stickerData of stickers) {
			try {
				if (stickerCount >= maxStickers) {
					failed.push({
						name: stickerData.name,
						error: getContentMessage('guild.bulk_create.sticker_limit', user.locale, {
							limit: Math.floor(maxStickers),
						}),
					});
					continue;
				}
				const bulkStickerModCtx = {
					userId: user.id,
					guildId,
					channelId: null,
					messageId: null,
					surface: 'sticker' as const,
				};
				contentModerationService.scanText(stickerData.name, bulkStickerModCtx);
				contentModerationService.scanText(stickerData.description, bulkStickerModCtx);
				contentModerationService.scanText(stickerData.tags.join(' '), bulkStickerModCtx);
				const {animated, imageBuffer} = await this.avatarService.processSticker({
					errorPath: `stickers[${success.length + failed.length}].image`,
					base64Image: stickerData.image,
					guildFeatures,
				});
				const stickerId = createStickerID(await this.snowflakeService.generate());
				await this.avatarService.uploadSticker({prefix: 'stickers', stickerId, imageBuffer});
				const sticker = await this.guildRepository.upsertSticker({
					guild_id: guildId,
					sticker_id: stickerId,
					name: stickerData.name,
					description: stickerData.description ?? null,
					tags: stickerData.tags,
					animated,
					creator_id: user.id,
					version: 1,
				});
				stickerCount++;
				newStickers.push(sticker);
				success.push(mapGuildStickerToResponse(sticker));
			} catch (error) {
				failed.push({name: stickerData.name, error: this.getLocalizedBulkCreateError(error, user.locale)});
			}
		}
		if (newStickers.length > 0) {
			const updatedStickers = [...allStickers, ...newStickers];
			await this.dispatchGuildStickersUpdate({guildId, stickers: updatedStickers});
			await Promise.all(
				newStickers.map((sticker) =>
					this.contentHelpers.recordAuditLog({
						guildId,
						userId: user.id,
						action: AuditLogActionType.STICKER_CREATE,
						targetId: sticker.id,
						auditLogReason: auditLogReason ?? null,
						changes: this.contentHelpers.guildAuditLogService.computeChanges(
							null,
							this.contentHelpers.serializeStickerForAudit(sticker),
						),
					}),
				),
			);
		}
		return {success, failed};
	}

	async updateSticker(
		params: {
			userId: UserID;
			guildId: GuildID;
			stickerId: StickerID;
			name: string;
			description?: string | null;
			tags: Array<string>;
		},
		auditLogReason?: string | null,
	): Promise<GuildStickerResponse> {
		const {userId, guildId, stickerId, name, description, tags} = params;
		const stickerModCtx = {
			userId,
			guildId,
			channelId: null,
			messageId: null,
			surface: 'sticker' as const,
		};
		contentModerationService.scanText(name, stickerModCtx);
		contentModerationService.scanText(description, stickerModCtx);
		contentModerationService.scanText(tags.join(' '), stickerModCtx);
		const allStickers = await this.guildRepository.listStickers(guildId);
		const sticker = allStickers.find((e) => e.id === stickerId);
		if (!sticker) throw new UnknownGuildStickerError();
		await this.contentHelpers.checkModifyExpressionPermission({userId, guildId, creatorId: sticker.creatorId});
		const previousSnapshot = this.contentHelpers.serializeStickerForAudit(sticker);
		const updatedSticker = await this.guildRepository.upsertSticker({
			...sticker.toRow(),
			name,
			description: description ?? null,
			tags,
		});
		const updatedStickers = allStickers.map((e) => (e.id === stickerId ? updatedSticker : e));
		await this.dispatchGuildStickersUpdate({guildId, stickers: updatedStickers});
		await this.contentHelpers.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.STICKER_UPDATE,
			targetId: stickerId,
			auditLogReason: auditLogReason ?? null,
			changes: this.contentHelpers.guildAuditLogService.computeChanges(
				previousSnapshot,
				this.contentHelpers.serializeStickerForAudit(updatedSticker),
			),
		});
		return mapGuildStickerToResponse(updatedSticker);
	}

	async deleteSticker(
		params: {
			userId: UserID;
			guildId: GuildID;
			stickerId: StickerID;
			purge?: boolean;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, stickerId, purge = false} = params;
		const guildData = await this.contentHelpers.getGuildData({userId, guildId});
		if (purge && !guildData.features.includes(GuildFeatures.EXPRESSION_PURGE_ALLOWED)) {
			throw new MissingAccessError();
		}
		const allStickers = await this.guildRepository.listStickers(guildId);
		const sticker = allStickers.find((e) => e.id === stickerId);
		if (!sticker) throw new UnknownGuildStickerError();
		await this.contentHelpers.checkModifyExpressionPermission({userId, guildId, creatorId: sticker.creatorId});
		const previousSnapshot = this.contentHelpers.serializeStickerForAudit(sticker);
		await this.guildRepository.deleteSticker(guildId, stickerId);
		const updatedStickers = allStickers.filter((e) => e.id !== stickerId);
		await this.dispatchGuildStickersUpdate({guildId, stickers: updatedStickers});
		if (purge) {
			await this.assetPurger.purgeSticker(sticker.id.toString());
		}
		await this.contentHelpers.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.STICKER_DELETE,
			targetId: stickerId,
			auditLogReason: auditLogReason ?? null,
			changes: this.contentHelpers.guildAuditLogService.computeChanges(previousSnapshot, null),
		});
	}

	private async dispatchGuildStickersUpdate(params: {guildId: GuildID; stickers: Array<GuildSticker>}): Promise<void> {
		const {guildId, stickers} = params;
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_STICKERS_UPDATE',
			data: {stickers: stickers.map(mapGuildStickerToResponse)},
		});
	}
}
