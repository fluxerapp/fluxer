// SPDX-License-Identifier: AGPL-3.0-or-later

import {createEmojiID, type EmojiID, type GuildID, type UserID} from '@app/api/BrandedTypes';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import {mapGuildEmojisWithUsersToResponse, mapGuildEmojiToResponse} from '@app/api/guild/GuildModel';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {ContentHelpers} from '@app/api/guild/services/content/ContentHelpers';
import type {ExpressionAssetPurger} from '@app/api/guild/services/content/ExpressionAssetPurger';
import type {AvatarService} from '@app/api/infrastructure/AvatarService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {GuildEmoji} from '@app/api/models/GuildEmoji';
import type {User} from '@app/api/models/User';
import {getCachedUserPartialResponse} from '@app/api/user/UserCacheHelpers';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_GUILD_EMOJIS, MAX_GUILD_EXPRESSION_SLOTS_UNLIMITED} from '@fluxer/constants/src/LimitConstants';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {MaxGuildEmojisError} from '@fluxer/errors/src/domains/guild/MaxGuildEmojisError';
import {UnknownGuildEmojiError} from '@fluxer/errors/src/domains/guild/UnknownGuildEmojiError';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {getErrorMessageUnsafe} from '@fluxer/errors/src/i18n/ErrorI18n';
import {resolveLimit} from '@fluxer/limits/src/LimitResolver';
import type {GuildEmojiResponse, GuildEmojiWithUserResponse} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export class EmojiService {
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

	async getEmojis(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildEmojiWithUserResponse>> {
		const {userId, guildId, requestCache} = params;
		await this.contentHelpers.getGuildData({userId, guildId});
		const emojis = await this.guildRepository.listEmojis(guildId);
		return await mapGuildEmojisWithUsersToResponse(emojis, this.userCacheService, requestCache);
	}

	async getEmojiUser(params: {
		userId: UserID;
		guildId: GuildID;
		emojiId: EmojiID;
		requestCache: RequestCache;
	}): Promise<UserPartialResponse> {
		const {userId, guildId, emojiId, requestCache} = params;
		await this.contentHelpers.getGuildData({userId, guildId});
		const emoji = await this.guildRepository.getEmoji(emojiId, guildId);
		if (!emoji) throw new UnknownGuildEmojiError();
		const userPartial = await getCachedUserPartialResponse({
			userId: emoji.creatorId,
			userCacheService: this.userCacheService,
			requestCache,
		});
		return userPartial;
	}

	async createEmoji(
		params: {
			user: User;
			guildId: GuildID;
			name: string;
			image: string;
		},
		auditLogReason?: string | null,
	): Promise<GuildEmojiResponse> {
		const {user, guildId, name, image} = params;
		const guildData = await this.contentHelpers.getGuildData({userId: user.id, guildId});
		await this.contentHelpers.checkCreateExpressionsPermission({userId: user.id, guildId});
		const allEmojis = await this.guildRepository.listEmojis(guildId);
		const guildFeatures = guildData.features;
		const hasUnlimitedEmoji = guildFeatures.includes(GuildFeatures.UNLIMITED_EMOJI);
		const maxEmojis = hasUnlimitedEmoji
			? MAX_GUILD_EXPRESSION_SLOTS_UNLIMITED
			: this.resolveGuildLimit('max_guild_emojis', MAX_GUILD_EMOJIS, guildFeatures);
		if (allEmojis.length >= maxEmojis) {
			throw new MaxGuildEmojisError(maxEmojis);
		}
		const {animated, imageBuffer, contentType} = await this.avatarService.processEmoji({
			errorPath: 'image',
			base64Image: image,
			guildFeatures,
		});
		const emojiId = createEmojiID(await this.snowflakeService.generate());
		await this.avatarService.uploadEmoji({
			prefix: 'emojis',
			emojiId,
			imageBuffer,
			contentType,
		});
		const emoji = await this.guildRepository.upsertEmoji({
			guild_id: guildId,
			emoji_id: emojiId,
			name,
			creator_id: user.id,
			animated,
			version: 1,
		});
		const updatedEmojis = [...allEmojis, emoji];
		await this.dispatchGuildEmojisUpdate({guildId, emojis: updatedEmojis});
		await this.contentHelpers.recordAuditLog({
			guildId,
			userId: user.id,
			action: AuditLogActionType.EMOJI_CREATE,
			targetId: emoji.id,
			auditLogReason: auditLogReason ?? null,
			changes: this.contentHelpers.guildAuditLogService.computeChanges(
				null,
				this.contentHelpers.serializeEmojiForAudit(emoji),
			),
		});
		return mapGuildEmojiToResponse(emoji);
	}

	async cloneEmoji(
		params: {
			user: User;
			guildId: GuildID;
			sourceEmojiId: EmojiID;
		},
		auditLogReason?: string | null,
	): Promise<GuildEmojiResponse> {
		const {user, guildId, sourceEmojiId} = params;
		const sourceEmoji = await this.guildRepository.getEmojiById(sourceEmojiId);
		if (!sourceEmoji) throw new UnknownGuildEmojiError();
		const sourceGuild = await this.guildRepository.findUnique(sourceEmoji.guildId);
		if (!sourceGuild?.features.has(GuildFeatures.CLONE_EMOJI_ENABLED)) {
			throw new MissingAccessError();
		}
		const guildData = await this.contentHelpers.getGuildData({userId: user.id, guildId});
		await this.contentHelpers.checkCreateExpressionsPermission({userId: user.id, guildId});
		const allEmojis = await this.guildRepository.listEmojis(guildId);
		const guildFeatures = guildData.features;
		const hasUnlimitedEmoji = guildFeatures.includes(GuildFeatures.UNLIMITED_EMOJI);
		const maxEmojis = hasUnlimitedEmoji
			? MAX_GUILD_EXPRESSION_SLOTS_UNLIMITED
			: this.resolveGuildLimit('max_guild_emojis', MAX_GUILD_EMOJIS, guildFeatures);
		if (allEmojis.length >= maxEmojis) {
			throw new MaxGuildEmojisError(maxEmojis);
		}
		const emojiId = createEmojiID(await this.snowflakeService.generate());
		await this.avatarService.cloneEmojiImage({sourceEmojiId, emojiId});
		const emoji = await this.guildRepository.upsertEmoji({
			guild_id: guildId,
			emoji_id: emojiId,
			name: sourceEmoji.name,
			creator_id: user.id,
			animated: sourceEmoji.isAnimated,
			version: 1,
		});
		const updatedEmojis = [...allEmojis, emoji];
		await this.dispatchGuildEmojisUpdate({guildId, emojis: updatedEmojis});
		await this.contentHelpers.recordAuditLog({
			guildId,
			userId: user.id,
			action: AuditLogActionType.EMOJI_CREATE,
			targetId: emoji.id,
			auditLogReason: auditLogReason ?? null,
			changes: this.contentHelpers.guildAuditLogService.computeChanges(
				null,
				this.contentHelpers.serializeEmojiForAudit(emoji),
			),
		});
		return mapGuildEmojiToResponse(emoji);
	}

	async bulkCreateEmojis(
		params: {
			user: User;
			guildId: GuildID;
			emojis: Array<{
				name: string;
				image: string;
			}>;
		},
		auditLogReason?: string | null,
	): Promise<{
		success: Array<GuildEmojiResponse>;
		failed: Array<{
			name: string;
			error: string;
		}>;
	}> {
		const {user, guildId, emojis} = params;
		const guildData = await this.contentHelpers.getGuildData({userId: user.id, guildId});
		await this.contentHelpers.checkCreateExpressionsPermission({userId: user.id, guildId});
		const allEmojis = await this.guildRepository.listEmojis(guildId);
		const guildFeatures = guildData.features;
		const hasUnlimitedEmoji = guildFeatures.includes(GuildFeatures.UNLIMITED_EMOJI);
		const maxEmojis = hasUnlimitedEmoji
			? MAX_GUILD_EXPRESSION_SLOTS_UNLIMITED
			: this.resolveGuildLimit('max_guild_emojis', MAX_GUILD_EMOJIS, guildFeatures);
		let emojiCount = allEmojis.length;
		const success: Array<GuildEmojiResponse> = [];
		const failed: Array<{
			name: string;
			error: string;
		}> = [];
		const newEmojis: Array<GuildEmoji> = [];
		for (const emojiData of emojis) {
			try {
				if (emojiCount >= maxEmojis) {
					failed.push({
						name: emojiData.name,
						error: getContentMessage('guild.bulk_create.emoji_limit', user.locale, {
							limit: Math.floor(maxEmojis),
						}),
					});
					continue;
				}
				const {animated, imageBuffer, contentType} = await this.avatarService.processEmoji({
					errorPath: `emojis[${success.length + failed.length}].image`,
					base64Image: emojiData.image,
					guildFeatures,
				});
				const emojiId = createEmojiID(await this.snowflakeService.generate());
				await this.avatarService.uploadEmoji({
					prefix: 'emojis',
					emojiId,
					imageBuffer,
					contentType,
				});
				const emoji = await this.guildRepository.upsertEmoji({
					guild_id: guildId,
					emoji_id: emojiId,
					name: emojiData.name,
					animated,
					creator_id: user.id,
					version: 1,
				});
				emojiCount++;
				newEmojis.push(emoji);
				success.push(mapGuildEmojiToResponse(emoji));
			} catch (error) {
				failed.push({name: emojiData.name, error: this.getLocalizedBulkCreateError(error, user.locale)});
			}
		}
		if (newEmojis.length > 0) {
			const updatedEmojis = [...allEmojis, ...newEmojis];
			await this.dispatchGuildEmojisUpdate({guildId, emojis: updatedEmojis});
			await Promise.all(
				newEmojis.map((emoji) =>
					this.contentHelpers.recordAuditLog({
						guildId,
						userId: user.id,
						action: AuditLogActionType.EMOJI_CREATE,
						targetId: emoji.id,
						auditLogReason: auditLogReason ?? null,
						changes: this.contentHelpers.guildAuditLogService.computeChanges(
							null,
							this.contentHelpers.serializeEmojiForAudit(emoji),
						),
					}),
				),
			);
		}
		return {success, failed};
	}

	async updateEmoji(
		params: {
			userId: UserID;
			guildId: GuildID;
			emojiId: EmojiID;
			name: string;
		},
		auditLogReason?: string | null,
	): Promise<GuildEmojiResponse> {
		const {userId, guildId, emojiId, name} = params;
		const allEmojis = await this.guildRepository.listEmojis(guildId);
		const emoji = allEmojis.find((e) => e.id === emojiId);
		if (!emoji) throw new UnknownGuildEmojiError();
		await this.contentHelpers.checkModifyExpressionPermission({userId, guildId, creatorId: emoji.creatorId});
		const previousSnapshot = this.contentHelpers.serializeEmojiForAudit(emoji);
		const updatedEmoji = await this.guildRepository.upsertEmoji({...emoji.toRow(), name});
		const updatedEmojis = allEmojis.map((e) => (e.id === emojiId ? updatedEmoji : e));
		await this.dispatchGuildEmojisUpdate({guildId, emojis: updatedEmojis});
		await this.contentHelpers.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.EMOJI_UPDATE,
			targetId: emojiId,
			auditLogReason: auditLogReason ?? null,
			changes: this.contentHelpers.guildAuditLogService.computeChanges(
				previousSnapshot,
				this.contentHelpers.serializeEmojiForAudit(updatedEmoji),
			),
		});
		return mapGuildEmojiToResponse(updatedEmoji);
	}

	async deleteEmoji(
		params: {
			userId: UserID;
			guildId: GuildID;
			emojiId: EmojiID;
			purge?: boolean;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, emojiId, purge = false} = params;
		const guildData = await this.contentHelpers.getGuildData({userId, guildId});
		if (purge && !guildData.features.includes(GuildFeatures.EXPRESSION_PURGE_ALLOWED)) {
			throw new MissingAccessError();
		}
		const allEmojis = await this.guildRepository.listEmojis(guildId);
		const emoji = allEmojis.find((e) => e.id === emojiId);
		if (!emoji) throw new UnknownGuildEmojiError();
		await this.contentHelpers.checkModifyExpressionPermission({userId, guildId, creatorId: emoji.creatorId});
		const previousSnapshot = this.contentHelpers.serializeEmojiForAudit(emoji);
		await this.guildRepository.deleteEmoji(guildId, emojiId);
		const updatedEmojis = allEmojis.filter((e) => e.id !== emojiId);
		await this.dispatchGuildEmojisUpdate({guildId, emojis: updatedEmojis});
		if (purge) {
			await this.assetPurger.purgeEmoji(emoji.id.toString());
		}
		await this.contentHelpers.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.EMOJI_DELETE,
			targetId: emojiId,
			auditLogReason: auditLogReason ?? null,
			changes: this.contentHelpers.guildAuditLogService.computeChanges(previousSnapshot, null),
		});
	}

	private async dispatchGuildEmojisUpdate(params: {guildId: GuildID; emojis: Array<GuildEmoji>}): Promise<void> {
		const {guildId, emojis} = params;
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_EMOJIS_UPDATE',
			data: {emojis: emojis.map(mapGuildEmojiToResponse)},
		});
	}
}
