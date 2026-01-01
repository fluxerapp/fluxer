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

import {createEmojiID, type EmojiID, type GuildID, type UserID} from '~/BrandedTypes';
import {
	GuildFeatures,
	MAX_GUILD_EMOJIS_ANIMATED,
	MAX_GUILD_EMOJIS_ANIMATED_MORE_EMOJI,
	MAX_GUILD_EMOJIS_STATIC,
	MAX_GUILD_EMOJIS_STATIC_MORE_EMOJI,
} from '~/Constants';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {
	MaxGuildEmojisAnimatedError,
	MaxGuildEmojisStaticError,
	MissingAccessError,
	UnknownGuildEmojiError,
} from '~/Errors';
import {
	type GuildEmojiResponse,
	type GuildEmojiWithUserResponse,
	mapGuildEmojisWithUsersToResponse,
	mapGuildEmojiToResponse,
} from '~/guild/GuildModel';
import type {AvatarService} from '~/infrastructure/AvatarService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {GuildEmoji, User} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {getCachedUserPartialResponse} from '~/user/UserCacheHelpers';
import type {UserPartialResponse} from '~/user/UserModel';
import type {IGuildRepository} from '../../IGuildRepository';
import type {ContentHelpers} from './ContentHelpers';
import type {ExpressionAssetPurger} from './ExpressionAssetPurger';

export class EmojiService {
	constructor(
		private readonly guildRepository: IGuildRepository,
		private readonly userCacheService: UserCacheService,
		private readonly gatewayService: IGatewayService,
		private readonly avatarService: AvatarService,
		private readonly snowflakeService: SnowflakeService,
		private readonly contentHelpers: ContentHelpers,
		private readonly assetPurger: ExpressionAssetPurger,
	) {}

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
		params: {user: User; guildId: GuildID; name: string; image: string},
		auditLogReason?: string | null,
	): Promise<GuildEmojiResponse> {
		const {user, guildId, name, image} = params;
		const guildData = await this.contentHelpers.getGuildData({userId: user.id, guildId});
		await this.contentHelpers.checkCreateExpressionsPermission({userId: user.id, guildId});

		const allEmojis = await this.guildRepository.listEmojis(guildId);
		const staticCount = allEmojis.filter((e) => !e.isAnimated).length;
		const animatedCount = allEmojis.filter((e) => e.isAnimated).length;

		const {animated, imageBuffer} = await this.avatarService.processEmoji({errorPath: 'image', base64Image: image});

		const hasUnlimitedEmoji = guildData.features.includes(GuildFeatures.UNLIMITED_EMOJI);

		if (!hasUnlimitedEmoji) {
			const hasMoreEmoji = guildData.features.includes(GuildFeatures.MORE_EMOJI);
			const maxStatic = hasMoreEmoji ? MAX_GUILD_EMOJIS_STATIC_MORE_EMOJI : MAX_GUILD_EMOJIS_STATIC;
			const maxAnimated = hasMoreEmoji ? MAX_GUILD_EMOJIS_ANIMATED_MORE_EMOJI : MAX_GUILD_EMOJIS_ANIMATED;

			if (!animated && staticCount >= maxStatic) {
				throw new MaxGuildEmojisStaticError();
			}
			if (animated && animatedCount >= maxAnimated) {
				throw new MaxGuildEmojisAnimatedError();
			}
		}

		const emojiId = createEmojiID(this.snowflakeService.generate());
		await this.avatarService.uploadEmoji({prefix: 'emojis', emojiId, imageBuffer});

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

	async bulkCreateEmojis(
		params: {user: User; guildId: GuildID; emojis: Array<{name: string; image: string}>},
		auditLogReason?: string | null,
	): Promise<{
		success: Array<GuildEmojiResponse>;
		failed: Array<{name: string; error: string}>;
	}> {
		const {user, guildId, emojis} = params;
		const guildData = await this.contentHelpers.getGuildData({userId: user.id, guildId});
		await this.contentHelpers.checkCreateExpressionsPermission({userId: user.id, guildId});

		const allEmojis = await this.guildRepository.listEmojis(guildId);

		const hasUnlimitedEmoji = guildData.features.includes(GuildFeatures.UNLIMITED_EMOJI);
		const hasMoreEmoji = guildData.features.includes(GuildFeatures.MORE_EMOJI);
		const maxStatic = hasUnlimitedEmoji
			? Number.POSITIVE_INFINITY
			: hasMoreEmoji
				? MAX_GUILD_EMOJIS_STATIC_MORE_EMOJI
				: MAX_GUILD_EMOJIS_STATIC;
		const maxAnimated = hasUnlimitedEmoji
			? Number.POSITIVE_INFINITY
			: hasMoreEmoji
				? MAX_GUILD_EMOJIS_ANIMATED_MORE_EMOJI
				: MAX_GUILD_EMOJIS_ANIMATED;

		let staticCount = allEmojis.filter((e) => !e.isAnimated).length;
		let animatedCount = allEmojis.filter((e) => e.isAnimated).length;

		const success: Array<GuildEmojiResponse> = [];
		const failed: Array<{name: string; error: string}> = [];
		const newEmojis: Array<GuildEmoji> = [];

		for (const emojiData of emojis) {
			try {
				const {animated, imageBuffer} = await this.avatarService.processEmoji({
					errorPath: `emojis[${success.length + failed.length}].image`,
					base64Image: emojiData.image,
				});

				if (!animated && staticCount >= maxStatic) {
					failed.push({name: emojiData.name, error: 'Maximum static emojis reached'});
					continue;
				}
				if (animated && animatedCount >= maxAnimated) {
					failed.push({name: emojiData.name, error: 'Maximum animated emojis reached'});
					continue;
				}

				const emojiId = createEmojiID(this.snowflakeService.generate());
				await this.avatarService.uploadEmoji({prefix: 'emojis', emojiId, imageBuffer});

				const emoji = await this.guildRepository.upsertEmoji({
					guild_id: guildId,
					emoji_id: emojiId,
					name: emojiData.name,
					animated,
					creator_id: user.id,
					version: 1,
				});

				if (animated) {
					animatedCount++;
				} else {
					staticCount++;
				}

				newEmojis.push(emoji);
				success.push(mapGuildEmojiToResponse(emoji));
			} catch (error) {
				const errorMessage = error instanceof Error ? error.message : 'Unknown error';
				failed.push({name: emojiData.name, error: errorMessage});
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
		params: {userId: UserID; guildId: GuildID; emojiId: EmojiID; name: string},
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
		params: {userId: UserID; guildId: GuildID; emojiId: EmojiID; purge?: boolean},
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
