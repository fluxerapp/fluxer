// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmojiID, GuildID, StickerID, UserID} from '@app/api/BrandedTypes';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import {ContentHelpers} from '@app/api/guild/services/content/ContentHelpers';
import {EmojiService} from '@app/api/guild/services/content/EmojiService';
import {ExpressionAssetPurger} from '@app/api/guild/services/content/ExpressionAssetPurger';
import {StickerService} from '@app/api/guild/services/content/StickerService';
import type {AvatarService} from '@app/api/infrastructure/AvatarService';
import type {IAssetDeletionQueue} from '@app/api/infrastructure/IAssetDeletionQueue';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import type {
	GuildEmojiResponse,
	GuildEmojiWithUserResponse,
	GuildStickerResponse,
	GuildStickerWithUserResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export class GuildContentService {
	private readonly contentHelpers: ContentHelpers;
	private readonly emojiService: EmojiService;
	private readonly stickerService: StickerService;

	constructor(
		guildRepository: IGuildRepositoryAggregate,
		userCacheService: UserCacheService,
		gatewayService: IGatewayService,
		avatarService: AvatarService,
		snowflakeService: ISnowflakeService,
		guildAuditLogService: GuildAuditLogService,
		assetDeletionQueue: IAssetDeletionQueue,
		limitConfigService: LimitConfigService,
	) {
		this.contentHelpers = new ContentHelpers(gatewayService, guildAuditLogService);
		const expressionAssetPurger = new ExpressionAssetPurger(assetDeletionQueue);
		this.emojiService = new EmojiService(
			guildRepository,
			userCacheService,
			gatewayService,
			avatarService,
			snowflakeService,
			this.contentHelpers,
			expressionAssetPurger,
			limitConfigService,
		);
		this.stickerService = new StickerService(
			guildRepository,
			userCacheService,
			gatewayService,
			avatarService,
			snowflakeService,
			this.contentHelpers,
			expressionAssetPurger,
			limitConfigService,
		);
	}

	async getEmojis(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildEmojiWithUserResponse>> {
		return this.emojiService.getEmojis(params);
	}

	async getEmojiUser(params: {
		userId: UserID;
		guildId: GuildID;
		emojiId: EmojiID;
		requestCache: RequestCache;
	}): Promise<UserPartialResponse> {
		return this.emojiService.getEmojiUser(params);
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
		return this.emojiService.createEmoji(params, auditLogReason);
	}

	async cloneEmoji(
		params: {
			user: User;
			guildId: GuildID;
			sourceEmojiId: EmojiID;
		},
		auditLogReason?: string | null,
	): Promise<GuildEmojiResponse> {
		return this.emojiService.cloneEmoji(params, auditLogReason);
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
		return this.emojiService.bulkCreateEmojis(params, auditLogReason);
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
		return this.emojiService.updateEmoji(params, auditLogReason);
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
		return this.emojiService.deleteEmoji(params, auditLogReason);
	}

	async getStickers(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildStickerWithUserResponse>> {
		return this.stickerService.getStickers(params);
	}

	async getStickerUser(params: {
		userId: UserID;
		guildId: GuildID;
		stickerId: StickerID;
		requestCache: RequestCache;
	}): Promise<UserPartialResponse> {
		return this.stickerService.getStickerUser(params);
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
		return this.stickerService.createSticker(params, auditLogReason);
	}

	async cloneSticker(
		params: {
			user: User;
			guildId: GuildID;
			sourceStickerId: StickerID;
		},
		auditLogReason?: string | null,
	): Promise<GuildStickerResponse> {
		return this.stickerService.cloneSticker(params, auditLogReason);
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
		return this.stickerService.bulkCreateStickers(params, auditLogReason);
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
		return this.stickerService.updateSticker(params, auditLogReason);
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
		return this.stickerService.deleteSticker(params, auditLogReason);
	}
}
