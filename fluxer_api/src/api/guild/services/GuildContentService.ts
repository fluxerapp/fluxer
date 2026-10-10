// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
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

export class GuildContentService {
	private readonly contentHelpers: ContentHelpers;
	readonly emojis: EmojiService;
	readonly stickers: StickerService;

	constructor(
		guildRepository: GuildRepository,
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
		this.emojis = new EmojiService(
			guildRepository,
			userCacheService,
			gatewayService,
			avatarService,
			snowflakeService,
			this.contentHelpers,
			expressionAssetPurger,
			limitConfigService,
		);
		this.stickers = new StickerService(
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
}
