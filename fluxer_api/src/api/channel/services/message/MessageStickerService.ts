// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import type {GuildID, StickerID, UserID} from '../../../BrandedTypes';
import type {MessageStickerItem} from '../../../database/types/MessageTypes';
import type {IGuildRepositoryAggregate} from '../../../guild/repositories/IGuildRepositoryAggregate';
import type {LimitConfigService} from '../../../limits/LimitConfigService';
import {resolveLimitSafe} from '../../../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../../../limits/LimitMatchContextBuilder';
import type {IUserRepository} from '../../../user/IUserRepository';

export class MessageStickerService {
	constructor(
		private userRepository: IUserRepository,
		private guildRepository: IGuildRepositoryAggregate,
		private readonly limitConfigService: LimitConfigService,
	) {}

	async computeStickerIds(params: {
		stickerIds: Array<StickerID>;
		userId: UserID | null;
		guildId: GuildID | null;
		hasPermission?: (permission: bigint) => Promise<boolean>;
	}): Promise<Array<MessageStickerItem>> {
		const {stickerIds, userId, guildId, hasPermission} = params;
		let hasGlobalExpressions = 0;
		if (userId) {
			const user = await this.userRepository.findUnique(userId);
			const ctx = createLimitMatchContext({user});
			hasGlobalExpressions = resolveLimitSafe(
				this.limitConfigService.getConfigSnapshot(),
				ctx,
				'feature_global_expressions',
				0,
			);
		}
		return Promise.all(
			stickerIds.map(async (stickerId) => {
				if (!guildId) {
					if (hasGlobalExpressions === 0) {
						throw InputValidationError.fromCode('sticker', ValidationErrorCodes.CUSTOM_STICKERS_IN_DMS_REQUIRE_PREMIUM);
					}
					const stickerFromAnyGuild = await this.guildRepository.getStickerById(stickerId);
					if (!stickerFromAnyGuild) {
						throw InputValidationError.fromCode('sticker', ValidationErrorCodes.CUSTOM_STICKER_NOT_FOUND);
					}
					return {
						sticker_id: stickerFromAnyGuild.id,
						name: stickerFromAnyGuild.name,
						animated: stickerFromAnyGuild.animated,
					};
				}
				const guildSticker = await this.guildRepository.getSticker(stickerId, guildId);
				if (guildSticker) {
					return {
						sticker_id: guildSticker.id,
						name: guildSticker.name,
						animated: guildSticker.animated,
					};
				}
				const stickerFromOtherGuild = await this.guildRepository.getStickerById(stickerId);
				if (!stickerFromOtherGuild) {
					throw InputValidationError.fromCode('sticker', ValidationErrorCodes.CUSTOM_STICKER_NOT_FOUND);
				}
				if (hasGlobalExpressions === 0) {
					throw InputValidationError.fromCode(
						'sticker',
						ValidationErrorCodes.CUSTOM_STICKERS_REQUIRE_PREMIUM_OUTSIDE_SOURCE,
					);
				}
				if (hasPermission) {
					const canUseExternalStickers = await hasPermission(Permissions.USE_EXTERNAL_STICKERS);
					if (!canUseExternalStickers) {
						throw new MissingPermissionsError();
					}
				}
				return {
					sticker_id: stickerFromOtherGuild.id,
					name: stickerFromOtherGuild.name,
					animated: stickerFromOtherGuild.animated,
				};
			}),
		);
	}
}
