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

import type {GuildID, StickerID, UserID} from '~/BrandedTypes';
import {Permissions} from '~/Constants';
import type {MessageStickerItem} from '~/database/CassandraTypes';
import {InputValidationError, MissingPermissionsError} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {PackService} from '~/pack/PackService';
import type {IUserRepository} from '~/user/IUserRepository';

export class MessageStickerService {
	constructor(
		private userRepository: IUserRepository,
		private guildRepository: IGuildRepository,
		private packService: PackService,
	) {}

	async computeStickerIds(params: {
		stickerIds: Array<StickerID>;
		userId: UserID | null;
		guildId: GuildID | null;
		hasPermission?: (permission: bigint) => Promise<boolean>;
	}): Promise<Array<MessageStickerItem>> {
		const {stickerIds, userId, guildId, hasPermission} = params;

		const packResolver = await this.packService.createPackExpressionAccessResolver({
			userId,
			type: 'sticker',
		});

		let isPremium = false;
		if (userId) {
			const user = await this.userRepository.findUnique(userId);
			isPremium = user?.canUseGlobalExpressions() ?? false;
		}

		return Promise.all(
			stickerIds.map(async (stickerId) => {
				if (!guildId) {
					if (!isPremium) {
						throw InputValidationError.create('sticker', 'Cannot use custom stickers in DMs without premium');
					}

					const stickerFromAnyGuild = await this.guildRepository.getStickerById(stickerId);
					if (!stickerFromAnyGuild) {
						throw InputValidationError.create('sticker', 'Custom sticker not found');
					}

					const packAccess = await packResolver.resolve(stickerFromAnyGuild.guildId);
					if (packAccess === 'not-accessible') {
						throw InputValidationError.create('sticker', 'Custom sticker not found');
					}

					return {
						sticker_id: stickerFromAnyGuild.id,
						name: stickerFromAnyGuild.name,
						format_type: stickerFromAnyGuild.formatType,
					};
				}

				const guildSticker = await this.guildRepository.getSticker(stickerId, guildId);
				if (guildSticker) {
					return {
						sticker_id: guildSticker.id,
						name: guildSticker.name,
						format_type: guildSticker.formatType,
					};
				}

				const stickerFromOtherGuild = await this.guildRepository.getStickerById(stickerId);
				if (!stickerFromOtherGuild) {
					throw InputValidationError.create('sticker', 'Custom sticker not found');
				}

				if (!isPremium) {
					throw InputValidationError.create(
						'sticker',
						'Cannot use custom stickers outside of source guilds without premium',
					);
				}

				if (hasPermission) {
					const canUseExternalStickers = await hasPermission(Permissions.USE_EXTERNAL_STICKERS);
					if (!canUseExternalStickers) {
						throw new MissingPermissionsError();
					}
				}

				const packAccess = await packResolver.resolve(stickerFromOtherGuild.guildId);
				if (packAccess === 'not-accessible') {
					throw InputValidationError.create('sticker', 'Custom sticker not found');
				}

				return {
					sticker_id: stickerFromOtherGuild.id,
					name: stickerFromOtherGuild.name,
					format_type: stickerFromOtherGuild.formatType,
				};
			}),
		);
	}
}
