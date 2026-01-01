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

import {createEmojiID, type EmojiID, type UserID} from '~/BrandedTypes';
import {InputValidationError} from '~/errors/InputValidationError';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {PackService} from '~/pack/PackService';
import type {z} from '~/Schema';
import type {IUserAccountRepository} from '~/user/repositories/IUserAccountRepository';
import type {CustomStatusPayload} from '~/user/UserTypes';

export interface ValidatedCustomStatus {
	text: string | null;
	expiresAt: Date | null;
	emojiId: EmojiID | null;
	emojiName: string | null;
	emojiAnimated: boolean;
}

export class CustomStatusValidator {
	constructor(
		private readonly userAccountRepository: IUserAccountRepository,
		private readonly guildRepository: IGuildRepository,
		private readonly packService: PackService,
	) {}

	async validate(userId: UserID, payload: z.infer<typeof CustomStatusPayload>): Promise<ValidatedCustomStatus> {
		const text = payload.text ?? null;
		const expiresAt = payload.expires_at ?? null;
		let emojiId: EmojiID | null = null;
		let emojiName: string | null = null;
		let emojiAnimated = false;

		if (payload.emoji_id != null) {
			emojiId = createEmojiID(payload.emoji_id);

			const emoji = await this.guildRepository.getEmojiById(emojiId);
			if (!emoji) {
				throw InputValidationError.create('custom_status.emoji_id', 'Custom emoji not found');
			}

			const user = await this.userAccountRepository.findUnique(userId);
			if (!user?.canUseGlobalExpressions()) {
				throw InputValidationError.create('custom_status.emoji_id', 'Premium required to use custom emoji');
			}

			const guildMember = await this.guildRepository.getMember(emoji.guildId, userId);

			let hasAccess = guildMember !== null;
			if (!hasAccess) {
				const resolver = await this.packService.createPackExpressionAccessResolver({
					userId,
					type: 'emoji',
				});
				const resolution = await resolver.resolve(emoji.guildId);
				hasAccess = resolution === 'accessible';
			}

			if (!hasAccess) {
				throw InputValidationError.create(
					'custom_status.emoji_id',
					'Cannot use this emoji without access to its guild or installed pack',
				);
			}

			emojiName = emoji.name;
			emojiAnimated = emoji.isAnimated;
		} else if (payload.emoji_name != null) {
			emojiName = payload.emoji_name;
		}

		return {
			text,
			expiresAt,
			emojiId,
			emojiName,
			emojiAnimated,
		};
	}
}
