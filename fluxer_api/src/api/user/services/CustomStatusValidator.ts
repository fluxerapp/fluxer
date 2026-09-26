// SPDX-License-Identifier: AGPL-3.0-or-later

import {createEmojiID, type EmojiID, type UserID} from '@app/api/BrandedTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {IUserAccountRepository} from '@app/api/user/repositories/IUserAccountRepository';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {CustomStatusPayload} from '@fluxer/schema/src/domains/user/UserRequestSchemas';

interface ValidatedCustomStatus {
	text: string | null;
	expiresAt: Date | null;
	emojiId: EmojiID | null;
	emojiName: string | null;
	emojiAnimated: boolean;
}

export class CustomStatusValidator {
	constructor(
		private readonly userAccountRepository: IUserAccountRepository,
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly limitConfigService: LimitConfigService,
	) {}

	async validate(userId: UserID, payload: CustomStatusPayload): Promise<ValidatedCustomStatus> {
		const text = payload.text ?? null;
		contentModerationService.scanText(text, {
			userId,
			guildId: null,
			channelId: null,
			messageId: null,
			surface: 'profile_field',
		});
		const expiresAt = payload.expires_at ?? null;
		let emojiId: EmojiID | null = null;
		let emojiName: string | null = null;
		let emojiAnimated = false;
		if (payload.emoji_id != null) {
			const requestedEmojiId = createEmojiID(payload.emoji_id);
			const emoji = await this.guildRepository.getEmojiById(requestedEmojiId);
			if (!emoji) {
				throw InputValidationError.fromCode('custom_status.emoji_id', ValidationErrorCodes.CUSTOM_EMOJI_NOT_FOUND);
			}
			const user = await this.userAccountRepository.findUnique(userId);
			const ctx = createLimitMatchContext({user});
			const hasGlobalExpressions = resolveLimitSafe(
				this.limitConfigService.getConfigSnapshot(),
				ctx,
				'feature_global_expressions',
				0,
			);
			if (hasGlobalExpressions !== 0) {
				emojiId = requestedEmojiId;
				emojiName = emoji.name;
				emojiAnimated = emoji.isAnimated;
			}
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
