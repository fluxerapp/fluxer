// SPDX-License-Identifier: AGPL-3.0-or-later

import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';

const FALLBACKS = {
	max_guilds: DEFAULT_STOCK_LIMITS.max_guilds,
	max_message_length: DEFAULT_STOCK_LIMITS.max_message_length,
	max_poll_question_length: DEFAULT_STOCK_LIMITS.max_poll_question_length,
	max_poll_answers: DEFAULT_STOCK_LIMITS.max_poll_answers,
	max_poll_answer_length: DEFAULT_STOCK_LIMITS.max_poll_answer_length,
	max_poll_duration_hours: DEFAULT_STOCK_LIMITS.max_poll_duration_hours,
	max_attachments_per_message: DEFAULT_STOCK_LIMITS.max_attachments_per_message,
	max_bio_length: DEFAULT_STOCK_LIMITS.max_bio_length,
	max_bookmarks: DEFAULT_STOCK_LIMITS.max_bookmarks,
	max_favorite_memes: DEFAULT_STOCK_LIMITS.max_favorite_memes,
	max_favorite_meme_tags: DEFAULT_STOCK_LIMITS.max_favorite_meme_tags,
	max_relationships: DEFAULT_STOCK_LIMITS.max_relationships,
	max_group_dm_recipients: DEFAULT_STOCK_LIMITS.max_group_dm_recipients,
	max_private_channels_per_user: DEFAULT_STOCK_LIMITS.max_private_channels_per_user,
	max_attachment_file_size: DEFAULT_STOCK_LIMITS.max_attachment_file_size,
} as const;

class LimitsClass {
	private getCurrentUser(): User | undefined {
		return Users.getCurrentUser();
	}

	getMaxMessageLength(): number {
		const user = this.getCurrentUser();
		if (user?.maxMessageLength) return user.maxMessageLength;
		return LimitResolver.resolve({key: 'max_message_length', fallback: FALLBACKS.max_message_length});
	}

	getMaxPollQuestionLength(): number {
		const user = this.getCurrentUser();
		if (user?.maxPollQuestionLength) return user.maxPollQuestionLength;
		return LimitResolver.resolve({key: 'max_poll_question_length', fallback: FALLBACKS.max_poll_question_length});
	}

	getMaxPollAnswers(): number {
		const user = this.getCurrentUser();
		if (user?.maxPollAnswers) return user.maxPollAnswers;
		return LimitResolver.resolve({key: 'max_poll_answers', fallback: FALLBACKS.max_poll_answers});
	}

	getMaxPollAnswerLength(): number {
		const user = this.getCurrentUser();
		if (user?.maxPollAnswerLength) return user.maxPollAnswerLength;
		return LimitResolver.resolve({key: 'max_poll_answer_length', fallback: FALLBACKS.max_poll_answer_length});
	}

	getMaxPollDurationHours(): number {
		const user = this.getCurrentUser();
		if (user?.maxPollDurationHours) return user.maxPollDurationHours;
		return LimitResolver.resolve({key: 'max_poll_duration_hours', fallback: FALLBACKS.max_poll_duration_hours});
	}

	getMaxAttachmentsPerMessage(): number {
		const user = this.getCurrentUser();
		if (user?.maxAttachmentsPerMessage) return user.maxAttachmentsPerMessage;
		return LimitResolver.resolve({key: 'max_attachments_per_message', fallback: FALLBACKS.max_attachments_per_message});
	}

	getMaxGroupDmRecipients(): number {
		const user = this.getCurrentUser();
		if (user?.maxGroupDmRecipients) return user.maxGroupDmRecipients;
		return LimitResolver.resolve({key: 'max_group_dm_recipients', fallback: FALLBACKS.max_group_dm_recipients});
	}

	getStockValue(key: LimitKey, fallback: number): number {
		return LimitResolver.resolvePremium(key, fallback);
	}

	getRestrictedValue(key: LimitKey, fallback: number): number {
		return LimitResolver.resolveFree(key, fallback);
	}

	hasStockFeature(key: LimitKey, fallback: boolean): boolean {
		return this.getStockValue(key, fallback ? 1 : 0) > 0;
	}

	hasRestrictedFeature(key: LimitKey, fallback: boolean): boolean {
		return this.getRestrictedValue(key, fallback ? 1 : 0) > 0;
	}

	getPremiumValue(key: LimitKey, fallback: number): number {
		return this.getStockValue(key, fallback);
	}
}

export const Limits = new LimitsClass();
