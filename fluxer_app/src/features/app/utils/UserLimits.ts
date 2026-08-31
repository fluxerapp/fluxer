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

	getMaxGuilds(): number {
		const user = this.getCurrentUser();
		if (user?.maxGuilds) return user.maxGuilds;
		return LimitResolver.resolve({key: 'max_guilds', fallback: FALLBACKS.max_guilds});
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

	getMaxAttachmentsPerMessage(): number {
		const user = this.getCurrentUser();
		if (user?.maxAttachmentsPerMessage) return user.maxAttachmentsPerMessage;
		return LimitResolver.resolve({key: 'max_attachments_per_message', fallback: FALLBACKS.max_attachments_per_message});
	}

	getMaxAttachmentFileSize(): number {
		const user = this.getCurrentUser();
		if (user?.maxAttachmentFileSize) return user.maxAttachmentFileSize;
		return LimitResolver.resolve({key: 'max_attachment_file_size', fallback: FALLBACKS.max_attachment_file_size});
	}

	getMaxBioLength(): number {
		const user = this.getCurrentUser();
		if (user?.maxBioLength) return user.maxBioLength;
		return LimitResolver.resolve({key: 'max_bio_length', fallback: FALLBACKS.max_bio_length});
	}

	getMaxBookmarks(): number {
		const user = this.getCurrentUser();
		if (user?.maxBookmarks) return user.maxBookmarks;
		return LimitResolver.resolve({key: 'max_bookmarks', fallback: FALLBACKS.max_bookmarks});
	}

	getMaxFavoriteMemes(): number {
		const user = this.getCurrentUser();
		if (user?.maxFavoriteMemes) return user.maxFavoriteMemes;
		return LimitResolver.resolve({key: 'max_favorite_memes', fallback: FALLBACKS.max_favorite_memes});
	}

	getMaxFavoriteMemeTags(): number {
		const user = this.getCurrentUser();
		if (user?.maxFavoriteMemeTags) return user.maxFavoriteMemeTags;
		return LimitResolver.resolve({key: 'max_favorite_meme_tags', fallback: FALLBACKS.max_favorite_meme_tags});
	}

	getMaxRelationships(): number {
		const user = this.getCurrentUser();
		if (user?.maxRelationships) return user.maxRelationships;
		return LimitResolver.resolve({key: 'max_relationships', fallback: FALLBACKS.max_relationships});
	}

	getMaxGroupDmRecipients(): number {
		const user = this.getCurrentUser();
		if (user?.maxGroupDmRecipients) return user.maxGroupDmRecipients;
		return LimitResolver.resolve({key: 'max_group_dm_recipients', fallback: FALLBACKS.max_group_dm_recipients});
	}

	getMaxPrivateChannels(): number {
		const user = this.getCurrentUser();
		if (user?.maxPrivateChannels) return user.maxPrivateChannels;
		return LimitResolver.resolve({
			key: 'max_private_channels_per_user',
			fallback: FALLBACKS.max_private_channels_per_user,
		});
	}

	getStockValue(key: LimitKey, fallback: number): number {
		return LimitResolver.resolvePremium(key, fallback);
	}

	getRestrictedValue(key: LimitKey, fallback: number): number {
		return LimitResolver.resolveFree(key, fallback);
	}

	getPremiumValue(key: LimitKey, fallback: number): number {
		return this.getStockValue(key, fallback);
	}

	getFreeValue(key: LimitKey, fallback: number): number {
		return this.getRestrictedValue(key, fallback);
	}

	getMultiple(keys: Array<LimitKey>, fallbacks: Partial<Record<LimitKey, number>> = {}): Record<string, number> {
		const result: Record<string, number> = {};
		for (const key of keys) {
			const fallback = (fallbacks as Record<string, number>)[key];
			const defaultFallback = this._getDefaultFallback(key);
			result[key] = LimitResolver.resolve({
				key,
				fallback: fallback ?? defaultFallback,
			});
		}
		return result;
	}

	private _getDefaultFallback(key: LimitKey): number {
		const fallbackMap: Record<string, () => number> = {
			max_guilds: () => FALLBACKS.max_guilds,
			max_message_length: () => FALLBACKS.max_message_length,
			max_attachments_per_message: () => FALLBACKS.max_attachments_per_message,
			max_bio_length: () => FALLBACKS.max_bio_length,
			max_bookmarks: () => FALLBACKS.max_bookmarks,
			max_favorite_memes: () => FALLBACKS.max_favorite_memes,
			max_favorite_meme_tags: () => FALLBACKS.max_favorite_meme_tags,
			max_relationships: () => FALLBACKS.max_relationships,
			max_group_dm_recipients: () => FALLBACKS.max_group_dm_recipients,
			max_private_channels_per_user: () => FALLBACKS.max_private_channels_per_user,
			max_attachment_file_size: () => FALLBACKS.max_attachment_file_size,
		};
		const getter = fallbackMap[key];
		return getter ? getter() : 0;
	}
}

export const Limits = new LimitsClass();
