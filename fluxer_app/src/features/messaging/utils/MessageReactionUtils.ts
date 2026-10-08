// SPDX-License-Identifier: AGPL-3.0-or-later
import type {UnicodeEmoji} from '@app/features/emoji/types/EmojiTypes';
import {getSkinTonedSurrogate} from '@app/features/expressions/utils/SkinToneUtils';
import type {ReactionEmoji} from '@app/features/messaging/utils/ReactionEmoji';

export type {ReactionEmoji};

const isCustomEmoji = (emoji: UnicodeEmoji | ReactionEmoji): emoji is ReactionEmoji =>
	'id' in emoji && emoji.id != null;

export function toReactionEmoji(emoji: UnicodeEmoji | ReactionEmoji): ReactionEmoji {
	if (isCustomEmoji(emoji)) {
		const canonicalName =
			emoji.uniqueName === null || emoji.uniqueName === undefined ? emoji.name.replace(/~\d+$/, '') : emoji.uniqueName;
		if (canonicalName === emoji.name) {
			return emoji;
		}
		return {...emoji, name: canonicalName};
	}
	return {name: getSkinTonedSurrogate(emoji)};
}
