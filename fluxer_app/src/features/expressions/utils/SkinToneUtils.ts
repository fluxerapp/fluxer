// SPDX-License-Identifier: AGPL-3.0-or-later

import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji, UnicodeEmoji} from '@app/features/emoji/types/EmojiTypes';
import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';

export interface EmojiDisplayData {
	surrogates: string;
	url: string | undefined;
}

export function getSkinTonedEmoji(emoji: FlatEmoji, skinTone: string): UnicodeEmoji | null {
	if (!emoji.hasSkinTones || !skinTone || !emoji.uniqueName) {
		return null;
	}
	return UnicodeEmojis.findEmojiWithSkinTone(emoji.uniqueName, skinTone);
}

export function getEmojiDisplayData(emoji: FlatEmoji): EmojiDisplayData {
	const skinTone = Emoji.skinTone;
	const skinTonedEmoji = getSkinTonedEmoji(emoji, skinTone);
	return {
		surrogates: skinTonedEmoji?.surrogates ?? emoji.surrogates ?? '',
		url: skinTonedEmoji?.url ?? emoji.url,
	};
}

export function getEmojiDisplayDataWithSkinTone(emoji: FlatEmoji, skinTone: string): EmojiDisplayData {
	const skinTonedEmoji = getSkinTonedEmoji(emoji, skinTone);
	return {
		surrogates: skinTonedEmoji?.surrogates ?? emoji.surrogates ?? '',
		url: skinTonedEmoji?.url ?? emoji.url,
	};
}

export function getSkinTonedSurrogate(emoji: FlatEmoji): string {
	const skinTone = Emoji.skinTone;
	if (!emoji.hasSkinTones || !skinTone || !emoji.uniqueName) {
		return emoji.surrogates ?? '';
	}
	const skinTonedEmoji = UnicodeEmojis.findEmojiWithSkinTone(emoji.uniqueName, skinTone);
	return skinTonedEmoji?.surrogates ?? emoji.surrogates ?? '';
}
