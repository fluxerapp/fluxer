// SPDX-License-Identifier: AGPL-3.0-or-later

export interface UnicodeEmoji {
	surrogates: string;
}

export interface EmojiSurrogateMatch {
	start: number;
	end: number;
	name: string | null;
}

export interface EmojiProvider {
	matchEmojiSurrogates(text: string): Iterable<EmojiSurrogateMatch>;
	findEmojiByName(name: string): UnicodeEmoji | null;
	findEmojiWithSkinTone(baseName: string, skinToneSurrogate: string): UnicodeEmoji | null;
}

export interface EmojiParserConfig {
	emojiProvider?: EmojiProvider;
	skinToneSurrogates?: ReadonlyArray<string>;
}

let globalEmojiConfig: EmojiParserConfig | null = null;

export function setEmojiParserConfig(config: EmojiParserConfig): void {
	globalEmojiConfig = config;
}

export function getEmojiParserConfig(): EmojiParserConfig | null {
	return globalEmojiConfig;
}
