// SPDX-License-Identifier: AGPL-3.0-or-later

import {createEmojiSearchIndex} from '@app/features/emoji/state/EmojiSearchIndex';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {test} from 'vitest';

const EMOJI_COUNT = 20_000;
const QUERY_COUNT = 1_000;
const NAME_ROOTS = ['party', 'heart', 'rocket', 'wave', 'thumb', 'sparkle', 'blob', 'cat', 'dance', 'coffee'] as const;
const KEYWORD_ROOTS = ['celebrate', 'approve', 'launch', 'love', 'work', 'sleep'] as const;

function makeEmoji(index: number): FlatEmoji {
	const root = NAME_ROOTS[index % NAME_ROOTS.length];
	const variant = Math.floor(index / NAME_ROOTS.length);
	const guildId = `guild-${index % 80}`;
	return {
		id: `emoji-${index}`,
		guildId,
		name: `${root}_${variant}`,
		uniqueName: `${root}_${variant}`,
		allNamesString: `:${root}_${variant}:`,
		keywords: [KEYWORD_ROOTS[index % KEYWORD_ROOTS.length], `${root}_keyword_${index % 31}`, `guild_${index % 80}`],
	};
}

const EMOJIS = Object.freeze(Array.from({length: EMOJI_COUNT}, (_, index) => makeEmoji(index)));
const INDEX = createEmojiSearchIndex(EMOJIS);
const QUERIES = Object.freeze(
	Array.from({length: QUERY_COUNT}, (_, index) => {
		switch (index % 5) {
			case 0:
				return NAME_ROOTS[index % NAME_ROOTS.length];
			case 1:
				return 'art';
			case 2:
				return KEYWORD_ROOTS[index % KEYWORD_ROOTS.length];
			case 3:
				return `guild_${index % 80}`;
			default:
				return `${NAME_ROOTS[index % NAME_ROOTS.length]}_${index % 100}`;
		}
	}),
);

test('EmojiSearchIndex benchmarks', async ({bench}) => {
	await bench('builds an n-gram index for 20k emoji-like records', () => {
		const index = createEmojiSearchIndex(EMOJIS);
		(globalThis as {__emojiSearchIndexBenchSink?: number}).__emojiSearchIndexBenchSink = index.getStats().postingCount;
	}).run();

	await bench('serves 1k mixed top-10 searches over 20k records', () => {
		let total = 0;
		for (const query of QUERIES) {
			total += INDEX.search(query, {count: 10}).length;
		}
		(globalThis as {__emojiSearchIndexBenchSink?: number}).__emojiSearchIndexBenchSink = total;
	}).run();

	await bench('serves 1k filtered ranked searches over 20k records', () => {
		let total = 0;
		for (const query of QUERIES) {
			total += INDEX.search(query, {
				count: 10,
				canUse: (emoji) => emoji.guildId !== 'guild-13',
				getFrecencyScore: (emoji) => (emoji.id?.endsWith('7') ? 100 : 0),
			}).length;
		}
		(globalThis as {__emojiSearchIndexBenchSink?: number}).__emojiSearchIndexBenchSink = total;
	}).run();
});
