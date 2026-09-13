// SPDX-License-Identifier: AGPL-3.0-or-later

import {createEmojiSearchIndex} from '@app/features/emoji/state/EmojiSearchIndex';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {describe, expect, test} from 'vitest';

function emoji(name: string, options: Partial<FlatEmoji> = {}): FlatEmoji {
	return {
		name,
		uniqueName: options.uniqueName ?? name,
		allNamesString: `:${name}:`,
		keywords: options.keywords,
		id: options.id,
		guildId: options.guildId,
	};
}

describe('EmojiSearchIndex', () => {
	test('orders exact, prefix, boundary, contains, and keyword matches', () => {
		const index = createEmojiSearchIndex([
			emoji('black_heart'),
			emoji('heart'),
			emoji('red_heart'),
			emoji('party_parrot', {keywords: ['celebration', 'heartfelt']}),
		]);

		expect(index.search('heart').map((result) => result.name)).toEqual([
			'heart',
			'black_heart',
			'red_heart',
			'party_parrot',
		]);
	});

	test('uses n-gram candidates for short and long substring queries', () => {
		const index = createEmojiSearchIndex([emoji('party_parrot'), emoji('rocket'), emoji('sparkles')]);

		expect(index.search('ar').map((result) => result.name)).toEqual(['party_parrot', 'sparkles']);
		expect(index.search('rro').map((result) => result.name)).toEqual(['party_parrot']);
	});

	test('applies availability filtering before top-k ranking', () => {
		const index = createEmojiSearchIndex([
			emoji('party_a', {id: '1', guildId: 'guild'}),
			emoji('party_b', {id: '2', guildId: 'guild'}),
			emoji('party_c', {id: '3', guildId: 'guild'}),
		]);

		const results = index.search('party', {
			count: 2,
			canUse: (result) => result.id !== '2',
			getFrecencyScore: (result) => (result.id === '3' ? 10 : 1),
		});

		expect(results.map((result) => result.id)).toEqual(['3', '1']);
	});

	test('reports compact index stats for benchmarks and regression checks', () => {
		const index = createEmojiSearchIndex([
			emoji('thumbsup', {keywords: ['approve', 'yes']}),
			emoji('thumbsdown', {keywords: ['reject', 'no']}),
		]);

		expect(index.getStats()).toMatchObject({
			emojiCount: 2,
		});
		expect(index.getStats().gramCount).toBeGreaterThan(0);
		expect(index.getStats().postingCount).toBeGreaterThanOrEqual(index.getStats().gramCount);
	});
});
