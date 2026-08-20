// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {describe, expect, it} from 'vitest';
import {
	buildSearchResultGroups,
	buildSearchResultGroupsByMessageId,
	countSearchResultChannels,
} from './SearchResultGrouping';

const message = (id: string, channelId: string): Message => ({id, channelId}) as Message;

describe('SearchResultGrouping', () => {
	it('keeps the order the server returned instead of bucketing per channel', () => {
		const groups = buildSearchResultGroups([
			message('500', 'general'),
			message('400', 'random'),
			message('300', 'general'),
		]);
		expect(groups.map((group) => group.messages.map((result) => result.id))).toEqual([['500'], ['400'], ['300']]);
		expect(groups.map((group) => group.channelId)).toEqual(['general', 'random', 'general']);
	});

	it('merges consecutive results from the same channel into one group', () => {
		const groups = buildSearchResultGroups([
			message('500', 'general'),
			message('490', 'general'),
			message('400', 'random'),
		]);
		expect(groups).toHaveLength(2);
		expect(groups[0].messages.map((result) => result.id)).toEqual(['500', '490']);
		expect(groups[1].messages.map((result) => result.id)).toEqual(['400']);
	});

	it('gives every group a unique key even when a channel appears more than once', () => {
		const groups = buildSearchResultGroups([
			message('500', 'general'),
			message('400', 'random'),
			message('300', 'general'),
		]);
		expect(new Set(groups.map((group) => group.key)).size).toBe(3);
	});

	it('indexes each message to the group it renders in', () => {
		const groups = buildSearchResultGroups([
			message('500', 'general'),
			message('400', 'random'),
			message('300', 'general'),
		]);
		const groupsByMessageId = buildSearchResultGroupsByMessageId(groups);
		expect(groupsByMessageId.get('500')).toBe(groups[0]);
		expect(groupsByMessageId.get('400')).toBe(groups[1]);
		expect(groupsByMessageId.get('300')).toBe(groups[2]);
	});

	it('counts distinct channels rather than groups', () => {
		const groups = buildSearchResultGroups([
			message('500', 'general'),
			message('400', 'random'),
			message('300', 'general'),
		]);
		expect(countSearchResultChannels(groups)).toBe(2);
		expect(countSearchResultChannels(buildSearchResultGroups([]))).toBe(0);
	});
});
