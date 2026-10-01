// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	type ChannelOrderingChannel,
	computeGuildChannelReorderPlan,
	computePositionFromPrecedingSiblingId,
	computePrecedingSiblingIdFromPosition,
	sortChannelsForOrdering,
} from '@fluxer/schema/src/domains/channel/GuildChannelOrdering';
import {describe, expect, it} from 'vitest';

type Ch = ChannelOrderingChannel<string>;

const createFixture = (): Array<Ch> => [
	{id: 'cat1', parentId: null, type: ChannelTypes.GUILD_CATEGORY, position: 1},
	{id: 'c1_text1', parentId: 'cat1', type: ChannelTypes.GUILD_TEXT, position: 2},
	{id: 'c1_voice1', parentId: 'cat1', type: ChannelTypes.GUILD_VOICE, position: 3},
	{id: 'cat2', parentId: null, type: ChannelTypes.GUILD_CATEGORY, position: 4},
	{id: 'c2_text1', parentId: 'cat2', type: ChannelTypes.GUILD_TEXT, position: 5},
	{id: 'c2_voice1', parentId: 'cat2', type: ChannelTypes.GUILD_VOICE, position: 6},
	{id: 'top_text', parentId: null, type: ChannelTypes.GUILD_TEXT, position: 7},
];

describe('GuildChannelOrdering', () => {
	it('round-trips position <-> preceding sibling for a parent', () => {
		const channels = createFixture();
		const preceding = computePrecedingSiblingIdFromPosition<string, Ch>({
			channels,
			targetId: 'c2_voice1',
			desiredParentId: 'cat2',
			position: 1,
		});
		expect(preceding).toBe('c2_text1');
		const position = computePositionFromPrecedingSiblingId<string, Ch>({
			channels,
			targetId: 'c2_voice1',
			desiredParentId: 'cat2',
			precedingSiblingId: preceding,
		});
		expect(position).toBe(1);
	});
	it('inserts after a preceding category block (category + children)', () => {
		const channels = createFixture();
		const result = computeGuildChannelReorderPlan<string, Ch>({
			channels,
			operation: {channelId: 'cat1', parentId: null, precedingSiblingId: 'cat2'},
		});
		expect(result.ok).toBe(true);
		if (!result.ok) return;
		expect(result.plan.finalChannels.map((c) => c.id)).toEqual([
			'cat2',
			'c2_text1',
			'c2_voice1',
			'cat1',
			'c1_text1',
			'c1_voice1',
			'top_text',
		]);
	});
	it('rejects positioning relative to a channel inside the moved category block', () => {
		const channels = createFixture();
		const result = computeGuildChannelReorderPlan<string, Ch>({
			channels,
			operation: {channelId: 'cat1', parentId: null, precedingSiblingId: 'c1_text1'},
		});
		expect(result.ok).toBe(false);
		if (result.ok) return;
		expect(result.code).toBe('CANNOT_POSITION_RELATIVE_TO_SELF_BLOCK');
	});
	it('computes sibling positions against the list with the moved block removed', () => {
		const channels = createFixture();
		const position = computePositionFromPrecedingSiblingId<string, Ch>({
			channels,
			targetId: 'c1_text1',
			desiredParentId: 'cat1',
			precedingSiblingId: 'c1_voice1',
		});
		expect(position).toBe(1);
	});
	it('moves a channel across categories via preceding sibling', () => {
		const channels = createFixture();
		const result = computeGuildChannelReorderPlan<string, Ch>({
			channels,
			operation: {channelId: 'c1_text1', parentId: 'cat2', precedingSiblingId: 'c2_text1'},
		});
		expect(result.ok).toBe(true);
		if (!result.ok) return;
		expect(result.plan.desiredParentById.get('c1_text1')).toBe('cat2');
		const ids = result.plan.finalChannels.map((c) => c.id);
		const c2TextIdx = ids.indexOf('c2_text1');
		const c1TextIdx = ids.indexOf('c1_text1');
		expect(c1TextIdx).toBe(c2TextIdx + 1);
	});
	it('moves a channel to the first position inside a category (no preceding sibling)', () => {
		const channels = createFixture();
		const result = computeGuildChannelReorderPlan<string, Ch>({
			channels,
			operation: {channelId: 'top_text', parentId: 'cat1', precedingSiblingId: null},
		});
		expect(result.ok).toBe(true);
		if (!result.ok) return;
		expect(result.plan.desiredParentById.get('top_text')).toBe('cat1');
		const ids = result.plan.finalChannels.map((c) => c.id);
		const cat1Idx = ids.indexOf('cat1');
		const topTextIdx = ids.indexOf('top_text');
		expect(topTextIdx).toBe(cat1Idx + 1);
	});
	it('returns orderUnchanged when the channel is already in the requested position', () => {
		const channels = createFixture();
		const result = computeGuildChannelReorderPlan<string, Ch>({
			channels,
			operation: {channelId: 'c1_text1', parentId: 'cat1', precedingSiblingId: null},
		});
		expect(result.ok).toBe(true);
		if (!result.ok) return;
		expect(result.plan.orderUnchanged).toBe(true);
	});
	it('rejects PRECEDING_PARENT_MISMATCH when sibling is in wrong parent', () => {
		const channels = createFixture();
		const result = computeGuildChannelReorderPlan<string, Ch>({
			channels,
			operation: {channelId: 'c1_text1', parentId: 'cat1', precedingSiblingId: 'c2_text1'},
		});
		expect(result.ok).toBe(false);
		if (result.ok) return;
		expect(result.code).toBe('PRECEDING_PARENT_MISMATCH');
	});
	it('ranks announcement channels with text channels ahead of voice channels', () => {
		const channels: Array<Ch> = [
			{id: 'cat', parentId: null, type: ChannelTypes.GUILD_CATEGORY, position: 0},
			{id: 'voice', parentId: 'cat', type: ChannelTypes.GUILD_VOICE, position: 1},
			{id: 'announcements', parentId: 'cat', type: ChannelTypes.GUILD_ANNOUNCEMENT, position: 3},
			{id: 'text', parentId: 'cat', type: ChannelTypes.GUILD_TEXT, position: 2},
		];
		expect(sortChannelsForOrdering<string, Ch>(channels).map((c) => c.id)).toEqual([
			'cat',
			'text',
			'announcements',
			'voice',
		]);
	});
});
