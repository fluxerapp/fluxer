// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';
import {filterViewableChannels, pickDefaultGuildChannelId} from './ChannelShared';

describe('filterViewableChannels', () => {
	it('excludes link channels from default navigation fallbacks', () => {
		const channels = [
			{id: 'link', type: ChannelTypes.GUILD_LINK, position: 0, guildId: 'guild'},
			{id: 'text', type: ChannelTypes.GUILD_TEXT, position: 1, guildId: 'guild'},
			{id: 'voice', type: ChannelTypes.GUILD_VOICE, position: 2, guildId: 'guild'},
			{id: 'category', type: ChannelTypes.GUILD_CATEGORY, position: 3, guildId: 'guild'},
		];
		expect(filterViewableChannels(channels).map((channel) => channel.id)).toEqual(['text', 'voice']);
	});
});

describe('pickDefaultGuildChannelId', () => {
	const channels = [
		{id: 'category', type: ChannelTypes.GUILD_CATEGORY, position: 1, guildId: 'guild'},
		{id: 'link', type: ChannelTypes.GUILD_LINK, position: 2, guildId: 'guild'},
		{id: 'rules', type: ChannelTypes.GUILD_TEXT, position: 3, guildId: 'guild'},
		{id: 'general', type: ChannelTypes.GUILD_TEXT, position: 4, guildId: 'guild'},
		{id: 'voice', type: ChannelTypes.GUILD_VOICE, position: 5, guildId: 'guild'},
	];

	it('returns null while the guild has no channels yet', () => {
		expect(pickDefaultGuildChannelId({guildId: 'guild', channels: []})).toBeNull();
	});

	it('picks the first channel by position, skipping categories and links', () => {
		expect(pickDefaultGuildChannelId({guildId: 'guild', channels})).toBe('rules');
	});

	it('keeps the remembered channel for the guild', () => {
		expect(pickDefaultGuildChannelId({guildId: 'guild', channels, selectedChannelId: 'general'})).toBe('general');
	});

	it('falls back when the remembered channel is gone', () => {
		expect(pickDefaultGuildChannelId({guildId: 'guild', channels, selectedChannelId: 'deleted'})).toBe('rules');
	});

	it('falls back when the remembered channel is not navigable', () => {
		expect(pickDefaultGuildChannelId({guildId: 'guild', channels, selectedChannelId: 'link'})).toBe('rules');
		expect(pickDefaultGuildChannelId({guildId: 'guild', channels, selectedChannelId: 'category'})).toBe('rules');
	});

	it('falls back when the remembered channel belongs to another guild', () => {
		const withForeign = [...channels, {id: 'other', type: ChannelTypes.GUILD_TEXT, position: 0, guildId: 'other'}];
		expect(pickDefaultGuildChannelId({guildId: 'guild', channels: withForeign, selectedChannelId: 'other'})).toBe(
			'rules',
		);
	});
});
