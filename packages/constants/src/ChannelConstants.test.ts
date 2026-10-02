// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ANNOUNCEMENT_CONVERTIBLE_CHANNEL_TYPES,
	CHANNEL_FOLLOW_TARGET_TYPES,
	ChannelTypes,
	CROSSPOST_SERVER_FLAGS,
	GUILD_TEXT_BASED_CHANNEL_TYPES,
	isMessageTypeDeletable,
	MessageFlags,
	MessageTypes,
	SENDABLE_MESSAGE_FLAGS,
	TEXT_BASED_CHANNEL_TYPES,
	WebhookTypes,
} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

describe('announcement channel constants', () => {
	it('treats announcement channels as guild text-based channels', () => {
		expect(GUILD_TEXT_BASED_CHANNEL_TYPES.has(ChannelTypes.GUILD_ANNOUNCEMENT)).toBe(true);
		expect(TEXT_BASED_CHANNEL_TYPES.has(ChannelTypes.GUILD_ANNOUNCEMENT)).toBe(true);
	});

	it('converts only between text and announcement channels', () => {
		expect([...ANNOUNCEMENT_CONVERTIBLE_CHANNEL_TYPES].sort()).toEqual([
			ChannelTypes.GUILD_TEXT,
			ChannelTypes.GUILD_ANNOUNCEMENT,
		]);
	});

	it('lets follows post only into text channels', () => {
		expect([...CHANNEL_FOLLOW_TARGET_TYPES]).toEqual([ChannelTypes.GUILD_TEXT]);
	});

	it('lets members delete channel follow system messages', () => {
		expect(isMessageTypeDeletable(MessageTypes.CHANNEL_FOLLOW_ADD)).toBe(true);
	});

	it('keeps the crosspost flags out of the sendable flags', () => {
		expect(CROSSPOST_SERVER_FLAGS).toBe(
			MessageFlags.CROSSPOSTED | MessageFlags.IS_CROSSPOST | MessageFlags.SOURCE_MESSAGE_DELETED,
		);
		expect(CROSSPOST_SERVER_FLAGS & SENDABLE_MESSAGE_FLAGS).toBe(0);
	});

	it('names both webhook types', () => {
		expect(WebhookTypes).toEqual({INCOMING: 1, CHANNEL_FOLLOWER: 2});
	});
});
