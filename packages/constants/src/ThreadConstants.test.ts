// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ChannelConstants from '@fluxer/constants/src/ChannelConstants';
import {
	ChannelTypes,
	GUILD_TEXT_BASED_CHANNEL_TYPES,
	isMessageTypeDeletable,
	MessageTypes,
	TEXT_BASED_CHANNEL_TYPES,
} from '@fluxer/constants/src/ChannelConstants';
import * as ThreadConstants from '@fluxer/constants/src/ThreadConstants';
import {
	ChannelFlags,
	isThreadAutoArchiveDuration,
	isValidThreadMemberSettingsFlags,
	publicThreadTypeFor,
	resolveTextThreadType,
	settableChannelFlags,
	THREAD_CHANNEL_TYPES,
	THREAD_FEATURE_CHANNEL_TYPES,
	THREAD_MESSAGE_FLAG_MASK,
	THREAD_ONLY_CHANNEL_TYPES,
	THREAD_PARENT_CHANNEL_TYPES,
	ThreadMemberFlags,
} from '@fluxer/constants/src/ThreadConstants';
import {describe, expect, it} from 'vitest';

function numericValues(value: unknown): Array<number> {
	if (typeof value === 'number') return [value];
	if (value instanceof Set) return [...value].flatMap(numericValues);
	if (Array.isArray(value)) return value.flatMap(numericValues);
	if (value !== null && typeof value === 'object') return Object.values(value).flatMap(numericValues);
	return [];
}

describe('thread channel and message types', () => {
	it('uses the expected channel type numbers', () => {
		expect(ChannelTypes.ANNOUNCEMENT_THREAD).toBe(10);
		expect(ChannelTypes.PUBLIC_THREAD).toBe(11);
		expect(ChannelTypes.PRIVATE_THREAD).toBe(12);
		expect(ChannelTypes.GUILD_FORUM).toBe(15);
		expect(ChannelTypes.GUILD_MEDIA).toBe(16);
		expect(MessageTypes.THREAD_CREATED).toBe(18);
		expect(MessageTypes.THREAD_STARTER_MESSAGE).toBe(21);
	});

	it('keeps the text type sets unchanged', () => {
		expect([...GUILD_TEXT_BASED_CHANNEL_TYPES].sort((a, b) => a - b)).toEqual([
			ChannelTypes.GUILD_TEXT,
			ChannelTypes.GUILD_VOICE,
			ChannelTypes.GUILD_ANNOUNCEMENT,
		]);
		expect([...TEXT_BASED_CHANNEL_TYPES].sort((a, b) => a - b)).toEqual([0, 1, 2, 3, 5, 999]);
		for (const type of THREAD_FEATURE_CHANNEL_TYPES) {
			expect(TEXT_BASED_CHANNEL_TYPES.has(type)).toBe(false);
		}
	});

	it('makes announcement channels thread parents of announcement threads', () => {
		expect(THREAD_CHANNEL_TYPES.has(ChannelTypes.ANNOUNCEMENT_THREAD)).toBe(true);
		expect(THREAD_PARENT_CHANNEL_TYPES.has(ChannelTypes.GUILD_ANNOUNCEMENT)).toBe(true);
		expect(THREAD_ONLY_CHANNEL_TYPES.has(ChannelTypes.GUILD_ANNOUNCEMENT)).toBe(false);
		expect(publicThreadTypeFor(ChannelTypes.GUILD_ANNOUNCEMENT)).toBe(ChannelTypes.ANNOUNCEMENT_THREAD);
		expect(publicThreadTypeFor(ChannelTypes.GUILD_TEXT)).toBe(ChannelTypes.PUBLIC_THREAD);
		expect(publicThreadTypeFor(ChannelTypes.GUILD_FORUM)).toBe(ChannelTypes.PUBLIC_THREAD);
	});

	it('resolves the thread type a text or announcement parent creates', () => {
		const {GUILD_TEXT, GUILD_ANNOUNCEMENT, GUILD_FORUM, ANNOUNCEMENT_THREAD, PUBLIC_THREAD, PRIVATE_THREAD} =
			ChannelTypes;
		expect(resolveTextThreadType(GUILD_ANNOUNCEMENT, ANNOUNCEMENT_THREAD)).toBe(ANNOUNCEMENT_THREAD);
		expect(resolveTextThreadType(GUILD_ANNOUNCEMENT, PUBLIC_THREAD)).toBe(ANNOUNCEMENT_THREAD);
		expect(resolveTextThreadType(GUILD_ANNOUNCEMENT, PRIVATE_THREAD)).toBeNull();
		expect(resolveTextThreadType(GUILD_TEXT, PUBLIC_THREAD)).toBe(PUBLIC_THREAD);
		expect(resolveTextThreadType(GUILD_TEXT, PRIVATE_THREAD)).toBe(PRIVATE_THREAD);
		expect(resolveTextThreadType(GUILD_TEXT, ANNOUNCEMENT_THREAD)).toBeNull();
		expect(resolveTextThreadType(GUILD_FORUM, PUBLIC_THREAD)).toBeNull();
	});

	it('lets thread created messages be deleted but never the synthesized starter', () => {
		expect(isMessageTypeDeletable(MessageTypes.THREAD_CREATED)).toBe(true);
		expect(isMessageTypeDeletable(MessageTypes.THREAD_STARTER_MESSAGE)).toBe(false);
	});

	it('masks both server-only message flags', () => {
		expect(THREAD_MESSAGE_FLAG_MASK).toBe(0x120);
		expect(THREAD_MESSAGE_FLAG_MASK & ChannelConstants.SENDABLE_MESSAGE_FLAGS).toBe(0);
	});
});

describe('thread constants', () => {
	it('keeps every numeric value inside a non-negative int32', () => {
		for (const value of numericValues(ThreadConstants)) {
			expect(value).toBeGreaterThanOrEqual(0);
			expect(value).toBeLessThan(2 ** 31);
		}
	});

	it('accepts only the supported auto archive durations', () => {
		for (const duration of [60, 1440, 4320, 10080]) {
			expect(isThreadAutoArchiveDuration(duration)).toBe(true);
		}
		for (const duration of [0, 59, 61, 10081]) {
			expect(isThreadAutoArchiveDuration(duration)).toBe(false);
		}
	});

	it('allows at most one notification bit on thread member settings', () => {
		expect(isValidThreadMemberSettingsFlags(0)).toBe(true);
		expect(isValidThreadMemberSettingsFlags(ThreadMemberFlags.ALL_MESSAGES)).toBe(true);
		expect(isValidThreadMemberSettingsFlags(ThreadMemberFlags.NO_MESSAGES)).toBe(true);
		expect(isValidThreadMemberSettingsFlags(ThreadMemberFlags.ALL_MESSAGES | ThreadMemberFlags.ONLY_MENTIONS)).toBe(
			false,
		);
		expect(isValidThreadMemberSettingsFlags(ThreadMemberFlags.HAS_INTERACTED)).toBe(false);
		expect(isValidThreadMemberSettingsFlags(1 << 4)).toBe(false);
		expect(isValidThreadMemberSettingsFlags(-1)).toBe(false);
		expect(isValidThreadMemberSettingsFlags(2 ** 32 + 2)).toBe(false);
		expect(isValidThreadMemberSettingsFlags(2 ** 32)).toBe(false);
	});

	it('limits settable channel flags by type', () => {
		expect(settableChannelFlags(ChannelTypes.GUILD_TEXT)).toBe(0);
		expect(settableChannelFlags(ChannelTypes.PRIVATE_THREAD)).toBe(0);
		expect(settableChannelFlags(ChannelTypes.PUBLIC_THREAD)).toBe(0);
		expect(settableChannelFlags(ChannelTypes.PUBLIC_THREAD, ChannelTypes.GUILD_TEXT)).toBe(0);
		expect(settableChannelFlags(ChannelTypes.PUBLIC_THREAD, ChannelTypes.GUILD_FORUM)).toBe(ChannelFlags.PINNED);
		expect(settableChannelFlags(ChannelTypes.PUBLIC_THREAD, ChannelTypes.GUILD_MEDIA)).toBe(ChannelFlags.PINNED);
		expect(settableChannelFlags(ChannelTypes.PRIVATE_THREAD, ChannelTypes.GUILD_TEXT)).toBe(0);
		expect(settableChannelFlags(ChannelTypes.ANNOUNCEMENT_THREAD, ChannelTypes.GUILD_ANNOUNCEMENT)).toBe(0);
		expect(settableChannelFlags(ChannelTypes.GUILD_FORUM)).toBe(ChannelFlags.REQUIRE_TAG);
		expect(settableChannelFlags(ChannelTypes.GUILD_MEDIA)).toBe(
			ChannelFlags.REQUIRE_TAG | ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS,
		);
	});
});
