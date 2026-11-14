// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ChannelConstants from '@fluxer/constants/src/ChannelConstants';
import {ChannelTypes, isMessageTypeDeletable, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import * as ThreadConstants from '@fluxer/constants/src/ThreadConstants';
import {
	ChannelFlags,
	isValidThreadMemberSettingsFlags,
	resolveTextThreadType,
	settableChannelFlags,
	THREAD_MESSAGE_FLAG_MASK,
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
