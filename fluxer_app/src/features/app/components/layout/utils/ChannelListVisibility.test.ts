// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	shouldShowCategoryWhenHidingMutedChannels,
	shouldShowChannelInCollapsedCategory,
	shouldShowChannelWhenHidingMutedChannels,
} from './ChannelListVisibility';

describe('shouldShowChannelWhenHidingMutedChannels', () => {
	it('keeps muted channels with visible unread state so mentions are findable', () => {
		expect(
			shouldShowChannelWhenHidingMutedChannels({
				isMuted: true,
				isSelected: false,
				isConnected: false,
				hasVisibleUnread: true,
			}),
		).toBe(true);
	});

	it('hides muted channels without visible unread state', () => {
		expect(
			shouldShowChannelWhenHidingMutedChannels({
				isMuted: true,
				isSelected: false,
				isConnected: false,
				hasVisibleUnread: false,
			}),
		).toBe(false);
	});

	it('keeps selected and connected muted channels visible', () => {
		expect(
			shouldShowChannelWhenHidingMutedChannels({
				isMuted: true,
				isSelected: true,
				isConnected: false,
				hasVisibleUnread: false,
			}),
		).toBe(true);
		expect(
			shouldShowChannelWhenHidingMutedChannels({
				isMuted: true,
				isSelected: false,
				isConnected: true,
				hasVisibleUnread: false,
			}),
		).toBe(true);
	});

	it('keeps unmuted channels visible', () => {
		expect(
			shouldShowChannelWhenHidingMutedChannels({
				isMuted: false,
				isSelected: false,
				isConnected: false,
				hasVisibleUnread: false,
			}),
		).toBe(true);
	});
});

describe('shouldShowCategoryWhenHidingMutedChannels', () => {
	it('keeps a category that has no channels at all', () => {
		expect(
			shouldShowCategoryWhenHidingMutedChannels({
				hasChannels: false,
				hasVisibleChannels: false,
			}),
		).toBe(true);
	});

	it('hides a category whose channels were all filtered out as muted', () => {
		expect(
			shouldShowCategoryWhenHidingMutedChannels({
				hasChannels: true,
				hasVisibleChannels: false,
			}),
		).toBe(false);
	});

	it('keeps a category that still has visible channels', () => {
		expect(
			shouldShowCategoryWhenHidingMutedChannels({
				hasChannels: true,
				hasVisibleChannels: true,
			}),
		).toBe(true);
	});
});

describe('shouldShowChannelInCollapsedCategory', () => {
	it('keeps visible unread channels reachable in collapsed categories', () => {
		expect(
			shouldShowChannelInCollapsedCategory({
				isSelected: false,
				isConnected: false,
				hasVisibleUnread: true,
			}),
		).toBe(true);
	});

	it('keeps selected and connected channels visible without unread state', () => {
		expect(
			shouldShowChannelInCollapsedCategory({
				isSelected: true,
				isConnected: false,
				hasVisibleUnread: false,
			}),
		).toBe(true);
		expect(
			shouldShowChannelInCollapsedCategory({
				isSelected: false,
				isConnected: true,
				hasVisibleUnread: false,
			}),
		).toBe(true);
	});

	it('hides read channels in collapsed categories', () => {
		expect(
			shouldShowChannelInCollapsedCategory({
				isSelected: false,
				isConnected: false,
				hasVisibleUnread: false,
			}),
		).toBe(false);
	});
});
