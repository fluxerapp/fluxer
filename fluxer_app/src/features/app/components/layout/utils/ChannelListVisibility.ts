// SPDX-License-Identifier: AGPL-3.0-or-later

export interface HiddenMutedChannelVisibilityInput {
	isCategoryMuted: boolean;
	isChannelMuted: boolean;
	isSelected: boolean;
	isConnected: boolean;
	hasVisibleUnread: boolean;
}

export function shouldShowChannelWhenHidingMutedChannels({
	isCategoryMuted,
	isChannelMuted,
	isSelected,
	isConnected,
	hasVisibleUnread,
}: HiddenMutedChannelVisibilityInput): boolean {
	return isSelected || isConnected || !(isCategoryMuted || isChannelMuted) || hasVisibleUnread;
}

export interface HiddenMutedCategoryVisibilityInput {
	hasChannels: boolean;
	hasVisibleChannels: boolean;
}

export function shouldShowCategoryWhenHidingMutedChannels({
	hasChannels,
	hasVisibleChannels,
}: HiddenMutedCategoryVisibilityInput): boolean {
	return !hasChannels || hasVisibleChannels;
}

export interface CollapsedCategoryChannelVisibilityInput {
	isSelected: boolean;
	isConnected: boolean;
	hasVisibleUnread: boolean;
}

export function shouldShowChannelInCollapsedCategory({
	isSelected,
	isConnected,
	hasVisibleUnread,
}: CollapsedCategoryChannelVisibilityInput): boolean {
	return isSelected || isConnected || hasVisibleUnread;
}
