// SPDX-License-Identifier: AGPL-3.0-or-later

export interface HiddenMutedChannelVisibilityInput {
	isMuted: boolean;
	isSelected: boolean;
	isConnected: boolean;
	hasVisibleUnread: boolean;
}

export function shouldShowChannelWhenHidingMutedChannels({
	isMuted,
	isSelected,
	isConnected,
	hasVisibleUnread,
}: HiddenMutedChannelVisibilityInput): boolean {
	return isSelected || isConnected || !isMuted || hasVisibleUnread;
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
