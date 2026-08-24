// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelMessages} from '@app/features/messaging/state/ChannelMessages';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import {type JumpType, JumpTypes} from '@fluxer/constants/src/JumpConstants';
import {compare} from '@fluxer/snowflake/src/SnowflakeUtils';

export type ScrollerRef = React.RefObject<ScrollerHandle | null> | React.RefObject<ScrollerHandle>;
export type DebouncedFunction<T> = T extends (...args: infer P) => infer R
	? {
			(...args: P): R;
			cancel(): void;
			flush(): void;
		}
	: never;

export interface AnchorData {
	id: string;
	offsetFromTop: number;
	offsetTop: number;
	offsetHeight: number;
	clamped: boolean;
}

export interface ScrollerState {
	scrollTop: number;
	scrollHeight: number;
	offsetHeight: number;
}

export const DEFAULT_SCROLLER_STATE: ScrollerState = {
	scrollTop: 0,
	scrollHeight: 0,
	offsetHeight: 0,
};
export const BOTTOM_LOCK_TOLERANCE = 8;
export const RESIZE_STICK_MIN_THRESHOLD = 64;

export type ContainerResizeShift = {kind: 'none'} | {kind: 'pin'} | {kind: 'shift'; targetScrollTop: number};

export function resolveContainerResizeShift(options: {
	heightDelta: number;
	isPinned: boolean;
	editIsActive: boolean;
	state: ScrollerState;
}): ContainerResizeShift {
	const {heightDelta, isPinned, editIsActive, state} = options;
	if (heightDelta === 0) {
		return {kind: 'none'};
	}
	if (isPinned) {
		return {kind: 'pin'};
	}
	if (editIsActive) {
		return {kind: 'none'};
	}
	const distanceFromBottom = Math.max(state.scrollHeight - state.offsetHeight - state.scrollTop, 0);
	const stickThreshold = Math.max(Math.abs(heightDelta) + BOTTOM_LOCK_TOLERANCE, RESIZE_STICK_MIN_THRESHOLD);
	if (distanceFromBottom > stickThreshold) {
		return {kind: 'none'};
	}
	const maxScrollTop = Math.max(0, state.scrollHeight - state.offsetHeight);
	const targetScrollTop = Math.max(0, Math.min(state.scrollTop + heightDelta, maxScrollTop));
	return {kind: 'shift', targetScrollTop};
}

export enum ScrollRegion {
	None = 0,
	Top = 1,
	Bottom = 2,
}

export function shouldAnimateMessageJump(jumpType: JumpType): boolean {
	return jumpType === JumpTypes.ANIMATED;
}

export function resolveJumpTargetId(messages: ChannelMessages): string | null {
	const {jumpDestinationId, jumpDestinationOffset} = messages;
	if (!jumpDestinationId || !messages.ready) return null;
	if (messages.has(jumpDestinationId) || (!messages.hasMoreBefore && jumpDestinationId === messages.channelId)) {
		if (jumpDestinationOffset === 0) {
			return jumpDestinationId;
		}
		const index = messages.indexOf(jumpDestinationId);
		const targetMessage = messages.atIndex(index + jumpDestinationOffset);
		return targetMessage?.id ?? jumpDestinationId;
	}
	const allIds = [jumpDestinationId, ...messages.map((m) => m.id)].sort(compare);
	const jumpIndex = allIds.indexOf(jumpDestinationId);
	const offset = Math.abs(jumpDestinationOffset) > 0 ? jumpDestinationOffset : 1;
	const closestId = allIds[jumpIndex + offset] ?? allIds[jumpIndex - 1];
	return closestId ?? null;
}
