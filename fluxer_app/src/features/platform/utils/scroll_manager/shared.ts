// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelMessages} from '@app/features/messaging/state/ChannelMessages';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {type JumpType, JumpTypes} from '@fluxer/constants/src/JumpConstants';
import {compare, extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';

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
export const CENTRE_ALIGNMENT_LIFT = 8;
export const MESSAGE_REVEAL_PADDING = 16;
export const UNREAD_LOAD_TRIGGER_LIFT = 16;
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

export const InitialScrollIntent = Object.freeze({
	SAVED_OFFSET: 'savedOffset',
	UNREAD_BOUNDARY: 'unreadBoundary',
	BOTTOM: 'bottom',
} as const);

export type InitialScrollIntent = (typeof InitialScrollIntent)[keyof typeof InitialScrollIntent];

export const UNREAD_ANCHOR_EXEMPT_CHANNEL_TYPES: ReadonlySet<number> = new Set<number>([ChannelTypes.GUILD_VOICE]);

export interface InitialScrollIntentInput {
	readonly channelType: number | null | undefined;
	readonly rememberedScrollTop: number | null;
	readonly hasPendingUnreads: boolean;
}

export function resolveInitialScrollIntent(input: InitialScrollIntentInput): InitialScrollIntent {
	if (input.hasPendingUnreads && !UNREAD_ANCHOR_EXEMPT_CHANNEL_TYPES.has(input.channelType ?? -1)) {
		return InitialScrollIntent.UNREAD_BOUNDARY;
	}
	return input.rememberedScrollTop != null ? InitialScrollIntent.SAVED_OFFSET : InitialScrollIntent.BOTTOM;
}

export enum ScrollRegion {
	None = 0,
	Top = 1,
	Bottom = 2,
}

export function shouldAnimateMessageJump(jumpType: JumpType): boolean {
	return jumpType === JumpTypes.ANIMATED;
}

export const JumpPreSnap = Object.freeze({
	NONE: 'none',
	START: 'start',
	END: 'end',
} as const);

export type JumpPreSnap = (typeof JumpPreSnap)[keyof typeof JumpPreSnap];

export interface JumpPreSnapInput {
	readonly targetId: string;
	readonly fromTimestamp: number | null;
	readonly animate: boolean;
	readonly alreadyJumping: boolean;
	readonly reducedMotion: boolean;
}

export function resolveJumpPreSnap(input: JumpPreSnapInput): JumpPreSnap {
	if (input.alreadyJumping) return JumpPreSnap.NONE;
	if (!input.animate) return JumpPreSnap.NONE;
	if (input.fromTimestamp == null) return JumpPreSnap.NONE;
	if (input.reducedMotion) return JumpPreSnap.NONE;
	return extractTimestamp(input.targetId) > input.fromTimestamp ? JumpPreSnap.START : JumpPreSnap.END;
}

export function resolveJumpPreSnapOrigin(previous: ChannelMessages, next: ChannelMessages): number | null {
	const previousOldest = previous.first();
	if (previousOldest == null) return null;
	if (next.last()?.id === previous.last()?.id) return null;
	if (next.first()?.id === previous.first()?.id) return null;
	return extractTimestamp(previousOldest.id);
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
