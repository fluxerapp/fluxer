// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {JumpTypes} from '@fluxer/constants/src/JumpConstants';
import {NEW_MESSAGES_BAR_BUFFER} from '@fluxer/constants/src/LimitConstants';
import {describe, expect, it} from 'vitest';
import {
	CENTRE_ALIGNMENT_LIFT,
	InitialScrollIntent,
	MESSAGE_REVEAL_PADDING,
	resolveContainerResizeShift,
	resolveInitialScrollIntent,
	type ScrollerState,
	shouldAnimateMessageJump,
} from './shared';

describe('the scroll geometry constants', () => {
	it('lifts a centred node by exactly eight pixels', () => {
		expect(CENTRE_ALIGNMENT_LIFT).toBe(8);
	});

	it('keeps the two centre paddings distinct, with the unread buffer the larger of the two', () => {
		expect(MESSAGE_REVEAL_PADDING).toBe(16);
		expect(NEW_MESSAGES_BAR_BUFFER).toBe(32);
		expect(NEW_MESSAGES_BAR_BUFFER).toBeGreaterThan(MESSAGE_REVEAL_PADDING);
	});
});

describe('shouldAnimateMessageJump', () => {
	it('animates only for explicit ANIMATED jumps', () => {
		expect(shouldAnimateMessageJump(JumpTypes.ANIMATED)).toBe(true);
		expect(shouldAnimateMessageJump(JumpTypes.INSTANT)).toBe(false);
		expect(shouldAnimateMessageJump(JumpTypes.NONE)).toBe(false);
	});
});

describe('resolveContainerResizeShift', () => {
	const state = (scrollTop: number, offsetHeight: number, scrollHeight: number): ScrollerState => ({
		scrollTop,
		offsetHeight,
		scrollHeight,
	});
	const resolve = (
		heightDelta: number,
		scrollerState: ScrollerState,
		overrides?: {isPinned?: boolean; editIsActive?: boolean},
	) =>
		resolveContainerResizeShift({
			heightDelta,
			isPinned: overrides?.isPinned ?? false,
			editIsActive: overrides?.editIsActive ?? false,
			state: scrollerState,
		});

	it('does nothing when the container height is unchanged', () => {
		expect(resolve(0, state(500, 400, 2000), {isPinned: true})).toEqual({kind: 'none'});
	});

	it('re-pins to the bottom when pinned, regardless of edit state or direction', () => {
		expect(resolve(40, state(1600, 400, 2000), {isPinned: true})).toEqual({kind: 'pin'});
		expect(resolve(-40, state(1600, 400, 2000), {isPinned: true})).toEqual({kind: 'pin'});
		expect(resolve(40, state(1600, 400, 2000), {isPinned: true, editIsActive: true})).toEqual({kind: 'pin'});
	});

	it('does nothing while an inline edit is active and not pinned', () => {
		expect(resolve(40, state(1590, 400, 2000), {editIsActive: true})).toEqual({kind: 'none'});
	});

	it('shifts by the delta when near the bottom so the bottom edge stays stable', () => {
		expect(resolve(40, state(1540, 400, 2000))).toEqual({kind: 'shift', targetScrollTop: 1580});
		expect(resolve(-40, state(1590, 400, 2040))).toEqual({kind: 'shift', targetScrollTop: 1550});
	});

	it('clamps the shifted target into the scrollable range', () => {
		expect(resolve(100, state(1590, 400, 2000))).toEqual({kind: 'shift', targetScrollTop: 1600});
		expect(resolve(-100, state(30, 400, 480))).toEqual({kind: 'shift', targetScrollTop: 0});
	});

	it('does nothing when far from the bottom', () => {
		expect(resolve(40, state(100, 400, 2000))).toEqual({kind: 'none'});
		expect(resolve(-40, state(100, 400, 2000))).toEqual({kind: 'none'});
	});

	it('widens the stick threshold for large deltas', () => {
		expect(resolve(200, state(1400, 400, 2000))).toEqual({kind: 'shift', targetScrollTop: 1600});
		expect(resolve(10, state(1400, 400, 2000))).toEqual({kind: 'none'});
	});
});

describe('resolveInitialScrollIntent', () => {
	it('anchors to the unread boundary only when the divider is expected on screen', () => {
		expect(
			resolveInitialScrollIntent({
				channelType: ChannelTypes.GUILD_TEXT,
				rememberedScrollTop: 900,
				hasPendingUnreads: true,
			}),
		).toBe(InitialScrollIntent.UNREAD_BOUNDARY);
		expect(
			resolveInitialScrollIntent({
				channelType: ChannelTypes.GUILD_TEXT,
				rememberedScrollTop: 900,
				hasPendingUnreads: false,
			}),
		).toBe(InitialScrollIntent.SAVED_OFFSET);
	});

	it('falls back to the bottom rather than the top when there is no unread boundary and no saved offset', () => {
		expect(
			resolveInitialScrollIntent({
				channelType: ChannelTypes.GUILD_TEXT,
				rememberedScrollTop: null,
				hasPendingUnreads: false,
			}),
		).toBe(InitialScrollIntent.BOTTOM);
	});

	it('never anchors to the unread boundary in voice channels', () => {
		expect(
			resolveInitialScrollIntent({
				channelType: ChannelTypes.GUILD_VOICE,
				rememberedScrollTop: null,
				hasPendingUnreads: true,
			}),
		).toBe(InitialScrollIntent.BOTTOM);
	});
});
