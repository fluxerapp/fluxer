// SPDX-License-Identifier: AGPL-3.0-or-later

import {HoldSources} from '@app/features/input/state/HoldSources';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const RELEASE_DELAY_MS = 250;

describe('HoldSources', () => {
	beforeEach(() => {
		vi.useFakeTimers();
	});

	afterEach(() => {
		vi.useRealTimers();
	});

	it('keeps push-to-talk open while a portal source is held after the DOM source is released, for example on window blur', () => {
		const sources = new HoldSources(() => RELEASE_DELAY_MS);
		const onReleased = vi.fn();
		expect(sources.press('voice_push_to_talk', 'custom:a')).toBe(true);
		expect(sources.press('voice_push_to_talk', 'portal:voice_push_to_talk')).toBe(false);
		sources.release('voice_push_to_talk', 'custom:a', onReleased);
		vi.advanceTimersByTime(RELEASE_DELAY_MS * 4);
		expect(onReleased).not.toHaveBeenCalled();
		sources.release('voice_push_to_talk', 'portal:voice_push_to_talk', onReleased);
		vi.advanceTimersByTime(RELEASE_DELAY_MS - 1);
		expect(onReleased).not.toHaveBeenCalled();
		vi.advanceTimersByTime(1);
		expect(onReleased).toHaveBeenCalledTimes(1);
	});

	it('cancels the pending release when a source is pressed again', () => {
		const sources = new HoldSources(() => RELEASE_DELAY_MS);
		const onReleased = vi.fn();
		sources.press('voice_push_to_talk', 'custom:a');
		sources.release('voice_push_to_talk', 'custom:a', onReleased);
		expect(sources.press('voice_push_to_talk_priority', 'custom:b')).toBe(true);
		expect(sources.prioritySpeakerHeld).toBe(true);
		vi.advanceTimersByTime(RELEASE_DELAY_MS * 4);
		expect(onReleased).not.toHaveBeenCalled();
		sources.release('voice_push_to_talk_priority', 'custom:b', onReleased);
		expect(sources.prioritySpeakerHeld).toBe(false);
		vi.advanceTimersByTime(RELEASE_DELAY_MS);
		expect(onReleased).toHaveBeenCalledTimes(1);
	});

	it('ignores releases for sources that were never pressed', () => {
		const sources = new HoldSources(() => RELEASE_DELAY_MS);
		const onReleased = vi.fn();
		sources.release('voice_push_to_talk', 'custom:unknown', onReleased);
		sources.release('voice_push_to_mute', 'custom:unknown', onReleased);
		sources.press('voice_push_to_talk', 'custom:a');
		sources.release('voice_push_to_talk', 'custom:unknown', onReleased);
		vi.advanceTimersByTime(RELEASE_DELAY_MS * 4);
		expect(onReleased).not.toHaveBeenCalled();
	});

	it('clears held sources and the pending release on reset', () => {
		const sources = new HoldSources(() => RELEASE_DELAY_MS);
		const onReleased = vi.fn();
		sources.press('voice_push_to_talk', 'custom:a');
		sources.press('voice_push_to_talk', 'custom:b');
		sources.release('voice_push_to_talk', 'custom:a', onReleased);
		sources.release('voice_push_to_talk', 'custom:b', onReleased);
		sources.resetPushToTalk();
		vi.advanceTimersByTime(RELEASE_DELAY_MS * 4);
		expect(onReleased).not.toHaveBeenCalled();
		sources.press('voice_push_to_talk', 'custom:a');
		sources.resetPushToTalk();
		sources.release('voice_push_to_talk', 'custom:a', onReleased);
		vi.advanceTimersByTime(RELEASE_DELAY_MS * 4);
		expect(onReleased).not.toHaveBeenCalled();
		expect(sources.press('voice_push_to_talk', 'custom:a')).toBe(true);
	});

	it('releases push-to-mute immediately when its last source is released', () => {
		const sources = new HoldSources(() => RELEASE_DELAY_MS);
		const onReleased = vi.fn();
		expect(sources.press('voice_push_to_mute', 'custom:a')).toBe(true);
		expect(sources.press('voice_push_to_mute', 'gamepad:a')).toBe(false);
		sources.release('voice_push_to_mute', 'custom:a', onReleased);
		expect(onReleased).not.toHaveBeenCalled();
		sources.release('voice_push_to_mute', 'gamepad:a', onReleased);
		expect(onReleased).toHaveBeenCalledTimes(1);
		sources.press('voice_push_to_mute', 'custom:a');
		sources.resetPushToMute();
		sources.release('voice_push_to_mute', 'custom:a', onReleased);
		expect(onReleased).toHaveBeenCalledTimes(1);
	});
});
