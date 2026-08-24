// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createMediaControlsVisibilitySnapshot,
	getMediaControlsVisibilityValue,
	type MediaControlsVisibilityEvent,
	type MediaControlsVisibilitySignals,
	type MediaControlsVisibilitySnapshot,
	selectMediaControlsVisible,
	transitionMediaControlsVisibilitySnapshot,
} from '@app/features/voice/components/media_player/MediaControlsVisibilityStateMachine';
import {describe, expect, it} from 'vitest';

function signals(overrides: Partial<MediaControlsVisibilitySignals> = {}): MediaControlsVisibilitySignals {
	return {
		disabled: false,
		isPlaying: true,
		isInteracting: false,
		...overrides,
	};
}

function send(
	snapshot: MediaControlsVisibilitySnapshot,
	event: MediaControlsVisibilityEvent,
): MediaControlsVisibilitySnapshot {
	return transitionMediaControlsVisibilitySnapshot(snapshot, event);
}

describe('MediaControlsVisibilityStateMachine', () => {
	it('starts visible and can be explicitly shown or hidden while media is playing', () => {
		let snapshot = createMediaControlsVisibilitySnapshot();
		expect(getMediaControlsVisibilityValue(snapshot)).toBe('visible');
		expect(selectMediaControlsVisible(snapshot, signals())).toBe(true);

		snapshot = send(snapshot, {type: 'controls.hide'});
		expect(getMediaControlsVisibilityValue(snapshot)).toBe('hidden');
		expect(selectMediaControlsVisible(snapshot, signals())).toBe(false);

		snapshot = send(snapshot, {type: 'controls.show'});
		expect(getMediaControlsVisibilityValue(snapshot)).toBe('visible');
		expect(selectMediaControlsVisible(snapshot, signals())).toBe(true);
	});

	it('keeps controls visible when disabled, paused, or interacting', () => {
		let snapshot = createMediaControlsVisibilitySnapshot();
		snapshot = send(snapshot, {type: 'controls.hide'});

		expect(selectMediaControlsVisible(snapshot, signals({disabled: true}))).toBe(true);
		expect(selectMediaControlsVisible(snapshot, signals({isPlaying: false}))).toBe(true);
		expect(selectMediaControlsVisible(snapshot, signals({isInteracting: true}))).toBe(true);
	});

	it('lets a resting pointer inside the player still lose the controls', () => {
		let snapshot = createMediaControlsVisibilitySnapshot();
		snapshot = send(snapshot, {type: 'controls.mouseEnter'});
		expect(selectMediaControlsVisible(snapshot, signals())).toBe(true);

		snapshot = send(snapshot, {type: 'controls.hide'});

		expect(getMediaControlsVisibilityValue(snapshot)).toBe('hidden');
		expect(selectMediaControlsVisible(snapshot, signals())).toBe(false);
	});

	it('hides on pointer leave only while playback is active and not interacting', () => {
		let snapshot = createMediaControlsVisibilitySnapshot();
		snapshot = send(snapshot, {type: 'controls.mouseLeave', signals: signals({isInteracting: true})});
		expect(getMediaControlsVisibilityValue(snapshot)).toBe('visible');

		snapshot = send(snapshot, {type: 'controls.mouseLeave', signals: signals({isPlaying: false})});
		expect(getMediaControlsVisibilityValue(snapshot)).toBe('visible');

		snapshot = send(snapshot, {type: 'controls.mouseLeave', signals: signals()});
		expect(getMediaControlsVisibilityValue(snapshot)).toBe('hidden');
		expect(selectMediaControlsVisible(snapshot, signals())).toBe(false);
	});

	it('reveals hidden controls on mouse move', () => {
		let snapshot = createMediaControlsVisibilitySnapshot();
		snapshot = send(snapshot, {type: 'controls.hide'});
		snapshot = send(snapshot, {type: 'controls.mouseMove'});

		expect(getMediaControlsVisibilityValue(snapshot)).toBe('visible');
		expect(selectMediaControlsVisible(snapshot, signals())).toBe(true);
	});

	it('toggles on touch while playing and otherwise reveals controls', () => {
		let snapshot = createMediaControlsVisibilitySnapshot();
		snapshot = send(snapshot, {type: 'controls.touchStart', signals: signals()});
		expect(getMediaControlsVisibilityValue(snapshot)).toBe('hidden');

		snapshot = send(snapshot, {type: 'controls.touchStart', signals: signals()});
		expect(getMediaControlsVisibilityValue(snapshot)).toBe('visible');

		snapshot = send(snapshot, {type: 'controls.touchStart', signals: signals({isInteracting: true})});
		expect(getMediaControlsVisibilityValue(snapshot)).toBe('visible');

		snapshot = send(snapshot, {type: 'controls.hide'});
		snapshot = send(snapshot, {type: 'controls.touchStart', signals: signals({isPlaying: false})});
		expect(getMediaControlsVisibilityValue(snapshot)).toBe('visible');
	});
});
