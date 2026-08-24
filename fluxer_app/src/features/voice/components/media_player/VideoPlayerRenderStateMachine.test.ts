// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVideoPlayerRenderSnapshot,
	selectVideoPlayerPlayPauseIndicator,
	selectVideoPlayerRenderModel,
	transitionVideoPlayerRenderSnapshot,
	type VideoPlayerRenderSignals,
	type VideoPlayerRenderSnapshot,
} from '@app/features/voice/components/media_player/VideoPlayerRenderStateMachine';
import {describe, expect, it} from 'vitest';

function signals(overrides: Partial<VideoPlayerRenderSignals> = {}): VideoPlayerRenderSignals {
	return {
		autoPlay: false,
		hasPlayed: false,
		wantsMetadata: false,
		isPlaying: false,
		isPaused: true,
		isEnded: false,
		hasError: false,
		...overrides,
	};
}

function observePlayback(
	snapshot: VideoPlayerRenderSnapshot,
	hasPlayed: boolean,
	isPlaying: boolean,
): VideoPlayerRenderSnapshot {
	return transitionVideoPlayerRenderSnapshot(snapshot, {
		type: 'video.observePlayback',
		signals: {hasPlayed, isPlaying},
	});
}

describe('VideoPlayerRenderStateMachine', () => {
	it('keeps source detached and shows the poster before non-autoplay playback starts', () => {
		const model = selectVideoPlayerRenderModel(signals());

		expect(model.renderState).toBe('poster');
		expect(model.shouldAttachSource).toBe(false);
		expect(model.shouldHideVideo).toBe(true);
		expect(model.shouldShowPosterOverlay).toBe(true);
		expect(model.shouldShowControlsOverlay).toBe(false);
	});

	it('attaches source and controls after the initial play intent', () => {
		const model = selectVideoPlayerRenderModel(signals({hasPlayed: true}));

		expect(model.renderState).toBe('paused');
		expect(model.shouldAttachSource).toBe(true);
		expect(model.shouldHideVideo).toBe(false);
		expect(model.shouldShowPosterOverlay).toBe(false);
		expect(model.shouldShowControlsOverlay).toBe(true);
	});

	it('matches the existing autoplay prop branch before a played signal is recorded', () => {
		const model = selectVideoPlayerRenderModel(signals({autoPlay: true}));

		expect(model.renderState).toBe('paused');
		expect(model.shouldAttachSource).toBe(false);
		expect(model.shouldHideVideo).toBe(true);
		expect(model.shouldShowPosterOverlay).toBe(false);
		expect(model.shouldShowControlsOverlay).toBe(true);
	});

	it('prioritizes ended and error render states over base playback flags', () => {
		expect(selectVideoPlayerRenderModel(signals({hasPlayed: true, isPlaying: true})).renderState).toBe('playing');
		expect(selectVideoPlayerRenderModel(signals({hasPlayed: true, isPaused: true, isEnded: true})).renderState).toBe(
			'ended',
		);
		const errorModel = selectVideoPlayerRenderModel(signals({hasPlayed: true, isPlaying: true, hasError: true}));
		expect(errorModel.renderState).toBe('error');
	});

	it('does not emit a play/pause indicator for the first playback observation', () => {
		const snapshot = observePlayback(createVideoPlayerRenderSnapshot(), true, false);

		expect(selectVideoPlayerPlayPauseIndicator(snapshot)).toBeNull();
	});

	it('emits play and pause indicators only after playback has started', () => {
		let snapshot = createVideoPlayerRenderSnapshot();
		snapshot = observePlayback(snapshot, true, false);
		snapshot = observePlayback(snapshot, true, true);
		expect(selectVideoPlayerPlayPauseIndicator(snapshot)).toBe('play');

		snapshot = observePlayback(snapshot, true, false);
		expect(selectVideoPlayerPlayPauseIndicator(snapshot)).toBe('pause');
	});

	it('updates the previous playback value without flashing before first play', () => {
		let snapshot = createVideoPlayerRenderSnapshot();
		snapshot = observePlayback(snapshot, false, false);
		snapshot = observePlayback(snapshot, false, true);
		expect(selectVideoPlayerPlayPauseIndicator(snapshot)).toBeNull();

		snapshot = observePlayback(snapshot, true, true);
		expect(selectVideoPlayerPlayPauseIndicator(snapshot)).toBeNull();
	});
});

describe('video player preload rungs', () => {
	it('holds a resting player at the bottom rung with nothing attached', () => {
		const model = selectVideoPlayerRenderModel(signals());

		expect(model.shouldAttachSource).toBe(false);
		expect(model.preloadAttribute).toBe('none');
	});

	it('climbs to the metadata rung on interest without committing to playback', () => {
		const model = selectVideoPlayerRenderModel(signals({wantsMetadata: true}));

		expect(model.shouldAttachSource).toBe(true);
		expect(model.preloadAttribute).toBe('metadata');
		expect(model.shouldHideVideo).toBe(true);
		expect(model.shouldShowPosterOverlay).toBe(true);
	});

	it('hands the element over to the browser once playback has started', () => {
		const model = selectVideoPlayerRenderModel(signals({hasPlayed: true, wantsMetadata: true}));

		expect(model.shouldAttachSource).toBe(true);
		expect(model.preloadAttribute).toBeUndefined();
	});

	it('does not climb back down when playback pauses', () => {
		const model = selectVideoPlayerRenderModel(
			signals({hasPlayed: true, wantsMetadata: true, isPlaying: false, isPaused: true}),
		);

		expect(model.shouldAttachSource).toBe(true);
		expect(model.preloadAttribute).toBeUndefined();
	});

	it('keeps an autoplaying player off the metadata rung, since it goes straight to playback', () => {
		const resting = selectVideoPlayerRenderModel(signals({autoPlay: true}));
		const playing = selectVideoPlayerRenderModel(signals({autoPlay: true, hasPlayed: true}));

		expect(resting.preloadAttribute).toBe('none');
		expect(playing.preloadAttribute).toBeUndefined();
	});

	it('separates the three rungs from one another', () => {
		const rungs = [
			selectVideoPlayerRenderModel(signals()).preloadAttribute,
			selectVideoPlayerRenderModel(signals({wantsMetadata: true})).preloadAttribute,
			selectVideoPlayerRenderModel(signals({hasPlayed: true})).preloadAttribute,
		];

		expect(rungs).toEqual(['none', 'metadata', undefined]);
	});
});
