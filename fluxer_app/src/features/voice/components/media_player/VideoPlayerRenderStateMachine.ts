// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export type VideoPlayerRenderState = 'poster' | 'playing' | 'paused' | 'ended' | 'error';
export type VideoPlayerPlayPauseIndicator = 'play' | 'pause';

export type VideoPlayerPreloadAttribute = 'none' | 'metadata' | undefined;

export interface VideoPlayerRenderSignals {
	autoPlay: boolean;
	hasPlayed: boolean;
	wantsMetadata: boolean;
	isPlaying: boolean;
	isPaused: boolean;
	isEnded: boolean;
	hasError: boolean;
}

export interface VideoPlayerRenderModel {
	renderState: VideoPlayerRenderState;
	shouldAttachSource: boolean;
	preloadAttribute: VideoPlayerPreloadAttribute;
	shouldHideVideo: boolean;
	shouldShowPosterOverlay: boolean;
	shouldShowControlsOverlay: boolean;
}

export interface VideoPlayerPlaybackSignals {
	hasPlayed: boolean;
	isPlaying: boolean;
}

export type VideoPlayerRenderEvent =
	| {type: 'video.evaluateRender'; signals: VideoPlayerRenderSignals}
	| {type: 'video.observePlayback'; signals: VideoPlayerPlaybackSignals};

interface VideoPlayerRenderContext {
	renderSignals: VideoPlayerRenderSignals;
	renderModel: VideoPlayerRenderModel;
	previousIsPlaying: boolean | null;
	playPauseIndicator: VideoPlayerPlayPauseIndicator | null;
}

const DEFAULT_RENDER_SIGNALS: VideoPlayerRenderSignals = {
	autoPlay: false,
	hasPlayed: false,
	wantsMetadata: false,
	isPlaying: false,
	isPaused: true,
	isEnded: false,
	hasError: false,
};

function selectRenderState(signals: VideoPlayerRenderSignals): VideoPlayerRenderState {
	if (signals.hasError) return 'error';
	if (!signals.hasPlayed && !signals.autoPlay) return 'poster';
	if (signals.isEnded) return 'ended';
	if (signals.isPlaying) return 'playing';
	if (signals.isPaused) return 'paused';
	return 'paused';
}

function createRenderModel(signals: VideoPlayerRenderSignals): VideoPlayerRenderModel {
	const renderState = selectRenderState(signals);
	return {
		renderState,
		shouldAttachSource: signals.hasPlayed || signals.wantsMetadata,
		preloadAttribute: signals.hasPlayed ? undefined : signals.wantsMetadata ? 'metadata' : 'none',
		shouldHideVideo: !signals.hasPlayed,
		shouldShowPosterOverlay: !signals.hasPlayed && !signals.autoPlay,
		shouldShowControlsOverlay: signals.hasPlayed || signals.autoPlay,
	};
}

function selectPlayPauseIndicator(
	previousIsPlaying: boolean | null,
	signals: VideoPlayerPlaybackSignals,
): VideoPlayerPlayPauseIndicator | null {
	if (previousIsPlaying === null) return null;
	if (previousIsPlaying === signals.isPlaying) return null;
	if (!signals.hasPlayed) return null;
	return signals.isPlaying ? 'play' : 'pause';
}

const DEFAULT_RENDER_MODEL = createRenderModel(DEFAULT_RENDER_SIGNALS);

export const videoPlayerRenderStateMachine = setup({
	types: {} as {
		context: VideoPlayerRenderContext;
		events: VideoPlayerRenderEvent;
	},
	actions: {
		assignRenderModel: assign(({event}) => {
			if (event.type !== 'video.evaluateRender') return {};
			return {
				renderSignals: event.signals,
				renderModel: createRenderModel(event.signals),
			};
		}),
		assignPlaybackObservation: assign(({context, event}) => {
			if (event.type !== 'video.observePlayback') return {};
			return {
				previousIsPlaying: event.signals.isPlaying,
				playPauseIndicator: selectPlayPauseIndicator(context.previousIsPlaying, event.signals),
			};
		}),
	},
}).createMachine({
	id: 'videoPlayerRender',
	context: () => ({
		renderSignals: DEFAULT_RENDER_SIGNALS,
		renderModel: DEFAULT_RENDER_MODEL,
		previousIsPlaying: null,
		playPauseIndicator: null,
	}),
	initial: 'ready',
	on: {
		'video.evaluateRender': {actions: 'assignRenderModel'},
		'video.observePlayback': {actions: 'assignPlaybackObservation'},
	},
	states: {
		ready: {},
	},
});

export type VideoPlayerRenderSnapshot = SnapshotFrom<typeof videoPlayerRenderStateMachine>;

export function createVideoPlayerRenderSnapshot(): VideoPlayerRenderSnapshot {
	return getInitialSnapshot(videoPlayerRenderStateMachine);
}

export function transitionVideoPlayerRenderSnapshot(
	snapshot: VideoPlayerRenderSnapshot,
	event: VideoPlayerRenderEvent,
): VideoPlayerRenderSnapshot {
	const [nextSnapshot] = transition(videoPlayerRenderStateMachine, snapshot, event);
	return nextSnapshot;
}

export function selectVideoPlayerRenderModel(signals: VideoPlayerRenderSignals): VideoPlayerRenderModel {
	const snapshot = transitionVideoPlayerRenderSnapshot(createVideoPlayerRenderSnapshot(), {
		type: 'video.evaluateRender',
		signals,
	});
	return snapshot.context.renderModel;
}

export function selectVideoPlayerPlayPauseIndicator(
	snapshot: VideoPlayerRenderSnapshot,
): VideoPlayerPlayPauseIndicator | null {
	return snapshot.context.playPauseIndicator;
}
