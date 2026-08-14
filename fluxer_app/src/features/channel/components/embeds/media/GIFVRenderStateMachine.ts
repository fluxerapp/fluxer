// SPDX-License-Identifier: AGPL-3.0-or-later

export const GIFVPosterState = Object.freeze({
	IDLE: 'idle',
	LOADING: 'loading',
	READY: 'ready',
	ERROR: 'error',
} as const);

export type GIFVPosterState = (typeof GIFVPosterState)[keyof typeof GIFVPosterState];

export const GIFVVideoState = Object.freeze({
	IDLE: 'idle',
	LOADING: 'loading',
	READY: 'ready',
	PRESENTED: 'presented',
	ERROR: 'error',
} as const);

export type GIFVVideoState = (typeof GIFVVideoState)[keyof typeof GIFVVideoState];

export const GIFVPlaybackState = Object.freeze({
	IDLE: 'idle',
	REQUESTED: 'requested',
	PLAYING: 'playing',
	PAUSED: 'paused',
	BLOCKED: 'blocked',
} as const);

export type GIFVPlaybackState = (typeof GIFVPlaybackState)[keyof typeof GIFVPlaybackState];

export const GIFVRenderEventType = Object.freeze({
	SOURCE_CHANGED: 'SOURCE_CHANGED',
	POSTER_LOAD_STARTED: 'POSTER_LOAD_STARTED',
	POSTER_LOADED: 'POSTER_LOADED',
	POSTER_FAILED: 'POSTER_FAILED',
	VIDEO_LOAD_STARTED: 'VIDEO_LOAD_STARTED',
	VIDEO_READY: 'VIDEO_READY',
	VIDEO_FRAME_PRESENTED: 'VIDEO_FRAME_PRESENTED',
	VIDEO_FAILED: 'VIDEO_FAILED',
	PLAY_REQUESTED: 'PLAY_REQUESTED',
	PLAYING: 'PLAYING',
	PLAYBACK_PAUSED: 'PLAYBACK_PAUSED',
	PLAYBACK_BLOCKED: 'PLAYBACK_BLOCKED',
} as const);

export type GIFVRenderEventType = (typeof GIFVRenderEventType)[keyof typeof GIFVRenderEventType];

export interface GIFVRenderState {
	sourceKey: string;
	poster: GIFVPosterState;
	video: GIFVVideoState;
	playback: GIFVPlaybackState;
}

export interface GIFVSourceChangedEvent {
	type: typeof GIFVRenderEventType.SOURCE_CHANGED;
	sourceKey: string;
	posterCached: boolean;
}

export type GIFVRenderEvent =
	| GIFVSourceChangedEvent
	| {type: typeof GIFVRenderEventType.POSTER_LOAD_STARTED}
	| {type: typeof GIFVRenderEventType.POSTER_LOADED}
	| {type: typeof GIFVRenderEventType.POSTER_FAILED}
	| {type: typeof GIFVRenderEventType.VIDEO_LOAD_STARTED}
	| {type: typeof GIFVRenderEventType.VIDEO_READY}
	| {type: typeof GIFVRenderEventType.VIDEO_FRAME_PRESENTED}
	| {type: typeof GIFVRenderEventType.VIDEO_FAILED}
	| {type: typeof GIFVRenderEventType.PLAY_REQUESTED}
	| {type: typeof GIFVRenderEventType.PLAYING}
	| {type: typeof GIFVRenderEventType.PLAYBACK_PAUSED}
	| {type: typeof GIFVRenderEventType.PLAYBACK_BLOCKED};

export interface GIFVRenderLayers {
	showPlaceholder: boolean;
	showPoster: boolean;
	showVideo: boolean;
}

export interface InitialGIFVRenderStateRequest {
	posterCached: boolean;
	sourceKey: string;
}

export function createInitialGIFVRenderState({
	sourceKey,
	posterCached,
}: InitialGIFVRenderStateRequest): GIFVRenderState {
	let posterState: GIFVPosterState = GIFVPosterState.IDLE;
	if (posterCached) posterState = GIFVPosterState.READY;
	return {
		sourceKey,
		poster: posterState,
		video: GIFVVideoState.IDLE,
		playback: GIFVPlaybackState.IDLE,
	};
}

function reduceGIFVSourceChange(state: GIFVRenderState, event: GIFVSourceChangedEvent): GIFVRenderState {
	if (state.sourceKey !== event.sourceKey) {
		return createInitialGIFVRenderState({
			sourceKey: event.sourceKey,
			posterCached: event.posterCached,
		});
	}
	if (!event.posterCached) return state;
	if (state.poster === GIFVPosterState.READY) return state;
	return {...state, poster: GIFVPosterState.READY};
}

export function reduceGIFVRenderState(state: GIFVRenderState, event: GIFVRenderEvent): GIFVRenderState {
	switch (event.type) {
		case GIFVRenderEventType.SOURCE_CHANGED:
			return reduceGIFVSourceChange(state, event);
		case GIFVRenderEventType.POSTER_LOAD_STARTED:
			if (state.poster === GIFVPosterState.READY) return state;
			return {...state, poster: GIFVPosterState.LOADING};
		case GIFVRenderEventType.POSTER_LOADED:
			if (state.poster === GIFVPosterState.READY) return state;
			return {...state, poster: GIFVPosterState.READY};
		case GIFVRenderEventType.POSTER_FAILED:
			if (state.poster === GIFVPosterState.READY) return state;
			return {...state, poster: GIFVPosterState.ERROR};
		case GIFVRenderEventType.VIDEO_LOAD_STARTED:
			if (state.video === GIFVVideoState.PRESENTED) return state;
			return {...state, video: GIFVVideoState.LOADING};
		case GIFVRenderEventType.VIDEO_READY:
			if (state.video === GIFVVideoState.PRESENTED) return state;
			return {...state, video: GIFVVideoState.READY};
		case GIFVRenderEventType.VIDEO_FRAME_PRESENTED:
			return {...state, video: GIFVVideoState.PRESENTED};
		case GIFVRenderEventType.VIDEO_FAILED:
			return {...state, video: GIFVVideoState.ERROR, playback: GIFVPlaybackState.PAUSED};
		case GIFVRenderEventType.PLAY_REQUESTED:
			return {...state, playback: GIFVPlaybackState.REQUESTED};
		case GIFVRenderEventType.PLAYING:
			return {...state, playback: GIFVPlaybackState.PLAYING};
		case GIFVRenderEventType.PLAYBACK_PAUSED:
			return {...state, playback: GIFVPlaybackState.PAUSED};
		case GIFVRenderEventType.PLAYBACK_BLOCKED:
			return {...state, playback: GIFVPlaybackState.BLOCKED};
	}
}

export function getGIFVRenderLayers(state: GIFVRenderState): GIFVRenderLayers {
	const showVideo = state.video === GIFVVideoState.PRESENTED;
	const showPoster = !showVideo && state.poster === GIFVPosterState.READY;
	return {
		showPlaceholder: !showVideo && !showPoster,
		showPoster,
		showVideo,
	};
}
