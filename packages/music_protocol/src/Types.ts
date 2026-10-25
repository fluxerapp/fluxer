// SPDX-License-Identifier: AGPL-3.0-or-later

export const MUSIC_STATE_TOPIC = 'music.state';
export const MUSIC_STATE_MAX_BYTES = 14336;

export type TrackSource = 'youtube' | 'spotify';

export interface SearchResult {
	title: string;
	author: string;
	durationMs: number;
	thumbnailUrl: string | null;
	url: string;
	source: TrackSource;
}

export interface Track extends SearchResult {
	id: string;
	addedBy: string;
}

export type PlayerStatus = 'idle' | 'loading' | 'playing' | 'paused';
export type LoopMode = 'off' | 'track' | 'queue';

export type ActionKind =
	| 'add'
	| 'pause'
	| 'resume'
	| 'skip'
	| 'seek'
	| 'remove'
	| 'move'
	| 'clear'
	| 'shuffle'
	| 'loop'
	| 'stop'
	| 'failed'
	| 'unmatched'
	| 'blocked';

export interface LastAction {
	kind: ActionKind;
	/** User id, or null when the bot itself did it (failures). */
	by: string | null;
	at: number;
	trackTitle: string | null;
}

export interface MusicState {
	v: 1;
	status: PlayerStatus;
	current: Track | null;
	positionMs: number;
	positionAt: number;
	loop: LoopMode;
	upcoming: Array<Track>;
	/** Number of tracks after `current`. */
	queueLength: number;
	lastAction: LastAction | null;
}

export type MusicErrorCode =
	| 'NOT_FOUND'
	| 'QUEUE_FULL'
	| 'TOO_LONG'
	| 'LIVE_NOT_SUPPORTED'
	| 'NO_RESULTS'
	| 'SOURCE_FAILED'
	| 'BAD_REQUEST';
