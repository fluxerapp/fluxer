// SPDX-License-Identifier: AGPL-3.0-or-later

import {MUSIC_STATE_MAX_BYTES, type MusicState, type Track} from '@fluxer/music_protocol/src/Types';

const MAX_TEXT_LENGTH = 200;
const MAX_UPCOMING = 25;

const encoder = new TextEncoder();
const decoder = new TextDecoder();

function clip(text: string): string {
	return text.length > MAX_TEXT_LENGTH ? `${text.slice(0, MAX_TEXT_LENGTH - 1)}…` : text;
}

function clipTrack(track: Track): Track {
	return {...track, title: clip(track.title), author: clip(track.author)};
}

export function encodeState(state: MusicState): Uint8Array {
	const trimmed: MusicState = {
		...state,
		current: state.current ? clipTrack(state.current) : null,
		upcoming: state.upcoming.slice(0, MAX_UPCOMING).map(clipTrack),
	};
	let bytes = encoder.encode(JSON.stringify(trimmed));
	while (bytes.byteLength > MUSIC_STATE_MAX_BYTES && trimmed.upcoming.length > 0) {
		trimmed.upcoming.pop();
		bytes = encoder.encode(JSON.stringify(trimmed));
	}
	return bytes;
}

export function decodeState(bytes: Uint8Array): MusicState | null {
	try {
		const parsed: unknown = JSON.parse(decoder.decode(bytes));
		if (typeof parsed !== 'object' || parsed === null || (parsed as {v?: unknown}).v !== 1) return null;
		return parsed as MusicState;
	} catch {
		return null;
	}
}

export function livePositionMs(state: MusicState, now: number): number {
	if (state.status !== 'playing') return state.positionMs;
	const position = state.positionMs + Math.max(0, now - state.positionAt);
	return state.current ? Math.min(position, state.current.durationMs) : position;
}
