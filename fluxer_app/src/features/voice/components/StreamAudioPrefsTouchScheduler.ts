// SPDX-License-Identifier: AGPL-3.0-or-later

import StreamAudioPrefs from '@app/features/voice/state/StreamAudioPrefs';
import {STREAM_AUDIO_PREFS_TOUCH_INTERVAL_MS} from '@fluxer/constants/src/StreamConstants';

const MAX_STREAM_AUDIO_PREF_TOUCH_STREAMS = 512;

const streamTouchRefs = new Map<string, number>();
let streamTouchIntervalId: number | null = null;

function stopStreamTouchIntervalIfIdle(): void {
	if (streamTouchRefs.size > 0) return;
	if (streamTouchIntervalId === null) return;
	window.clearInterval(streamTouchIntervalId);
	streamTouchIntervalId = null;
}

function sweepStreamAudioPrefsTouches(): void {
	for (const streamKey of streamTouchRefs.keys()) {
		StreamAudioPrefs.touchStream(streamKey);
	}
}

function ensureStreamTouchInterval(): void {
	if (streamTouchIntervalId !== null) return;
	streamTouchIntervalId = window.setInterval(sweepStreamAudioPrefsTouches, STREAM_AUDIO_PREFS_TOUCH_INTERVAL_MS);
}

export function registerStreamAudioPrefsTouch(streamKey: string): () => void {
	const existingRefCount = streamTouchRefs.get(streamKey);
	if (existingRefCount == null && streamTouchRefs.size >= MAX_STREAM_AUDIO_PREF_TOUCH_STREAMS) {
		throw new Error('Stream audio prefs touch stream limit exceeded');
	}
	streamTouchRefs.set(streamKey, (existingRefCount ?? 0) + 1);
	StreamAudioPrefs.touchStream(streamKey);
	ensureStreamTouchInterval();
	let disposed = false;
	return () => {
		if (disposed) return;
		disposed = true;
		const refCount = streamTouchRefs.get(streamKey);
		if (refCount == null) return;
		if (refCount > 1) {
			streamTouchRefs.set(streamKey, refCount - 1);
		} else {
			streamTouchRefs.delete(streamKey);
		}
		stopStreamTouchIntervalIfIdle();
	};
}
