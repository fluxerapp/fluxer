// SPDX-License-Identifier: AGPL-3.0-or-later

import {screenShareWatchAttemptKey} from '@app/features/voice/components/useScreenShareWatchFailure';
import {
	createVoiceMediaGraphSnapshot,
	selectVoiceMediaGraphAttempt,
	selectVoiceMediaGraphDeadline,
	transitionVoiceMediaGraph,
	type VoiceMediaGraphSnapshot,
	voiceMediaGraphAttemptKeyIsOperation,
	voiceMediaGraphWatchAttemptDeadlineKey,
	WATCH_ATTEMPT_TIMEOUT_MS,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {
	selectVoiceMediaGraphStreamTileState,
	type VoiceMediaGraphStreamTileState,
} from '@app/features/voice/engine/VoiceMediaGraphTileState';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {describe, expect, it} from 'vitest';

const STREAM_KEY = 'dm:channel-a:connection-a';
const PARTICIPANT_IDENTITY = 'user_2_connection-a';

function watchAttemptKey(trackSid: string | null): string {
	return screenShareWatchAttemptKey({streamKey: STREAM_KEY, watchGeneration: 1, trackSid, operationKey: null});
}

describe('screenShareWatchAttemptKey', () => {
	it('gives a republished track its own attempt key', () => {
		expect(watchAttemptKey('TR_2')).not.toBe(watchAttemptKey('TR_1'));
	});

	it('puts the stream, the watch generation and the published track in the key', () => {
		expect(watchAttemptKey('TR_1')).toBe(`${STREAM_KEY}:1:TR_1:watch`);
	});

	it('separates a stream with no published track from one with a track', () => {
		expect(watchAttemptKey(null)).not.toBe(watchAttemptKey('TR_1'));
	});

	it('marks a republish buffering attempt as an operation attempt', () => {
		const buffering = screenShareWatchAttemptKey({
			streamKey: STREAM_KEY,
			watchGeneration: 1,
			trackSid: 'TR_1',
			operationKey: 'republish:3',
		});

		expect(voiceMediaGraphAttemptKeyIsOperation(buffering)).toBe(true);
		expect(voiceMediaGraphAttemptKeyIsOperation(watchAttemptKey('TR_1'))).toBe(false);
	});
});

function subscribedScreenShareGraph(): VoiceMediaGraphSnapshot {
	const graph = transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {
		type: 'subscription.subscribe',
		participantIdentity: PARTICIPANT_IDENTITY,
		source: VoiceTrackSource.ScreenShare,
		hasPublication: true,
		observedElement: null,
		context: 'focused',
	});
	return transitionVoiceMediaGraph(graph, {type: 'subscription.clearCommands'});
}

function screenShareTileState(graph: VoiceMediaGraphSnapshot): VoiceMediaGraphStreamTileState {
	return selectVoiceMediaGraphStreamTileState(graph, {
		streamKey: STREAM_KEY,
		participantIdentity: PARTICIPANT_IDENTITY,
		source: VoiceTrackSource.ScreenShare,
	});
}

describe('screen share watch attempts across a republish', () => {
	it('arms a fresh first frame deadline when a new track replaces a rendered one', () => {
		const firstAttemptKey = watchAttemptKey('TR_1');
		let graph = transitionVoiceMediaGraph(subscribedScreenShareGraph(), {
			type: 'watch.started',
			streamKey: STREAM_KEY,
			at: 0,
		});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'publication.observed',
			participantIdentity: PARTICIPANT_IDENTITY,
			source: VoiceTrackSource.ScreenShare,
			trackSid: 'TR_1',
			at: 0,
		});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.attemptEnsured',
			streamKey: STREAM_KEY,
			attemptKey: firstAttemptKey,
			startedAt: 0,
		});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.renderedFrame',
			streamKey: STREAM_KEY,
			attemptKey: firstAttemptKey,
			renderedAt: 500,
		});
		expect(screenShareTileState(graph)).toBe('rendering');

		graph = transitionVoiceMediaGraph(graph, {
			type: 'publication.lost',
			participantIdentity: PARTICIPANT_IDENTITY,
			source: VoiceTrackSource.ScreenShare,
			at: 4000,
		});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'publication.observed',
			participantIdentity: PARTICIPANT_IDENTITY,
			source: VoiceTrackSource.ScreenShare,
			trackSid: 'TR_2',
			at: 4500,
		});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.attemptEnsured',
			streamKey: STREAM_KEY,
			attemptKey: watchAttemptKey('TR_2'),
			startedAt: 4600,
		});

		expect(selectVoiceMediaGraphAttempt(graph, STREAM_KEY)?.hasRenderedVideoFrame).toBe(false);
		expect(screenShareTileState(graph)).not.toBe('rendering');
		expect(selectVoiceMediaGraphDeadline(graph, voiceMediaGraphWatchAttemptDeadlineKey(STREAM_KEY))?.dueAt).toBe(
			4600 + WATCH_ATTEMPT_TIMEOUT_MS,
		);
	});
});
