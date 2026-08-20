// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildVoiceMediaGraphNativeCameraQualityCommand,
	buildVoiceMediaGraphNativeCameraSubscriptionCommand,
	buildVoiceMediaGraphNativeScreenShareQualityCommand,
	buildVoiceMediaGraphNativeScreenShareSubscriptionCommands,
	createVoiceMediaGraphSnapshot,
	getVoiceMediaGraphWatchIntentStateValue,
	mergeVoiceMediaGraphTrackInfo,
	normalizeVoiceMediaGraphViewerStreamKeys,
	PUBLICATION_MISSING_TIMEOUT_MS,
	PUBLISHER_REPUBLISH_GRACE_MS,
	selectVoiceMediaGraphAttempt,
	selectVoiceMediaGraphDeadline,
	selectVoiceMediaGraphDeferredStopKeys,
	selectVoiceMediaGraphFailure,
	selectVoiceMediaGraphHasFailureForStreamKey,
	selectVoiceMediaGraphStatsTrackInfo,
	selectVoiceMediaGraphSubscriptionCommands,
	selectVoiceMediaGraphSubscriptionEntry,
	selectVoiceMediaGraphViewerStreamKeys,
	selectVoiceMediaGraphWatchGeneration,
	transitionVoiceMediaGraph,
	transitionVoiceMediaGraphViewerStreamKeys,
	type VoiceMediaGraphDeadline,
	type VoiceMediaGraphFailure,
	type VoiceMediaGraphSnapshot,
	type VoiceMediaGraphStatsTrackObservation,
	voiceMediaGraphDeferredStopDeadlineKey,
	voiceMediaGraphPublicationMissingDeadlineKey,
	voiceMediaGraphWatchAttemptDeadlineKey,
	WATCH_ATTEMPT_TIMEOUT_MS,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {resolveVoiceMediaGraphPerTrackInfo} from '@app/features/voice/engine/VoiceMediaGraphStats';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import type {VoiceEngineV2PerTrackStats} from '@fluxer/voice_engine_v2';
import {describe, expect, it} from 'vitest';

function failure(overrides: Partial<VoiceMediaGraphFailure>): VoiceMediaGraphFailure {
	return {
		code: -2202,
		reason: 'remote-track-subscription-failed',
		reportedAt: 1000,
		source: 'screen_share',
		...overrides,
	};
}

const STREAM_A = 'dm:channel-a:connection-a';
const STREAM_B = 'dm:channel-a:connection-b';
const STREAM_C = 'guild-a:channel-a:connection-c';

describe('VoiceMediaGraph watch generations', () => {
	it('clears stale failures for the same rejoined connection when a watch starts', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'failure.reported',
			failure: failure({participantIdentity: 'user_2_connection_1'}),
		});

		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: 'dm:channel:connection_1'});

		expect(selectVoiceMediaGraphWatchGeneration(graph, 'dm:channel:connection_1')).toBe(1);
		expect(selectVoiceMediaGraphFailure(graph, {participantIdentity: 'user_2_connection_1'})).toBeNull();
	});

	it('scopes attempts to the current attempt key', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.attemptEnsured',
			streamKey: 'dm:channel:connection_1',
			attemptKey: 'attempt-1',
			startedAt: 1000,
		});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.renderedFrame',
			streamKey: 'dm:channel:connection_1',
			attemptKey: 'attempt-2',
			renderedAt: 1100,
		});

		expect(selectVoiceMediaGraphAttempt(graph, 'dm:channel:connection_1')).toEqual({
			attemptKey: 'attempt-1',
			startedAt: 1000,
			hasRenderedVideoFrame: false,
			generation: 0,
		});
	});

	it('stops watches by clearing generation, attempt, and stream failure state', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: 'dm:channel:connection_1'});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'failure.reported',
			failure: failure({streamKey: 'dm:channel:connection_1'}),
		});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.attemptEnsured',
			streamKey: 'dm:channel:connection_1',
			attemptKey: 'attempt-1',
			startedAt: 1000,
		});

		graph = transitionVoiceMediaGraph(graph, {type: 'watch.stopped', streamKey: 'dm:channel:connection_1'});

		expect(selectVoiceMediaGraphWatchGeneration(graph, 'dm:channel:connection_1')).toBe(0);
		expect(selectVoiceMediaGraphAttempt(graph, 'dm:channel:connection_1')).toBeNull();
		expect(selectVoiceMediaGraphHasFailureForStreamKey(graph, 'dm:channel:connection_1')).toBe(false);
	});
});

describe('VoiceMediaGraph watch intent', () => {
	it('normalizes viewer stream keys by dropping empty and duplicate keys', () => {
		expect(normalizeVoiceMediaGraphViewerStreamKeys([STREAM_A, '', STREAM_A, STREAM_B, STREAM_B])).toEqual([
			STREAM_A,
			STREAM_B,
		]);
	});

	it('starts idle and moves to watching when a stream is added', () => {
		let graph = createVoiceMediaGraphSnapshot();
		expect(getVoiceMediaGraphWatchIntentStateValue(graph)).toBe('idle');
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.add', key: STREAM_A});
		expect(getVoiceMediaGraphWatchIntentStateValue(graph)).toBe('watching');
		expect(selectVoiceMediaGraphViewerStreamKeys(graph)).toEqual([STREAM_A]);
	});

	it('keeps add idempotent for the same stream key', () => {
		let graph = transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {type: 'watchIntent.add', key: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.add', key: STREAM_A});
		expect(selectVoiceMediaGraphViewerStreamKeys(graph)).toEqual([STREAM_A]);
	});

	it('replaces the watcher set with canonical order', () => {
		const keys = transitionVoiceMediaGraphViewerStreamKeys([STREAM_A], {
			type: 'watchIntent.replace',
			keys: [STREAM_B, STREAM_A, STREAM_B],
		});
		expect(keys).toEqual([STREAM_B, STREAM_A]);
	});

	it('removes a single stream and returns to idle after the last stream is removed', () => {
		let graph = transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {
			type: 'watchIntent.replace',
			keys: [STREAM_A, STREAM_B],
		});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.remove', key: STREAM_A});
		expect(getVoiceMediaGraphWatchIntentStateValue(graph)).toBe('watching');
		expect(selectVoiceMediaGraphViewerStreamKeys(graph)).toEqual([STREAM_B]);
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.remove', key: STREAM_B});
		expect(getVoiceMediaGraphWatchIntentStateValue(graph)).toBe('idle');
		expect(selectVoiceMediaGraphViewerStreamKeys(graph)).toEqual([]);
	});

	it('removes many inactive streams without disturbing unrelated keys', () => {
		const keys = transitionVoiceMediaGraphViewerStreamKeys([STREAM_A, STREAM_B, STREAM_C], {
			type: 'watchIntent.removeMany',
			keys: [STREAM_A, STREAM_C],
		});
		expect(keys).toEqual([STREAM_B]);
	});

	it('tracks deferred stops only for currently watched streams', () => {
		let graph = transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {type: 'watchIntent.add', key: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.deferRemove', key: STREAM_B});
		expect(selectVoiceMediaGraphDeferredStopKeys(graph).has(STREAM_B)).toBe(false);
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.deferRemove', key: STREAM_A});
		expect(selectVoiceMediaGraphDeferredStopKeys(graph).has(STREAM_A)).toBe(true);
	});

	it('cancels deferred stops when a stream is republished', () => {
		let graph = transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {type: 'watchIntent.add', key: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.deferRemove', key: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.cancelDeferredRemove', key: STREAM_A});
		expect(selectVoiceMediaGraphViewerStreamKeys(graph)).toEqual([STREAM_A]);
		expect(selectVoiceMediaGraphDeferredStopKeys(graph).size).toBe(0);
	});

	it('drops deferred stops when the stream is removed or replaced away', () => {
		let graph = transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {
			type: 'watchIntent.replace',
			keys: [STREAM_A, STREAM_B],
		});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.deferRemove', key: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.remove', key: STREAM_A});
		expect(selectVoiceMediaGraphDeferredStopKeys(graph).has(STREAM_A)).toBe(false);
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.deferRemove', key: STREAM_B});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.replace', keys: [STREAM_C]});
		expect(selectVoiceMediaGraphViewerStreamKeys(graph)).toEqual([STREAM_C]);
		expect(selectVoiceMediaGraphDeferredStopKeys(graph).size).toBe(0);
	});

	it('handles stop, restart, stop cycles without multiplying keys', () => {
		let graph = transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {type: 'watchIntent.add', key: STREAM_A});
		for (let index = 0; index < 50; index += 1) {
			graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.deferRemove', key: STREAM_A});
			graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.cancelDeferredRemove', key: STREAM_A});
			graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.add', key: STREAM_A});
		}
		expect(selectVoiceMediaGraphViewerStreamKeys(graph)).toEqual([STREAM_A]);
		expect(selectVoiceMediaGraphDeferredStopKeys(graph).size).toBe(0);
	});

	it('reset clears watched and deferred streams', () => {
		let graph = transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {type: 'watchIntent.add', key: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.deferRemove', key: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.reset'});
		expect(getVoiceMediaGraphWatchIntentStateValue(graph)).toBe('idle');
		expect(selectVoiceMediaGraphViewerStreamKeys(graph)).toEqual([]);
		expect(selectVoiceMediaGraphDeferredStopKeys(graph).size).toBe(0);
	});
});

describe('VoiceMediaGraph subscription commands', () => {
	it('records source-scoped screen-share subscribe commands in the graph', () => {
		let graph = createVoiceMediaGraphSnapshot();

		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.subscribe',
			participantIdentity: 'user_2_connection_2',
			source: VoiceTrackSource.ScreenShare,
			hasPublication: true,
			observedElement: null,
			context: 'focused',
		});

		expect(
			selectVoiceMediaGraphSubscriptionEntry(graph, 'user_2_connection_2', VoiceTrackSource.ScreenShare),
		).toMatchObject({
			subscribed: true,
			enabled: true,
			quality: 'high',
			context: 'focused',
		});
		expect(selectVoiceMediaGraphSubscriptionCommands(graph)).toEqual([
			{
				type: 'subscribePublication',
				participantIdentity: 'user_2_connection_2',
				source: VoiceTrackSource.ScreenShare,
				enabled: true,
				quality: 'high',
			},
		]);
	});

	it('emits disable and low-quality commands when a screen-share subscription becomes hidden', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.subscribe',
			participantIdentity: 'user_2_connection_2',
			source: VoiceTrackSource.ScreenShare,
			hasPublication: true,
			observedElement: null,
			context: 'focused',
		});
		graph = transitionVoiceMediaGraph(graph, {type: 'subscription.clearCommands'});

		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.setContext',
			participantIdentity: 'user_2_connection_2',
			source: VoiceTrackSource.ScreenShare,
			hasPublication: true,
			context: 'hidden',
		});

		expect(selectVoiceMediaGraphSubscriptionCommands(graph)).toEqual([
			{
				type: 'setPublicationEnabled',
				participantIdentity: 'user_2_connection_2',
				source: VoiceTrackSource.ScreenShare,
				enabled: false,
			},
			{
				type: 'setPublicationQuality',
				participantIdentity: 'user_2_connection_2',
				source: VoiceTrackSource.ScreenShare,
				quality: 'low',
			},
		]);
	});

	it('emits a forced resubscribe command after screen-share republish', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.subscribe',
			participantIdentity: 'user_2_connection_2',
			source: VoiceTrackSource.ScreenShare,
			hasPublication: true,
			observedElement: null,
			context: 'focused',
		});
		graph = transitionVoiceMediaGraph(graph, {type: 'subscription.clearCommands'});

		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.reattachAfterPublish',
			participantIdentity: 'user_2_connection_2',
			source: VoiceTrackSource.ScreenShare,
			hasPublication: true,
			forceResubscribe: true,
		});

		expect(selectVoiceMediaGraphSubscriptionCommands(graph)).toEqual([
			{
				type: 'resubscribePublication',
				participantIdentity: 'user_2_connection_2',
				source: VoiceTrackSource.ScreenShare,
				enabled: true,
				quality: 'high',
			},
		]);
	});

	it('cleans up only the requested subscription source', () => {
		const element = {};
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.subscribe',
			participantIdentity: 'user_2_connection_2',
			source: VoiceTrackSource.ScreenShare,
			hasPublication: true,
			observedElement: element,
			context: 'focused',
		});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.subscribe',
			participantIdentity: 'user_2_connection_2',
			source: VoiceTrackSource.Camera,
			hasPublication: true,
			observedElement: null,
			quality: 'low',
		});
		graph = transitionVoiceMediaGraph(graph, {type: 'subscription.clearCommands'});

		graph = transitionVoiceMediaGraph(graph, {type: 'subscription.cleanup', source: VoiceTrackSource.ScreenShare});

		expect(
			selectVoiceMediaGraphSubscriptionEntry(graph, 'user_2_connection_2', VoiceTrackSource.ScreenShare),
		).toBeNull();
		expect(selectVoiceMediaGraphSubscriptionEntry(graph, 'user_2_connection_2', VoiceTrackSource.Camera)).toMatchObject(
			{
				subscribed: true,
				quality: 'low',
			},
		);
		expect(selectVoiceMediaGraphSubscriptionCommands(graph)).toEqual([
			{
				type: 'disconnectObserver',
				participantIdentity: 'user_2_connection_2',
				source: VoiceTrackSource.ScreenShare,
			},
			{
				type: 'unsubscribePublication',
				participantIdentity: 'user_2_connection_2',
				source: VoiceTrackSource.ScreenShare,
			},
		]);
	});

	it('does not subscribe hidden native screen-share media', () => {
		expect(
			buildVoiceMediaGraphNativeScreenShareSubscriptionCommands({
				participantIdentity: 'user_2_connection_2',
				subscribed: false,
				enabled: false,
				quality: 'low',
			}),
		).toEqual([
			{
				participantIdentity: 'user_2_connection_2',
				source: VoiceTrackSource.ScreenShare,
				subscribed: false,
				enabled: false,
			},
			{
				participantIdentity: 'user_2_connection_2',
				source: VoiceTrackSource.ScreenShareAudio,
				subscribed: false,
				enabled: false,
			},
		]);
	});

	it('only emits native screen-share quality commands while enabled', () => {
		expect(
			buildVoiceMediaGraphNativeScreenShareQualityCommand({
				participantIdentity: 'user_2_connection_2',
				enabled: false,
				quality: 'high',
			}),
		).toBeNull();
	});

	it('builds native camera placeholder subscriptions by participant identity', () => {
		expect(
			buildVoiceMediaGraphNativeCameraSubscriptionCommand({
				participantIdentity: 'user_2_connection_2',
				subscribed: true,
				quality: 'low',
			}),
		).toEqual({
			participantIdentity: 'user_2_connection_2',
			source: VoiceTrackSource.Camera,
			subscribed: true,
			enabled: true,
			quality: 'low',
		});
	});

	it('builds native camera quality updates without toggling enabled state', () => {
		expect(
			buildVoiceMediaGraphNativeCameraQualityCommand({
				participantIdentity: 'user_2_connection_2',
				quality: 'high',
			}),
		).toEqual({
			participantIdentity: 'user_2_connection_2',
			source: VoiceTrackSource.Camera,
			subscribed: true,
			quality: 'high',
		});
	});
});

function observation(overrides: Partial<VoiceMediaGraphStatsTrackObservation>): VoiceMediaGraphStatsTrackObservation {
	return {
		trackSid: null,
		trackIdentifier: null,
		mediaSourceId: null,
		mid: null,
		rid: null,
		ssrc: null,
		participantIdentity: null,
		participantSid: null,
		source: null,
		direction: 'recv',
		kind: 'video',
		fps: null,
		width: null,
		height: null,
		sourceFps: null,
		sourceWidth: null,
		sourceHeight: null,
		...overrides,
	};
}

function subscribedScreenShareGraph(participantIdentity: string): VoiceMediaGraphSnapshot {
	let graph = createVoiceMediaGraphSnapshot();
	graph = transitionVoiceMediaGraph(graph, {
		type: 'subscription.subscribe',
		participantIdentity,
		source: VoiceTrackSource.ScreenShare,
		hasPublication: true,
		observedElement: null,
		context: 'focused',
	});
	return transitionVoiceMediaGraph(graph, {type: 'subscription.clearCommands'});
}

describe('VoiceMediaGraph generation scoping', () => {
	it('drops attempt events that carry a stale generation', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A});

		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.attemptEnsured',
			streamKey: STREAM_A,
			attemptKey: 'attempt-stale',
			startedAt: 1000,
			generation: 1,
		});

		expect(selectVoiceMediaGraphAttempt(graph, STREAM_A)).toBeNull();
	});

	it('drops rendered-frame events that carry a stale generation', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A});

		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.renderedFrame',
			streamKey: STREAM_A,
			attemptKey: 'attempt-stale',
			renderedAt: 1100,
			generation: 1,
		});

		expect(selectVoiceMediaGraphAttempt(graph, STREAM_A)).toBeNull();
	});

	it('drops failures reported for a stale generation and records the generation otherwise', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A});

		graph = transitionVoiceMediaGraph(graph, {
			type: 'failure.reported',
			failure: failure({streamKey: STREAM_A}),
			generation: 1,
		});
		expect(selectVoiceMediaGraphHasFailureForStreamKey(graph, STREAM_A)).toBe(false);

		graph = transitionVoiceMediaGraph(graph, {
			type: 'failure.reported',
			failure: failure({streamKey: STREAM_A}),
			generation: 2,
		});
		expect(selectVoiceMediaGraphFailure(graph, {streamKey: STREAM_A})?.generation).toBe(2);
	});

	it('stamps the current generation on failures without an explicit generation', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A});

		graph = transitionVoiceMediaGraph(graph, {type: 'failure.reported', failure: failure({streamKey: STREAM_A})});

		expect(selectVoiceMediaGraphFailure(graph, {streamKey: STREAM_A})?.generation).toBe(1);
	});
});

describe('VoiceMediaGraph deadlines', () => {
	it('creates a watch attempt deadline when a watch starts with a timestamp', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A, at: 2000});

		expect(selectVoiceMediaGraphDeadline(graph, voiceMediaGraphWatchAttemptDeadlineKey(STREAM_A))).toEqual({
			kind: 'watchAttempt',
			streamKey: STREAM_A,
			subscriptionKey: null,
			generation: 1,
			attemptKey: null,
			dueAt: 2000 + WATCH_ATTEMPT_TIMEOUT_MS,
		});
	});

	it('removes the watch attempt deadline when the watch stops', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A, at: 2000});
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.stopped', streamKey: STREAM_A});

		expect(selectVoiceMediaGraphDeadline(graph, voiceMediaGraphWatchAttemptDeadlineKey(STREAM_A))).toBeNull();
	});

	it('removes the watch attempt deadline when a frame renders', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A, at: 2000});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.renderedFrame',
			streamKey: STREAM_A,
			attemptKey: 'attempt-1',
			renderedAt: 2500,
		});

		expect(selectVoiceMediaGraphDeadline(graph, voiceMediaGraphWatchAttemptDeadlineKey(STREAM_A))).toBeNull();
	});

	it('reports a timeout failure when a watch attempt deadline fires', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A, at: 0});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.attemptEnsured',
			streamKey: STREAM_A,
			attemptKey: 'attempt-1',
			startedAt: 0,
		});

		graph = transitionVoiceMediaGraph(graph, {
			type: 'time.deadlineFired',
			key: voiceMediaGraphWatchAttemptDeadlineKey(STREAM_A),
			at: WATCH_ATTEMPT_TIMEOUT_MS,
		});

		const recorded = selectVoiceMediaGraphFailure(graph, {streamKey: STREAM_A});
		expect(recorded?.code).toBe(-2302);
		expect(recorded?.reason).toBe('subscription-attach-timeout');
		expect(selectVoiceMediaGraphDeadline(graph, voiceMediaGraphWatchAttemptDeadlineKey(STREAM_A))).toBeNull();
	});

	it('reports subscription attach timeout when a publication exists but no track attached', () => {
		const participantIdentity = 'user_2_connection-a';
		let graph = subscribedScreenShareGraph(participantIdentity);
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A, at: 0});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.attemptEnsured',
			streamKey: STREAM_A,
			attemptKey: 'attempt-1',
			startedAt: 0,
		});

		graph = transitionVoiceMediaGraph(graph, {
			type: 'time.deadlineFired',
			key: voiceMediaGraphWatchAttemptDeadlineKey(STREAM_A),
			at: WATCH_ATTEMPT_TIMEOUT_MS,
		});

		const recorded = selectVoiceMediaGraphFailure(graph, {streamKey: STREAM_A});
		expect(recorded?.code).toBe(-2302);
		expect(recorded?.reason).toBe('subscription-attach-timeout');
	});

	it('reports republish timeout when an operation watch attempt deadline fires', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A, at: 0});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.attemptEnsured',
			streamKey: STREAM_A,
			attemptKey: `${STREAM_A}:operation:7`,
			startedAt: 0,
		});

		graph = transitionVoiceMediaGraph(graph, {
			type: 'time.deadlineFired',
			key: voiceMediaGraphWatchAttemptDeadlineKey(STREAM_A),
			at: WATCH_ATTEMPT_TIMEOUT_MS,
		});

		const recorded = selectVoiceMediaGraphFailure(graph, {streamKey: STREAM_A});
		expect(recorded?.code).toBe(-2304);
		expect(recorded?.reason).toBe('republish-timeout');
	});

	it('reports a first-frame timeout when the subscription attached without rendering', () => {
		const participantIdentity = 'user_2_connection-a';
		let graph = subscribedScreenShareGraph(participantIdentity);
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A, at: 0});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.attemptEnsured',
			streamKey: STREAM_A,
			attemptKey: 'attempt-1',
			startedAt: 0,
		});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.actualChanged',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: 100,
			subscribed: true,
			enabled: true,
			quality: 'high',
		});

		graph = transitionVoiceMediaGraph(graph, {
			type: 'time.deadlineFired',
			key: voiceMediaGraphWatchAttemptDeadlineKey(STREAM_A),
			at: WATCH_ATTEMPT_TIMEOUT_MS,
		});

		expect(selectVoiceMediaGraphFailure(graph, {streamKey: STREAM_A})?.reason).toBe('first-frame-timeout');
	});

	it('treats a stale deadline fire as a no-op aside from cleanup', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {type: 'watch.started', streamKey: STREAM_A, at: 0});
		const deadlineKey = voiceMediaGraphWatchAttemptDeadlineKey(STREAM_A);
		const staleDeadline: VoiceMediaGraphDeadline = {
			kind: 'watchAttempt',
			streamKey: STREAM_A,
			subscriptionKey: null,
			generation: 0,
			attemptKey: null,
			dueAt: WATCH_ATTEMPT_TIMEOUT_MS,
		};
		graph = {...graph, deadlinesByKey: new Map([[deadlineKey, staleDeadline]])};

		graph = transitionVoiceMediaGraph(graph, {
			type: 'time.deadlineFired',
			key: deadlineKey,
			at: WATCH_ATTEMPT_TIMEOUT_MS,
		});

		expect(selectVoiceMediaGraphHasFailureForStreamKey(graph, STREAM_A)).toBe(false);
		expect(selectVoiceMediaGraphDeadline(graph, deadlineKey)).toBeNull();
	});

	it('ignores fires for deadlines that no longer exist', () => {
		const graph = createVoiceMediaGraphSnapshot();
		const next = transitionVoiceMediaGraph(graph, {
			type: 'time.deadlineFired',
			key: voiceMediaGraphWatchAttemptDeadlineKey(STREAM_A),
			at: 1,
		});
		expect(next).toBe(graph);
	});

	it('finalizes a deferred stop when its grace deadline fires', () => {
		let graph = transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {type: 'watchIntent.add', key: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.deferRemove', key: STREAM_A, at: 1000});

		expect(selectVoiceMediaGraphDeadline(graph, voiceMediaGraphDeferredStopDeadlineKey(STREAM_A))?.dueAt).toBe(
			1000 + PUBLISHER_REPUBLISH_GRACE_MS,
		);

		graph = transitionVoiceMediaGraph(graph, {
			type: 'time.deadlineFired',
			key: voiceMediaGraphDeferredStopDeadlineKey(STREAM_A),
			at: 1000 + PUBLISHER_REPUBLISH_GRACE_MS,
		});

		expect(selectVoiceMediaGraphViewerStreamKeys(graph)).toEqual([]);
		expect(selectVoiceMediaGraphDeferredStopKeys(graph).size).toBe(0);
	});

	it('drops the grace deadline when the deferred stop is cancelled', () => {
		let graph = transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {type: 'watchIntent.add', key: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.deferRemove', key: STREAM_A, at: 1000});
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.cancelDeferredRemove', key: STREAM_A});

		expect(selectVoiceMediaGraphDeadline(graph, voiceMediaGraphDeferredStopDeadlineKey(STREAM_A))).toBeNull();
	});

	it('tracks publication loss with a deadline while watch intent exists', () => {
		const participantIdentity = 'user_2_connection-a';
		let graph = subscribedScreenShareGraph(participantIdentity);
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.add', key: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'publication.lost',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: 1000,
		});

		const deadlineKey = voiceMediaGraphPublicationMissingDeadlineKey(`${participantIdentity}:screen_share`);
		expect(selectVoiceMediaGraphDeadline(graph, deadlineKey)?.dueAt).toBe(1000 + PUBLICATION_MISSING_TIMEOUT_MS);

		graph = transitionVoiceMediaGraph(graph, {
			type: 'publication.observed',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			trackSid: 'TR_1',
			at: 1500,
		});

		expect(selectVoiceMediaGraphDeadline(graph, deadlineKey)).toBeNull();
		expect(
			selectVoiceMediaGraphSubscriptionEntry(graph, participantIdentity, VoiceTrackSource.ScreenShare)?.publication,
		).toEqual({available: true, trackSid: 'TR_1', observedAt: 1500});
	});

	it('reports a publication-missing failure when the loss deadline fires', () => {
		const participantIdentity = 'user_2_connection-a';
		let graph = subscribedScreenShareGraph(participantIdentity);
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.add', key: STREAM_A});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'publication.lost',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: 1000,
		});

		graph = transitionVoiceMediaGraph(graph, {
			type: 'time.deadlineFired',
			key: voiceMediaGraphPublicationMissingDeadlineKey(`${participantIdentity}:screen_share`),
			at: 1000 + PUBLICATION_MISSING_TIMEOUT_MS,
		});

		const recorded = selectVoiceMediaGraphFailure(graph, {streamKey: STREAM_A});
		expect(recorded?.code).toBe(-2301);
		expect(recorded?.reason).toBe('publication-missing-timeout');
	});
});

describe('VoiceMediaGraph desired and actual subscription state', () => {
	const participantIdentity = 'user_2_connection-a';

	it('splits subscribe intent into desired, actual, publication, and first frame state', () => {
		const graph = subscribedScreenShareGraph(participantIdentity);
		const entry = selectVoiceMediaGraphSubscriptionEntry(graph, participantIdentity, VoiceTrackSource.ScreenShare);

		expect(entry?.desired).toEqual({
			enabled: true,
			quality: 'high',
			context: 'focused',
			isIntersecting: false,
			observedElement: null,
		});
		expect(entry?.actual).toEqual({
			subscribed: null,
			enabled: null,
			quality: null,
			lastCommandAt: null,
			lastError: null,
		});
		expect(entry?.publication).toEqual({available: true, trackSid: null, observedAt: null});
		expect(entry?.firstFrame).toEqual({renderedAt: null});
		expect(entry?.subscribed).toBe(true);
		expect(entry?.publicationAvailable).toBe(true);
		expect(entry?.enabled).toBe(true);
		expect(entry?.quality).toBe('high');
		expect(entry?.context).toBe('focused');
	});

	it('applies actual changes and clears the last error', () => {
		let graph = subscribedScreenShareGraph(participantIdentity);
		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.commandFailed',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: 100,
			code: -2102,
			reason: 'subscription-set-enabled-failed',
		});

		let entry = selectVoiceMediaGraphSubscriptionEntry(graph, participantIdentity, VoiceTrackSource.ScreenShare);
		expect(entry?.actual.lastError).toEqual({code: -2102, reason: 'subscription-set-enabled-failed', at: 100});

		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.actualChanged',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: 200,
			subscribed: true,
			enabled: true,
			quality: 'high',
		});

		entry = selectVoiceMediaGraphSubscriptionEntry(graph, participantIdentity, VoiceTrackSource.ScreenShare);
		expect(entry?.actual).toEqual({
			subscribed: true,
			enabled: true,
			quality: 'high',
			lastCommandAt: 200,
			lastError: null,
		});
	});

	it('ignores actual changes for unknown subscriptions', () => {
		const graph = createVoiceMediaGraphSnapshot();
		const next = transitionVoiceMediaGraph(graph, {
			type: 'subscription.actualChanged',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: 100,
			subscribed: true,
		});
		expect(next).toBe(graph);
	});

	it('reconciles desired against actual idempotently', () => {
		let graph = subscribedScreenShareGraph(participantIdentity);

		graph = transitionVoiceMediaGraph(graph, {type: 'subscription.reconcile'});
		expect(selectVoiceMediaGraphSubscriptionCommands(graph)).toEqual([
			{
				type: 'subscribePublication',
				participantIdentity,
				source: VoiceTrackSource.ScreenShare,
				enabled: true,
				quality: 'high',
			},
		]);

		graph = transitionVoiceMediaGraph(graph, {type: 'subscription.reconcile'});
		expect(selectVoiceMediaGraphSubscriptionCommands(graph)).toHaveLength(1);
	});

	it('emits no reconcile commands once desired and actual converge', () => {
		let graph = subscribedScreenShareGraph(participantIdentity);
		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.actualChanged',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: 100,
			subscribed: true,
			enabled: true,
			quality: 'high',
		});

		graph = transitionVoiceMediaGraph(graph, {type: 'subscription.reconcile'});

		expect(selectVoiceMediaGraphSubscriptionCommands(graph)).toEqual([]);
	});

	it('reconciles drifted enabled and quality through targeted commands', () => {
		let graph = subscribedScreenShareGraph(participantIdentity);
		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.actualChanged',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: 100,
			subscribed: true,
			enabled: false,
			quality: 'low',
		});

		graph = transitionVoiceMediaGraph(graph, {type: 'subscription.reconcile'});

		expect(selectVoiceMediaGraphSubscriptionCommands(graph)).toEqual([
			{type: 'setPublicationEnabled', participantIdentity, source: VoiceTrackSource.ScreenShare, enabled: true},
			{type: 'setPublicationQuality', participantIdentity, source: VoiceTrackSource.ScreenShare, quality: 'high'},
		]);
	});
});

describe('VoiceMediaGraph sticky failure clearing', () => {
	it('clears identity-keyed failures for the stream when the watch stops', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'failure.reported',
			failure: failure({participantIdentity: 'user_2_connection_1'}),
		});

		graph = transitionVoiceMediaGraph(graph, {type: 'watch.stopped', streamKey: 'dm:channel:connection_1'});

		expect(selectVoiceMediaGraphFailure(graph, {participantIdentity: 'user_2_connection_1'})).toBeNull();
	});

	it('clears stream-keyed failures whose connection matches the stopped stream', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'failure.reported',
			failure: failure({streamKey: 'guild-1:channel:connection_1'}),
		});

		graph = transitionVoiceMediaGraph(graph, {type: 'watch.stopped', streamKey: 'dm:channel:connection_1'});

		expect(selectVoiceMediaGraphFailure(graph, {streamKey: 'guild-1:channel:connection_1'})).toBeNull();
	});

	it('keeps failures for unrelated connections when a watch stops', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'failure.reported',
			failure: failure({participantIdentity: 'user_2_connection_2'}),
		});

		graph = transitionVoiceMediaGraph(graph, {type: 'watch.stopped', streamKey: 'dm:channel:connection_1'});

		expect(selectVoiceMediaGraphFailure(graph, {participantIdentity: 'user_2_connection_2'})).not.toBeNull();
	});
});

describe('VoiceMediaGraph stats observations', () => {
	it('stores observations and serves track info through one selector', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'stats.observed',
			at: 1000,
			connectionId: 'connection-a',
			platform: 'native',
			tracks: [
				observation({
					trackSid: 'TR_screen',
					participantIdentity: 'user_2_connection-a',
					source: 'screen_share',
					fps: 24.7,
					width: 1920,
					height: 1080,
				}),
			],
		});

		expect(selectVoiceMediaGraphStatsTrackInfo(graph, {trackSid: 'TR_screen'})).toEqual({
			width: 1920,
			height: 1080,
			fps: 24.7,
		});
		expect(
			selectVoiceMediaGraphStatsTrackInfo(graph, {participantIdentity: 'user_2_connection-a', source: 'screen_share'}),
		).toEqual({width: 1920, height: 1080, fps: 24.7});
	});

	it('falls back to source dimensions when encoded ones are missing', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'stats.observed',
			at: 1000,
			connectionId: 'connection-a',
			platform: 'web',
			tracks: [observation({trackIdentifier: 'media-1', sourceFps: 30, sourceWidth: 3840, sourceHeight: 2160})],
		});

		expect(selectVoiceMediaGraphStatsTrackInfo(graph, {trackIdentifier: 'media-1'})).toEqual({
			width: 3840,
			height: 2160,
			fps: 30,
		});
	});

	it('drops observations for a stale connection id', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'stats.observed',
			at: 1000,
			connectionId: 'connection-a',
			platform: 'web',
			tracks: [observation({trackIdentifier: 'media-1', fps: 30})],
		});

		const next = transitionVoiceMediaGraph(graph, {
			type: 'stats.observed',
			at: 2000,
			connectionId: 'connection-b',
			platform: 'web',
			tracks: [observation({trackIdentifier: 'media-2', fps: 60})],
		});

		expect(next).toBe(graph);
	});

	it('wipes stored observations when the connection changes', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'stats.observed',
			at: 1000,
			connectionId: 'connection-a',
			platform: 'web',
			tracks: [observation({trackIdentifier: 'media-1', fps: 30})],
		});

		graph = transitionVoiceMediaGraph(graph, {type: 'stats.connectionChanged', connectionId: 'connection-b'});

		expect(selectVoiceMediaGraphStatsTrackInfo(graph, {trackIdentifier: 'media-1'})).toBeNull();
		expect(graph.statsConnectionId).toBe('connection-b');

		graph = transitionVoiceMediaGraph(graph, {
			type: 'stats.observed',
			at: 3000,
			connectionId: 'connection-b',
			platform: 'web',
			tracks: [observation({trackIdentifier: 'media-2', fps: 60})],
		});

		expect(selectVoiceMediaGraphStatsTrackInfo(graph, {trackIdentifier: 'media-2'})).toEqual({fps: 60});
	});

	it('clears stats on clear.all', () => {
		let graph = createVoiceMediaGraphSnapshot();
		graph = transitionVoiceMediaGraph(graph, {
			type: 'stats.observed',
			at: 1000,
			connectionId: 'connection-a',
			platform: 'web',
			tracks: [observation({trackIdentifier: 'media-1', fps: 30})],
		});

		graph = transitionVoiceMediaGraph(graph, {type: 'clear.all'});

		expect(graph.statsConnectionId).toBeNull();
		expect(graph.statsByTrackKey.size).toBe(0);
	});
});

describe('VoiceMediaGraph stats selectors', () => {
	it('matches web stats by media track id and can return fps-only observations', () => {
		const tracks: Array<VoiceEngineV2PerTrackStats> = [
			{
				direction: 'recv',
				kind: 'video',
				trackIdentifier: 'screen-media-track',
				bitrateKbps: 4200,
				framesPerSecond: 25,
			},
		];

		expect(resolveVoiceMediaGraphPerTrackInfo(tracks, {mediaTrackId: 'screen-media-track'})).toEqual({fps: 25});
	});

	it('fills missing rendered-track fps from matched stats without replacing rendered dimensions', () => {
		const renderedInfo = {width: 3840, height: 2160, fps: 0};
		const statsInfo = {width: 1920, height: 1080, fps: 24.7};

		expect(mergeVoiceMediaGraphTrackInfo(renderedInfo, statsInfo)).toEqual({width: 3840, height: 2160, fps: 25});
	});
});
