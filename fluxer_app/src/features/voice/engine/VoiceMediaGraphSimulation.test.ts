// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVoiceMediaGraphSnapshot,
	selectVoiceMediaGraphStatsEntry,
	selectVoiceMediaGraphSubscriptionEntry,
	transitionVoiceMediaGraph,
	type VoiceMediaGraphEvent,
	type VoiceMediaGraphSnapshot,
	type VoiceMediaGraphStatsTrackObservation,
	type VoiceMediaGraphSubscriptionObservedElement,
	type VoiceMediaGraphSubscriptionTarget,
	voiceMediaGraphPublicationMissingDeadlineKey,
	voiceMediaGraphWatchAttemptDeadlineKey,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {checkVoiceMediaGraphInvariants} from '@app/features/voice/engine/VoiceMediaGraphInvariants';
import {
	selectVoiceMediaGraphStreamTileState,
	type VoiceMediaGraphStreamTileState,
} from '@app/features/voice/engine/VoiceMediaGraphTileState';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {describe, expect, it} from 'vitest';

const SIMULATION_SEEDS: ReadonlyArray<number> = Array.from({length: 20}, (_, index) => 1 + index * 7919);
const SIMULATION_STEP_COUNT = 1000;
const SIMULATION_EPOCH_MS = 1_750_000_000_000;
const SIMULATION_STEP_ADVANCE_MS_MAX = 500;
const SIMULATION_COMMAND_DRAIN_THRESHOLD = 256;
const SIMULATION_GATE_RETRY_LIMIT = 8;
const SIMULATION_QUALITIES = ['low', 'medium', 'high'] as const;
const SIMULATION_CONTEXTS = ['focused', 'carousel', 'hidden'] as const;
const SIMULATION_STATS_CONNECTION_IDS = ['stats-conn-0', 'stats-conn-1', 'stats-conn-2'] as const;
const SIMULATION_OBSERVED_ELEMENTS: ReadonlyArray<VoiceMediaGraphSubscriptionObservedElement> = [
	null,
	{simulationElementId: 0},
	{simulationElementId: 1},
];
const SIMULATION_SOURCES = [VoiceTrackSource.ScreenShare, VoiceTrackSource.Camera] as const;

interface SimulationParticipant {
	connectionId: string;
	participantIdentity: string;
	streamKey: string;
}

const SIMULATION_PARTICIPANTS: ReadonlyArray<SimulationParticipant> = [0, 1, 2].map((index) => ({
	connectionId: `conn-${index}`,
	participantIdentity: `user_${index}_conn-${index}`,
	streamKey: `dm:channel-sim:conn-${index}`,
}));

interface SimulationRng {
	int(maxExclusive: number): number;
	pick<T>(items: ReadonlyArray<T>): T;
	bool(): boolean;
}

function createSimulationRng(seed: number): SimulationRng {
	let state = seed >>> 0;
	if (state === 0) state = 0x9e3779b9;
	const nextU32 = (): number => {
		state = (Math.imul(state, 1664525) + 1013904223) >>> 0;
		return state;
	};
	const int = (maxExclusive: number): number => Math.floor((nextU32() / 0x1_0000_0000) * maxExclusive);
	return {
		int,
		pick: <T>(items: ReadonlyArray<T>): T => items[int(items.length)] as T,
		bool: (): boolean => nextU32() >= 0x8000_0000,
	};
}

type SimulationEventBuilder = (
	rng: SimulationRng,
	snapshot: VoiceMediaGraphSnapshot,
	nowMs: number,
) => VoiceMediaGraphEvent | null;

function simulationEventGeneration(
	rng: SimulationRng,
	snapshot: VoiceMediaGraphSnapshot,
	streamKey: string,
): number | undefined {
	const current = snapshot.watchGenerationByStreamKey.get(streamKey) ?? 0;
	switch (rng.int(4)) {
		case 0:
			return undefined;
		case 1:
			return current > 0 ? current - 1 : current;
		default:
			return current;
	}
}

function simulationTriState(rng: SimulationRng): boolean | undefined {
	switch (rng.int(3)) {
		case 0:
			return true;
		case 1:
			return false;
		default:
			return undefined;
	}
}

function isSimulationRendering(snapshot: VoiceMediaGraphSnapshot, participant: SimulationParticipant): boolean {
	const attempt = snapshot.attemptsByStreamKey.get(participant.streamKey);
	if (!attempt) return false;
	if (!attempt.hasRenderedVideoFrame) return false;
	const generation = snapshot.watchGenerationByStreamKey.get(participant.streamKey) ?? 0;
	if (attempt.generation !== generation) return false;
	const entry = selectVoiceMediaGraphSubscriptionEntry(
		snapshot,
		participant.participantIdentity,
		VoiceTrackSource.ScreenShare,
	);
	if (!entry) return false;
	return entry.firstFrame.renderedAt !== null;
}

function canSimulationRenderFrame(snapshot: VoiceMediaGraphSnapshot, participant: SimulationParticipant): boolean {
	const tileState = selectVoiceMediaGraphStreamTileState(snapshot, {
		streamKey: participant.streamKey,
		participantIdentity: participant.participantIdentity,
		source: VoiceTrackSource.ScreenShare,
	});
	if (tileState === 'failed') return false;
	const entry = selectVoiceMediaGraphSubscriptionEntry(
		snapshot,
		participant.participantIdentity,
		VoiceTrackSource.ScreenShare,
	);
	if (!entry) return true;
	if (!entry.publication.available) return false;
	return entry.actual.lastError === null;
}

function buildSimulationWatchEvent(
	rng: SimulationRng,
	snapshot: VoiceMediaGraphSnapshot,
	nowMs: number,
): VoiceMediaGraphEvent | null {
	const participant = rng.pick(SIMULATION_PARTICIPANTS);
	const streamKey = participant.streamKey;
	switch (rng.int(4)) {
		case 0:
			return rng.bool() ? {type: 'watch.started', streamKey, at: nowMs} : {type: 'watch.started', streamKey};
		case 1:
			return {type: 'watch.stopped', streamKey};
		case 2: {
			const generation = simulationEventGeneration(rng, snapshot, streamKey);
			return {
				type: 'watch.attemptEnsured',
				streamKey,
				attemptKey: `attempt-${rng.int(6)}`,
				startedAt: nowMs,
				...(generation === undefined ? {} : {generation}),
			};
		}
		default: {
			if (!canSimulationRenderFrame(snapshot, participant)) return null;
			const attempt = snapshot.attemptsByStreamKey.get(streamKey);
			const generation = simulationEventGeneration(rng, snapshot, streamKey);
			return {
				type: 'watch.renderedFrame',
				streamKey,
				attemptKey: attempt?.attemptKey ?? `attempt-${rng.int(6)}`,
				renderedAt: nowMs,
				...(generation === undefined ? {} : {generation}),
			};
		}
	}
}

function buildSimulationIntentEvent(
	rng: SimulationRng,
	_snapshot: VoiceMediaGraphSnapshot,
	nowMs: number,
): VoiceMediaGraphEvent | null {
	const participant = rng.pick(SIMULATION_PARTICIPANTS);
	const key = participant.streamKey;
	switch (rng.int(7)) {
		case 0: {
			const count = rng.int(SIMULATION_PARTICIPANTS.length + 1);
			return {
				type: 'watchIntent.replace',
				keys: SIMULATION_PARTICIPANTS.slice(0, count).map((candidate) => candidate.streamKey),
			};
		}
		case 1:
			return {type: 'watchIntent.add', key};
		case 2:
			return {type: 'watchIntent.remove', key};
		case 3:
			return {type: 'watchIntent.removeMany', keys: [key, rng.pick(SIMULATION_PARTICIPANTS).streamKey]};
		case 4:
			return rng.bool() ? {type: 'watchIntent.deferRemove', key, at: nowMs} : {type: 'watchIntent.deferRemove', key};
		case 5:
			return {type: 'watchIntent.cancelDeferredRemove', key};
		default:
			return {type: 'watchIntent.reset'};
	}
}

function buildSimulationFailureEvent(
	rng: SimulationRng,
	snapshot: VoiceMediaGraphSnapshot,
	nowMs: number,
): VoiceMediaGraphEvent | null {
	const participant = rng.pick(SIMULATION_PARTICIPANTS);
	switch (rng.int(4)) {
		case 0: {
			if (isSimulationRendering(snapshot, participant)) return null;
			const generation = simulationEventGeneration(rng, snapshot, participant.streamKey);
			return {
				type: 'failure.reported',
				failure: {
					code: -2202,
					reason: 'screen-share-watch-failed',
					reportedAt: nowMs,
					streamKey: participant.streamKey,
					participantIdentity: participant.participantIdentity,
					source: VoiceTrackSource.ScreenShare,
				},
				...(generation === undefined ? {} : {generation}),
			};
		}
		case 1:
			return {
				type: 'failure.reported',
				failure: {
					code: -2210,
					reason: 'camera-subscribe-failed',
					reportedAt: nowMs,
					participantIdentity: participant.participantIdentity,
					source: VoiceTrackSource.Camera,
				},
			};
		case 2:
			return {
				type: 'failure.cleared',
				target: rng.bool()
					? {streamKey: participant.streamKey}
					: {participantIdentity: participant.participantIdentity, source: rng.pick(SIMULATION_SOURCES)},
			};
		default:
			return {type: 'failureWatch.clearAll'};
	}
}

function buildSimulationSubscriptionShapeEvent(
	rng: SimulationRng,
	target: VoiceMediaGraphSubscriptionTarget,
	hasPublication: boolean,
): VoiceMediaGraphEvent {
	switch (rng.int(4)) {
		case 0: {
			const context = rng.bool() ? rng.pick(SIMULATION_CONTEXTS) : undefined;
			const quality = rng.bool() ? rng.pick(SIMULATION_QUALITIES) : undefined;
			return {
				type: 'subscription.subscribe',
				...target,
				hasPublication,
				observedElement: rng.pick(SIMULATION_OBSERVED_ELEMENTS),
				...(context === undefined ? {} : {context}),
				...(quality === undefined ? {} : {quality}),
			};
		}
		case 1:
			return {type: 'subscription.unsubscribe', ...target};
		case 2:
			return {
				type: 'subscription.replaceObserver',
				...target,
				hasPublication,
				observedElement: rng.pick(SIMULATION_OBSERVED_ELEMENTS),
			};
		default:
			return {type: 'subscription.intersection', ...target, hasPublication, isIntersecting: rng.bool()};
	}
}

function buildSimulationSubscriptionTuningEvent(
	rng: SimulationRng,
	target: VoiceMediaGraphSubscriptionTarget,
	hasPublication: boolean,
): VoiceMediaGraphEvent {
	switch (rng.int(8)) {
		case 0:
			return {type: 'subscription.setEnabled', ...target, hasPublication, enabled: rng.bool()};
		case 1:
			return {type: 'subscription.setQuality', ...target, hasPublication, quality: rng.pick(SIMULATION_QUALITIES)};
		case 2:
			return {type: 'subscription.setContext', ...target, hasPublication, context: rng.pick(SIMULATION_CONTEXTS)};
		case 3:
			return {
				type: 'subscription.reattachAfterPublish',
				...target,
				hasPublication,
				forceResubscribe: rng.bool(),
			};
		case 4:
			return {type: 'subscription.publicationMissing', ...target};
		case 5:
			return rng.bool() ? {type: 'subscription.cleanup'} : {type: 'subscription.cleanup', source: target.source};
		case 6:
			return {type: 'subscription.reconcile'};
		default:
			return {type: 'subscription.clearCommands'};
	}
}

function buildSimulationSubscriptionEvent(
	rng: SimulationRng,
	snapshot: VoiceMediaGraphSnapshot,
	_nowMs: number,
): VoiceMediaGraphEvent | null {
	const participant = rng.pick(SIMULATION_PARTICIPANTS);
	const source = rng.pick(SIMULATION_SOURCES);
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, participant.participantIdentity, source);
	const hasPublication = entry?.publication.available ?? false;
	const target: VoiceMediaGraphSubscriptionTarget = {participantIdentity: participant.participantIdentity, source};
	if (rng.bool()) return buildSimulationSubscriptionShapeEvent(rng, target, hasPublication);
	return buildSimulationSubscriptionTuningEvent(rng, target, hasPublication);
}

function buildSimulationFeedbackEvent(
	rng: SimulationRng,
	snapshot: VoiceMediaGraphSnapshot,
	nowMs: number,
): VoiceMediaGraphEvent | null {
	const participant = rng.pick(SIMULATION_PARTICIPANTS);
	const source = rng.pick(SIMULATION_SOURCES);
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, participant.participantIdentity, source);
	if (!entry) return null;
	const generation = simulationEventGeneration(rng, snapshot, participant.streamKey);
	if (rng.int(3) === 0) {
		if (source === VoiceTrackSource.ScreenShare && isSimulationRendering(snapshot, participant)) return null;
		return {
			type: 'subscription.commandFailed',
			participantIdentity: participant.participantIdentity,
			source,
			at: nowMs,
			code: -2302,
			reason: 'simulated-command-failure',
			...(source === VoiceTrackSource.ScreenShare ? {streamKey: participant.streamKey} : {}),
			...(generation === undefined ? {} : {generation}),
		};
	}
	const subscribed = simulationTriState(rng);
	const enabled = simulationTriState(rng);
	const quality = rng.bool() ? rng.pick(SIMULATION_QUALITIES) : undefined;
	return {
		type: 'subscription.actualChanged',
		participantIdentity: participant.participantIdentity,
		source,
		at: nowMs,
		...(subscribed === undefined ? {} : {subscribed}),
		...(enabled === undefined ? {} : {enabled}),
		...(quality === undefined ? {} : {quality}),
		...(entry.publication.available && rng.bool() ? {trackSid: `TS_${rng.int(8)}`} : {}),
		...(rng.bool() ? {streamKey: participant.streamKey} : {}),
		...(generation === undefined ? {} : {generation}),
	};
}

function buildSimulationPublicationEvent(
	rng: SimulationRng,
	_snapshot: VoiceMediaGraphSnapshot,
	nowMs: number,
): VoiceMediaGraphEvent | null {
	const participant = rng.pick(SIMULATION_PARTICIPANTS);
	const source = rng.pick(SIMULATION_SOURCES);
	if (rng.bool()) {
		return {
			type: 'publication.observed',
			participantIdentity: participant.participantIdentity,
			source,
			trackSid: rng.bool() ? `TS_${rng.int(8)}` : null,
			at: nowMs,
		};
	}
	return {type: 'publication.lost', participantIdentity: participant.participantIdentity, source, at: nowMs};
}

function buildSimulationDeadlineEvent(
	rng: SimulationRng,
	snapshot: VoiceMediaGraphSnapshot,
	nowMs: number,
): VoiceMediaGraphEvent | null {
	const keys = [...snapshot.deadlinesByKey.keys()];
	if (rng.bool() && keys.length > 0) {
		return {type: 'time.deadlineFired', key: rng.pick(keys), at: nowMs};
	}
	const participant = rng.pick(SIMULATION_PARTICIPANTS);
	const key = rng.bool()
		? voiceMediaGraphWatchAttemptDeadlineKey(participant.streamKey)
		: voiceMediaGraphPublicationMissingDeadlineKey(`${participant.participantIdentity}:screen_share`);
	return {type: 'time.deadlineFired', key, at: nowMs};
}

function buildSimulationStatsObservation(rng: SimulationRng): VoiceMediaGraphStatsTrackObservation {
	const participant = rng.pick(SIMULATION_PARTICIPANTS);
	const variant = rng.int(3);
	const dimension = rng.pick([null, 640, 1280, 1920]);
	return {
		trackSid: variant === 0 ? `TR_${rng.int(24)}` : null,
		trackIdentifier: variant === 1 ? `TI_${rng.int(24)}` : null,
		mediaSourceId: null,
		mid: rng.bool() ? `${rng.int(8)}` : null,
		rid: null,
		ssrc: rng.bool() ? 1000 + rng.int(50) : null,
		participantIdentity: variant === 2 ? participant.participantIdentity : null,
		participantSid: null,
		source: variant === 2 ? VoiceTrackSource.ScreenShare : null,
		direction: rng.bool() ? 'send' : 'recv',
		kind: rng.bool() ? 'audio' : 'video',
		fps: rng.pick([null, 0, 30, 60]),
		width: dimension,
		height: dimension === null ? null : Math.floor((dimension * 9) / 16),
		sourceFps: rng.pick([null, 30, 60]),
		sourceWidth: rng.pick([null, 1920]),
		sourceHeight: rng.pick([null, 1080]),
	};
}

function buildSimulationStatsEvent(
	rng: SimulationRng,
	snapshot: VoiceMediaGraphSnapshot,
	nowMs: number,
): VoiceMediaGraphEvent {
	if (rng.int(4) === 0) {
		return {
			type: 'stats.connectionChanged',
			connectionId: rng.bool() ? rng.pick(SIMULATION_STATS_CONNECTION_IDS) : null,
		};
	}
	const connectionId = rng.bool()
		? (snapshot.statsConnectionId ?? rng.pick(SIMULATION_STATS_CONNECTION_IDS))
		: rng.pick(SIMULATION_STATS_CONNECTION_IDS);
	const trackCount = 1 + rng.int(3);
	const tracks = Array.from({length: trackCount}, () => buildSimulationStatsObservation(rng));
	return {
		type: 'stats.observed',
		at: nowMs,
		connectionId,
		platform: rng.bool() ? 'native' : 'web',
		tracks,
	};
}

function buildSimulationResetEvent(rng: SimulationRng): VoiceMediaGraphEvent | null {
	if (rng.int(8) !== 0) return null;
	return {type: 'clear.all'};
}

function buildSimulationScenarioEvent(
	rng: SimulationRng,
	snapshot: VoiceMediaGraphSnapshot,
	nowMs: number,
): VoiceMediaGraphEvent | null {
	const participant = rng.pick(SIMULATION_PARTICIPANTS);
	const participantIdentity = participant.participantIdentity;
	const streamKey = participant.streamKey;
	const source = VoiceTrackSource.ScreenShare;
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, participantIdentity, source);
	if (!entry) {
		return {
			type: 'subscription.subscribe',
			participantIdentity,
			source,
			hasPublication: false,
			observedElement: rng.pick(SIMULATION_OBSERVED_ELEMENTS),
			context: 'focused',
		};
	}
	if (!entry.publication.available) {
		return {type: 'publication.observed', participantIdentity, source, trackSid: `TS_${rng.int(8)}`, at: nowMs};
	}
	if ((snapshot.watchGenerationByStreamKey.get(streamKey) ?? 0) === 0) {
		return {type: 'watch.started', streamKey, at: nowMs};
	}
	const attempt = snapshot.attemptsByStreamKey.get(streamKey);
	if (!attempt) {
		return {type: 'watch.attemptEnsured', streamKey, attemptKey: `attempt-${rng.int(6)}`, startedAt: nowMs};
	}
	if (entry.actual.subscribed !== true) {
		return {
			type: 'subscription.actualChanged',
			participantIdentity,
			source,
			at: nowMs,
			subscribed: true,
			enabled: true,
		};
	}
	if (attempt.hasRenderedVideoFrame && entry.firstFrame.renderedAt !== null) return null;
	if (!canSimulationRenderFrame(snapshot, participant)) return null;
	return {type: 'watch.renderedFrame', streamKey, attemptKey: attempt.attemptKey, renderedAt: nowMs};
}

const SIMULATION_WEIGHTED_BUILDERS: ReadonlyArray<SimulationEventBuilder> = [
	buildSimulationWatchEvent,
	buildSimulationIntentEvent,
	buildSimulationFailureEvent,
	buildSimulationSubscriptionEvent,
	buildSimulationFeedbackEvent,
	buildSimulationPublicationEvent,
	buildSimulationDeadlineEvent,
	buildSimulationStatsEvent,
	buildSimulationScenarioEvent,
];

function buildSimulationBuilderPool(rng: SimulationRng): Array<SimulationEventBuilder> {
	const pool: Array<SimulationEventBuilder> = [];
	for (const builder of SIMULATION_WEIGHTED_BUILDERS) {
		const weight = 1 + rng.int(4);
		for (let copy = 0; copy < weight; copy += 1) pool.push(builder);
	}
	pool.push(buildSimulationResetEvent);
	return pool;
}

function nextSimulationEvent(
	rng: SimulationRng,
	snapshot: VoiceMediaGraphSnapshot,
	pool: ReadonlyArray<SimulationEventBuilder>,
	nowMs: number,
): VoiceMediaGraphEvent {
	for (let attempt = 0; attempt < SIMULATION_GATE_RETRY_LIMIT; attempt += 1) {
		const event = rng.pick(pool)(rng, snapshot, nowMs);
		if (event) return event;
	}
	return buildSimulationStatsEvent(rng, snapshot, nowMs);
}

interface SimulationStepContext {
	seed: number;
	step: number;
	eventType: string;
}

function simulationStepMessage(context: SimulationStepContext): string {
	return `seed=${context.seed} step=${context.step} event=${context.eventType}`;
}

function assertSimulationActualSubscribed(snapshot: VoiceMediaGraphSnapshot, message: string): void {
	for (const [key, entry] of snapshot.subscriptionsByKey) {
		if (entry.actual.subscribed !== true) continue;
		expect(entry.subscribed, `${message}: ${key} actually subscribed without desired entry`).toBe(true);
	}
}

function assertSimulationDeferredSubset(snapshot: VoiceMediaGraphSnapshot, message: string): void {
	for (const key of snapshot.watchIntent.deferredStopKeys) {
		expect(
			snapshot.watchIntent.viewerStreamKeys.includes(key),
			`${message}: deferred stop key ${key} is not a viewer stream key`,
		).toBe(true);
	}
}

function assertSimulationFailureRenderOrdering(snapshot: VoiceMediaGraphSnapshot, message: string): void {
	for (const failure of snapshot.failuresByKey.values()) {
		if (!failure.streamKey) continue;
		const participant = SIMULATION_PARTICIPANTS.find((candidate) => candidate.streamKey === failure.streamKey);
		if (!participant) continue;
		const currentGeneration = snapshot.watchGenerationByStreamKey.get(failure.streamKey) ?? 0;
		if ((failure.generation ?? currentGeneration) !== currentGeneration) continue;
		const entry = selectVoiceMediaGraphSubscriptionEntry(
			snapshot,
			participant.participantIdentity,
			VoiceTrackSource.ScreenShare,
		);
		if (!entry) continue;
		if (entry.firstFrame.renderedAt === null) continue;
		expect(
			entry.firstFrame.renderedAt <= failure.reportedAt,
			`${message}: stream ${failure.streamKey} rendered a frame after its current-generation failure`,
		).toBe(true);
	}
}

function selectSimulationTileState(
	snapshot: VoiceMediaGraphSnapshot,
	participant: SimulationParticipant,
	source: VoiceTrackSource,
	message: string,
): VoiceMediaGraphStreamTileState {
	try {
		return selectVoiceMediaGraphStreamTileState(snapshot, {
			streamKey: source === VoiceTrackSource.ScreenShare ? participant.streamKey : null,
			participantIdentity: participant.participantIdentity,
			source,
		});
	} catch (error) {
		throw new Error(`${message}: tile selector threw for ${participant.participantIdentity}:${source}: ${error}`);
	}
}

function assertSimulationTileStates(snapshot: VoiceMediaGraphSnapshot, message: string): void {
	for (const participant of SIMULATION_PARTICIPANTS) {
		for (const source of SIMULATION_SOURCES) {
			const state = selectSimulationTileState(snapshot, participant, source, message);
			if (source !== VoiceTrackSource.ScreenShare) continue;
			if (!isSimulationRendering(snapshot, participant)) continue;
			expect(
				state,
				`${message}: tile for ${participant.streamKey} must render while current generation has a frame`,
			).toBe('rendering');
		}
	}
}

function assertSimulationStats(snapshot: VoiceMediaGraphSnapshot, message: string): void {
	if (snapshot.statsConnectionId === null) {
		expect(snapshot.statsByTrackKey.size, `${message}: stats entries without a connection`).toBe(0);
		return;
	}
	for (const [key, entry] of snapshot.statsByTrackKey) {
		expect(entry.connectionId, `${message}: stats entry ${key} for cleared connection`).toBe(
			snapshot.statsConnectionId,
		);
	}
	const probe = selectVoiceMediaGraphStatsEntry(snapshot, {direction: 'recv', kind: 'video'});
	if (probe) {
		expect(probe.connectionId, `${message}: stats selector returned a cleared connection`).toBe(
			snapshot.statsConnectionId,
		);
	}
}

function assertSimulationStep(snapshot: VoiceMediaGraphSnapshot, context: SimulationStepContext): void {
	const message = simulationStepMessage(context);
	expect(checkVoiceMediaGraphInvariants(snapshot), `${message}: invariant violations`).toEqual([]);
	assertSimulationActualSubscribed(snapshot, message);
	assertSimulationDeferredSubset(snapshot, message);
	assertSimulationFailureRenderOrdering(snapshot, message);
	assertSimulationTileStates(snapshot, message);
	assertSimulationStats(snapshot, message);
}

function runSimulation(seed: number): VoiceMediaGraphSnapshot {
	const rng = createSimulationRng(seed);
	const pool = buildSimulationBuilderPool(rng);
	let snapshot = createVoiceMediaGraphSnapshot();
	let nowMs = SIMULATION_EPOCH_MS;
	for (let step = 0; step < SIMULATION_STEP_COUNT; step += 1) {
		nowMs += 1 + rng.int(SIMULATION_STEP_ADVANCE_MS_MAX);
		const event = nextSimulationEvent(rng, snapshot, pool, nowMs);
		snapshot = transitionVoiceMediaGraph(snapshot, event);
		if (snapshot.subscriptionCommands.length > SIMULATION_COMMAND_DRAIN_THRESHOLD) {
			snapshot = transitionVoiceMediaGraph(snapshot, {type: 'subscription.clearCommands'});
		}
		assertSimulationStep(snapshot, {seed, step, eventType: event.type});
	}
	return snapshot;
}

describe('VoiceMediaGraphSimulation', () => {
	for (const seed of SIMULATION_SEEDS) {
		it(`maintains every invariant across ${SIMULATION_STEP_COUNT} random steps for seed ${seed}`, () => {
			runSimulation(seed);
		});
	}

	it('replays every seed deterministically to a deeply equal final snapshot', () => {
		for (const seed of SIMULATION_SEEDS) {
			const first = runSimulation(seed);
			const second = runSimulation(seed);
			expect(second, `seed=${seed} replay diverged`).toEqual(first);
		}
	}, 60_000);
});
