// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createInitialVoiceStats,
	createVoiceStatsSnapshot,
	getVoiceStatsStateValue,
	MAX_LATENCY_HISTORY,
	MAX_TIME_SERIES_SAMPLES,
	selectVoiceStatsCollectionDecision,
	transitionVoiceStatsSnapshot,
	type VoiceStatsSnapshot,
} from '@app/features/voice/engine/VoiceStatsStateMachine';
import type {VoiceEngineV2PerTrackStats, VoiceEngineV2TransportInfo} from '@fluxer/voice_engine_v2';
import {describe, expect, it} from 'vitest';

const roomA = {id: 'room-a'};
const roomB = {id: 'room-b'};

const publisherTransport: VoiceEngineV2TransportInfo = {
	candidatePairState: 'succeeded',
	currentRoundTripTimeMs: 42,
};

const sampleTracks: Array<VoiceEngineV2PerTrackStats> = [
	{direction: 'send', kind: 'audio', bitrateKbps: 16},
	{direction: 'recv', kind: 'audio', bitrateKbps: 32, packetsLossPercent: 1.2, jitterMs: 4.8},
	{direction: 'send', kind: 'video', bitrateKbps: 512},
	{direction: 'recv', kind: 'video', bitrateKbps: 256, packetsLossPercent: 2.4},
];

function startStats(snapshot: VoiceStatsSnapshot, roomIdentity: unknown = roomA): VoiceStatsSnapshot {
	snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'room.set', roomIdentity, now: 1000});
	return transitionVoiceStatsSnapshot(snapshot, {type: 'stats.start'});
}

function applyStatsCollection(
	snapshot: VoiceStatsSnapshot,
	options: {
		timestamp?: number;
		tracks?: Array<VoiceEngineV2PerTrackStats>;
		activeCounterIds?: Set<string>;
		rtpCounters?: Map<string, {timestamp: number; bytes?: number}>;
	} = {},
): VoiceStatsSnapshot {
	const decision = selectVoiceStatsCollectionDecision(snapshot);
	if (decision.type !== 'collect') throw new Error(`expected collect decision, got ${decision.reason}`);
	snapshot = transitionVoiceStatsSnapshot(snapshot, {
		type: 'stats.collectionStarted',
		generation: decision.generation,
		roomIdentity: decision.roomIdentity,
	});
	snapshot = transitionVoiceStatsSnapshot(snapshot, {
		type: 'stats.collectionSucceeded',
		generation: decision.generation,
		roomIdentity: decision.roomIdentity,
		timestamp: options.timestamp ?? 3000,
		participantCount: 3,
		rtt: 37.6,
		tracks: options.tracks ?? sampleTracks,
		publisherTransport,
		subscriberTransport: null,
		rtpCounters: options.rtpCounters ?? new Map([['publisher:outbound-rtp:audio-a', {timestamp: 3000, bytes: 100}]]),
		activeCounterIds: options.activeCounterIds ?? new Set(['publisher:outbound-rtp:audio-a']),
	});
	return transitionVoiceStatsSnapshot(snapshot, {type: 'stats.collectionFinished', generation: decision.generation});
}

describe('VoiceStatsStateMachine', () => {
	it('keeps start and stop idempotent', () => {
		let snapshot = createVoiceStatsSnapshot();
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'latency.start'});
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'latency.start'});
		expect(getVoiceStatsStateValue(snapshot)).toBe('trackingLatency');
		expect(snapshot.context.latencyIntervalActive).toBe(true);

		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'stats.start'});
		const generationAfterStart = snapshot.context.statsGeneration;
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'stats.start'});
		expect(getVoiceStatsStateValue(snapshot)).toBe('trackingBoth');
		expect(snapshot.context.statsGeneration).toBe(generationAfterStart);

		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'latency.stop'});
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'latency.stop'});
		expect(getVoiceStatsStateValue(snapshot)).toBe('trackingStats');

		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'stats.stop'});
		const generationAfterStop = snapshot.context.statsGeneration;
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'stats.stop'});
		expect(getVoiceStatsStateValue(snapshot)).toBe('idle');
		expect(snapshot.context.statsGeneration).toBe(generationAfterStop);
	});

	it('clears RTP counters when the room identity changes', () => {
		let snapshot = startStats(createVoiceStatsSnapshot());
		snapshot = applyStatsCollection(snapshot, {
			rtpCounters: new Map([
				['counter-a', {timestamp: 2000, bytes: 100}],
				['counter-b', {timestamp: 2000, bytes: 200}],
			]),
			activeCounterIds: new Set(['counter-a']),
		});
		expect([...snapshot.context.rtpCounters.keys()]).toEqual(['counter-a']);

		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'room.set', roomIdentity: roomB, now: 4000});
		expect(snapshot.context.roomIdentity).toBe(roomB);
		expect(snapshot.context.rtpCounters.size).toBe(0);
	});

	it('discards stale async stats samples by generation', () => {
		let snapshot = startStats(createVoiceStatsSnapshot());
		const decision = selectVoiceStatsCollectionDecision(snapshot);
		if (decision.type !== 'collect') throw new Error('expected collect decision');
		snapshot = transitionVoiceStatsSnapshot(snapshot, {
			type: 'stats.collectionStarted',
			generation: decision.generation,
			roomIdentity: decision.roomIdentity,
		});
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'stats.stop'});
		snapshot = transitionVoiceStatsSnapshot(snapshot, {
			type: 'stats.collectionSucceeded',
			generation: decision.generation,
			roomIdentity: decision.roomIdentity,
			timestamp: 3000,
			participantCount: 2,
			rtt: 99,
			tracks: sampleTracks,
			publisherTransport,
			subscriberTransport: null,
			rtpCounters: new Map([['stale-counter', {timestamp: 3000, bytes: 1}]]),
			activeCounterIds: new Set(['stale-counter']),
		});
		snapshot = transitionVoiceStatsSnapshot(snapshot, {
			type: 'stats.collectionFinished',
			generation: decision.generation,
		});

		expect(snapshot.context.voiceStats).toEqual(createInitialVoiceStats());
		expect(snapshot.context.statsTimeSeries).toEqual([]);
		expect(snapshot.context.rtpCounters.size).toBe(0);
	});

	it('guards collection while another stats collection is in flight', () => {
		let snapshot = startStats(createVoiceStatsSnapshot());
		const decision = selectVoiceStatsCollectionDecision(snapshot);
		if (decision.type !== 'collect') throw new Error('expected collect decision');
		snapshot = transitionVoiceStatsSnapshot(snapshot, {
			type: 'stats.collectionStarted',
			generation: decision.generation,
			roomIdentity: decision.roomIdentity,
		});

		expect(selectVoiceStatsCollectionDecision(snapshot)).toEqual({type: 'skip', reason: 'in-flight'});

		snapshot = transitionVoiceStatsSnapshot(snapshot, {
			type: 'stats.collectionFinished',
			generation: decision.generation,
		});
		expect(snapshot.context.statsCollectionInFlight).toBe(false);
	});

	it('caps latency history and recomputes current and average latency', () => {
		let snapshot = transitionVoiceStatsSnapshot(createVoiceStatsSnapshot(), {type: 'latency.start'});
		for (let i = 0; i < MAX_LATENCY_HISTORY + 5; i++) {
			snapshot = transitionVoiceStatsSnapshot(snapshot, {
				type: 'latency.sample',
				timestamp: i,
				latency: 10 + i,
			});
		}

		expect(snapshot.context.latencyHistory).toHaveLength(MAX_LATENCY_HISTORY);
		expect(snapshot.context.latencyHistory[0]).toEqual({timestamp: 5, latency: 15});
		expect(snapshot.context.currentLatency).toBe(74);
		expect(snapshot.context.averageLatency).toBe(45);
	});

	it('preserves the last measured latency across missing latency samples', () => {
		let snapshot = transitionVoiceStatsSnapshot(createVoiceStatsSnapshot(), {type: 'latency.start'});
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'latency.sample', timestamp: 1000, latency: 42});
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'latency.gap', timestamp: 1500});

		expect(snapshot.context.currentLatency).toBe(42);
		expect(snapshot.context.averageLatency).toBe(42);
		expect(snapshot.context.latency.status).toBe('fresh');
		expect(snapshot.context.latencyHistory).toEqual([{timestamp: 1000, latency: 42}]);
	});

	it('marks old latency samples stale without clearing their displayed value', () => {
		let snapshot = transitionVoiceStatsSnapshot(createVoiceStatsSnapshot(), {type: 'latency.start'});
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'latency.sample', timestamp: 1000, latency: 42});
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'latency.gap', timestamp: 12_001});

		expect(snapshot.context.currentLatency).toBe(42);
		expect(snapshot.context.latency.status).toBe('stale');
	});

	it('caps stats time-series samples', () => {
		let snapshot = startStats(createVoiceStatsSnapshot());
		for (let i = 0; i < MAX_TIME_SERIES_SAMPLES + 7; i++) {
			snapshot = applyStatsCollection(snapshot, {timestamp: 2000 + i});
		}

		expect(snapshot.context.statsTimeSeries).toHaveLength(MAX_TIME_SERIES_SAMPLES);
		expect(snapshot.context.statsTimeSeries[0]?.timestamp).toBe(2007);
		expect(snapshot.context.statsTimeSeries.at(-1)?.timestamp).toBe(2066);
	});

	it('ticks call duration without requiring a stats collection result', () => {
		let snapshot = startStats(createVoiceStatsSnapshot());
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'stats.tick', timestamp: 3500});

		expect(snapshot.context.voiceStats.duration).toBe(2);
		expect(snapshot.context.statsTimeSeries).toEqual([]);

		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'stats.stop'});
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'stats.tick', timestamp: 4500});

		expect(snapshot.context.voiceStats.duration).toBe(2);
	});

	it('cleanup and reset clear all derived state', () => {
		for (const eventType of ['stats.cleanup', 'stats.reset'] as const) {
			let snapshot = startStats(createVoiceStatsSnapshot());
			snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'latency.start'});
			snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'latency.sample', timestamp: 1200, latency: 42});
			snapshot = applyStatsCollection(snapshot);
			snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'reconnection.increment'});
			snapshot = transitionVoiceStatsSnapshot(snapshot, {type: eventType});

			expect(getVoiceStatsStateValue(snapshot)).toBe('idle');
			expect(snapshot.context.currentLatency).toBeNull();
			expect(snapshot.context.averageLatency).toBeNull();
			expect(snapshot.context.latencyHistory).toEqual([]);
			expect(snapshot.context.voiceStats).toEqual(createInitialVoiceStats());
			expect(snapshot.context.perTrackStats).toEqual([]);
			expect(snapshot.context.statsTimeSeries).toEqual([]);
			expect(snapshot.context.publisherTransport).toBeNull();
			expect(snapshot.context.subscriberTransport).toBeNull();
			expect(snapshot.context.rtpCounters.size).toBe(0);
			expect(snapshot.context.connectionStartTime).toBeNull();
			expect(snapshot.context.reconnectionCount).toBe(0);
			expect(snapshot.context.statsCollectionInFlight).toBe(false);
		}
	});

	it('tracks reconnect count independently of stats samples', () => {
		let snapshot = createVoiceStatsSnapshot();
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'reconnection.increment'});
		snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'reconnection.increment'});
		expect(snapshot.context.reconnectionCount).toBe(2);
	});

	it('handles repeated start and stop cycles without leaking active state', () => {
		let snapshot = createVoiceStatsSnapshot();
		for (let i = 0; i < 50; i++) {
			snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'latency.start'});
			snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'stats.start'});
			expect(getVoiceStatsStateValue(snapshot)).toBe('trackingBoth');
			snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'latency.stop'});
			snapshot = transitionVoiceStatsSnapshot(snapshot, {type: 'stats.stop'});
			expect(getVoiceStatsStateValue(snapshot)).toBe('idle');
			expect(snapshot.context.statsCollectionInFlight).toBe(false);
		}
	});
});
