// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVoiceLatencySnapshot,
	type LatencyDataPoint,
	observeVoiceLatencyGap,
	recordVoiceLatencySample,
	resetVoiceLatencySnapshot,
	startVoiceLatencyTracking,
	stopVoiceLatencyTracking,
	type VoiceLatencySnapshot,
} from '@app/features/voice/engine/VoiceLatencyTracker';
import type {
	VoiceEngineV2PerTrackStats as PerTrackStats,
	VoiceEngineV2TransportInfo as TransportInfo,
	VoiceEngineV2VoiceStats as VoiceStats,
	VoiceEngineV2StatsSample as VoiceStatsSample,
} from '@fluxer/voice_engine_v2';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export const MAX_LATENCY_HISTORY = 60;
export const MAX_TIME_SERIES_SAMPLES = 60;

export interface VoiceStatsRtpCounter {
	bytes?: number;
	packetsLost?: number;
	packetsReceived?: number;
	timestamp: number;
}

export interface VoiceStatsMachineContext {
	roomIdentity: unknown | null;
	latencyIntervalActive: boolean;
	statsIntervalActive: boolean;
	statsCollectionInFlight: boolean;
	statsGeneration: number;
	latency: VoiceLatencySnapshot;
	currentLatency: number | null;
	averageLatency: number | null;
	latencyHistory: Array<LatencyDataPoint>;
	voiceStats: VoiceStats;
	perTrackStats: Array<PerTrackStats>;
	statsTimeSeries: Array<VoiceStatsSample>;
	publisherTransport: TransportInfo | null;
	subscriberTransport: TransportInfo | null;
	rtpCounters: Map<string, VoiceStatsRtpCounter>;
	connectionStartTime: number | null;
	reconnectionCount: number;
}

export type VoiceStatsEvent =
	| {type: 'room.set'; roomIdentity: unknown | null; now: number}
	| {type: 'latency.start'}
	| {type: 'latency.stop'}
	| {type: 'latency.sample'; timestamp: number; latency: number}
	| {type: 'latency.gap'; timestamp: number}
	| {type: 'stats.start'}
	| {type: 'stats.stop'}
	| {type: 'stats.tick'; timestamp: number}
	| {type: 'stats.collectionStarted'; generation: number; roomIdentity: unknown}
	| {
			type: 'stats.collectionSucceeded';
			generation: number;
			roomIdentity: unknown;
			timestamp: number;
			participantCount: number;
			rtt: number;
			tracks: Array<PerTrackStats>;
			publisherTransport: TransportInfo | null;
			subscriberTransport: TransportInfo | null;
			rtpCounters: Map<string, VoiceStatsRtpCounter>;
			activeCounterIds: Set<string>;
	  }
	| {type: 'stats.collectionFinished'; generation: number}
	| {type: 'reconnection.increment'}
	| {type: 'stats.reset'}
	| {type: 'stats.cleanup'};

export type VoiceStatsCollectionDecision =
	| {type: 'skip'; reason: 'no-room' | 'not-tracking' | 'in-flight'}
	| VoiceStatsCollectDecision;

export interface VoiceStatsCollectDecision {
	type: 'collect';
	generation: number;
	roomIdentity: unknown;
	rtpCounters: Map<string, VoiceStatsRtpCounter>;
}

export function createInitialVoiceStats(): VoiceStats {
	return {
		audioSendBitrate: 0,
		audioRecvBitrate: 0,
		videoSendBitrate: 0,
		videoRecvBitrate: 0,
		audioPacketLoss: 0,
		videoPacketLoss: 0,
		rtt: 0,
		jitter: 0,
		participantCount: 0,
		duration: 0,
	};
}

function initialContext(): VoiceStatsMachineContext {
	return {
		roomIdentity: null,
		latencyIntervalActive: false,
		statsIntervalActive: false,
		statsCollectionInFlight: false,
		statsGeneration: 0,
		latency: createVoiceLatencySnapshot(),
		currentLatency: null,
		averageLatency: null,
		latencyHistory: [],
		voiceStats: createInitialVoiceStats(),
		perTrackStats: [],
		statsTimeSeries: [],
		publisherTransport: null,
		subscriberTransport: null,
		rtpCounters: new Map(),
		connectionStartTime: null,
		reconnectionCount: 0,
	};
}

function syncLatencyContext(
	context: VoiceStatsMachineContext,
	latency: VoiceLatencySnapshot,
): VoiceStatsMachineContext {
	return {
		...context,
		latency,
		latencyIntervalActive: latency.tracking,
		currentLatency: latency.currentLatency,
		averageLatency: latency.averageLatency,
		latencyHistory: latency.latencyHistory,
	};
}

function setRoom(
	context: VoiceStatsMachineContext,
	roomIdentity: unknown | null,
	now: number,
): VoiceStatsMachineContext {
	const roomChanged = context.roomIdentity !== roomIdentity;
	const nextContext = {
		...context,
		roomIdentity,
		connectionStartTime: roomIdentity !== null ? now : null,
		statsCollectionInFlight: roomChanged ? false : context.statsCollectionInFlight,
		statsGeneration: roomChanged ? context.statsGeneration + 1 : context.statsGeneration,
		rtpCounters: roomChanged ? new Map() : context.rtpCounters,
	};
	if (!roomChanged) return nextContext;
	return syncLatencyContext(
		nextContext,
		resetVoiceLatencySnapshot({tracking: context.latency.tracking && roomIdentity != null, now}),
	);
}

function startLatency(context: VoiceStatsMachineContext): VoiceStatsMachineContext {
	if (context.latencyIntervalActive) return context;
	return syncLatencyContext(context, startVoiceLatencyTracking(context.latency));
}

function stopLatency(context: VoiceStatsMachineContext): VoiceStatsMachineContext {
	if (!context.latencyIntervalActive) return context;
	return syncLatencyContext(context, stopVoiceLatencyTracking(context.latency));
}

function recordLatencySample(
	context: VoiceStatsMachineContext,
	timestamp: number,
	latency: number,
): VoiceStatsMachineContext {
	if (!context.latencyIntervalActive) return context;
	return syncLatencyContext(
		context,
		recordVoiceLatencySample(context.latency, timestamp, latency, {historyLimit: MAX_LATENCY_HISTORY}),
	);
}

function recordLatencyGap(context: VoiceStatsMachineContext, timestamp: number): VoiceStatsMachineContext {
	if (!context.latencyIntervalActive) return context;
	return syncLatencyContext(context, observeVoiceLatencyGap(context.latency, timestamp));
}

function startStats(context: VoiceStatsMachineContext): VoiceStatsMachineContext {
	if (context.statsIntervalActive) return context;
	return {
		...context,
		statsIntervalActive: true,
		statsGeneration: context.statsGeneration + 1,
		statsCollectionInFlight: false,
	};
}

function stopStats(context: VoiceStatsMachineContext): VoiceStatsMachineContext {
	if (!context.statsIntervalActive && !context.statsCollectionInFlight) return context;
	return {
		...context,
		statsIntervalActive: false,
		statsGeneration: context.statsGeneration + 1,
		statsCollectionInFlight: false,
	};
}

function tickStats(context: VoiceStatsMachineContext, timestamp: number): VoiceStatsMachineContext {
	if (!context.statsIntervalActive || context.connectionStartTime == null) return context;
	const duration = Math.floor((timestamp - context.connectionStartTime) / 1000);
	if (duration === context.voiceStats.duration) return context;
	return {
		...context,
		voiceStats: {
			...context.voiceStats,
			duration,
		},
	};
}

function startCollection(
	context: VoiceStatsMachineContext,
	generation: number,
	roomIdentity: unknown,
): VoiceStatsMachineContext {
	if (
		!context.statsIntervalActive ||
		context.statsCollectionInFlight ||
		context.roomIdentity !== roomIdentity ||
		context.statsGeneration !== generation
	) {
		return context;
	}
	return {...context, statsCollectionInFlight: true};
}

function pruneRtpCounters(
	rtpCounters: Map<string, VoiceStatsRtpCounter>,
	activeCounterIds: Set<string>,
): Map<string, VoiceStatsRtpCounter> {
	const next = new Map<string, VoiceStatsRtpCounter>();
	for (const [counterId, counter] of rtpCounters) {
		if (activeCounterIds.has(counterId)) {
			next.set(counterId, counter);
		}
	}
	return next;
}

function buildVoiceStats(
	context: VoiceStatsMachineContext,
	event: Extract<VoiceStatsEvent, {type: 'stats.collectionSucceeded'}>,
): {stats: VoiceStats; sample: VoiceStatsSample} {
	let audioSendBitrate = 0;
	let audioRecvBitrate = 0;
	let videoSendBitrate = 0;
	let videoRecvBitrate = 0;
	let audioPacketLossSum = 0;
	let videoPacketLossSum = 0;
	let jitterSumMs = 0;
	let audioPacketLossCount = 0;
	let videoPacketLossCount = 0;
	let jitterCount = 0;
	for (const track of event.tracks) {
		if (track.direction === 'send' && track.kind === 'audio') audioSendBitrate += track.bitrateKbps;
		else if (track.direction === 'send' && track.kind === 'video') videoSendBitrate += track.bitrateKbps;
		else if (track.direction === 'recv' && track.kind === 'audio') {
			audioRecvBitrate += track.bitrateKbps;
			if (typeof track.packetsLossPercent === 'number') {
				audioPacketLossSum += track.packetsLossPercent;
				audioPacketLossCount += 1;
			}
			if (typeof track.jitterMs === 'number') {
				jitterSumMs += track.jitterMs;
				jitterCount += 1;
			}
		} else if (track.direction === 'recv' && track.kind === 'video') {
			videoRecvBitrate += track.bitrateKbps;
			if (typeof track.packetsLossPercent === 'number') {
				videoPacketLossSum += track.packetsLossPercent;
				videoPacketLossCount += 1;
			}
		}
	}
	const audioPacketLoss = audioPacketLossCount > 0 ? audioPacketLossSum / audioPacketLossCount : 0;
	const videoPacketLoss = videoPacketLossCount > 0 ? videoPacketLossSum / videoPacketLossCount : 0;
	const jitter = jitterCount > 0 ? jitterSumMs / jitterCount : 0;
	const duration = context.connectionStartTime ? Math.floor((event.timestamp - context.connectionStartTime) / 1000) : 0;
	const stats: VoiceStats = {
		audioSendBitrate: Math.round(audioSendBitrate),
		audioRecvBitrate: Math.round(audioRecvBitrate),
		videoSendBitrate: Math.round(videoSendBitrate),
		videoRecvBitrate: Math.round(videoRecvBitrate),
		audioPacketLoss: Math.round(audioPacketLoss * 10) / 10,
		videoPacketLoss: Math.round(videoPacketLoss * 10) / 10,
		rtt: Math.round(event.rtt),
		jitter: Math.round(jitter),
		participantCount: event.participantCount,
		duration,
	};
	return {
		stats,
		sample: {
			timestamp: event.timestamp,
			rtt: stats.rtt,
			jitter: stats.jitter,
			audioPacketLoss: stats.audioPacketLoss,
			videoPacketLoss: stats.videoPacketLoss,
			audioSendBitrate: stats.audioSendBitrate,
			audioRecvBitrate: stats.audioRecvBitrate,
			videoSendBitrate: stats.videoSendBitrate,
			videoRecvBitrate: stats.videoRecvBitrate,
		},
	};
}

function appendVoiceStatsSample(
	timeSeries: ReadonlyArray<VoiceStatsSample>,
	sample: VoiceStatsSample,
	limit: number,
): Array<VoiceStatsSample> {
	if (limit <= 0) return [];
	const retainedCount = Math.min(timeSeries.length, limit - 1);
	const nextTimeSeries = new Array<VoiceStatsSample>(retainedCount + 1);
	const startIndex = timeSeries.length - retainedCount;
	for (let i = 0; i < retainedCount; i += 1) {
		nextTimeSeries[i] = timeSeries[startIndex + i];
	}
	nextTimeSeries[retainedCount] = sample;
	return nextTimeSeries;
}

function applyCollectionResult(
	context: VoiceStatsMachineContext,
	event: Extract<VoiceStatsEvent, {type: 'stats.collectionSucceeded'}>,
): VoiceStatsMachineContext {
	if (
		!context.statsIntervalActive ||
		!context.statsCollectionInFlight ||
		context.statsGeneration !== event.generation ||
		context.roomIdentity !== event.roomIdentity
	) {
		return context;
	}
	const {stats, sample} = buildVoiceStats(context, event);
	return {
		...context,
		voiceStats: stats,
		perTrackStats: event.tracks,
		publisherTransport: event.publisherTransport,
		subscriberTransport: event.subscriberTransport,
		statsTimeSeries: appendVoiceStatsSample(context.statsTimeSeries, sample, MAX_TIME_SERIES_SAMPLES),
		rtpCounters: pruneRtpCounters(event.rtpCounters, event.activeCounterIds),
	};
}

function finishCollection(context: VoiceStatsMachineContext, generation: number): VoiceStatsMachineContext {
	if (context.statsGeneration !== generation) return context;
	return {...context, statsCollectionInFlight: false};
}

function incrementReconnectionCount(context: VoiceStatsMachineContext): VoiceStatsMachineContext {
	return {...context, reconnectionCount: context.reconnectionCount + 1};
}

function resetState(context: VoiceStatsMachineContext): VoiceStatsMachineContext {
	return {
		...initialContext(),
		statsGeneration: context.statsGeneration + 1,
	};
}

export const voiceStatsStateMachine = setup({
	types: {} as {
		context: VoiceStatsMachineContext;
		events: VoiceStatsEvent;
	},
	actions: {
		setRoom: assign(({context, event}) =>
			event.type === 'room.set' ? setRoom(context, event.roomIdentity, event.now) : context,
		),
		startLatency: assign(({context}) => startLatency(context)),
		stopLatency: assign(({context}) => stopLatency(context)),
		recordLatencySample: assign(({context, event}) =>
			event.type === 'latency.sample' ? recordLatencySample(context, event.timestamp, event.latency) : context,
		),
		recordLatencyGap: assign(({context, event}) =>
			event.type === 'latency.gap' ? recordLatencyGap(context, event.timestamp) : context,
		),
		startStats: assign(({context}) => startStats(context)),
		stopStats: assign(({context}) => stopStats(context)),
		tickStats: assign(({context, event}) =>
			event.type === 'stats.tick' ? tickStats(context, event.timestamp) : context,
		),
		startCollection: assign(({context, event}) =>
			event.type === 'stats.collectionStarted'
				? startCollection(context, event.generation, event.roomIdentity)
				: context,
		),
		applyCollectionResult: assign(({context, event}) =>
			event.type === 'stats.collectionSucceeded' ? applyCollectionResult(context, event) : context,
		),
		finishCollection: assign(({context, event}) =>
			event.type === 'stats.collectionFinished' ? finishCollection(context, event.generation) : context,
		),
		incrementReconnectionCount: assign(({context}) => incrementReconnectionCount(context)),
		reset: assign(({context}) => resetState(context)),
	},
	guards: {
		trackingBoth: ({context}) => context.latencyIntervalActive && context.statsIntervalActive,
		trackingLatency: ({context}) => context.latencyIntervalActive,
		trackingStats: ({context}) => context.statsIntervalActive,
	},
}).createMachine({
	id: 'voiceStats',
	context: () => initialContext(),
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'trackingBoth', target: 'trackingBoth'},
				{guard: 'trackingLatency', target: 'trackingLatency'},
				{guard: 'trackingStats', target: 'trackingStats'},
				{target: 'idle'},
			],
		},
		idle: {
			on: {
				'room.set': {target: 'routing', actions: 'setRoom'},
				'latency.start': {target: 'routing', actions: 'startLatency'},
				'latency.stop': {target: 'routing', actions: 'stopLatency'},
				'latency.sample': {target: 'routing', actions: 'recordLatencySample'},
				'latency.gap': {target: 'routing', actions: 'recordLatencyGap'},
				'stats.start': {target: 'routing', actions: 'startStats'},
				'stats.stop': {target: 'routing', actions: 'stopStats'},
				'stats.tick': {target: 'routing', actions: 'tickStats'},
				'stats.collectionStarted': {target: 'routing', actions: 'startCollection'},
				'stats.collectionSucceeded': {target: 'routing', actions: 'applyCollectionResult'},
				'stats.collectionFinished': {target: 'routing', actions: 'finishCollection'},
				'reconnection.increment': {target: 'routing', actions: 'incrementReconnectionCount'},
				'stats.reset': {target: 'routing', actions: 'reset'},
				'stats.cleanup': {target: 'routing', actions: 'reset'},
			},
		},
		trackingLatency: {
			on: {
				'room.set': {target: 'routing', actions: 'setRoom'},
				'latency.start': {target: 'routing', actions: 'startLatency'},
				'latency.stop': {target: 'routing', actions: 'stopLatency'},
				'latency.sample': {target: 'routing', actions: 'recordLatencySample'},
				'latency.gap': {target: 'routing', actions: 'recordLatencyGap'},
				'stats.start': {target: 'routing', actions: 'startStats'},
				'stats.stop': {target: 'routing', actions: 'stopStats'},
				'stats.tick': {target: 'routing', actions: 'tickStats'},
				'stats.collectionStarted': {target: 'routing', actions: 'startCollection'},
				'stats.collectionSucceeded': {target: 'routing', actions: 'applyCollectionResult'},
				'stats.collectionFinished': {target: 'routing', actions: 'finishCollection'},
				'reconnection.increment': {target: 'routing', actions: 'incrementReconnectionCount'},
				'stats.reset': {target: 'routing', actions: 'reset'},
				'stats.cleanup': {target: 'routing', actions: 'reset'},
			},
		},
		trackingStats: {
			on: {
				'room.set': {target: 'routing', actions: 'setRoom'},
				'latency.start': {target: 'routing', actions: 'startLatency'},
				'latency.stop': {target: 'routing', actions: 'stopLatency'},
				'latency.sample': {target: 'routing', actions: 'recordLatencySample'},
				'latency.gap': {target: 'routing', actions: 'recordLatencyGap'},
				'stats.start': {target: 'routing', actions: 'startStats'},
				'stats.stop': {target: 'routing', actions: 'stopStats'},
				'stats.tick': {target: 'routing', actions: 'tickStats'},
				'stats.collectionStarted': {target: 'routing', actions: 'startCollection'},
				'stats.collectionSucceeded': {target: 'routing', actions: 'applyCollectionResult'},
				'stats.collectionFinished': {target: 'routing', actions: 'finishCollection'},
				'reconnection.increment': {target: 'routing', actions: 'incrementReconnectionCount'},
				'stats.reset': {target: 'routing', actions: 'reset'},
				'stats.cleanup': {target: 'routing', actions: 'reset'},
			},
		},
		trackingBoth: {
			on: {
				'room.set': {target: 'routing', actions: 'setRoom'},
				'latency.start': {target: 'routing', actions: 'startLatency'},
				'latency.stop': {target: 'routing', actions: 'stopLatency'},
				'latency.sample': {target: 'routing', actions: 'recordLatencySample'},
				'latency.gap': {target: 'routing', actions: 'recordLatencyGap'},
				'stats.start': {target: 'routing', actions: 'startStats'},
				'stats.stop': {target: 'routing', actions: 'stopStats'},
				'stats.tick': {target: 'routing', actions: 'tickStats'},
				'stats.collectionStarted': {target: 'routing', actions: 'startCollection'},
				'stats.collectionSucceeded': {target: 'routing', actions: 'applyCollectionResult'},
				'stats.collectionFinished': {target: 'routing', actions: 'finishCollection'},
				'reconnection.increment': {target: 'routing', actions: 'incrementReconnectionCount'},
				'stats.reset': {target: 'routing', actions: 'reset'},
				'stats.cleanup': {target: 'routing', actions: 'reset'},
			},
		},
	},
});

export type VoiceStatsSnapshot = SnapshotFrom<typeof voiceStatsStateMachine>;
export type VoiceStatsStateValue = 'idle' | 'trackingLatency' | 'trackingStats' | 'trackingBoth';

export function createVoiceStatsSnapshot(): VoiceStatsSnapshot {
	return initialTransition(voiceStatsStateMachine)[0];
}

export function transitionVoiceStatsSnapshot(snapshot: VoiceStatsSnapshot, event: VoiceStatsEvent): VoiceStatsSnapshot {
	return transition(voiceStatsStateMachine, snapshot, event)[0] as VoiceStatsSnapshot;
}

export function getVoiceStatsStateValue(snapshot: VoiceStatsSnapshot): VoiceStatsStateValue {
	if (snapshot.value === 'trackingLatency' || snapshot.value === 'trackingStats' || snapshot.value === 'trackingBoth') {
		return snapshot.value;
	}
	return 'idle';
}

export function selectVoiceStatsCollectionDecision(snapshot: VoiceStatsSnapshot): VoiceStatsCollectionDecision {
	if (snapshot.context.roomIdentity === null) return {type: 'skip', reason: 'no-room'};
	if (!snapshot.context.statsIntervalActive) return {type: 'skip', reason: 'not-tracking'};
	if (snapshot.context.statsCollectionInFlight) return {type: 'skip', reason: 'in-flight'};
	return {
		type: 'collect',
		generation: snapshot.context.statsGeneration,
		roomIdentity: snapshot.context.roomIdentity,
		rtpCounters: new Map(snapshot.context.rtpCounters),
	};
}
