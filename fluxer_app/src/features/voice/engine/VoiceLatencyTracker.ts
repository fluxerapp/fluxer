// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceEngineV2LatencyDataPoint as LatencyDataPoint} from '@fluxer/voice_engine_v2/src/protocol/types';

const DEFAULT_VOICE_LATENCY_HISTORY_LIMIT = 60;
const VOICE_LATENCY_STALE_AFTER_MS = 10_000;

type VoiceLatencyStatus = 'idle' | 'measuring' | 'fresh' | 'stale';

export interface VoiceLatencySnapshot {
	tracking: boolean;
	status: VoiceLatencyStatus;
	currentLatency: number | null;
	averageLatency: number | null;
	latencyHistory: Array<LatencyDataPoint>;
	lastSampledAt: number | null;
	lastObservedAt: number | null;
}

export interface VoiceLatencySnapshotOptions {
	tracking?: boolean;
	now?: number;
}

export interface VoiceLatencyRecordOptions {
	historyLimit?: number;
}

function latencyStatusFor(
	snapshot: Pick<VoiceLatencySnapshot, 'tracking' | 'currentLatency' | 'lastSampledAt'>,
	now: number,
): VoiceLatencyStatus {
	if (!snapshot.tracking) return 'idle';
	if (snapshot.currentLatency == null || snapshot.lastSampledAt == null) return 'measuring';
	return now - snapshot.lastSampledAt > VOICE_LATENCY_STALE_AFTER_MS ? 'stale' : 'fresh';
}

function roundedLatency(latency: number): number | null {
	if (!Number.isFinite(latency) || latency < 0) return null;
	return Math.round(latency);
}

function averageLatency(history: ReadonlyArray<LatencyDataPoint>): number | null {
	if (history.length === 0) return null;
	let totalLatency = 0;
	for (const point of history) {
		totalLatency += point.latency;
	}
	return Math.round(totalLatency / history.length);
}

function appendLatencySample(
	history: ReadonlyArray<LatencyDataPoint>,
	sample: LatencyDataPoint,
	historyLimit: number,
): Array<LatencyDataPoint> {
	if (historyLimit <= 0) return [];
	const retainedCount = Math.min(history.length, historyLimit - 1);
	const nextHistory = new Array<LatencyDataPoint>(retainedCount + 1);
	const startIndex = history.length - retainedCount;
	for (let i = 0; i < retainedCount; i += 1) {
		nextHistory[i] = history[startIndex + i];
	}
	nextHistory[retainedCount] = sample;
	return nextHistory;
}

export function createVoiceLatencySnapshot(options: VoiceLatencySnapshotOptions = {}): VoiceLatencySnapshot {
	const tracking = options.tracking === true;
	return {
		tracking,
		status: tracking ? 'measuring' : 'idle',
		currentLatency: null,
		averageLatency: null,
		latencyHistory: [],
		lastSampledAt: null,
		lastObservedAt: options.now ?? null,
	};
}

export function startVoiceLatencyTracking(
	snapshot: VoiceLatencySnapshot,
	now: number = Date.now(),
): VoiceLatencySnapshot {
	const next = {...snapshot, tracking: true, lastObservedAt: snapshot.lastObservedAt ?? now};
	return {...next, status: latencyStatusFor(next, now)};
}

export function stopVoiceLatencyTracking(
	snapshot: VoiceLatencySnapshot,
	now: number = Date.now(),
): VoiceLatencySnapshot {
	const next = {...snapshot, tracking: false, lastObservedAt: snapshot.lastObservedAt ?? now};
	return {...next, status: latencyStatusFor(next, now)};
}

export function resetVoiceLatencySnapshot(options: VoiceLatencySnapshotOptions = {}): VoiceLatencySnapshot {
	return createVoiceLatencySnapshot(options);
}

export function observeVoiceLatencyGap(
	snapshot: VoiceLatencySnapshot,
	timestamp: number = Date.now(),
): VoiceLatencySnapshot {
	const next = {...snapshot, lastObservedAt: timestamp};
	return {...next, status: latencyStatusFor(next, timestamp)};
}

export function recordVoiceLatencySample(
	snapshot: VoiceLatencySnapshot,
	timestamp: number,
	latency: number,
	options: VoiceLatencyRecordOptions = {},
): VoiceLatencySnapshot {
	const sample = roundedLatency(latency);
	if (sample == null) return observeVoiceLatencyGap(snapshot, timestamp);
	const historyLimit = options.historyLimit ?? DEFAULT_VOICE_LATENCY_HISTORY_LIMIT;
	const latencyHistory = appendLatencySample(snapshot.latencyHistory, {timestamp, latency: sample}, historyLimit);
	const next = {
		...snapshot,
		tracking: true,
		status: 'fresh' as const,
		currentLatency: sample,
		averageLatency: averageLatency(latencyHistory),
		latencyHistory,
		lastSampledAt: timestamp,
		lastObservedAt: timestamp,
	};
	return next;
}
