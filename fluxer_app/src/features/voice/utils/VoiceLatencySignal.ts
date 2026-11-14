// SPDX-License-Identifier: AGPL-3.0-or-later

type LatencySignalTone = 'green' | 'yellow' | 'orange' | 'red';

export interface LatencySignalSample {
	latency: number;
}

interface LatencySignalLoadingState {
	kind: 'loading';
}

interface LatencySignalValueState {
	kind: 'value';
	baselineLatency: number;
	excessLatency: number;
	filledCount: 1 | 2 | 3 | 4;
	tone: LatencySignalTone;
}

export type LatencySignalState = LatencySignalLoadingState | LatencySignalValueState;
export type LatencySignalDeviationThresholds = readonly [number, number, number];

export const DEFAULT_LATENCY_SIGNAL_DEVIATION_THRESHOLDS: LatencySignalDeviationThresholds = [70, 160, 300];
const LATENCY_SIGNAL_BASELINE_SAMPLE_COUNT = 30;

function getMedian(values: ReadonlyArray<number>): number | null {
	if (values.length === 0) {
		return null;
	}
	const sorted = [...values].sort((a, b) => a - b);
	const middle = Math.floor(sorted.length / 2);
	if (sorted.length % 2 === 1) {
		return sorted[middle] ?? null;
	}
	const lower = sorted[middle - 1];
	const upper = sorted[middle];
	if (lower === undefined || upper === undefined) {
		return null;
	}
	return Math.round((lower + upper) / 2);
}

function getLatencySignalBaseline(latencyHistory: ReadonlyArray<LatencySignalSample>): number | null {
	const startIndex = Math.max(0, latencyHistory.length - LATENCY_SIGNAL_BASELINE_SAMPLE_COUNT);
	const values: Array<number> = [];
	for (let i = startIndex; i < latencyHistory.length; i += 1) {
		const latency = latencyHistory[i]?.latency;
		if (latency === undefined) continue;
		if (!Number.isFinite(latency)) continue;
		if (latency <= 0) continue;
		values.push(latency);
	}
	return getMedian(values);
}

export function getLatencySignalState(
	latency: number | null,
	latencyHistory: ReadonlyArray<LatencySignalSample> = [],
	deviationThresholds: LatencySignalDeviationThresholds = DEFAULT_LATENCY_SIGNAL_DEVIATION_THRESHOLDS,
): LatencySignalState {
	if (latency === null) {
		return {kind: 'loading'};
	}
	const baselineLatency = getLatencySignalBaseline(latencyHistory) ?? latency;
	const excessLatency = Math.max(0, latency - baselineLatency);
	const [good, ok, meh] = deviationThresholds;
	if (excessLatency < good) {
		return {kind: 'value', baselineLatency, excessLatency, filledCount: 4, tone: 'green'};
	}
	if (excessLatency < ok) {
		return {kind: 'value', baselineLatency, excessLatency, filledCount: 3, tone: 'yellow'};
	}
	if (excessLatency < meh) {
		return {kind: 'value', baselineLatency, excessLatency, filledCount: 2, tone: 'orange'};
	}
	return {kind: 'value', baselineLatency, excessLatency, filledCount: 1, tone: 'red'};
}
