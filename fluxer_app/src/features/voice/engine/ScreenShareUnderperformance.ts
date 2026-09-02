// SPDX-License-Identifier: AGPL-3.0-or-later

export const SCREEN_SHARE_UNDERPERFORMANCE_SAMPLE_INTERVAL_MS = 1000;

const SHORT_WINDOW_SAMPLES = 15;
const LONG_WINDOW_SAMPLES = 30;
const MIN_WINDOW_SAMPLES = 15;
const MIN_ENCODE_FRAME_RATE_FLOOR = 8;
const MIN_LIMITED_SAMPLE_RATIO = 0.5;
const QUALITY_CHANGE_SUPPRESSION_MS = 20_000;
const MAX_APPLIED_FRAME_RATE_RECORDS = 8;

export type ScreenShareQualityLimitationReason = 'none' | 'cpu' | 'bandwidth' | 'other' | 'unknown';

export type ScreenShareUnderperformanceReason = 'cpu' | 'bandwidth' | 'other';

export interface ScreenShareUnderperformanceSample {
	encodeFrameRate: number;
	limitationReason: ScreenShareQualityLimitationReason;
}

export interface ScreenShareUnderperformanceObservation {
	sample: ScreenShareUnderperformanceSample;
	requestedFrameRate: number;
	trackId: string;
	qualityChangeAt: number;
	now: number;
}

let deliberateQualityChangeAt = 0;
const appliedFrameRateByTrackId = new Map<string, number>();

export function noteDeliberateScreenShareQualityChange(): void {
	deliberateQualityChangeAt = Date.now();
}

export function getDeliberateScreenShareQualityChangeAt(): number {
	return deliberateQualityChangeAt;
}

export function noteAppliedScreenShareFrameRate(trackId: string, frameRate: number): void {
	appliedFrameRateByTrackId.delete(trackId);
	appliedFrameRateByTrackId.set(trackId, frameRate);
	while (appliedFrameRateByTrackId.size > MAX_APPLIED_FRAME_RATE_RECORDS) {
		const oldest = appliedFrameRateByTrackId.keys().next().value;
		if (oldest === undefined) break;
		appliedFrameRateByTrackId.delete(oldest);
	}
}

export function getAppliedScreenShareFrameRate(trackId: string): number | null {
	return appliedFrameRateByTrackId.get(trackId) ?? null;
}

export function parseScreenShareQualityLimitationReason(value: unknown): ScreenShareQualityLimitationReason {
	if (value === 'none' || value === 'cpu' || value === 'bandwidth' || value === 'other') {
		return value;
	}
	return 'unknown';
}

function getScreenShareEncodeFrameRateFloor(requestedFrameRate: number): number {
	return Math.max(MIN_ENCODE_FRAME_RATE_FLOOR, requestedFrameRate / 2);
}

function evaluateWindow(
	samples: ReadonlyArray<ScreenShareUnderperformanceSample>,
	windowSamples: number,
	requestedFrameRate: number,
): ScreenShareUnderperformanceReason | null {
	const window = samples.slice(-windowSamples);
	if (window.length < MIN_WINDOW_SAMPLES) return null;
	let totalFrameRate = 0;
	let cpuSamples = 0;
	let bandwidthSamples = 0;
	let otherSamples = 0;
	for (const sample of window) {
		totalFrameRate += sample.encodeFrameRate;
		if (sample.limitationReason === 'cpu') {
			cpuSamples += 1;
		} else if (sample.limitationReason === 'bandwidth') {
			bandwidthSamples += 1;
		} else if (sample.limitationReason === 'other') {
			otherSamples += 1;
		}
	}
	if (totalFrameRate / window.length >= getScreenShareEncodeFrameRateFloor(requestedFrameRate)) return null;
	if (cpuSamples + bandwidthSamples + otherSamples < window.length * MIN_LIMITED_SAMPLE_RATIO) return null;
	if (cpuSamples >= bandwidthSamples && cpuSamples >= otherSamples) return 'cpu';
	if (bandwidthSamples >= otherSamples) return 'bandwidth';
	return 'other';
}

export class ScreenShareUnderperformanceTracker {
	private samples: Array<ScreenShareUnderperformanceSample> = [];
	private requestedFrameRate: number | null = null;
	private trackId: string | null = null;
	private qualityChangeAt: number | null = null;

	reset(): void {
		this.samples = [];
		this.requestedFrameRate = null;
		this.trackId = null;
		this.qualityChangeAt = null;
	}

	observe(observation: ScreenShareUnderperformanceObservation): ScreenShareUnderperformanceReason | null {
		if (
			observation.requestedFrameRate !== this.requestedFrameRate ||
			observation.trackId !== this.trackId ||
			observation.qualityChangeAt !== this.qualityChangeAt
		) {
			this.samples = [];
			this.requestedFrameRate = observation.requestedFrameRate;
			this.trackId = observation.trackId;
			this.qualityChangeAt = observation.qualityChangeAt;
		}
		this.samples.push(observation.sample);
		if (this.samples.length > LONG_WINDOW_SAMPLES) {
			this.samples.splice(0, this.samples.length - LONG_WINDOW_SAMPLES);
		}
		if (observation.now - observation.qualityChangeAt < QUALITY_CHANGE_SUPPRESSION_MS) return null;
		return (
			evaluateWindow(this.samples, SHORT_WINDOW_SAMPLES, observation.requestedFrameRate) ??
			evaluateWindow(this.samples, LONG_WINDOW_SAMPLES, observation.requestedFrameRate)
		);
	}
}
