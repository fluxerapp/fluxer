// SPDX-License-Identifier: AGPL-3.0-or-later

export const HAVE_CURRENT_DATA = 2;

export const DECODABILITY_RECHECK_DELAY_MS = 1500;

export const DECODABILITY_MIN_ADVANCE_SECONDS = 0.5;

export interface VideoDecodabilitySample {
	videoHeight: number;
	readyState: number;
	currentTime: number;
}

export type VideoDecodability = 'decodable' | 'undecodable' | 'unknown';

export type DecodabilityFirstLook = 'decodable' | 'recheck' | 'unknown';

export function classifyDecodabilitySample(sample: VideoDecodabilitySample): DecodabilityFirstLook {
	if (!Number.isFinite(sample.videoHeight) || !Number.isFinite(sample.readyState)) return 'unknown';
	if (sample.videoHeight > 0) return 'decodable';
	if (sample.readyState >= HAVE_CURRENT_DATA) return 'recheck';
	return 'unknown';
}

export function classifyDecodabilityRecheck(
	first: VideoDecodabilitySample,
	second: VideoDecodabilitySample,
): VideoDecodability {
	if (!Number.isFinite(second.videoHeight)) return 'unknown';
	if (second.videoHeight > 0) return 'decodable';
	if (!Number.isFinite(first.currentTime) || !Number.isFinite(second.currentTime)) return 'unknown';
	if (second.currentTime > first.currentTime + DECODABILITY_MIN_ADVANCE_SECONDS) return 'undecodable';
	return 'unknown';
}

export type DecodabilityScheduler = (callback: () => void, delayMs: number) => () => void;

export interface VideoDecodabilityProbeOptions {
	readSample: () => VideoDecodabilitySample | null;
	schedule: DecodabilityScheduler;
	onVerdict: (verdict: Exclude<VideoDecodability, 'unknown'>) => void;
}

export interface VideoDecodabilityProbe {
	probe: () => void;
	cancel: () => void;
	getVerdict: () => VideoDecodability;
}

export function createVideoDecodabilityProbe(options: VideoDecodabilityProbeOptions): VideoDecodabilityProbe {
	const {readSample, schedule, onVerdict} = options;
	let verdict: VideoDecodability = 'unknown';
	let cancelRecheck: (() => void) | null = null;

	const settle = (next: Exclude<VideoDecodability, 'unknown'>) => {
		verdict = next;
		onVerdict(next);
	};

	const probe = () => {
		if (verdict !== 'unknown' || cancelRecheck !== null) return;
		const first = readSample();
		if (first === null) return;
		const firstLook = classifyDecodabilitySample(first);
		if (firstLook === 'decodable') {
			settle('decodable');
			return;
		}
		if (firstLook !== 'recheck') return;
		cancelRecheck = schedule(() => {
			cancelRecheck = null;
			const second = readSample();
			if (second === null) return;
			const rechecked = classifyDecodabilityRecheck(first, second);
			if (rechecked !== 'unknown') {
				settle(rechecked);
			}
		}, DECODABILITY_RECHECK_DELAY_MS);
	};

	const cancel = () => {
		if (cancelRecheck === null) return;
		const pending = cancelRecheck;
		cancelRecheck = null;
		pending();
	};

	return {probe, cancel, getVerdict: () => verdict};
}
