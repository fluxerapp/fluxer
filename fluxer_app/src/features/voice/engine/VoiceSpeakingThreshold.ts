// SPDX-License-Identifier: AGPL-3.0-or-later

const LOCAL_MIN_RMS = 0.0003;
export const LOCAL_AUTO_MIN_RMS = 0.000003;
const LOCAL_DEFAULT_RMS = 0.008;
export const LOCAL_MAX_RMS = 0.04;
const REMOTE_MIN_RMS = 0.002;
const REMOTE_DEFAULT_RMS = 0.006;
const REMOTE_MAX_RMS = 0.035;
export const SPEAKING_LOCAL_RELEASE_MS = 180;
const SPEAKING_REMOTE_RELEASE_MS = 220;
const SPEAKING_REMOTE_ATTACK_MS = 30;
const SLIDER_DEFAULT = 50;

export interface SpeakingHysteresis {
	speaking: boolean;
	aboveSinceMs: number | null;
	belowSinceMs: number | null;
}

export const SILENT_SPEAKING_HYSTERESIS: SpeakingHysteresis = {speaking: false, aboveSinceMs: null, belowSinceMs: null};

function clampSlider(slider: number): number {
	if (!Number.isFinite(slider)) return SLIDER_DEFAULT;
	if (slider < 0) return 0;
	if (slider > 100) return 100;
	return slider;
}

function interpolateThreshold(slider: number, min: number, def: number, max: number): number {
	const v = clampSlider(slider);
	if (v <= SLIDER_DEFAULT) {
		const t = v / SLIDER_DEFAULT;
		return min + (def - min) * t;
	}
	const t = (v - SLIDER_DEFAULT) / (100 - SLIDER_DEFAULT);
	return def + (max - def) * t;
}

export function getLocalSpeakingThresholdRms(slider: number): number {
	const value = clampSlider(slider);
	if (value <= SLIDER_DEFAULT) {
		return LOCAL_MIN_RMS * (LOCAL_DEFAULT_RMS / LOCAL_MIN_RMS) ** (value / SLIDER_DEFAULT);
	}
	return interpolateThreshold(value, LOCAL_MIN_RMS, LOCAL_DEFAULT_RMS, LOCAL_MAX_RMS);
}

export function getRemoteSpeakingThresholdRms(slider: number): number {
	return interpolateThreshold(slider, REMOTE_MIN_RMS, REMOTE_DEFAULT_RMS, REMOTE_MAX_RMS);
}

export function advanceSpeakingHysteresis(
	state: SpeakingHysteresis,
	rms: number,
	thresholdRms: number,
	nowMs: number,
): SpeakingHysteresis {
	if (rms >= thresholdRms) {
		const aboveSinceMs = state.aboveSinceMs ?? nowMs;
		return {
			speaking: state.speaking || nowMs - aboveSinceMs >= SPEAKING_REMOTE_ATTACK_MS,
			aboveSinceMs,
			belowSinceMs: null,
		};
	}
	const belowSinceMs = state.belowSinceMs ?? nowMs;
	return {
		speaking: state.speaking && nowMs - belowSinceMs < SPEAKING_REMOTE_RELEASE_MS,
		aboveSinceMs: null,
		belowSinceMs,
	};
}
