// SPDX-License-Identifier: AGPL-3.0-or-later

const LOCAL_MIN_RMS = 0.0003;
export const LOCAL_AUTO_MIN_RMS = 0.000003;
const LOCAL_DEFAULT_RMS = 0.008;
export const LOCAL_MAX_RMS = 0.04;
const SPEAKING_AUDIBLE_RMS = LOCAL_MIN_RMS;
export const SPEAKING_LOCAL_RELEASE_MS = 180;
const SPEAKING_REMOTE_RELEASE_MS = 220;
const SLIDER_DEFAULT = 50;

export interface SpeakingHysteresis {
	speaking: boolean;
	belowSinceMs: number | null;
}

export const SILENT_SPEAKING_HYSTERESIS: SpeakingHysteresis = {speaking: false, belowSinceMs: null};

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

export function advanceSpeakingHysteresis(state: SpeakingHysteresis, rms: number, nowMs: number): SpeakingHysteresis {
	if (rms >= SPEAKING_AUDIBLE_RMS) {
		return {speaking: true, belowSinceMs: null};
	}
	const belowSinceMs = state.belowSinceMs ?? nowMs;
	return {
		speaking: state.speaking && nowMs - belowSinceMs < SPEAKING_REMOTE_RELEASE_MS,
		belowSinceMs,
	};
}
