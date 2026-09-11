// SPDX-License-Identifier: AGPL-3.0-or-later

export const NOISE_GATE_OPEN_THRESHOLD_DB_AT_MIN_STRENGTH = -80;
export const NOISE_GATE_OPEN_THRESHOLD_DB_AT_MAX_STRENGTH = -32;
export const NOISE_GATE_HYSTERESIS_DB = 6;
export const NOISE_GATE_HOLD_MS = 180;

export interface NoiseGateTuning {
	openThreshold: number;
	closeThreshold: number;
	holdMs: number;
}

export function resolveNoiseGateTuning(suppressionStrength: number): NoiseGateTuning {
	const clamped = Number.isFinite(suppressionStrength) ? Math.min(100, Math.max(0, suppressionStrength)) : 80;
	const span = NOISE_GATE_OPEN_THRESHOLD_DB_AT_MAX_STRENGTH - NOISE_GATE_OPEN_THRESHOLD_DB_AT_MIN_STRENGTH;
	const openThreshold = NOISE_GATE_OPEN_THRESHOLD_DB_AT_MIN_STRENGTH + (span * clamped) / 100;
	return {
		openThreshold,
		closeThreshold: openThreshold - NOISE_GATE_HYSTERESIS_DB,
		holdMs: NOISE_GATE_HOLD_MS,
	};
}
