// SPDX-License-Identifier: AGPL-3.0-or-later

export const VOICE_VOLUME_MAX_PERCENT = 200;
export const VOICE_VOLUME_MAX_GAIN = 4;
export const VOICE_TRACK_MAX_TOTAL_GAIN = 12;
export const VOICE_VOLUME_MAX_SLIDER_VOLUME = VOICE_VOLUME_MAX_PERCENT / 100;
const UNITY_GAIN_PERCENT = 100;
const QUIET_RANGE_EXPONENT = 1.2;

export function clampVoiceVolumePercent(value: number): number {
	if (!Number.isFinite(value)) {
		return 100;
	}
	return Math.max(0, Math.min(VOICE_VOLUME_MAX_PERCENT, value));
}

export function voiceVolumePercentToTrackVolume(value: number): number {
	return Math.max(0, Math.min(1, clampVoiceVolumePercent(value) / 100));
}

export function inputVoiceVolumePercentToGain(value: number): number {
	return clampVoiceVolumePercent(value) / UNITY_GAIN_PERCENT;
}

export function boostedVoiceVolumePercentToTrackVolume(value: number): number {
	const clamped = clampVoiceVolumePercent(value);
	if (clamped === 0) {
		return 0;
	}
	if (clamped <= UNITY_GAIN_PERCENT) {
		return 10 ** (((clamped - UNITY_GAIN_PERCENT) / UNITY_GAIN_PERCENT) * QUIET_RANGE_EXPONENT);
	}
	return VOICE_VOLUME_MAX_GAIN ** ((clamped - UNITY_GAIN_PERCENT) / (VOICE_VOLUME_MAX_PERCENT - UNITY_GAIN_PERCENT));
}

export function composeVoiceVolumeGain(...volumePercents: Array<number>): number {
	return volumePercents.reduce((gain, percent) => gain * boostedVoiceVolumePercentToTrackVolume(percent), 1);
}

export function recalibrateStoredVoiceVolumePercent(value: number): number {
	if (!Number.isFinite(value) || value <= UNITY_GAIN_PERCENT) {
		return value;
	}
	return UNITY_GAIN_PERCENT;
}

export function recalibrateStoredVoiceVolumes<T extends Record<string, number>>(volumes: T): T {
	const entries = Object.entries(volumes);
	if (!entries.some(([, value]) => recalibrateStoredVoiceVolumePercent(value) !== value)) {
		return volumes;
	}
	return Object.fromEntries(entries.map(([key, value]) => [key, recalibrateStoredVoiceVolumePercent(value)])) as T;
}

export function clampVoiceTrackTotalGain(gain: number): number {
	if (!Number.isFinite(gain)) {
		return 1;
	}
	return Math.max(0, Math.min(VOICE_TRACK_MAX_TOTAL_GAIN, gain));
}
