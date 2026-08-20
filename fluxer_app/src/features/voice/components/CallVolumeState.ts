// SPDX-License-Identifier: AGPL-3.0-or-later

import {VOICE_VOLUME_MAX_PERCENT} from '@app/features/voice/utils/VoiceVolumeUtils';

export const CALL_VOLUME_DEFAULT_PERCENT = 100;
export const CALL_VOLUME_UNITY_PERCENT = 100;
export const CALL_VOLUME_SLIDER_MAX_PERCENT = VOICE_VOLUME_MAX_PERCENT;

export function callVolumePercentToSliderVolume(percent: number): number {
	if (!Number.isFinite(percent)) {
		return CALL_VOLUME_DEFAULT_PERCENT / CALL_VOLUME_UNITY_PERCENT;
	}
	const clamped = Math.max(0, Math.min(CALL_VOLUME_SLIDER_MAX_PERCENT, percent));
	return clamped / CALL_VOLUME_UNITY_PERCENT;
}

export function sliderVolumeToCallVolumePercent(volume: number): number {
	if (!Number.isFinite(volume)) {
		return CALL_VOLUME_DEFAULT_PERCENT;
	}
	const percent = Math.round(volume * CALL_VOLUME_UNITY_PERCENT);
	return Math.max(0, Math.min(CALL_VOLUME_SLIDER_MAX_PERCENT, percent));
}

export function resolveLastNonZeroCallVolume(currentPercent: number, previousLastNonZero: number): number {
	if (currentPercent > 0) {
		return currentPercent;
	}
	if (previousLastNonZero > 0) {
		return previousLastNonZero;
	}
	return CALL_VOLUME_DEFAULT_PERCENT;
}

export function resolveCallVolumeMuteToggle(currentPercent: number, lastNonZeroPercent: number): number {
	if (currentPercent > 0) {
		return 0;
	}
	if (lastNonZeroPercent > 0) {
		return lastNonZeroPercent;
	}
	return CALL_VOLUME_DEFAULT_PERCENT;
}
