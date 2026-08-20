// SPDX-License-Identifier: AGPL-3.0-or-later

import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import type VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {resolveEffectiveDeviceId} from '@app/features/voice/utils/VoiceDeviceManager';

export type VoiceProcessingMode = 'voice' | 'studio' | 'custom';

export interface VoiceProcessingSettingsLike {
	voiceProcessingMode: VoiceProcessingMode;
	echoCancellation: boolean;
	noiseSuppression: boolean;
	autoGainControl: boolean;
	deepFilterNoiseSuppression: boolean;
	deepFilterNoiseSuppressionLevel: number;
}

export interface ResolvedVoiceProcessing {
	mode: VoiceProcessingMode;
	echoCancellation: boolean;
	browserNoiseSuppression: boolean;
	autoGainControl: boolean;
	deepFilter: boolean;
	deepFilterNoiseReductionLevel: number;
	contentHint: '' | 'speech' | 'music';
}

export const DEFAULT_VOICE_PROCESSING_MODE: VoiceProcessingMode = 'voice';
export const DEEP_FILTER_NOISE_REDUCTION_LEVEL_MIN = 0;
export const DEEP_FILTER_NOISE_REDUCTION_LEVEL_MAX = 100;
export const FOCUSED_VOICE_DEEP_FILTER_NOISE_REDUCTION_LEVEL = 100;

export function clampDeepFilterNoiseReductionLevel(level: number): number {
	if (!Number.isFinite(level)) {
		return DEEP_FILTER_NOISE_REDUCTION_LEVEL_MAX;
	}
	return Math.min(DEEP_FILTER_NOISE_REDUCTION_LEVEL_MAX, Math.max(DEEP_FILTER_NOISE_REDUCTION_LEVEL_MIN, level));
}

export function resolveVoiceProcessing(settings: VoiceProcessingSettingsLike): ResolvedVoiceProcessing {
	switch (settings.voiceProcessingMode) {
		case 'studio':
			return {
				mode: 'studio',
				echoCancellation: false,
				browserNoiseSuppression: false,
				autoGainControl: false,
				deepFilter: false,
				deepFilterNoiseReductionLevel: DEEP_FILTER_NOISE_REDUCTION_LEVEL_MIN,
				contentHint: 'music',
			};
		case 'custom': {
			const browserNs = settings.noiseSuppression && !settings.deepFilterNoiseSuppression;
			return {
				mode: 'custom',
				echoCancellation: settings.echoCancellation,
				browserNoiseSuppression: browserNs,
				autoGainControl: settings.autoGainControl,
				deepFilter: settings.deepFilterNoiseSuppression,
				deepFilterNoiseReductionLevel: settings.deepFilterNoiseSuppression
					? clampDeepFilterNoiseReductionLevel(settings.deepFilterNoiseSuppressionLevel)
					: DEEP_FILTER_NOISE_REDUCTION_LEVEL_MIN,
				contentHint: '',
			};
		}
		default:
			return {
				mode: 'voice',
				echoCancellation: true,
				browserNoiseSuppression: false,
				autoGainControl: settings.autoGainControl,
				deepFilter: true,
				deepFilterNoiseReductionLevel: FOCUSED_VOICE_DEEP_FILTER_NOISE_REDUCTION_LEVEL,
				contentHint: 'speech',
			};
	}
}

export function resolveVoiceProcessingFromState(store: typeof VoiceSettings): ResolvedVoiceProcessing {
	return resolveVoiceProcessing({
		voiceProcessingMode: store.voiceProcessingMode,
		echoCancellation: store.echoCancellation,
		noiseSuppression: store.noiseSuppression,
		autoGainControl: store.autoGainControl,
		deepFilterNoiseSuppression: store.deepFilterNoiseSuppression,
		deepFilterNoiseSuppressionLevel: store.deepFilterNoiseSuppressionLevel,
	});
}

export function resolveVoiceProcessingFromStateForDeviceLabel(
	store: typeof VoiceSettings,
	label: string | null | undefined,
): ResolvedVoiceProcessing {
	return resolveVoiceProcessing({
		voiceProcessingMode: store.getVoiceProcessingModeForDeviceLabel(label),
		echoCancellation: store.echoCancellation,
		noiseSuppression: store.noiseSuppression,
		autoGainControl: store.autoGainControl,
		deepFilterNoiseSuppression: store.deepFilterNoiseSuppression,
		deepFilterNoiseSuppressionLevel: store.deepFilterNoiseSuppressionLevel,
	});
}

export function getActiveInputDeviceLabel(store: typeof VoiceSettings): string | null {
	const {inputDevices} = VoiceDevicePermissionState.getState();
	const effectiveId = resolveEffectiveDeviceId(store.inputDeviceId, inputDevices);
	if (!effectiveId) return null;
	const device = inputDevices.find((d) => d.deviceId === effectiveId);
	return device?.label || null;
}

export function getActiveVoiceProcessingMode(store: typeof VoiceSettings): VoiceProcessingMode {
	return store.getVoiceProcessingModeForDeviceLabel(getActiveInputDeviceLabel(store));
}

export function applyContentHintToTrack(track: MediaStreamTrack, hint: '' | 'speech' | 'music'): void {
	try {
		(
			track as MediaStreamTrack & {
				contentHint?: string;
			}
		).contentHint = hint;
	} catch {}
}
