// SPDX-License-Identifier: AGPL-3.0-or-later

import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import type VoiceSettings from '@app/features/voice/state/VoiceSettings';
import NoiseSuppressionAvailability from '@app/features/voice/utils/noise_suppression/NoiseSuppressionAvailability';
import {
	getNoiseSuppressionBackendDescriptor,
	type VoiceNoiseSuppressionBackend,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {
	resolveNoiseSuppressionBackend,
	resolveStereoCapture,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionSelection';
import {resolveEffectiveDeviceId} from '@app/features/voice/utils/VoiceDeviceManager';

export type VoiceProcessingMode = 'voice' | 'studio' | 'custom';

export interface VoiceProcessingSettingsLike {
	voiceProcessingMode: VoiceProcessingMode;
	echoCancellation: boolean;
	autoGainControl: boolean;
}

export interface ResolvedVoiceProcessing {
	mode: VoiceProcessingMode;
	echoCancellation: boolean;
	browserNoiseSuppression: boolean;
	autoGainControl: boolean;
	deepFilter: boolean;
	contentHint: '' | 'speech' | 'music';
	noiseSuppressionBackend: VoiceNoiseSuppressionBackend;
	stereoCapture: boolean;
}

export const DEFAULT_VOICE_PROCESSING_MODE: VoiceProcessingMode = 'voice';

function resolveBaseVoiceProcessing(
	settings: VoiceProcessingSettingsLike,
	backend: VoiceNoiseSuppressionBackend,
): ResolvedVoiceProcessing {
	const effectiveBackend =
		settings.voiceProcessingMode === 'voice'
			? NoiseSuppressionAvailability.resolveEffectiveBackend(resolveNoiseSuppressionBackend(null))
			: backend;
	const noiseSuppression = {
		browserNoiseSuppression: getNoiseSuppressionBackendDescriptor(effectiveBackend).browserNoiseSuppression,
		deepFilter: effectiveBackend === 'deep_filter',
		noiseSuppressionBackend: effectiveBackend,
		stereoCapture: false,
	};
	switch (settings.voiceProcessingMode) {
		case 'studio':
			return {
				mode: 'studio',
				echoCancellation: false,
				browserNoiseSuppression: false,
				autoGainControl: false,
				deepFilter: false,
				contentHint: 'music',
				noiseSuppressionBackend: 'none',
				stereoCapture: false,
			};
		case 'custom':
			return {
				...noiseSuppression,
				mode: 'custom',
				echoCancellation: settings.echoCancellation,
				autoGainControl: settings.autoGainControl,
				contentHint: '',
			};
		default:
			return {
				...noiseSuppression,
				mode: 'voice',
				echoCancellation: true,
				autoGainControl: settings.autoGainControl,
				contentHint: 'speech',
			};
	}
}

export function resolveVoiceProcessing(
	settings: VoiceProcessingSettingsLike,
	backend: VoiceNoiseSuppressionBackend,
	stereoPreferred: boolean,
): ResolvedVoiceProcessing {
	const profile = resolveBaseVoiceProcessing(settings, backend);
	return {...profile, stereoCapture: resolveStereoCapture(profile, stereoPreferred)};
}

export function resolveVoiceProcessingFromState(store: typeof VoiceSettings): ResolvedVoiceProcessing {
	return resolveVoiceProcessingFromStateForMode(store, store.voiceProcessingMode);
}

export function resolveVoiceProcessingFromStateForDeviceLabel(
	store: typeof VoiceSettings,
	label: string | null | undefined,
): ResolvedVoiceProcessing {
	return resolveVoiceProcessingFromStateForMode(store, store.getVoiceProcessingModeForDeviceLabel(label));
}

function resolveVoiceProcessingFromStateForMode(
	store: typeof VoiceSettings,
	voiceProcessingMode: VoiceProcessingMode,
): ResolvedVoiceProcessing {
	return resolveVoiceProcessing(
		{
			voiceProcessingMode,
			echoCancellation: store.echoCancellation,
			autoGainControl: store.autoGainControl,
		},
		NoiseSuppressionAvailability.resolveEffectiveBackend(
			resolveNoiseSuppressionBackend(store.getNoiseSuppressionBackend()),
		),
		store.getStereoMicrophone() === true,
	);
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
