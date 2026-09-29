// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	detectWasmSimdSupport,
	getNoiseSuppressionBackendDescriptor,
	isVoiceNoiseSuppressionBackend,
	type NoiseSuppressionRuntimeCapabilities,
	selectUsableNoiseSuppressionBackend,
	type VoiceNoiseSuppressionBackend,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import type {ResolvedVoiceProcessing} from '@app/features/voice/utils/VoiceProcessingProfile';

const NOISE_SUPPRESSION_SAMPLE_RATE = 48000;
const DEFAULT_NOISE_SUPPRESSION_BACKEND: VoiceNoiseSuppressionBackend = 'deep_filter';

export function readNoiseSuppressionRuntimeCapabilities(): NoiseSuppressionRuntimeCapabilities {
	return {
		sampleRate: NOISE_SUPPRESSION_SAMPLE_RATE,
		wasmSimd: detectWasmSimdSupport(),
		audioWorklet: typeof AudioWorkletNode === 'function',
	};
}

export function resolveNoiseSuppressionBackend(preference: unknown): VoiceNoiseSuppressionBackend {
	return selectUsableNoiseSuppressionBackend(
		isVoiceNoiseSuppressionBackend(preference) ? preference : DEFAULT_NOISE_SUPPRESSION_BACKEND,
		readNoiseSuppressionRuntimeCapabilities(),
	);
}

export function supportsStereoCapture(profile: ResolvedVoiceProcessing): boolean {
	if (profile.echoCancellation || profile.autoGainControl) return false;
	if (profile.browserNoiseSuppression || profile.deepFilter) return false;
	return getNoiseSuppressionBackendDescriptor(profile.noiseSuppressionBackend).preservesInputChannels;
}

export function resolveStereoCapture(profile: ResolvedVoiceProcessing, stereoPreferred: boolean): boolean {
	if (!supportsStereoCapture(profile)) return false;
	if (profile.mode === 'voice') return false;
	return stereoPreferred;
}
