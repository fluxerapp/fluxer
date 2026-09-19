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
import type {
	VoiceNoiseSuppressionAssignmentResponse,
	VoiceNoiseSuppressionResolutionSource,
} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {resolveVoiceNoiseSuppressionForCall} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';

export interface EffectiveNoiseSuppression {
	rolloutApplied: boolean;
	backend: VoiceNoiseSuppressionBackend | null;
	requestedBackend: VoiceNoiseSuppressionBackend | null;
	source: VoiceNoiseSuppressionResolutionSource | null;
	suppressionStrength: number;
	stereoPreferred: boolean;
	configVersion: number;
}

export const INERT_EFFECTIVE_NOISE_SUPPRESSION: EffectiveNoiseSuppression = {
	rolloutApplied: false,
	backend: null,
	requestedBackend: null,
	source: null,
	suppressionStrength: 80,
	stereoPreferred: false,
	configVersion: 0,
};

export function resolveEffectiveNoiseSuppression(
	assignment: VoiceNoiseSuppressionAssignmentResponse,
	guildId: string | null,
	userPreference: unknown,
	capabilities: NoiseSuppressionRuntimeCapabilities,
	stereoPreference: boolean | null = null,
): EffectiveNoiseSuppression {
	const preference = isVoiceNoiseSuppressionBackend(userPreference) ? userPreference : null;
	const resolution = resolveVoiceNoiseSuppressionForCall(assignment, guildId, preference);
	const stereoAllowed = stereoPreference === true;
	if (resolution == null) return {...INERT_EFFECTIVE_NOISE_SUPPRESSION, stereoPreferred: stereoAllowed};
	const backend = selectUsableNoiseSuppressionBackend(resolution.backend, capabilities);
	return {
		rolloutApplied: true,
		backend,
		requestedBackend: resolution.backend,
		source: resolution.source,
		suppressionStrength: resolution.suppressionStrength,
		stereoPreferred: stereoAllowed,
		configVersion: resolution.configVersion,
	};
}

export function readNoiseSuppressionRuntimeCapabilities(sampleRate: number): NoiseSuppressionRuntimeCapabilities {
	return {
		sampleRate,
		wasmSimd: detectWasmSimdSupport(),
		audioWorklet: typeof AudioWorkletNode === 'function',
	};
}

let activeScopeGuildId: string | null = null;

export function setNoiseSuppressionScopeGuildId(guildId: string | null): void {
	activeScopeGuildId = guildId;
}

export function getNoiseSuppressionScopeGuildId(): string | null {
	return activeScopeGuildId;
}

export function supportsStereoCapture(profile: ResolvedVoiceProcessing): boolean {
	if (profile.echoCancellation || profile.autoGainControl) return false;
	if (profile.browserNoiseSuppression || profile.deepFilter) return false;
	return getNoiseSuppressionBackendDescriptor(profile.noiseSuppressionBackend).preservesInputChannels;
}

export function resolveStereoCapture(effective: EffectiveNoiseSuppression, profile: ResolvedVoiceProcessing): boolean {
	if (!supportsStereoCapture(profile)) return false;
	if (profile.mode === 'studio') return true;
	if (profile.mode === 'custom') return effective.stereoPreferred;
	return false;
}

export function applyNoiseSuppressionOverride(
	profile: ResolvedVoiceProcessing,
	effective: EffectiveNoiseSuppression,
): ResolvedVoiceProcessing {
	if (!effective.rolloutApplied || effective.backend == null || profile.mode === 'studio') {
		return {...profile, stereoCapture: resolveStereoCapture(effective, profile)};
	}
	const backend = effective.backend;
	const descriptor = getNoiseSuppressionBackendDescriptor(backend);
	const next: ResolvedVoiceProcessing = {
		...profile,
		browserNoiseSuppression: descriptor.browserNoiseSuppression,
		deepFilter: backend === 'deep_filter',
		deepFilterNoiseReductionLevel: backend === 'deep_filter' ? effective.suppressionStrength : 0,
		noiseSuppressionBackend: backend,
		stereoCapture: false,
	};
	return {...next, stereoCapture: resolveStereoCapture(effective, next)};
}
