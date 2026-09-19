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
	stereoEnabled: boolean;
	configVersion: number;
}

export const INERT_EFFECTIVE_NOISE_SUPPRESSION: EffectiveNoiseSuppression = {
	rolloutApplied: false,
	backend: null,
	requestedBackend: null,
	source: null,
	suppressionStrength: 80,
	stereoEnabled: false,
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
	const stereoAllowed = assignment.enabled && assignment.stereo_enabled && stereoPreference !== false;
	if (resolution == null) return {...INERT_EFFECTIVE_NOISE_SUPPRESSION, stereoEnabled: stereoAllowed};
	const backend = selectUsableNoiseSuppressionBackend(resolution.backend, capabilities);
	return {
		rolloutApplied: true,
		backend,
		requestedBackend: resolution.backend,
		source: resolution.source,
		suppressionStrength: resolution.suppressionStrength,
		stereoEnabled:
			stereoAllowed && resolution.stereoEnabled && getNoiseSuppressionBackendDescriptor(backend).preservesInputChannels,
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

export function resolveStereoCapture(
	effective: EffectiveNoiseSuppression,
	backend: VoiceNoiseSuppressionBackend,
): boolean {
	return effective.stereoEnabled && getNoiseSuppressionBackendDescriptor(backend).preservesInputChannels;
}

export function applyNoiseSuppressionOverride(
	profile: ResolvedVoiceProcessing,
	effective: EffectiveNoiseSuppression,
): ResolvedVoiceProcessing {
	if (!effective.rolloutApplied || effective.backend == null || profile.mode === 'studio') {
		return {...profile, stereoCapture: resolveStereoCapture(effective, profile.noiseSuppressionBackend)};
	}
	const backend = effective.backend;
	const descriptor = getNoiseSuppressionBackendDescriptor(backend);
	return {
		...profile,
		browserNoiseSuppression: descriptor.browserNoiseSuppression,
		deepFilter: backend === 'deep_filter',
		deepFilterNoiseReductionLevel: backend === 'deep_filter' ? effective.suppressionStrength : 0,
		noiseSuppressionBackend: backend,
		stereoCapture: resolveStereoCapture(effective, backend),
	};
}
