// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	VOICE_NOISE_SUPPRESSION_BACKENDS,
	type VoiceNoiseSuppressionBackend,
} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';

export type {VoiceNoiseSuppressionBackend};
export {VOICE_NOISE_SUPPRESSION_BACKENDS};

export type NoiseSuppressionEngine = 'passthrough' | 'constraint' | 'worklet' | 'deep_filter';

export interface NoiseSuppressionBackendDescriptor {
	id: VoiceNoiseSuppressionBackend;
	engine: NoiseSuppressionEngine;
	browserNoiseSuppression: boolean;
	preservesInputChannels: boolean;
	supportedSampleRates: ReadonlyArray<number> | null;
	requiresWasmSimd: boolean;
	usesSuppressionStrength: boolean;
}

const DESCRIPTORS: Readonly<Record<VoiceNoiseSuppressionBackend, NoiseSuppressionBackendDescriptor>> = {
	none: {
		id: 'none',
		engine: 'passthrough',
		browserNoiseSuppression: false,
		preservesInputChannels: true,
		supportedSampleRates: null,
		requiresWasmSimd: false,
		usesSuppressionStrength: false,
	},
	standard: {
		id: 'standard',
		engine: 'constraint',
		browserNoiseSuppression: true,
		preservesInputChannels: true,
		supportedSampleRates: null,
		requiresWasmSimd: false,
		usesSuppressionStrength: false,
	},
	gate: {
		id: 'gate',
		engine: 'worklet',
		browserNoiseSuppression: false,
		preservesInputChannels: false,
		supportedSampleRates: null,
		requiresWasmSimd: false,
		usesSuppressionStrength: true,
	},
	speex: {
		id: 'speex',
		engine: 'worklet',
		browserNoiseSuppression: false,
		preservesInputChannels: false,
		supportedSampleRates: null,
		requiresWasmSimd: false,
		usesSuppressionStrength: false,
	},
	rnnoise: {
		id: 'rnnoise',
		engine: 'worklet',
		browserNoiseSuppression: false,
		preservesInputChannels: false,
		supportedSampleRates: [48000],
		requiresWasmSimd: false,
		usesSuppressionStrength: false,
	},
	gtcrn: {
		id: 'gtcrn',
		engine: 'worklet',
		browserNoiseSuppression: false,
		preservesInputChannels: false,
		supportedSampleRates: [16000, 48000],
		requiresWasmSimd: true,
		usesSuppressionStrength: false,
	},
	deep_filter: {
		id: 'deep_filter',
		engine: 'deep_filter',
		browserNoiseSuppression: false,
		preservesInputChannels: false,
		supportedSampleRates: [48000],
		requiresWasmSimd: false,
		usesSuppressionStrength: true,
	},
};

export function getNoiseSuppressionBackendDescriptor(
	backend: VoiceNoiseSuppressionBackend,
): NoiseSuppressionBackendDescriptor {
	return DESCRIPTORS[backend];
}

export function isVoiceNoiseSuppressionBackend(value: unknown): value is VoiceNoiseSuppressionBackend {
	return typeof value === 'string' && (VOICE_NOISE_SUPPRESSION_BACKENDS as ReadonlyArray<string>).includes(value);
}

export interface NoiseSuppressionRuntimeCapabilities {
	sampleRate: number;
	wasmSimd: boolean;
	audioWorklet: boolean;
}

export type NoiseSuppressionUnsupportedReason = 'sample_rate' | 'wasm_simd' | 'audio_worklet';

export function getNoiseSuppressionUnsupportedReason(
	backend: VoiceNoiseSuppressionBackend,
	capabilities: NoiseSuppressionRuntimeCapabilities,
): NoiseSuppressionUnsupportedReason | null {
	const descriptor = DESCRIPTORS[backend];
	if (descriptor.engine === 'passthrough' || descriptor.engine === 'constraint') return null;
	if (!capabilities.audioWorklet) return 'audio_worklet';
	if (descriptor.requiresWasmSimd && !capabilities.wasmSimd) return 'wasm_simd';
	if (descriptor.supportedSampleRates != null && !descriptor.supportedSampleRates.includes(capabilities.sampleRate)) {
		return 'sample_rate';
	}
	return null;
}

export function isNoiseSuppressionBackendSupported(
	backend: VoiceNoiseSuppressionBackend,
	capabilities: NoiseSuppressionRuntimeCapabilities,
): boolean {
	return getNoiseSuppressionUnsupportedReason(backend, capabilities) === null;
}

export function selectUsableNoiseSuppressionBackend(
	requested: VoiceNoiseSuppressionBackend,
	capabilities: NoiseSuppressionRuntimeCapabilities,
	fallbackOrder: ReadonlyArray<VoiceNoiseSuppressionBackend> = ['standard', 'none'],
): VoiceNoiseSuppressionBackend {
	if (isNoiseSuppressionBackendSupported(requested, capabilities)) return requested;
	for (const candidate of fallbackOrder) {
		if (candidate !== requested && isNoiseSuppressionBackendSupported(candidate, capabilities)) return candidate;
	}
	return 'none';
}

export function resolveNoiseSuppressionContextSampleRate(
	backend: VoiceNoiseSuppressionBackend,
	captureSampleRate: number,
): number {
	const descriptor = DESCRIPTORS[backend];
	const supported = descriptor.supportedSampleRates;
	if (supported == null || supported.includes(captureSampleRate)) return captureSampleRate;
	return supported.includes(48000) ? 48000 : (supported[0] ?? captureSampleRate);
}

const WASM_SIMD_PROBE = new Uint8Array([
	0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x60, 0x00, 0x00, 0x03, 0x02, 0x01, 0x00, 0x0a,
	0x09, 0x01, 0x07, 0x00, 0x41, 0x00, 0xfd, 0x0f, 0x1a, 0x0b,
]);

let cachedWasmSimd: boolean | null = null;

export function detectWasmSimdSupport(): boolean {
	if (cachedWasmSimd != null) return cachedWasmSimd;
	try {
		cachedWasmSimd = WebAssembly.validate(WASM_SIMD_PROBE);
	} catch {
		cachedWasmSimd = false;
	}
	return cachedWasmSimd;
}

export function resetWasmSimdSupportCacheForTests(): void {
	cachedWasmSimd = null;
}
