// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {
	detectWasmSimdSupport,
	getNoiseSuppressionBackendDescriptor,
	getNoiseSuppressionUnsupportedReason,
	isNoiseSuppressionBackendSupported,
	isVoiceNoiseSuppressionBackend,
	type NoiseSuppressionRuntimeCapabilities,
	resetWasmSimdSupportCacheForTests,
	resolveNoiseSuppressionContextSampleRate,
	selectUsableNoiseSuppressionBackend,
	VOICE_NOISE_SUPPRESSION_BACKENDS,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {
	NOISE_GATE_OPEN_THRESHOLD_DB_AT_MAX_STRENGTH,
	NOISE_GATE_OPEN_THRESHOLD_DB_AT_MIN_STRENGTH,
	resolveNoiseGateTuning,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionGateTuning';
import {beforeEach, describe, expect, it} from 'vitest';

const CAPABLE: NoiseSuppressionRuntimeCapabilities = {sampleRate: 48000, wasmSimd: true, audioWorklet: true};

describe('noise suppression backend descriptors', () => {
	it('describes every backend in the shared contract', () => {
		for (const backend of VOICE_NOISE_SUPPRESSION_BACKENDS) {
			expect(getNoiseSuppressionBackendDescriptor(backend).id).toBe(backend);
		}
	});

	it('only lets none and standard carry the input channel layout through', () => {
		const preserving = VOICE_NOISE_SUPPRESSION_BACKENDS.filter(
			(backend) => getNoiseSuppressionBackendDescriptor(backend).preservesInputChannels,
		);
		expect(preserving).toEqual(['none', 'standard']);
	});

	it('only asks the browser for its own suppression on standard', () => {
		const browserBacked = VOICE_NOISE_SUPPRESSION_BACKENDS.filter(
			(backend) => getNoiseSuppressionBackendDescriptor(backend).browserNoiseSuppression,
		);
		expect(browserBacked).toEqual(['standard']);
	});

	it('recognises only contract backends', () => {
		expect(isVoiceNoiseSuppressionBackend('rnnoise')).toBe(true);
		expect(isVoiceNoiseSuppressionBackend('krisp')).toBe(false);
		expect(isVoiceNoiseSuppressionBackend(null)).toBe(false);
		expect(isVoiceNoiseSuppressionBackend(undefined)).toBe(false);
		expect(isVoiceNoiseSuppressionBackend(3)).toBe(false);
	});
});

describe('runtime support', () => {
	it('never blocks the two zero-dependency backends', () => {
		const hostile: NoiseSuppressionRuntimeCapabilities = {sampleRate: 44100, wasmSimd: false, audioWorklet: false};
		expect(isNoiseSuppressionBackendSupported('none', hostile)).toBe(true);
		expect(isNoiseSuppressionBackendSupported('standard', hostile)).toBe(true);
	});

	it('blocks every worklet backend without AudioWorklet', () => {
		const noWorklet: NoiseSuppressionRuntimeCapabilities = {...CAPABLE, audioWorklet: false};
		for (const backend of ['gate', 'speex', 'rnnoise', 'gtcrn', 'deep_filter'] as const) {
			expect(getNoiseSuppressionUnsupportedReason(backend, noWorklet)).toBe('audio_worklet');
		}
	});

	it('blocks gtcrn without wasm simd and nothing else', () => {
		const noSimd: NoiseSuppressionRuntimeCapabilities = {...CAPABLE, wasmSimd: false};
		expect(getNoiseSuppressionUnsupportedReason('gtcrn', noSimd)).toBe('wasm_simd');
		expect(getNoiseSuppressionUnsupportedReason('rnnoise', noSimd)).toBeNull();
		expect(getNoiseSuppressionUnsupportedReason('speex', noSimd)).toBeNull();
		expect(getNoiseSuppressionUnsupportedReason('gate', noSimd)).toBeNull();
		expect(getNoiseSuppressionUnsupportedReason('deep_filter', noSimd)).toBeNull();
	});

	it('blocks fixed-rate backends at an unsupported context rate', () => {
		const at44k: NoiseSuppressionRuntimeCapabilities = {...CAPABLE, sampleRate: 44100};
		expect(getNoiseSuppressionUnsupportedReason('rnnoise', at44k)).toBe('sample_rate');
		expect(getNoiseSuppressionUnsupportedReason('gtcrn', at44k)).toBe('sample_rate');
		expect(getNoiseSuppressionUnsupportedReason('deep_filter', at44k)).toBe('sample_rate');
		expect(getNoiseSuppressionUnsupportedReason('speex', at44k)).toBeNull();
		expect(getNoiseSuppressionUnsupportedReason('gate', at44k)).toBeNull();
	});

	it('accepts gtcrn at both of its native rates', () => {
		expect(isNoiseSuppressionBackendSupported('gtcrn', {...CAPABLE, sampleRate: 16000})).toBe(true);
		expect(isNoiseSuppressionBackendSupported('gtcrn', {...CAPABLE, sampleRate: 48000})).toBe(true);
		expect(isNoiseSuppressionBackendSupported('gtcrn', {...CAPABLE, sampleRate: 32000})).toBe(false);
	});
});

describe('fallback selection', () => {
	it('keeps the requested backend when it is usable', () => {
		expect(selectUsableNoiseSuppressionBackend('gtcrn', CAPABLE)).toBe('gtcrn');
	});

	it('falls back to standard when the request cannot run', () => {
		expect(selectUsableNoiseSuppressionBackend('gtcrn', {...CAPABLE, wasmSimd: false})).toBe('standard');
	});

	it('falls all the way to none when nothing in the fallback order fits', () => {
		const at44k: NoiseSuppressionRuntimeCapabilities = {...CAPABLE, sampleRate: 44100};
		expect(selectUsableNoiseSuppressionBackend('rnnoise', at44k, ['gtcrn', 'speex'])).toBe('speex');
		expect(selectUsableNoiseSuppressionBackend('rnnoise', at44k, ['gtcrn'])).toBe('none');
	});

	it('never returns the failing request itself', () => {
		expect(selectUsableNoiseSuppressionBackend('gtcrn', {...CAPABLE, wasmSimd: false}, ['gtcrn', 'none'])).toBe('none');
	});
});

describe('context sample rate resolution', () => {
	it('leaves rate-agnostic backends on the capture rate', () => {
		expect(resolveNoiseSuppressionContextSampleRate('speex', 44100)).toBe(44100);
		expect(resolveNoiseSuppressionContextSampleRate('gate', 44100)).toBe(44100);
		expect(resolveNoiseSuppressionContextSampleRate('none', 44100)).toBe(44100);
	});

	it('pulls fixed-rate backends onto 48k when the capture rate does not fit', () => {
		expect(resolveNoiseSuppressionContextSampleRate('rnnoise', 44100)).toBe(48000);
		expect(resolveNoiseSuppressionContextSampleRate('gtcrn', 44100)).toBe(48000);
		expect(resolveNoiseSuppressionContextSampleRate('deep_filter', 44100)).toBe(48000);
	});

	it('leaves a fixed-rate backend alone when the capture rate already fits', () => {
		expect(resolveNoiseSuppressionContextSampleRate('gtcrn', 16000)).toBe(16000);
		expect(resolveNoiseSuppressionContextSampleRate('rnnoise', 48000)).toBe(48000);
	});
});

describe('wasm simd probe', () => {
	beforeEach(() => {
		resetWasmSimdSupportCacheForTests();
	});

	it('detects simd support on a runtime that has it', () => {
		expect(detectWasmSimdSupport()).toBe(true);
	});

	it('caches the probe result', () => {
		const first = detectWasmSimdSupport();
		expect(detectWasmSimdSupport()).toBe(first);
	});
});

describe('noise gate tuning', () => {
	it('maps strength onto the documented threshold range', () => {
		expect(resolveNoiseGateTuning(0).openThreshold).toBeCloseTo(NOISE_GATE_OPEN_THRESHOLD_DB_AT_MIN_STRENGTH, 6);
		expect(resolveNoiseGateTuning(100).openThreshold).toBeCloseTo(NOISE_GATE_OPEN_THRESHOLD_DB_AT_MAX_STRENGTH, 6);
	});

	it('is monotonically more aggressive as strength rises', () => {
		let previous = Number.NEGATIVE_INFINITY;
		for (let strength = 0; strength <= 100; strength += 5) {
			const {openThreshold} = resolveNoiseGateTuning(strength);
			expect(openThreshold).toBeGreaterThanOrEqual(previous);
			previous = openThreshold;
		}
	});

	it('always keeps the close threshold below the open threshold', () => {
		for (const strength of [0, 25, 50, 80, 100]) {
			const tuning = resolveNoiseGateTuning(strength);
			expect(tuning.closeThreshold).toBeLessThan(tuning.openThreshold);
		}
	});

	it('clamps out-of-range and non-finite strengths', () => {
		expect(resolveNoiseGateTuning(-40).openThreshold).toBe(resolveNoiseGateTuning(0).openThreshold);
		expect(resolveNoiseGateTuning(4000).openThreshold).toBe(resolveNoiseGateTuning(100).openThreshold);
		expect(resolveNoiseGateTuning(Number.NaN).openThreshold).toBe(resolveNoiseGateTuning(80).openThreshold);
	});
});

describe('worklet build-time codemod', () => {
	const require = createRequire(import.meta.url);
	const loader = require('../../../../../scripts/build/rspack/noise-suppressor-worklet-loader.cjs') as (
		this: {resourcePath: string},
		source: string,
	) => string;
	const readWorklet = (specifier: string) => {
		const resourcePath = require.resolve(specifier);
		const source = require('node:fs').readFileSync(resourcePath, 'utf8') as string;
		return {resourcePath, source};
	};

	it('teaches every wasm worklet to pass audio through, signal ready, and start its port', () => {
		for (const specifier of [
			'@sapphi-red/web-noise-suppressor/rnnoiseWorklet.js',
			'@sapphi-red/web-noise-suppressor/gtcrnWorklet.js',
			'@sapphi-red/web-noise-suppressor/speexWorklet.js',
		]) {
			const {resourcePath, source} = readWorklet(specifier);
			const output = loader.call({resourcePath}, source);
			expect(output).toContain('type:"ready"');
			expect(output).toContain('type:"error"');
			expect(output).toContain('this.port.start()');
			expect(output).toContain('__out[__ch].set(__src)');
			expect(output).not.toContain('!this.processor||this.processor.process(e[0],t[0])');
		}
	});

	it('signals ready from the synchronous gate worklet', () => {
		const {resourcePath, source} = readWorklet('@sapphi-red/web-noise-suppressor/noiseGateWorklet.js');
		const output = loader.call({resourcePath}, source);
		expect(output).toContain('type:"ready"');
	});

	it('fails loudly when the upstream bundle no longer matches', () => {
		expect(() => loader.call({resourcePath: '/tmp/rnnoise/workletProcessor.js'}, 'not the upstream bundle')).toThrow(
			/update this loader/u,
		);
	});
});
