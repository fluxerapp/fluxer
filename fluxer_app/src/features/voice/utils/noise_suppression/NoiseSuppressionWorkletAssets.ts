// SPDX-License-Identifier: AGPL-3.0-or-later

import type {NoiseSuppressionWorkletBackend} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletTypes';
import gtcrnWasmUrl from '@sapphi-red/web-noise-suppressor/gtcrn.wasm';
import gtcrnWorkletUrl from '@sapphi-red/web-noise-suppressor/gtcrnWorklet.js';
import noiseGateWorkletUrl from '@sapphi-red/web-noise-suppressor/noiseGateWorklet.js';
import rnnoiseWasmUrl from '@sapphi-red/web-noise-suppressor/rnnoise.wasm';
import rnnoiseSimdWasmUrl from '@sapphi-red/web-noise-suppressor/rnnoise_simd.wasm';
import rnnoiseWorkletUrl from '@sapphi-red/web-noise-suppressor/rnnoiseWorklet.js';
import speexWasmUrl from '@sapphi-red/web-noise-suppressor/speex.wasm';
import speexWorkletUrl from '@sapphi-red/web-noise-suppressor/speexWorklet.js';

export const NOISE_SUPPRESSION_WORKLET_MODULE_URLS: Readonly<Record<NoiseSuppressionWorkletBackend, string>> = {
	gate: noiseGateWorkletUrl,
	speex: speexWorkletUrl,
	rnnoise: rnnoiseWorkletUrl,
	gtcrn: gtcrnWorkletUrl,
};

export const NOISE_SUPPRESSION_WASM_URLS = {
	gtcrn: gtcrnWasmUrl,
	rnnoise: rnnoiseWasmUrl,
	rnnoiseSimd: rnnoiseSimdWasmUrl,
	speex: speexWasmUrl,
} as const;
