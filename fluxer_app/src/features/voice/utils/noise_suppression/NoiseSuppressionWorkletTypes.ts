// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceNoiseSuppressionBackend} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';

export type NoiseSuppressionWorkletBackend = Extract<
	VoiceNoiseSuppressionBackend,
	'gate' | 'speex' | 'rnnoise' | 'gtcrn'
>;

export const NOISE_SUPPRESSION_WORKLET_PROCESSOR_NAMES: Readonly<Record<NoiseSuppressionWorkletBackend, string>> = {
	gate: '@sapphi-red/web-noise-suppressor/noise-gate',
	speex: '@sapphi-red/web-noise-suppressor/speex',
	rnnoise: '@sapphi-red/web-noise-suppressor/rnnoise',
	gtcrn: '@sapphi-red/web-noise-suppressor/gtcrn',
};
