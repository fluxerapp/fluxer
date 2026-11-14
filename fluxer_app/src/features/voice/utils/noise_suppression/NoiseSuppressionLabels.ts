// SPDX-License-Identifier: AGPL-3.0-or-later

import NoiseSuppressionAvailability from '@app/features/voice/utils/noise_suppression/NoiseSuppressionAvailability';
import type {VoiceNoiseSuppressionBackend} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {
	readEffectiveNoiseSuppressionBackend,
	readRequestedNoiseSuppressionBackend,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionRuntime';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const NOISE_SUPPRESSION_NONE_DESCRIPTOR = msg({
	message: 'None',
	comment: 'Noise suppression option that disables suppression.',
	context: 'noise-suppression-option',
});
const NOISE_SUPPRESSION_GATE_DESCRIPTOR = msg({
	message: 'Noise gate',
	comment: 'Noise suppression option label for the noise gate backend. Keep it concise.',
	context: 'noise-suppression-option',
});
const STEREO_MICROPHONE_DESCRIPTOR = msg({
	message: 'Stereo microphone',
	comment: 'Switch label in the voice settings that sends both channels of a stereo microphone. Keep it concise.',
});
const STEREO_MICROPHONE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Sends both channels of a stereo microphone. Needs all processing off and a channel at 64 kbps or higher.',
	comment: 'Description for the stereo microphone switch in the voice settings.',
});

const BACKEND_MODEL_NAMES: Readonly<Partial<Record<VoiceNoiseSuppressionBackend, string>>> = {
	standard: 'WebRTC',
	speex: 'Speex',
	rnnoise: 'RNNoise',
	gtcrn: 'GTCRN',
	deep_filter: 'DeepFilterNet',
};

const TRANSLATED_LABELS: Readonly<Partial<Record<VoiceNoiseSuppressionBackend, MessageDescriptor>>> = {
	none: NOISE_SUPPRESSION_NONE_DESCRIPTOR,
	gate: NOISE_SUPPRESSION_GATE_DESCRIPTOR,
};

export function getNoiseSuppressionChoiceLabel(i18n: I18n, backend: VoiceNoiseSuppressionBackend): string {
	const modelName = BACKEND_MODEL_NAMES[backend];
	if (modelName) return modelName;
	const descriptor = TRANSLATED_LABELS[backend];
	return descriptor ? i18n._(descriptor) : backend;
}

export function getNoiseSuppressionFallbackMessage(i18n: I18n): string | null {
	const requested = readRequestedNoiseSuppressionBackend();
	const fallback = NoiseSuppressionAvailability.getFallback(requested);
	if (!fallback) return null;
	const requestedLabel = getNoiseSuppressionChoiceLabel(i18n, requested);
	const effectiveLabel = getNoiseSuppressionChoiceLabel(i18n, readEffectiveNoiseSuppressionBackend());
	return fallback === 'loading'
		? i18n._(msg`Using ${effectiveLabel} until ${requestedLabel} is ready.`)
		: i18n._(msg`${requestedLabel} could not start. Using ${effectiveLabel} instead.`);
}

export {STEREO_MICROPHONE_DESCRIPTION_DESCRIPTOR, STEREO_MICROPHONE_DESCRIPTOR};
