// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceNoiseSuppressionBackend} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const NOISE_SUPPRESSION_ENHANCED_DESCRIPTOR = msg({
	message: 'Enhanced',
	comment: 'Noise suppression option label in the voice tab (neural filter). Keep it concise.',
});
const NOISE_SUPPRESSION_ENHANCED_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Neural filter that removes most background noise.',
	comment: 'Description for the enhanced (DeepFilterNet3) noise suppression option.',
});
const NOISE_SUPPRESSION_STANDARD_DESCRIPTOR = msg({
	message: 'Standard',
	comment: 'Noise suppression option label in the voice tab (browser default). Keep it concise.',
});
const NOISE_SUPPRESSION_STANDARD_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Built-in suppression from your browser or system.',
	comment: 'Description for the standard noise suppression option, which uses the platform suppression.',
});
const NOISE_SUPPRESSION_NONE_DESCRIPTOR = msg({
	message: 'None',
	comment: 'Noise suppression option that disables suppression.',
	context: 'noise-suppression-option',
});
const NOISE_SUPPRESSION_NONE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Sends your microphone audio with no suppression.',
	comment: 'Description for the noise suppression option that disables suppression.',
});
const NOISE_SUPPRESSION_GATE_DESCRIPTOR = msg({
	message: 'Noise gate',
	comment: 'Noise suppression option label for the noise gate backend. Keep it concise.',
	context: 'noise-suppression-option',
});
const NOISE_SUPPRESSION_GATE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Cuts audio below a loudness threshold, lowest CPU cost.',
	comment: 'Description for the noise gate suppression option.',
});
const NOISE_SUPPRESSION_SPEEX_DESCRIPTOR = msg({
	message: 'Classic',
	comment: 'Noise suppression option label for the Speex backend. Keep it concise.',
	context: 'noise-suppression-option',
});
const NOISE_SUPPRESSION_SPEEX_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Lightweight classic filter, works on any device.',
	comment: 'Description for the Speex noise suppression option.',
});
const NOISE_SUPPRESSION_RNNOISE_DESCRIPTOR = msg({
	message: 'Balanced',
	comment: 'Noise suppression option label for the RNNoise backend. Keep it concise.',
	context: 'noise-suppression-option',
});
const NOISE_SUPPRESSION_RNNOISE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Neural filter, full audio bandwidth.',
	comment: 'Description for the RNNoise noise suppression option.',
});
const NOISE_SUPPRESSION_GTCRN_DESCRIPTOR = msg({
	message: 'Focused',
	comment: 'Noise suppression option label for the GTCRN speech backend. Keep it concise.',
	context: 'noise-suppression-option',
});
const NOISE_SUPPRESSION_GTCRN_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Strongest neural filter for speech, limits audio to the speech range.',
	comment: 'Description for the GTCRN noise suppression option.',
});
const STEREO_MICROPHONE_DESCRIPTOR = msg({
	message: 'Stereo microphone',
	comment: 'Switch label in the voice settings that sends both channels of a stereo microphone. Keep it concise.',
});
const STEREO_MICROPHONE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Sends both channels of a stereo microphone. Works only with no suppression or standard suppression.',
	comment: 'Description for the stereo microphone switch in the voice settings.',
});

const LABELS: Readonly<Record<VoiceNoiseSuppressionBackend, MessageDescriptor>> = {
	none: NOISE_SUPPRESSION_NONE_DESCRIPTOR,
	standard: NOISE_SUPPRESSION_STANDARD_DESCRIPTOR,
	gate: NOISE_SUPPRESSION_GATE_DESCRIPTOR,
	speex: NOISE_SUPPRESSION_SPEEX_DESCRIPTOR,
	rnnoise: NOISE_SUPPRESSION_RNNOISE_DESCRIPTOR,
	gtcrn: NOISE_SUPPRESSION_GTCRN_DESCRIPTOR,
	deep_filter: NOISE_SUPPRESSION_ENHANCED_DESCRIPTOR,
};

const DESCRIPTIONS: Readonly<Record<VoiceNoiseSuppressionBackend, MessageDescriptor>> = {
	none: NOISE_SUPPRESSION_NONE_DESCRIPTION_DESCRIPTOR,
	standard: NOISE_SUPPRESSION_STANDARD_DESCRIPTION_DESCRIPTOR,
	gate: NOISE_SUPPRESSION_GATE_DESCRIPTION_DESCRIPTOR,
	speex: NOISE_SUPPRESSION_SPEEX_DESCRIPTION_DESCRIPTOR,
	rnnoise: NOISE_SUPPRESSION_RNNOISE_DESCRIPTION_DESCRIPTOR,
	gtcrn: NOISE_SUPPRESSION_GTCRN_DESCRIPTION_DESCRIPTOR,
	deep_filter: NOISE_SUPPRESSION_ENHANCED_DESCRIPTION_DESCRIPTOR,
};

export function getNoiseSuppressionChoiceLabelDescriptor(backend: VoiceNoiseSuppressionBackend): MessageDescriptor {
	return LABELS[backend];
}

export function getNoiseSuppressionChoiceDescriptionDescriptor(
	backend: VoiceNoiseSuppressionBackend,
): MessageDescriptor {
	return DESCRIPTIONS[backend];
}

export {STEREO_MICROPHONE_DESCRIPTION_DESCRIPTOR, STEREO_MICROPHONE_DESCRIPTOR};
