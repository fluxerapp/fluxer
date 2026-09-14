// SPDX-License-Identifier: AGPL-3.0-or-later

import VoiceNoiseSuppressionRollout from '@app/features/voice/state/VoiceNoiseSuppressionRollout';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	type EffectiveNoiseSuppression,
	getNoiseSuppressionScopeGuildId,
	readNoiseSuppressionRuntimeCapabilities,
	resolveEffectiveNoiseSuppression,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionSelection';

export function readEffectiveNoiseSuppression(sampleRate: number): EffectiveNoiseSuppression {
	return resolveEffectiveNoiseSuppression(
		VoiceNoiseSuppressionRollout.assignment,
		getNoiseSuppressionScopeGuildId(),
		VoiceSettings.getNoiseSuppressionBackend(),
		readNoiseSuppressionRuntimeCapabilities(sampleRate),
		VoiceSettings.getStereoMicrophone(),
	);
}
