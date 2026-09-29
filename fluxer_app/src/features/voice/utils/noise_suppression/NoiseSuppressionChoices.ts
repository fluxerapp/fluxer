// SPDX-License-Identifier: AGPL-3.0-or-later

import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	isNoiseSuppressionBackendSupported,
	VOICE_NOISE_SUPPRESSION_BACKENDS,
	type VoiceNoiseSuppressionBackend,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {readNoiseSuppressionBackend} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionRuntime';
import {
	readNoiseSuppressionRuntimeCapabilities,
	supportsStereoCapture,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionSelection';
import {resolveVoiceProcessingFromState} from '@app/features/voice/utils/VoiceProcessingProfile';

export function getNoiseSuppressionChoiceValues(): ReadonlyArray<VoiceNoiseSuppressionBackend> {
	const capabilities = readNoiseSuppressionRuntimeCapabilities();
	return VOICE_NOISE_SUPPRESSION_BACKENDS.filter((backend) =>
		isNoiseSuppressionBackendSupported(backend, capabilities),
	);
}

export function getSelectedNoiseSuppressionChoice(): VoiceNoiseSuppressionBackend {
	return readNoiseSuppressionBackend();
}

export function setNoiseSuppressionChoice(backend: VoiceNoiseSuppressionBackend): void {
	VoiceSettingsCommands.update({noiseSuppressionBackend: backend});
}

export function isStereoMicrophoneChoiceAvailable(): boolean {
	return supportsStereoCapture(resolveVoiceProcessingFromState(VoiceSettings));
}

export function isStereoMicrophoneEnabled(): boolean {
	return VoiceSettings.getStereoMicrophone() === true;
}
