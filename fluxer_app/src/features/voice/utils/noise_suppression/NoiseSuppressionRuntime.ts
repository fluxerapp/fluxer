// SPDX-License-Identifier: AGPL-3.0-or-later

import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import NoiseSuppressionAvailability from '@app/features/voice/utils/noise_suppression/NoiseSuppressionAvailability';
import type {VoiceNoiseSuppressionBackend} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {resolveNoiseSuppressionBackend} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionSelection';
import {
	getActiveInputDeviceLabel,
	getActiveVoiceProcessingMode,
	resolveVoiceProcessingFromStateForDeviceLabel,
} from '@app/features/voice/utils/VoiceProcessingProfile';

export function readNoiseSuppressionBackend(): VoiceNoiseSuppressionBackend {
	return resolveNoiseSuppressionBackend(VoiceSettings.getNoiseSuppressionBackend());
}

export function readRequestedNoiseSuppressionBackend(): VoiceNoiseSuppressionBackend {
	const mode = getActiveVoiceProcessingMode(VoiceSettings);
	if (mode === 'studio') return 'none';
	return resolveNoiseSuppressionBackend(mode === 'voice' ? null : VoiceSettings.getNoiseSuppressionBackend());
}

export function readEffectiveNoiseSuppressionBackend(): VoiceNoiseSuppressionBackend {
	return (
		NoiseSuppressionAvailability.runningBackend ??
		resolveVoiceProcessingFromStateForDeviceLabel(VoiceSettings, getActiveInputDeviceLabel(VoiceSettings))
			.noiseSuppressionBackend
	);
}

export function readVoiceInputDiagnostics() {
	const profile = resolveVoiceProcessingFromStateForDeviceLabel(
		VoiceSettings,
		getActiveInputDeviceLabel(VoiceSettings),
	);
	const capture = NoiseSuppressionAvailability.captureSettings;
	return {
		echoCancellation:
			capture?.echoCancellation === undefined ? profile.echoCancellation : Boolean(capture.echoCancellation),
		browserNoiseSuppression: capture?.noiseSuppression ?? profile.browserNoiseSuppression,
		autoGainControl: capture?.autoGainControl ?? profile.autoGainControl,
		noiseSuppressionBackend: readEffectiveNoiseSuppressionBackend(),
		requestedNoiseSuppressionBackend: readRequestedNoiseSuppressionBackend(),
		processingMode: profile.mode,
		voiceInputGraphUnavailable: NoiseSuppressionAvailability.voiceInputGraphUnavailable,
		deepFilterLevels: NoiseSuppressionAvailability.deepFilterLevels,
	};
}

export function readCaptureNoiseSuppressionBackend(track: MediaStreamTrack): 'standard' | 'none' {
	const actual = track.getSettings().noiseSuppression;
	if (actual !== undefined) return actual ? 'standard' : 'none';
	const constraint = track.getConstraints().noiseSuppression;
	const enabled = typeof constraint === 'object' ? (constraint.exact ?? constraint.ideal) : constraint;
	return enabled === true ? 'standard' : 'none';
}
