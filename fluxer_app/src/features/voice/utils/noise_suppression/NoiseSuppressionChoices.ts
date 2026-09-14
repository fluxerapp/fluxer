// SPDX-License-Identifier: AGPL-3.0-or-later

import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import VoiceNoiseSuppressionRollout from '@app/features/voice/state/VoiceNoiseSuppressionRollout';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	getNoiseSuppressionBackendDescriptor,
	isNoiseSuppressionBackendSupported,
	isVoiceNoiseSuppressionBackend,
	VOICE_NOISE_SUPPRESSION_BACKENDS,
	type VoiceNoiseSuppressionBackend,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {readEffectiveNoiseSuppression} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionRuntime';
import {readNoiseSuppressionRuntimeCapabilities} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionSelection';
import {legacyNoiseSuppressionBackend} from '@app/features/voice/utils/VoiceProcessingProfile';

export const NOISE_SUPPRESSION_UI_SAMPLE_RATE = 48000;

export function isNoiseSuppressionChoiceExpanded(): boolean {
	const assignment = VoiceNoiseSuppressionRollout.assignment;
	if (!assignment.enabled || !assignment.allow_user_override) return false;
	return readEffectiveNoiseSuppression(NOISE_SUPPRESSION_UI_SAMPLE_RATE).rolloutApplied;
}

export function getNoiseSuppressionChoiceValues(): ReadonlyArray<VoiceNoiseSuppressionBackend> {
	if (!isNoiseSuppressionChoiceExpanded()) return ['deep_filter', 'standard', 'none'];
	const capabilities = readNoiseSuppressionRuntimeCapabilities(NOISE_SUPPRESSION_UI_SAMPLE_RATE);
	const allowed = new Set(VoiceNoiseSuppressionRollout.assignment.enabled_backends);
	return VOICE_NOISE_SUPPRESSION_BACKENDS.filter(
		(backend) => allowed.has(backend) && isNoiseSuppressionBackendSupported(backend, capabilities),
	);
}

export function getSelectedNoiseSuppressionChoice(): VoiceNoiseSuppressionBackend {
	const values = getNoiseSuppressionChoiceValues();
	if (isNoiseSuppressionChoiceExpanded()) {
		const preference = VoiceSettings.getNoiseSuppressionBackend();
		if (isVoiceNoiseSuppressionBackend(preference) && values.includes(preference)) return preference;
		const effective = readEffectiveNoiseSuppression(NOISE_SUPPRESSION_UI_SAMPLE_RATE);
		if (effective.backend != null && values.includes(effective.backend)) return effective.backend;
		return values[0] ?? 'standard';
	}
	return legacyNoiseSuppressionBackend(
		VoiceSettings.getDeepFilterNoiseSuppression(),
		VoiceSettings.getNoiseSuppression(),
	);
}

export function setNoiseSuppressionChoice(backend: VoiceNoiseSuppressionBackend): void {
	if (isNoiseSuppressionChoiceExpanded()) {
		VoiceSettings.noiseSuppressionBackend = backend;
		if (backend === 'deep_filter' || backend === 'standard' || backend === 'none') {
			VoiceSettingsCommands.update({
				deepFilterNoiseSuppression: backend === 'deep_filter',
				noiseSuppression: backend === 'standard',
			});
		}
		return;
	}
	VoiceSettingsCommands.update({
		deepFilterNoiseSuppression: backend === 'deep_filter',
		noiseSuppression: backend === 'standard',
	});
}

export function isStereoMicrophoneChoiceAvailable(): boolean {
	const assignment = VoiceNoiseSuppressionRollout.assignment;
	if (!assignment.enabled || !assignment.stereo_enabled) return false;
	const effective = readEffectiveNoiseSuppression(NOISE_SUPPRESSION_UI_SAMPLE_RATE);
	if (!effective.rolloutApplied || effective.backend == null) return false;
	return getNoiseSuppressionBackendDescriptor(effective.backend).preservesInputChannels;
}

export function isStereoMicrophoneEnabled(): boolean {
	return VoiceSettings.getStereoMicrophone() !== false;
}
