// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {readEffectiveNoiseSuppression} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionRuntime';
import {applyNoiseSuppressionOverride} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionSelection';
import {isVoiceActivityGateEnabled} from '@app/features/voice/utils/VoiceInputProcessor';
import {
	getActiveInputDeviceLabel,
	resolveVoiceProcessingFromStateForDeviceLabel,
	type VoiceProcessingMode,
} from '@app/features/voice/utils/VoiceProcessingProfile';
import type {VoiceNoiseSuppressionBackend} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';

const MICROPHONE_CAPTURE_SAMPLE_RATE = 48000;

export interface VoiceEngineV2AppAudioSettingsSnapshot {
	readonly inputVolume: number;
	readonly outputVolume: number;
	readonly vadThreshold: number;
	readonly vadAutoSensitivity: boolean;
	readonly voiceActivityGate: boolean;
	readonly requestedInputDeviceId: string;
	readonly effectiveInputDeviceId: string;
	readonly activeInputDeviceLabel: string | null;
	readonly processingMode: VoiceProcessingMode;
	readonly echoCancellation: boolean;
	readonly browserNoiseSuppression: boolean;
	readonly autoGainControl: boolean;
	readonly deepFilter: boolean;
	readonly deepFilterNoiseReductionLevel: number;
	readonly contentHint: '' | 'speech' | 'music';
	readonly noiseSuppressionBackend: VoiceNoiseSuppressionBackend;
	readonly suppressionStrength: number;
	readonly stereoCapture: boolean;
}

function resolveEffectiveInputDeviceId(): string {
	const requested = VoiceSettings.getInputDeviceId();
	const {inputDevices} = VoiceDevicePermissionState.getState();
	assert.equal(typeof requested, 'string', 'audio settings requested input device id must be a string');
	if (inputDevices.length === 0) return requested;
	if (inputDevices.some((device) => device.deviceId === requested)) return requested;
	return inputDevices[0]?.deviceId ?? requested;
}

function assertAudioSettingsSnapshot(snapshot: VoiceEngineV2AppAudioSettingsSnapshot, name: string): void {
	assert.ok(snapshot !== null && typeof snapshot === 'object', `${name} must be an object`);
	assert.equal(typeof snapshot.inputVolume, 'number', `${name}.inputVolume must be a number`);
	assert.equal(typeof snapshot.outputVolume, 'number', `${name}.outputVolume must be a number`);
	assert.equal(typeof snapshot.vadThreshold, 'number', `${name}.vadThreshold must be a number`);
	assert.equal(typeof snapshot.vadAutoSensitivity, 'boolean', `${name}.vadAutoSensitivity must be a boolean`);
	assert.equal(typeof snapshot.voiceActivityGate, 'boolean', `${name}.voiceActivityGate must be a boolean`);
	assert.equal(typeof snapshot.requestedInputDeviceId, 'string', `${name}.requestedInputDeviceId must be a string`);
	assert.equal(typeof snapshot.effectiveInputDeviceId, 'string', `${name}.effectiveInputDeviceId must be a string`);
	assert.equal(typeof snapshot.echoCancellation, 'boolean', `${name}.echoCancellation must be a boolean`);
	assert.equal(typeof snapshot.browserNoiseSuppression, 'boolean', `${name}.browserNoiseSuppression must be a boolean`);
	assert.equal(typeof snapshot.autoGainControl, 'boolean', `${name}.autoGainControl must be a boolean`);
	assert.equal(typeof snapshot.deepFilter, 'boolean', `${name}.deepFilter must be a boolean`);
	assert.equal(typeof snapshot.noiseSuppressionBackend, 'string', `${name}.noiseSuppressionBackend must be a string`);
	assert.equal(typeof snapshot.suppressionStrength, 'number', `${name}.suppressionStrength must be a number`);
	assert.equal(typeof snapshot.stereoCapture, 'boolean', `${name}.stereoCapture must be a boolean`);
}

export function createVoiceEngineV2AppAudioSettingsSnapshot(): VoiceEngineV2AppAudioSettingsSnapshot {
	const activeInputDeviceLabel = getActiveInputDeviceLabel(VoiceSettings);
	const effective = readEffectiveNoiseSuppression(MICROPHONE_CAPTURE_SAMPLE_RATE);
	const profile = applyNoiseSuppressionOverride(
		resolveVoiceProcessingFromStateForDeviceLabel(VoiceSettings, activeInputDeviceLabel),
		effective,
	);
	return {
		inputVolume: VoiceSettings.getInputVolume(),
		outputVolume: VoiceSettings.getOutputVolume(),
		vadThreshold: VoiceSettings.getVadThreshold(),
		vadAutoSensitivity: VoiceSettings.getVadAutoSensitivity(),
		voiceActivityGate: isVoiceActivityGateEnabled(),
		requestedInputDeviceId: VoiceSettings.getInputDeviceId(),
		effectiveInputDeviceId: resolveEffectiveInputDeviceId(),
		activeInputDeviceLabel,
		processingMode: profile.mode,
		echoCancellation: profile.echoCancellation,
		browserNoiseSuppression: profile.browserNoiseSuppression,
		autoGainControl: profile.autoGainControl,
		deepFilter: profile.deepFilter,
		deepFilterNoiseReductionLevel: profile.deepFilterNoiseReductionLevel,
		contentHint: profile.contentHint,
		noiseSuppressionBackend: profile.noiseSuppressionBackend,
		suppressionStrength: effective.suppressionStrength,
		stereoCapture: profile.stereoCapture,
	};
}

export function hasVoiceEngineV2MicrophoneCaptureSettingsChanged(
	previous: VoiceEngineV2AppAudioSettingsSnapshot,
	current: VoiceEngineV2AppAudioSettingsSnapshot,
): boolean {
	assertAudioSettingsSnapshot(previous, 'previous');
	assertAudioSettingsSnapshot(current, 'current');
	if (previous.effectiveInputDeviceId !== current.effectiveInputDeviceId) return true;
	if (previous.processingMode !== current.processingMode) return true;
	if (previous.echoCancellation !== current.echoCancellation) return true;
	if (previous.browserNoiseSuppression !== current.browserNoiseSuppression) return true;
	if (previous.autoGainControl !== current.autoGainControl) return true;
	if (previous.contentHint !== current.contentHint) return true;
	if (previous.stereoCapture !== current.stereoCapture) return true;
	return false;
}

export function hasVoiceEngineV2InputProcessorSettingsChanged(
	previous: VoiceEngineV2AppAudioSettingsSnapshot,
	current: VoiceEngineV2AppAudioSettingsSnapshot,
): boolean {
	assertAudioSettingsSnapshot(previous, 'previous');
	assertAudioSettingsSnapshot(current, 'current');
	if (previous.deepFilter !== current.deepFilter) return true;
	if (previous.deepFilterNoiseReductionLevel !== current.deepFilterNoiseReductionLevel) return true;
	if (previous.voiceActivityGate !== current.voiceActivityGate) return true;
	if (previous.vadAutoSensitivity !== current.vadAutoSensitivity) return true;
	if (previous.noiseSuppressionBackend !== current.noiseSuppressionBackend) return true;
	if (previous.suppressionStrength !== current.suppressionStrength) return true;
	return false;
}
