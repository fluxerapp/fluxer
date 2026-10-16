// SPDX-License-Identifier: AGPL-3.0-or-later

import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import type {VoiceSettingsUpdate} from '@app/features/voice/state/VoiceSettings';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {getActiveInputDeviceLabel, type VoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';

type VoiceSettingsPatch = VoiceSettingsUpdate;

interface VoiceSettingsUpdateOptions {
	refreshCameraBackground?: boolean;
}

const CAMERA_BACKGROUND_REFRESH_KEYS: Array<keyof VoiceSettingsPatch> = [
	'backgroundImageId',
	'backgroundImages',
	'backgroundBlurStrength',
	'mirrorCamera',
];
const CAMERA_CAPTURE_REFRESH_KEYS: Array<keyof VoiceSettingsPatch> = ['cameraResolution', 'videoDeviceId'];
const SCREEN_SHARE_CODEC_NEGOTIATION_REFRESH_KEYS: Array<keyof VoiceSettingsPatch> = [
	'preferredScreenShareCodec',
	'screenShareAv1OptIn',
	'screenShareHevcOptIn',
	'screenShareEncoderMode',
];
function refreshCameraBackground(): void {
	MediaEngine.refreshCameraBackgroundFromSettings();
}

function refreshCameraCapture(): void {
	MediaEngine.refreshCameraCaptureFromSettings();
}

function refreshScreenShareCodecNegotiation(): void {
	MediaEngine.refreshScreenShareCodecNegotiationFromSettings();
}

function shouldRefreshCameraBackground(settings: VoiceSettingsPatch): boolean {
	return CAMERA_BACKGROUND_REFRESH_KEYS.some((key) => settings[key] !== undefined);
}

function shouldRefreshScreenShareCodecNegotiation(settings: VoiceSettingsPatch): boolean {
	return SCREEN_SHARE_CODEC_NEGOTIATION_REFRESH_KEYS.some((key) => settings[key] !== undefined);
}

function readCameraCaptureRefreshValues(): VoiceSettingsPatch {
	return {
		cameraResolution: VoiceSettings.cameraResolution,
		videoDeviceId: VoiceSettings.videoDeviceId,
	};
}

function shouldRefreshCameraCapture(
	settings: VoiceSettingsPatch,
	before: VoiceSettingsPatch,
	after: VoiceSettingsPatch,
): boolean {
	return CAMERA_CAPTURE_REFRESH_KEYS.some((key) => settings[key] !== undefined && before[key] !== after[key]);
}

function applyUpdatedVoiceSettings(settings: VoiceSettingsPatch, options: VoiceSettingsUpdateOptions = {}): void {
	const cameraCaptureBefore = readCameraCaptureRefreshValues();
	VoiceSettings.updateSettings(settings);
	if (settings.muteStreamAudio !== undefined) {
		MediaEngine.setScreenShareAudioMuted(settings.muteStreamAudio);
	}
	if (settings.outputVolume !== undefined) {
		if (MediaEngine.room) {
			MediaEngine.applyAllLocalAudioPreferences();
		}
	}
	if (options.refreshCameraBackground !== false && shouldRefreshCameraBackground(settings)) {
		refreshCameraBackground();
	}
	if (shouldRefreshCameraCapture(settings, cameraCaptureBefore, readCameraCaptureRefreshValues())) {
		refreshCameraCapture();
	}
	if (shouldRefreshScreenShareCodecNegotiation(settings)) {
		refreshScreenShareCodecNegotiation();
	}
}

export function setActiveInputVoiceProcessingMode(mode: VoiceProcessingMode): void {
	const label = getActiveInputDeviceLabel(VoiceSettings);
	if (label) {
		VoiceSettings.setVoiceProcessingModeForDeviceLabel(label, mode);
	} else {
		applyUpdatedVoiceSettings({voiceProcessingMode: mode});
	}
}

export function update(settings: VoiceSettingsPatch, options?: VoiceSettingsUpdateOptions): void {
	applyUpdatedVoiceSettings(settings, options);
}
