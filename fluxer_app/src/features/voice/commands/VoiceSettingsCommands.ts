// SPDX-License-Identifier: AGPL-3.0-or-later

import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {noteDeliberateScreenShareQualityChange} from '@app/features/voice/engine/ScreenShareUnderperformance';
import VoiceSettings, {
	type CameraResolution,
	type ScreenshareResolution,
	type StreamingMode,
} from '@app/features/voice/state/VoiceSettings';
import type {
	CodecPreference,
	ScreenShareBackupCodecMode,
	ScreenShareContentHint,
	ScreenShareEncoderMode,
	ScreenShareScalabilityModePreference,
	ScreenShareSoftwareQuality,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {getActiveInputDeviceLabel, type VoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';

type VoiceSettingsPatch = Partial<{
	inputDeviceId: string;
	outputDeviceId: string;
	videoDeviceId: string;
	inputVolume: number;
	outputVolume: number;
	echoCancellation: boolean;
	noiseSuppression: boolean;
	autoGainControl: boolean;
	deepFilterNoiseSuppression: boolean;
	deepFilterNoiseSuppressionLevel: number;
	voiceProcessingMode: VoiceProcessingMode;
	cameraResolution: CameraResolution;
	mirrorCamera: boolean;
	screenshareResolution: ScreenshareResolution;
	videoFrameRate: number;
	streamingMode: StreamingMode;
	hideStreamPreview: boolean;
	muteStreamAudio: boolean;
	shareAppAudio: boolean;
	shareDesktopAudio: boolean;
	shareDeviceAudio: boolean;
	screenShareAudioDeviceId: string;
	backgroundImageId: string;
	backgroundImages: Array<{
		id: string;
		createdAt: number;
	}>;
	backgroundBlurStrength: number;
	showGridView: boolean;
	showMyOwnCamera: boolean;
	showMyOwnScreenShare: boolean;
	showNonVideoParticipants: boolean;
	showParticipantsCarousel: boolean;
	showVoiceConnectionAvatarStack: boolean;
	showVoiceConnectionId: boolean;
	showConnectionVolumeControls: boolean;
	pauseOwnScreenSharePreviewOnUnfocus: boolean;
	disablePictureInPicturePopoutScreenShare: boolean;
	preferredVideoCodec: CodecPreference;
	preferredScreenShareCodec: CodecPreference;
	screenShareAv1OptIn: boolean;
	screenShareHevcOptIn: boolean;
	screenShareContentHint: ScreenShareContentHint;
	screenShareEncoderMode: ScreenShareEncoderMode;
	screenShareSoftwareQuality: ScreenShareSoftwareQuality;
	screenShareScalabilityMode: ScreenShareScalabilityModePreference;
	screenShareBackupCodecMode: ScreenShareBackupCodecMode;
	vadThreshold: number;
	vadAutoSensitivity: boolean;
	vadEnhanced: boolean;
	linuxAudioCaptureWorkaround: boolean;
	linuxAudioCaptureOnlySpeakers: boolean;
	linuxAudioCaptureOnlyDefaultSpeakers: boolean;
	linuxAudioCaptureIgnoreInputMedia: boolean;
	linuxAudioCaptureIgnoreVirtual: boolean;
	linuxAudioCaptureIgnoreDevices: boolean;
	linuxAudioCaptureGranularSelect: boolean;
	linuxAudioCaptureDeviceSelect: boolean;
	screenShareManualAudioSourcesOptIn: boolean;
	screenShareAudioSourceMode: 'none' | 'system' | 'specific';
	screenShareAudioIncludeSources: Array<Record<string, string>>;
	screenShareAudioExcludeSources: Array<Record<string, string>>;
	openH264Enabled: boolean;
}>;

interface VoiceSettingsUpdateOptions {
	refreshCameraBackground?: boolean;
}

const MICROPHONE_REFRESH_KEYS: Array<keyof VoiceSettingsPatch> = [
	'inputDeviceId',
	'echoCancellation',
	'noiseSuppression',
	'autoGainControl',
	'deepFilterNoiseSuppression',
	'deepFilterNoiseSuppressionLevel',
	'voiceProcessingMode',
];
const CAMERA_BACKGROUND_REFRESH_KEYS: Array<keyof VoiceSettingsPatch> = [
	'backgroundImageId',
	'backgroundImages',
	'backgroundBlurStrength',
	'mirrorCamera',
];
const CAMERA_CAPTURE_REFRESH_KEYS: Array<keyof VoiceSettingsPatch> = ['cameraResolution', 'videoDeviceId'];
function refreshMicrophone(): void {
	MediaEngine.refreshMicrophoneFromSettings();
}

function refreshCameraBackground(): void {
	MediaEngine.refreshCameraBackgroundFromSettings();
}

function refreshCameraCapture(): void {
	MediaEngine.refreshCameraCaptureFromSettings();
}

function shouldRefreshMicrophone(settings: VoiceSettingsPatch): boolean {
	return MICROPHONE_REFRESH_KEYS.some((key) => settings[key] !== undefined);
}

function shouldRefreshCameraBackground(settings: VoiceSettingsPatch): boolean {
	return CAMERA_BACKGROUND_REFRESH_KEYS.some((key) => settings[key] !== undefined);
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

function readScreenShareQualityValues(): VoiceSettingsPatch {
	return {
		screenshareResolution: VoiceSettings.getScreenshareResolution(),
		videoFrameRate: VoiceSettings.getVideoFrameRate(),
		streamingMode: VoiceSettings.getStreamingMode(),
		screenShareContentHint: VoiceSettings.getScreenShareContentHint(),
		preferredScreenShareCodec: VoiceSettings.getPreferredScreenShareCodec(),
		screenShareAv1OptIn: VoiceSettings.getScreenShareAv1OptIn(),
		screenShareHevcOptIn: VoiceSettings.getScreenShareHevcOptIn(),
		screenShareEncoderMode: VoiceSettings.getScreenShareEncoderMode(),
		screenShareSoftwareQuality: VoiceSettings.getScreenShareSoftwareQuality(),
		screenShareScalabilityMode: VoiceSettings.getScreenShareScalabilityMode(),
		screenShareBackupCodecMode: VoiceSettings.getScreenShareBackupCodecMode(),
		openH264Enabled: VoiceSettings.getOpenH264Enabled(),
	};
}

function changesScreenShareQualityDeliberately(
	settings: VoiceSettingsPatch,
	before: VoiceSettingsPatch,
	after: VoiceSettingsPatch,
): boolean {
	return (Object.keys(before) as Array<keyof VoiceSettingsPatch>).some(
		(key) => settings[key] !== undefined && before[key] !== after[key],
	);
}

function applyUpdatedVoiceSettings(
	settings: VoiceSettingsPatch,
	refreshInput: boolean,
	options: VoiceSettingsUpdateOptions = {},
): void {
	const cameraCaptureBefore = readCameraCaptureRefreshValues();
	const screenShareQualityBefore = readScreenShareQualityValues();
	VoiceSettings.updateSettings(settings);
	if (changesScreenShareQualityDeliberately(settings, screenShareQualityBefore, readScreenShareQualityValues())) {
		noteDeliberateScreenShareQualityChange();
	}
	if (settings.muteStreamAudio !== undefined) {
		MediaEngine.setScreenShareAudioMuted(settings.muteStreamAudio);
	}
	if (settings.outputVolume !== undefined) {
		if (MediaEngine.room) {
			MediaEngine.applyAllLocalAudioPreferences();
		}
	}
	if (settings.inputVolume !== undefined && !refreshInput) {
		MediaEngine.applyLocalInputVolume();
	}
	if (refreshInput) {
		refreshMicrophone();
	}
	if (options.refreshCameraBackground !== false && shouldRefreshCameraBackground(settings)) {
		refreshCameraBackground();
	}
	if (shouldRefreshCameraCapture(settings, cameraCaptureBefore, readCameraCaptureRefreshValues())) {
		refreshCameraCapture();
	}
}

export function setVoiceProcessingModeForDeviceLabel(label: string, mode: VoiceProcessingMode): void {
	VoiceSettings.setVoiceProcessingModeForDeviceLabel(label, mode);
	refreshMicrophone();
}

export function clearVoiceProcessingModeForDeviceLabel(label: string): void {
	VoiceSettings.clearVoiceProcessingModeForDeviceLabel(label);
	refreshMicrophone();
}

export function setActiveInputVoiceProcessingMode(mode: VoiceProcessingMode): void {
	const label = getActiveInputDeviceLabel(VoiceSettings);
	if (label) {
		VoiceSettings.setVoiceProcessingModeForDeviceLabel(label, mode);
		refreshMicrophone();
	} else {
		applyUpdatedVoiceSettings({voiceProcessingMode: mode}, true);
	}
}

export function update(settings: VoiceSettingsPatch, options?: VoiceSettingsUpdateOptions): void {
	applyUpdatedVoiceSettings(settings, shouldRefreshMicrophone(settings), options);
}
