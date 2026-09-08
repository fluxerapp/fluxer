// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, it, vi} from 'vitest';

const refreshMicrophoneFromSettings = vi.fn();
const refreshCameraBackgroundFromSettings = vi.fn();
const refreshCameraCaptureFromSettings = vi.fn();
const refreshScreenShareCodecNegotiationFromSettings = vi.fn();

const settings = {
	cameraResolution: 720,
	videoDeviceId: 'camera',
	screenshareResolution: 1080,
	videoFrameRate: 60,
	streamingMode: 'smooth',
	screenShareContentHint: 'auto',
	preferredScreenShareCodec: 'auto',
	screenShareAv1OptIn: true,
	screenShareHevcOptIn: false,
	screenShareEncoderMode: 'auto',
	screenShareSoftwareQuality: 'auto',
	screenShareScalabilityMode: 'auto',
	screenShareBackupCodecMode: 'auto',
	openH264Enabled: false,
};

vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({
	default: {
		room: null,
		refreshMicrophoneFromSettings,
		refreshCameraBackgroundFromSettings,
		refreshCameraCaptureFromSettings,
		refreshScreenShareCodecNegotiationFromSettings,
		setScreenShareAudioMuted: vi.fn(),
		applyAllLocalAudioPreferences: vi.fn(),
		applyLocalInputVolume: vi.fn(),
	},
}));

vi.mock('@app/features/voice/engine/ScreenShareUnderperformance', () => ({
	noteDeliberateScreenShareQualityChange: vi.fn(),
}));

vi.mock('@app/features/voice/utils/VoiceProcessingProfile', () => ({
	getActiveInputDeviceLabel: () => null,
}));

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {
		updateSettings: (patch: Record<string, unknown>) => Object.assign(settings, patch),
		get cameraResolution() {
			return settings.cameraResolution;
		},
		get videoDeviceId() {
			return settings.videoDeviceId;
		},
		getScreenshareResolution: () => settings.screenshareResolution,
		getVideoFrameRate: () => settings.videoFrameRate,
		getStreamingMode: () => settings.streamingMode,
		getScreenShareContentHint: () => settings.screenShareContentHint,
		getPreferredScreenShareCodec: () => settings.preferredScreenShareCodec,
		getScreenShareAv1OptIn: () => settings.screenShareAv1OptIn,
		getScreenShareHevcOptIn: () => settings.screenShareHevcOptIn,
		getScreenShareEncoderMode: () => settings.screenShareEncoderMode,
		getScreenShareSoftwareQuality: () => settings.screenShareSoftwareQuality,
		getScreenShareScalabilityMode: () => settings.screenShareScalabilityMode,
		getScreenShareBackupCodecMode: () => settings.screenShareBackupCodecMode,
		getOpenH264Enabled: () => settings.openH264Enabled,
	},
}));

const VoiceSettingsCommands = await import('./VoiceSettingsCommands');

describe('screen-share codec settings reactions', () => {
	beforeEach(() => {
		settings.screenShareAv1OptIn = true;
		settings.screenShareHevcOptIn = false;
		settings.preferredScreenShareCodec = 'auto';
		refreshScreenShareCodecNegotiationFromSettings.mockClear();
	});

	it('renegotiates the live screen share when the AV1 opt-in is turned off', () => {
		VoiceSettingsCommands.update({screenShareAv1OptIn: false});
		expect(refreshScreenShareCodecNegotiationFromSettings).toHaveBeenCalledTimes(1);
	});

	it('renegotiates the live screen share when the HEVC opt-in changes', () => {
		VoiceSettingsCommands.update({screenShareHevcOptIn: true});
		expect(refreshScreenShareCodecNegotiationFromSettings).toHaveBeenCalledTimes(1);
	});

	it('renegotiates the live screen share when the preferred codec changes', () => {
		VoiceSettingsCommands.update({preferredScreenShareCodec: 'vp9'});
		expect(refreshScreenShareCodecNegotiationFromSettings).toHaveBeenCalledTimes(1);
	});

	it('leaves the negotiation alone for unrelated screen share settings', () => {
		VoiceSettingsCommands.update({videoFrameRate: 30});
		expect(refreshScreenShareCodecNegotiationFromSettings).not.toHaveBeenCalled();
	});
});
