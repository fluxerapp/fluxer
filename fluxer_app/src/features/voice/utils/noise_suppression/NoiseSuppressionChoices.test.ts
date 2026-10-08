import type {VoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const {deviceModes, voiceSettings} = vi.hoisted(() => ({
	deviceModes: new Map<string, VoiceProcessingMode>(),
	voiceSettings: {
		voiceProcessingMode: 'voice' as VoiceProcessingMode,
		echoCancellation: true,
		autoGainControl: false,
		getNoiseSuppressionBackend: () => 'none',
		getStereoMicrophone: () => false,
		getVoiceProcessingModeForDeviceLabel: vi.fn<(label: string | null | undefined) => VoiceProcessingMode>(),
	},
}));

vi.mock('@app/features/voice/state/VoiceSettings', () => ({default: voiceSettings}));
vi.mock('@app/features/voice/commands/VoiceSettingsCommands', () => ({update: vi.fn()}));
vi.mock('@app/features/voice/engine/VoiceDevicePermissionState', () => ({
	default: {getState: () => ({inputDevices: []})},
}));
vi.mock('@app/features/voice/utils/VoiceDeviceManager', () => ({resolveEffectiveDeviceId: () => null}));

import {isStereoMicrophoneChoiceAvailable} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionChoices';

describe('stereo microphone choice for the selected device', () => {
	beforeEach(() => {
		deviceModes.clear();
		voiceSettings.voiceProcessingMode = 'voice';
		voiceSettings.echoCancellation = true;
		voiceSettings.autoGainControl = false;
		voiceSettings.getVoiceProcessingModeForDeviceLabel.mockReset();
		voiceSettings.getVoiceProcessingModeForDeviceLabel.mockImplementation(
			(label) => (label ? deviceModes.get(label) : undefined) ?? voiceSettings.voiceProcessingMode,
		);
	});

	it('shows the switch for a device using direct input when the global mode is focused voice', () => {
		deviceModes.set('USB audio interface', 'studio');
		expect(isStereoMicrophoneChoiceAvailable('USB audio interface')).toBe(true);
		expect(voiceSettings.getVoiceProcessingModeForDeviceLabel).toHaveBeenCalledWith('USB audio interface');
	});

	it('hides the switch for a device with custom processing when the global mode is direct input', () => {
		voiceSettings.voiceProcessingMode = 'studio';
		deviceModes.set('Headset microphone', 'custom');
		expect(isStereoMicrophoneChoiceAvailable('Headset microphone')).toBe(false);
	});

	it('shows the switch for a custom device profile with processing disabled', () => {
		deviceModes.set('USB audio interface', 'custom');
		voiceSettings.echoCancellation = false;
		expect(isStereoMicrophoneChoiceAvailable('USB audio interface')).toBe(true);
	});

	it.each([
		{mode: 'voice' as const, available: false},
		{mode: 'studio' as const, available: true},
	])('uses the global $mode profile when the device has no override', ({mode, available}) => {
		voiceSettings.voiceProcessingMode = mode;
		expect(isStereoMicrophoneChoiceAvailable('Unconfigured microphone')).toBe(available);
		expect(isStereoMicrophoneChoiceAvailable(null)).toBe(available);
	});
});
