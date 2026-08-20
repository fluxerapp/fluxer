// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	FOCUSED_VOICE_DEEP_FILTER_NOISE_REDUCTION_LEVEL,
	resolveVoiceProcessing,
	type VoiceProcessingSettingsLike,
} from '@app/features/voice/utils/VoiceProcessingProfile';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/voice/engine/VoiceDevicePermissionState', () => ({
	default: {getState: () => ({inputDevices: []})},
}));

vi.mock('@app/features/voice/utils/VoiceDeviceManager', () => ({
	resolveEffectiveDeviceId: () => null,
}));

function createSettings(overrides: Partial<VoiceProcessingSettingsLike> = {}): VoiceProcessingSettingsLike {
	return {
		voiceProcessingMode: 'voice',
		echoCancellation: true,
		noiseSuppression: true,
		autoGainControl: true,
		deepFilterNoiseSuppression: true,
		deepFilterNoiseSuppressionLevel: 80,
		...overrides,
	};
}

describe('resolveVoiceProcessing', () => {
	it('keeps automatic gain control on for focused voice at factory defaults', () => {
		const resolved = resolveVoiceProcessing(createSettings());
		expect(resolved.mode).toBe('voice');
		expect(resolved.autoGainControl).toBe(true);
		expect(resolved.deepFilter).toBe(true);
		expect(resolved.deepFilterNoiseReductionLevel).toBe(FOCUSED_VOICE_DEEP_FILTER_NOISE_REDUCTION_LEVEL);
	});

	it('honours an explicit opt out of automatic gain control in focused voice', () => {
		const resolved = resolveVoiceProcessing(createSettings({autoGainControl: false}));
		expect(resolved.autoGainControl).toBe(false);
	});

	it('keeps automatic gain control on in custom mode while deep filter is enabled', () => {
		const resolved = resolveVoiceProcessing(
			createSettings({voiceProcessingMode: 'custom', deepFilterNoiseSuppression: true}),
		);
		expect(resolved.autoGainControl).toBe(true);
		expect(resolved.deepFilter).toBe(true);
	});

	it('still suppresses browser noise suppression when deep filter is enabled in custom mode', () => {
		const resolved = resolveVoiceProcessing(
			createSettings({voiceProcessingMode: 'custom', noiseSuppression: true, deepFilterNoiseSuppression: true}),
		);
		expect(resolved.browserNoiseSuppression).toBe(false);
	});

	it('leaves studio mode as the untouched bypass', () => {
		const resolved = resolveVoiceProcessing(createSettings({voiceProcessingMode: 'studio'}));
		expect(resolved.autoGainControl).toBe(false);
		expect(resolved.echoCancellation).toBe(false);
		expect(resolved.browserNoiseSuppression).toBe(false);
		expect(resolved.deepFilter).toBe(false);
		expect(resolved.contentHint).toBe('music');
	});
});
