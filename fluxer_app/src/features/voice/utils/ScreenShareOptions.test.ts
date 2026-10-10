// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildScreenShareOptions,
	resolveScreenShareDegradationPreference,
	resolveScreenShareLayering,
	resolveScreenShareTarget,
} from '@app/features/voice/utils/ScreenShareOptions';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/voice/utils/NativeAudioCaptureBridge', () => ({
	rememberCapturedDisplayAudioTrack: () => undefined,
}));
vi.mock('@app/features/voice/engine/voice_screen_share_manager/shared', () => ({
	stopMediaTrack: () => undefined,
	stopUnselectedStreamTracks: () => undefined,
}));

const {getDisplayMediaOptions} = await import(
	'@app/features/voice/engine/voice_screen_share_manager/DisplayMediaCapture'
);

function collectKeys(value: unknown, keys: Set<string>): Set<string> {
	if (typeof value !== 'object' || value === null) return keys;
	for (const [key, entry] of Object.entries(value)) {
		keys.add(key);
		collectKeys(entry, keys);
	}
	return keys;
}

function targetOf(overrides: {mode?: 'gaming' | 'screenshare' | 'custom'; softwareEncoderClamp?: boolean} = {}) {
	return resolveScreenShareTarget({
		mode: overrides.mode ?? 'screenshare',
		storedResolution: 'medium',
		storedFrameRate: 30,
		entitled: true,
		context: 'display',
		sourceDimensions: null,
		hintSetting: 'auto',
		...(overrides.softwareEncoderClamp === undefined ? {} : {softwareEncoderClamp: overrides.softwareEncoderClamp}),
	});
}

describe('screen share layering', () => {
	it('never asks for temporal layers on the codecs livekit forwards without a dependency descriptor', () => {
		for (const codec of ['h264', 'vp8'] as const) {
			expect(resolveScreenShareLayering({codec, svcSetting: 'auto'}).scalabilityMode).toBeUndefined();
			expect(resolveScreenShareLayering({codec, svcSetting: 'temporal'}).scalabilityMode).toBeUndefined();
		}
	});

	it('keeps temporal layers for the SVC codecs that have one', () => {
		for (const codec of ['av1', 'vp9'] as const) {
			expect(resolveScreenShareLayering({codec, svcSetting: 'auto'}).scalabilityMode).toBe('L1T3');
		}
	});
});

describe('display capture constraints', () => {
	it('asks for no min and no exact, which getDisplayMedia rejects before the picker runs', () => {
		const {captureOptions} = buildScreenShareOptions({
			resolution: 'high',
			frameRate: 60,
			context: 'display',
			includeAudio: true,
			contentHint: 'text',
			sourceDimensions: {width: 3840, height: 2160},
			preferredDisplaySurface: 'monitor',
		});
		const keys = collectKeys(getDisplayMediaOptions(captureOptions).video, new Set<string>());
		expect(keys.has('min')).toBe(false);
		expect(keys.has('exact')).toBe(false);
	});
});

describe('screen share quality', () => {
	it('applies the software H.264 clamp', () => {
		expect(targetOf({mode: 'gaming', softwareEncoderClamp: true})).toMatchObject({
			resolution: 'medium',
			frameRate: 30,
			softwareEncoderClamped: true,
		});
	});
});

describe('screen share degradation preference', () => {
	it('keeps device shares balanced', () => {
		expect(
			resolveScreenShareDegradationPreference({
				context: 'device',
				rung: 'medium',
				contentHint: undefined,
				maxBitrate: 3_000_000,
			}),
		).toBe('balanced');
	});

	it('refuses maintain-framerate below the initial frame dropper cliff', () => {
		expect(
			resolveScreenShareDegradationPreference({
				context: 'display',
				rung: 'low_240p',
				contentHint: undefined,
				maxBitrate: 300_000,
			}),
		).toBe('maintain-resolution');
		expect(
			resolveScreenShareDegradationPreference({
				context: 'display',
				rung: 'medium',
				contentHint: undefined,
				maxBitrate: 3_000_000,
			}),
		).toBe('maintain-framerate');
	});
});
