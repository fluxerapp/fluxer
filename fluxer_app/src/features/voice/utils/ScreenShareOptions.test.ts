// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildScreenShareOptions,
	getScreenShareBitrateBps,
	resolveScreenShareDegradationPreference,
	resolveScreenShareFrameRate,
	resolveScreenShareLayering,
	resolveScreenShareTarget,
} from '@app/features/voice/utils/ScreenShareOptions';
import {afterEach, describe, expect, it, vi} from 'vitest';

const rollout = vi.hoisted(() => ({enabled: false}));

vi.mock('@app/features/voice/state/ScreenShareDeliveryRollout', () => ({
	ScreenShareDeliveryRollout: rollout,
	default: rollout,
}));

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

afterEach(() => {
	rollout.enabled = false;
});

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

	it('keeps temporal layers for the SVC codecs that carry one', () => {
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

describe('the screen share delivery experiment', () => {
	it('keeps the source preset and the 90 and 120 FPS rungs off the experiment', () => {
		expect(targetOf()).toMatchObject({resolution: 'source', frameRate: 15});
		expect(resolveScreenShareFrameRate(120)).toBe(120);
		expect(resolveScreenShareFrameRate(90)).toBe(90);
		expect(resolveScreenShareFrameRate(60)).toBe(60);
	});

	it('moves the preset to 1080p30 and lands the faster rungs on 60 FPS on the experiment', () => {
		rollout.enabled = true;
		expect(targetOf()).toMatchObject({resolution: 'high', frameRate: 30});
		expect(resolveScreenShareFrameRate(120)).toBe(60);
		expect(resolveScreenShareFrameRate(90)).toBe(60);
		expect(resolveScreenShareFrameRate(60)).toBe(60);
	});

	it('reads the rung table off the experiment and the pixel budget on it', () => {
		expect(getScreenShareBitrateBps('source', 60)).toBe(6_000_000);
		rollout.enabled = true;
		expect(getScreenShareBitrateBps('source', 60)).toBe(9_000_000);
	});

	it('publishes the stored frame rate and the rung bitrate off the experiment', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'source',
			frameRate: 90,
			context: 'display',
			includeAudio: false,
			sourceDimensions: {width: 3840, height: 2160},
		});
		expect(publishOptions.screenShareEncoding).toEqual({
			maxBitrate: 6_000_000,
			maxFramerate: 90,
			priority: 'high',
		});
		expect(publishOptions.degradationPreference).toBe('maintain-resolution');
	});

	it('holds the motion hint for every surface off the experiment and only for a camera on it', () => {
		expect(targetOf({mode: 'gaming'}).contentHint).toBe('motion');
		rollout.enabled = true;
		expect(targetOf({mode: 'gaming'}).contentHint).toBeUndefined();
	});

	it('ignores the software H.264 clamp off the experiment', () => {
		expect(targetOf({mode: 'gaming', softwareEncoderClamp: true})).toMatchObject({
			resolution: 'ultra',
			frameRate: 60,
			softwareEncoderClamped: false,
		});
		rollout.enabled = true;
		expect(targetOf({mode: 'gaming', softwareEncoderClamp: true})).toMatchObject({
			resolution: 'medium',
			frameRate: 30,
			softwareEncoderClamped: true,
		});
	});
});

describe('screen share degradation preference', () => {
	it('holds the resolution for every share off the experiment', () => {
		expect(
			resolveScreenShareDegradationPreference({
				context: 'display',
				rung: 'medium',
				contentHint: undefined,
				maxBitrate: 3_000_000,
			}),
		).toBe('maintain-resolution');
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
		rollout.enabled = true;
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
