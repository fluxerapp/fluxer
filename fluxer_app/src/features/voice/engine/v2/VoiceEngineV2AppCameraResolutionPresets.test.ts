// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CameraResolution} from '@app/features/voice/state/VoiceSettings';
import {VideoPresets} from 'livekit-client';
import {describe, expect, it} from 'vitest';
import {getCameraCaptureDimensions, pickCameraSubscriptionQuality} from './VoiceEngineV2AppCameraResolutionPresets';

const EXPECTED_PRESET_TABLE = [
	{resolution: 'high', width: 1920, height: 1080, frameRate: 30, videoPreset: VideoPresets.h1080},
	{resolution: 'medium', width: 1280, height: 720, frameRate: 30, videoPreset: VideoPresets.h720},
	{resolution: 'low', width: 640, height: 360, frameRate: 20, videoPreset: VideoPresets.h360},
] as const;

describe('getCameraCaptureDimensions', () => {
	for (const row of EXPECTED_PRESET_TABLE) {
		it(`maps ${row.resolution} to ${row.width}x${row.height}@${row.frameRate}`, () => {
			expect(getCameraCaptureDimensions(row.resolution)).toEqual({
				width: row.width,
				height: row.height,
				frameRate: row.frameRate,
			});
		});
	}

	it('falls back to the low entry for an unknown resolution key', () => {
		const dimensions = getCameraCaptureDimensions('ultra' as CameraResolution);
		expect(dimensions).toEqual({width: 640, height: 360, frameRate: 20});
	});
});

describe('camera preset table consistency', () => {
	for (const row of EXPECTED_PRESET_TABLE) {
		it(`keeps capture dimensions and the LiveKit encode preset in agreement for ${row.resolution}`, () => {
			const capture = getCameraCaptureDimensions(row.resolution);
			expect(row.videoPreset.width).toBe(capture.width);
			expect(row.videoPreset.height).toBe(capture.height);
			expect(row.videoPreset.encoding.maxFramerate).toBe(capture.frameRate);
		});
	}

	it('orders the table strictly by descending pixel area', () => {
		const areas = EXPECTED_PRESET_TABLE.map((row) => row.width * row.height);
		const sorted = [...areas].sort((first, second) => second - first);
		expect(areas).toEqual(sorted);
		expect(new Set(areas).size).toBe(areas.length);
	});
});

describe('camera capture constraint shape', () => {
	for (const row of EXPECTED_PRESET_TABLE) {
		it(`hands getUserMedia a plain video resolution for ${row.resolution}`, () => {
			const capture = getCameraCaptureDimensions(row.resolution);
			expect(Object.keys(capture).sort()).toEqual(['frameRate', 'height', 'width']);
			expect(capture.frameRate).toBeGreaterThan(0);
		});
	}
});

describe('pickCameraSubscriptionQuality', () => {
	it('requests the lowest layer for a hidden or zero-sized tile', () => {
		expect(pickCameraSubscriptionQuality(0, 0)).toBe('low');
	});

	it('requests the lowest layer when the display fits within the base layer', () => {
		expect(pickCameraSubscriptionQuality(VideoPresets.h180.width, VideoPresets.h180.height)).toBe('low');
		expect(pickCameraSubscriptionQuality(300, 160)).toBe('low');
	});

	it('requests the middle layer when the display exceeds the base layer', () => {
		expect(pickCameraSubscriptionQuality(VideoPresets.h180.width + 1, VideoPresets.h180.height + 1)).toBe('medium');
		expect(pickCameraSubscriptionQuality(VideoPresets.h360.width, VideoPresets.h360.height)).toBe('medium');
	});

	it('requests the top layer when the display exceeds the middle layer', () => {
		expect(pickCameraSubscriptionQuality(VideoPresets.h360.width + 1, VideoPresets.h360.height + 1)).toBe('high');
		expect(pickCameraSubscriptionQuality(VideoPresets.h720.width, VideoPresets.h720.height)).toBe('high');
	});

	it('caps at the top layer for displays larger than the top reference layer', () => {
		expect(pickCameraSubscriptionQuality(3840, 2160)).toBe('high');
	});

	it('escalates quality as either dimension grows', () => {
		const widthEscalation = [
			pickCameraSubscriptionQuality(200, 120),
			pickCameraSubscriptionQuality(500, 280),
			pickCameraSubscriptionQuality(1000, 560),
		];
		expect(widthEscalation).toEqual(['low', 'medium', 'high']);
	});
});
