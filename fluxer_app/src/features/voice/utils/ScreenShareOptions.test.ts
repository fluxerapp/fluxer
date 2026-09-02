// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	buildScreenShareOptions,
	capScreenShareEncodingToDimensions,
	getScreenShareBitrateBps,
	getScreenShareEncoding,
	resolveStreamingModeSettings,
	SCREEN_SHARE_MAX_VIDEO_BITRATE_BPS,
	STREAMING_MODE_PRESETS,
	SUPPORTED_SCREEN_SHARE_FRAME_RATES,
	type SupportedScreenShareFrameRate,
} from './ScreenShareOptions';

const RESOLUTIONS = ['low_240p', 'low_480p', 'medium', 'high', 'ultra', 'source'] as const;

describe('buildScreenShareOptions', () => {
	it('asks display capture to omit the cursor for app windows', () => {
		const {captureOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: false,
			preferredDisplaySurface: 'window',
		});
		expect(captureOptions.video).toMatchObject({cursor: 'never'});
	});
	it('asks display capture to include the cursor for full displays', () => {
		const {captureOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: false,
			preferredDisplaySurface: 'monitor',
		});
		expect(captureOptions.video).toMatchObject({
			cursor: 'always',
			displaySurface: 'monitor',
		});
	});
	it('preserves the preferred app display surface while omitting the cursor', () => {
		const {captureOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: true,
			preferredDisplaySurface: 'window',
		});
		expect(captureOptions.video).toMatchObject({
			cursor: 'never',
			displaySurface: 'window',
		});
	});
	it('requests own-audio restriction without offering monitor system audio for app window shares', () => {
		const {captureOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: true,
			preferredDisplaySurface: 'window',
		});
		expect(captureOptions).toMatchObject({
			audio: true,
			restrictOwnAudio: true,
			systemAudio: 'exclude',
			windowAudio: 'window',
			monitorTypeSurfaces: 'exclude',
		});
	});
	it('does not offer system audio for full display shares', () => {
		const {captureOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: true,
			preferredDisplaySurface: 'monitor',
		});
		expect(captureOptions).toMatchObject({
			audio: true,
			restrictOwnAudio: true,
			systemAudio: 'exclude',
			windowAudio: 'window',
			monitorTypeSurfaces: 'include',
		});
	});
	it('excludes window and system audio hints when audio is disabled', () => {
		const {captureOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: false,
		});
		expect(captureOptions).toMatchObject({
			audio: false,
			systemAudio: 'exclude',
			windowAudio: 'exclude',
		});
	});
	it('prefers resolution for detail-oriented shares', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: true,
		});
		expect(publishOptions.degradationPreference).toBe('maintain-resolution');
	});
	it('keeps resolution for gaming-style high-framerate shares', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'ultra',
			frameRate: 60,
			includeAudio: true,
		});
		expect(publishOptions.degradationPreference).toBe('maintain-resolution');
	});
	it('passes the selected content hint through capture options', () => {
		const {captureOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: false,
			contentHint: 'motion',
		});
		expect(captureOptions.contentHint).toBe('motion');
	});
	it('leaves screen share content hint unset by default', () => {
		const {captureOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: false,
		});
		expect(captureOptions.contentHint).toBeUndefined();
	});
	it('publishes the ceiling bitrate for the largest preset', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'source',
			frameRate: 60,
			includeAudio: false,
		});
		expect(publishOptions.screenShareEncoding?.maxBitrate).toBe(SCREEN_SHARE_MAX_VIDEO_BITRATE_BPS);
	});
	it('publishes a lower bitrate for a smaller preset', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: false,
		});
		expect(publishOptions.screenShareEncoding?.maxBitrate).toBe(3_000_000);
	});
	it('bills the source preset at the rung the capture geometry actually fills', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'source',
			frameRate: 15,
			includeAudio: false,
			sourceDimensions: {width: 1920, height: 1080},
		});
		expect(publishOptions.screenShareEncoding?.maxBitrate).toBe(3_000_000);
	});
	it('bills a shared window at the rung its pixel count reaches', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'source',
			frameRate: 30,
			includeAudio: false,
			sourceDimensions: {width: 1080, height: 1920},
		});
		expect(publishOptions.screenShareEncoding?.maxBitrate).toBe(4_500_000);
	});
	it('keeps the source preset on its own rung when the capture fills it', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'source',
			frameRate: 15,
			includeAudio: false,
			sourceDimensions: {width: 3840, height: 2160},
		});
		expect(publishOptions.screenShareEncoding?.maxBitrate).toBe(4_500_000);
	});
	it('defaults the high-tier gaming preset to 60 fps', () => {
		expect(resolveStreamingModeSettings('gaming', 'medium', 30, true)).toEqual({
			resolution: 'ultra',
			frameRate: 60,
		});
	});
	it('keeps free-tier gaming capped at 30 fps', () => {
		expect(resolveStreamingModeSettings('gaming', 'medium', 30, false)).toEqual({
			resolution: 'medium',
			frameRate: 30,
		});
	});
	it('starts the free-tier screenshare preset at 720p 30 fps', () => {
		expect(resolveStreamingModeSettings('screenshare', 'medium', 15, false)).toEqual({
			resolution: 'medium',
			frameRate: 30,
		});
	});
	it('lifts a retired free-tier 240p custom choice back to 720p', () => {
		expect(resolveStreamingModeSettings('custom', 'low_240p', 30, false)).toEqual({
			resolution: 'medium',
			frameRate: 30,
		});
	});
	it('keeps 480p as the lowest free-tier rung', () => {
		expect(resolveStreamingModeSettings('custom', 'low_480p', 30, false)).toEqual({
			resolution: 'low_480p',
			frameRate: 30,
		});
	});
	it('offers the browser picker system audio for every surface when audio is requested', () => {
		const {captureOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: true,
			useBrowserAudioPicker: true,
		});
		expect(captureOptions).toMatchObject({
			audio: true,
			systemAudio: 'include',
			windowAudio: 'system',
		});
	});
	it('keeps the browser picker audio hints excluded when audio is not requested', () => {
		const {captureOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: false,
			useBrowserAudioPicker: true,
		});
		expect(captureOptions).toMatchObject({
			audio: false,
			systemAudio: 'exclude',
			windowAudio: 'exclude',
		});
	});
});

describe('screen share bitrate ladder', () => {
	it('publishes the Twitch anchor bitrates', () => {
		expect(getScreenShareBitrateBps('high', 60)).toBe(6_000_000);
		expect(getScreenShareBitrateBps('high', 30)).toBe(4_500_000);
		expect(getScreenShareBitrateBps('medium', 60)).toBe(4_500_000);
		expect(getScreenShareBitrateBps('medium', 30)).toBe(3_000_000);
	});

	it('never exceeds the published ceiling', () => {
		for (const resolution of RESOLUTIONS) {
			for (const frameRate of SUPPORTED_SCREEN_SHARE_FRAME_RATES) {
				expect(getScreenShareBitrateBps(resolution, frameRate)).toBeLessThanOrEqual(SCREEN_SHARE_MAX_VIDEO_BITRATE_BPS);
			}
		}
	});

	it('rises with frame rate at every resolution', () => {
		for (const resolution of RESOLUTIONS) {
			let previous = 0;
			for (const frameRate of SUPPORTED_SCREEN_SHARE_FRAME_RATES) {
				const bitrate = getScreenShareBitrateBps(resolution, frameRate);
				expect(bitrate).toBeGreaterThanOrEqual(previous);
				previous = bitrate;
			}
		}
	});

	it('rises with resolution at every frame rate', () => {
		for (const frameRate of SUPPORTED_SCREEN_SHARE_FRAME_RATES) {
			let previous = 0;
			for (const resolution of RESOLUTIONS) {
				const bitrate = getScreenShareBitrateBps(resolution, frameRate);
				expect(bitrate).toBeGreaterThanOrEqual(previous);
				previous = bitrate;
			}
		}
	});

	it('reuses the 60 fps cell above 60 fps', () => {
		const highFrameRates: Array<SupportedScreenShareFrameRate> = [90, 120];
		for (const resolution of RESOLUTIONS) {
			for (const frameRate of highFrameRates) {
				expect(getScreenShareBitrateBps(resolution, frameRate)).toBe(getScreenShareBitrateBps(resolution, 60));
			}
		}
	});

	it('scales the screenshare preset to its own rung', () => {
		const {resolution, frameRate} = STREAMING_MODE_PRESETS.screenshare;
		expect(getScreenShareEncoding(resolution, frameRate).maxBitrate).toBe(4_500_000);
	});

	it('scales the gaming preset to its own rung', () => {
		const {resolution, frameRate} = STREAMING_MODE_PRESETS.gaming;
		expect(getScreenShareEncoding(resolution, frameRate).maxBitrate).toBe(6_000_000);
	});

	it('varies with frame rate', () => {
		expect(getScreenShareEncoding('medium', 15).maxBitrate).toBe(2_000_000);
		expect(getScreenShareEncoding('medium', 60).maxBitrate).toBe(4_500_000);
	});

	it('rounds an unsupported frame rate down to a supported rung', () => {
		expect(getScreenShareEncoding('medium', 45).maxBitrate).toBe(getScreenShareEncoding('medium', 30).maxBitrate);
		expect(getScreenShareEncoding('medium', 45).maxFramerate).toBe(30);
	});

	it('caps a selected rung at the rung the captured picture actually fills', () => {
		const encoding = getScreenShareEncoding('source', 30);
		expect(capScreenShareEncodingToDimensions(encoding, {width: 800, height: 600}).maxBitrate).toBe(2_000_000);
		expect(capScreenShareEncodingToDimensions(encoding, {width: 1920, height: 1080}).maxBitrate).toBe(4_500_000);
	});

	it('never raises a selected rung to match a larger capture', () => {
		const encoding = getScreenShareEncoding('low_480p', 30);
		expect(capScreenShareEncodingToDimensions(encoding, {width: 3840, height: 2160})).toEqual(encoding);
	});

	it('leaves the selected rung alone without usable capture dimensions', () => {
		const encoding = getScreenShareEncoding('high', 60);
		expect(capScreenShareEncodingToDimensions(encoding, undefined)).toEqual(encoding);
		expect(capScreenShareEncodingToDimensions(encoding, {width: 0, height: 1080})).toEqual(encoding);
		expect(capScreenShareEncodingToDimensions(encoding, {width: 1920})).toEqual(encoding);
	});
});
