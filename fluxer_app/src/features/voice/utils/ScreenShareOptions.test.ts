// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	buildScreenShareOptions,
	getScreenShareEncoding,
	resolveStreamingModeSettings,
	SCREEN_SHARE_FORCED_VIDEO_BITRATE_BPS,
	STREAMING_MODE_PRESETS,
} from './ScreenShareOptions';

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
	it('prefers framerate for detail-oriented shares', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'medium',
			frameRate: 30,
			includeAudio: true,
		});
		expect(publishOptions.degradationPreference).toBe('maintain-framerate');
	});
	it('prefers framerate for non-gaming high-framerate shares', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'ultra',
			frameRate: 60,
			includeAudio: true,
			streamingMode: 'screenshare',
		});
		expect(publishOptions.degradationPreference).toBe('maintain-framerate');
	});
	it('prefers framerate degradation for gaming streams', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'ultra',
			frameRate: 60,
			includeAudio: true,
			streamingMode: 'gaming',
		});
		expect(publishOptions.degradationPreference).toBe('maintain-framerate');
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
	it('publishes the forced bitrate for the largest preset', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'source',
			frameRate: 60,
			includeAudio: false,
		});
		expect(publishOptions.screenShareEncoding?.maxBitrate).toBe(SCREEN_SHARE_FORCED_VIDEO_BITRATE_BPS);
	});
	it('publishes the same forced bitrate for a smaller preset', () => {
		const {publishOptions} = buildScreenShareOptions({
			resolution: 'ultra',
			frameRate: 60,
			includeAudio: false,
		});
		expect(publishOptions.screenShareEncoding?.maxBitrate).toBe(SCREEN_SHARE_FORCED_VIDEO_BITRATE_BPS);
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

describe('forced screen share bitrate', () => {
	it('forces the screenshare preset to the fixed bitrate', () => {
		const {frameRate} = STREAMING_MODE_PRESETS.screenshare;
		expect(getScreenShareEncoding(frameRate).maxBitrate).toBe(SCREEN_SHARE_FORCED_VIDEO_BITRATE_BPS);
	});

	it('forces the gaming preset to the fixed bitrate', () => {
		const {frameRate} = STREAMING_MODE_PRESETS.gaming;
		expect(getScreenShareEncoding(frameRate).maxBitrate).toBe(SCREEN_SHARE_FORCED_VIDEO_BITRATE_BPS);
	});

	it('never exceeds the fixed bitrate even when a higher ceiling is supplied', () => {
		expect(getScreenShareEncoding(60, 50000000).maxBitrate).toBe(SCREEN_SHARE_FORCED_VIDEO_BITRATE_BPS);
	});

	it('still honours a lower ceiling so adaptive step-down keeps working', () => {
		expect(getScreenShareEncoding(60, 2000000).maxBitrate).toBe(2000000);
	});

	it('does not vary with frame rate', () => {
		expect(getScreenShareEncoding(15).maxBitrate).toBe(getScreenShareEncoding(120).maxBitrate);
	});
});
