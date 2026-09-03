// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {screenCaptureToDisplayMediaStreamOptions} from './track/utils.ts';
import {selectPreferredVideoCodec, supportsAV1, supportsH265, supportsVideoCodec, supportsVP9} from './utils.ts';

const originalNavigator = globalThis.navigator;
const originalSender = globalThis.RTCRtpSender;

function setUserAgent(userAgent: string): void {
	Object.defineProperty(globalThis, 'navigator', {
		configurable: true,
		value: {userAgent},
	});
}

function setSenderCapabilities(mimeTypes: ReadonlyArray<string>): void {
	Object.defineProperty(globalThis, 'RTCRtpSender', {
		configurable: true,
		writable: true,
		value: {
			getCapabilities: () => ({
				codecs: mimeTypes.map((mimeType) => ({mimeType})),
			}),
		},
	});
}

describe('video codec capability helpers', () => {
	beforeEach(() => {
		setUserAgent('Mozilla/5.0 AppleWebKit/537.36 Chrome/136.0.0.0 Safari/537.36');
		setSenderCapabilities(['video/VP8', 'video/H264']);
	});

	afterEach(() => {
		Object.defineProperty(globalThis, 'navigator', {
			configurable: true,
			value: originalNavigator,
		});
		Object.defineProperty(globalThis, 'RTCRtpSender', {
			configurable: true,
			writable: true,
			value: originalSender,
		});
	});

	it('keeps the opt-in codecs last even when sender capabilities offer them', () => {
		setSenderCapabilities(['video/VP8', 'video/H264', 'video/VP9', 'video/H265', 'video/AV1']);
		expect(supportsAV1()).toBe(true);
		expect(supportsH265()).toBe(true);
		expect(supportsVP9()).toBe(true);
		expect(selectPreferredVideoCodec()).toBe('h264');
	});

	it('falls back to VP9 then VP8 before any opt-in codec', () => {
		setSenderCapabilities(['video/VP8', 'video/VP9', 'video/H265', 'video/AV1']);
		expect(selectPreferredVideoCodec()).toBe('vp9');
		setSenderCapabilities(['video/VP8', 'video/H265', 'video/AV1']);
		expect(selectPreferredVideoCodec()).toBe('vp8');
	});

	it('treats legacy AV1X as AV1 and falls through to HEVC when AV1 is absent', () => {
		setSenderCapabilities(['video/VP8', 'video/H264', 'video/H265', 'video/AV1X']);
		expect(supportsVideoCodec('av1')).toBe(true);
		expect(selectPreferredVideoCodec(['h265', 'h264', 'vp9', 'vp8'])).toBe('h265');
	});

	it('does not get stuck on the AV1 default when Chromium only exposes H.264 and VP8', () => {
		setSenderCapabilities(['video/VP8', 'video/H264']);
		expect(supportsVideoCodec('av1')).toBe(false);
		expect(selectPreferredVideoCodec()).toBe('h264');
	});

	it('blocks HEVC on Firefox even if a test shim advertises it', () => {
		setUserAgent('Mozilla/5.0 Firefox/136.0');
		setSenderCapabilities(['video/VP8', 'video/H264', 'video/H265']);
		expect(supportsH265()).toBe(false);
		expect(selectPreferredVideoCodec()).toBe('h264');
	});
});

describe('screenCaptureToDisplayMediaStreamOptions', () => {
	it('translates own-audio restriction into display audio constraints', () => {
		const options = screenCaptureToDisplayMediaStreamOptions({
			audio: true,
			restrictOwnAudio: true,
			systemAudio: 'exclude',
		}) as DisplayMediaStreamOptions & {systemAudio?: string};
		expect(options.audio).toMatchObject({restrictOwnAudio: true});
		expect(options.systemAudio).toBe('exclude');
	});
});
