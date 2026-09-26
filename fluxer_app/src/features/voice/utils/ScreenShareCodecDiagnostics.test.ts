// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	computeInboundVideoDecodeHealth,
	findInboundVideoDecodeSample,
	findStalledVideoDecoder,
} from '@app/features/voice/utils/ScreenShareCodecDiagnostics';
import {
	markScreenShareDecodeFailure,
	resetVideoDecoderExclusions,
} from '@app/features/voice/utils/VideoDecoderCapabilities';
import {afterEach, describe, expect, it} from 'vitest';

function buildStats(inbound: Record<string, unknown>): RTCStatsReport {
	return new Map<string, unknown>([
		['codec-1', {type: 'codec', id: 'codec-1', mimeType: 'video/H264'}],
		['inbound-1', {type: 'inbound-rtp', id: 'inbound-1', kind: 'video', codecId: 'codec-1', ...inbound}],
	]) as unknown as RTCStatsReport;
}

function buildSample(inbound: Record<string, unknown>) {
	const sample = findInboundVideoDecodeSample(buildStats(inbound));
	if (!sample) throw new Error('expected an inbound video sample');
	return sample;
}

describe('the screen share decode stall detector', () => {
	it('calls a stream that never decoded a keyframe a stall', () => {
		const stall = findStalledVideoDecoder(
			buildSample({framesDecoded: 0, keyFramesDecoded: 0, framesReceived: 6, framesDropped: 6}),
		);
		expect(stall?.codec).toBe('h264');
	});

	it('refuses to call a stream that decoded and then froze a stall', () => {
		expect(
			findStalledVideoDecoder(
				buildSample({framesDecoded: 240, keyFramesDecoded: 2, framesReceived: 260, framesDropped: 20}),
			),
		).toBeNull();
	});

	it('refuses to call anything a stall when the stats report no keyframe counter', () => {
		expect(findStalledVideoDecoder(buildSample({framesDecoded: 0, framesReceived: 6, framesDropped: 6}))).toBeNull();
	});
});

describe('the baseline decode floor', () => {
	afterEach(() => {
		resetVideoDecoderExclusions();
	});

	it('never withdraws a codec every WebRTC endpoint is required to decode', () => {
		expect(markScreenShareDecodeFailure('h264', 'test')).toBe(false);
		expect(markScreenShareDecodeFailure('vp8', 'test')).toBe(false);
	});

	it('still withdraws an optional codec', () => {
		expect(markScreenShareDecodeFailure('av1', 'test')).toBe(true);
	});
});

describe('inbound video smoothness', () => {
	it('reports decoded frame rate and mean inter-frame delay in milliseconds', () => {
		const previous = buildSample({
			timestamp: 1000,
			framesDecoded: 100,
			totalInterFrameDelay: 2,
			totalSquaredInterFrameDelay: 0.04,
			freezeCount: 1,
		});
		const current = buildSample({
			timestamp: 3000,
			framesDecoded: 160,
			totalInterFrameDelay: 4,
			totalSquaredInterFrameDelay: 0.08,
			freezeCount: 3,
		});
		const health = computeInboundVideoDecodeHealth(previous, current);
		expect(health?.decodedFps).toBe(30);
		expect(health?.meanInterFrameDelayMs).toBe(33.3);
		expect(health?.freezes).toBe(2);
	});
});
