// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FluxerCodecAdvertisement} from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import type {HardwareEncodeReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const gpuReport: HardwareEncodeReport = {
	av1: 'hardware',
	h265: 'hardware',
	h264: 'hardware',
	vp9: 'software',
	vp8: 'software',
};

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {
		getScreenShareAv1OptIn: () => false,
		getScreenShareHevcOptIn: () => false,
		getPreferredScreenShareCodec: () => 'auto',
		getScreenShareEncoderMode: () => 'auto',
	},
}));

vi.mock('@app/features/devtools/utils/DesktopTroubleshootingUtils', () => ({
	getCachedDesktopTroubleshootingSettings: () => null,
}));

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	guessPlatform: () => 'windows',
	isChromiumBrowser: () => true,
	isDesktop: () => true,
	isFirefoxBrowser: () => false,
}));

vi.mock('@app/features/voice/utils/GpuEncoderCapabilities', () => ({
	getGpuEncoderReportSync: () => gpuReport,
	loadGpuEncoderReport: async () => gpuReport,
}));

vi.mock('@app/features/voice/utils/NativeHardwareEncoderCapabilities', () => ({
	getNativeHardwareEncoderCapabilitiesSync: () => null,
	hasNativeHardwareEncoder: () => false,
	resetNativeHardwareEncoderCapabilities: () => undefined,
	loadNativeHardwareEncoderCapabilities: async () => null,
}));

vi.mock('@app/features/voice/utils/OpenH264Status', () => ({
	getOpenH264StatusSync: () => null,
	resetOpenH264Status: () => undefined,
	loadOpenH264Status: async () => null,
}));

const VIDEO_CAPABILITIES = {
	codecs: [
		{mimeType: 'video/VP8'},
		{mimeType: 'video/VP9'},
		{mimeType: 'video/H264'},
		{mimeType: 'video/H265'},
		{mimeType: 'video/AV1'},
	],
};

Object.defineProperty(globalThis, 'RTCRtpSender', {
	configurable: true,
	writable: true,
	value: {getCapabilities: () => VIDEO_CAPABILITIES},
});

Object.defineProperty(globalThis, 'RTCRtpReceiver', {
	configurable: true,
	writable: true,
	value: {getCapabilities: () => VIDEO_CAPABILITIES},
});

const {
	default: ScreenShareCodecNegotiation,
	buildLocalCodecAdvertisements,
	computeNegotiatedVideoCodec,
} = await import('./ScreenShareCodecNegotiation');
const {findStalledVideoDecoder, scheduleScreenShareDecoderVerification} = await import(
	'@app/features/voice/utils/ScreenShareCodecDiagnostics'
);
const {getVideoDecoderExclusionsSync, markScreenShareDecodeFailure, resetVideoDecoderExclusions} = await import(
	'@app/features/voice/utils/VideoDecoderCapabilities'
);
const {resetCachedCodecCapabilities} = await import('@app/features/voice/utils/CodecCapabilityDetector');

function screenShareStats(mimeType: string, frames: {framesReceived: number; framesDecoded: number}): RTCStatsReport {
	const entries: Array<Record<string, unknown>> = [
		{id: 'codec-1', type: 'codec', mimeType},
		{
			id: 'inbound-1',
			type: 'inbound-rtp',
			kind: 'video',
			codecId: 'codec-1',
			packetsReceived: 4200,
			bytesReceived: 3_500_000,
			decoderImplementation: 'D3D11VideoDecoder',
			powerEfficientDecoder: true,
			...frames,
		},
	];
	return new Map(entries.map((entry) => [entry.id as string, entry])) as unknown as RTCStatsReport;
}

function stalledScreenShareStats(mimeType: string): RTCStatsReport {
	return screenShareStats(mimeType, {framesReceived: 180, framesDecoded: 0});
}

function statsSequence(...reports: Array<RTCStatsReport>): () => Promise<RTCStatsReport | undefined> {
	const queue = [...reports];
	return () => Promise.resolve(queue.shift());
}

function decodeAdvertisedFor(name: 'H264' | 'VP8' | 'VP9'): boolean | undefined {
	return buildLocalCodecAdvertisements().find((codec) => codec.name === name)?.decode;
}

function publisherThatEncodes(): Array<FluxerCodecAdvertisement> {
	return [
		{name: 'H264', type: 'video', payload_type: 102, priority: 1, encode: true, decode: true},
		{name: 'VP9', type: 'video', payload_type: 109, priority: 2, encode: true, decode: true},
	];
}

describe('a stalled H.264 screen share decode', () => {
	beforeEach(() => {
		resetVideoDecoderExclusions();
		resetCachedCodecCapabilities();
		ScreenShareCodecNegotiation.dispose();
	});

	it('is reported as an h264 decode stall by the stats diagnostic', () => {
		expect(findStalledVideoDecoder(stalledScreenShareStats('video/H264'))?.codec).toBe('h264');
	});

	it('withdraws the local H.264 decode advertisement so the publisher renegotiates away from it', () => {
		const stall = findStalledVideoDecoder(stalledScreenShareStats('video/H264'));
		if (!stall) throw new Error('expected the stats diagnostic to report a stalled decoder');
		expect(stall.codec).toBe('h264');
		expect(decodeAdvertisedFor('H264')).toBe(true);
		expect(
			computeNegotiatedVideoCodec(publisherThatEncodes(), [buildLocalCodecAdvertisements()], 0, ['h264', 'vp9']).codec,
		).toBe('h264');

		expect(markScreenShareDecodeFailure(stall.codec, 'screen-share-decode-stalled')).toBe(true);

		expect(decodeAdvertisedFor('H264')).toBe(false);
		expect(
			computeNegotiatedVideoCodec(publisherThatEncodes(), [buildLocalCodecAdvertisements()], 0, ['h264', 'vp9']).codec,
		).toBe('vp9');
	});

	it('never removes h264 or vp8 from the SDP-level subscriber exclusions', () => {
		markScreenShareDecodeFailure('h264', 'screen-share-decode-stalled');
		expect(getVideoDecoderExclusionsSync() ?? []).not.toContain('h264');
		expect(getVideoDecoderExclusionsSync() ?? []).not.toContain('vp8');
	});

	it('keeps a non-baseline runtime stall out of the SDP-level subscriber exclusions too', () => {
		expect(markScreenShareDecodeFailure('av1', 'screen-share-decode-stalled')).toBe(true);
		expect(getVideoDecoderExclusionsSync() ?? []).not.toContain('av1');
	});

	it('restores the H.264 decode advertisement when the room is torn down', () => {
		expect(markScreenShareDecodeFailure('h264', 'screen-share-decode-stalled')).toBe(true);
		expect(decodeAdvertisedFor('H264')).toBe(false);
		ScreenShareCodecNegotiation.dispose();
		expect(decodeAdvertisedFor('H264')).toBe(true);
	});

	it('stops after one codec change instead of cycling through the remaining codecs', () => {
		expect(markScreenShareDecodeFailure('h264', 'screen-share-decode-stalled')).toBe(true);
		expect(markScreenShareDecodeFailure('h264', 'screen-share-decode-stalled')).toBe(false);
		expect(markScreenShareDecodeFailure('vp8', 'screen-share-decode-stalled')).toBe(false);
		expect(decodeAdvertisedFor('VP8')).toBe(true);
	});
});

describe('confirming a screen share decode stall before a codec is withdrawn', () => {
	beforeEach(() => {
		vi.useFakeTimers();
		resetVideoDecoderExclusions();
		resetCachedCodecCapabilities();
		ScreenShareCodecNegotiation.dispose();
	});

	afterEach(() => {
		vi.useRealTimers();
	});

	it('withdraws nothing from a single stalled sample', async () => {
		const onDecodeFailure = vi.fn();
		scheduleScreenShareDecoderVerification(
			statsSequence(
				screenShareStats('video/H264', {framesReceived: 180, framesDecoded: 0}),
				screenShareStats('video/H264', {framesReceived: 240, framesDecoded: 0}),
			),
			undefined,
			onDecodeFailure,
		);
		await vi.advanceTimersByTimeAsync(5000);
		expect(onDecodeFailure).not.toHaveBeenCalled();
		expect(decodeAdvertisedFor('H264')).toBe(true);
	});

	it('withdraws nothing when packets arrive but no whole frames do', async () => {
		const onDecodeFailure = vi.fn();
		scheduleScreenShareDecoderVerification(
			statsSequence(
				screenShareStats('video/H264', {framesReceived: 0, framesDecoded: 0}),
				screenShareStats('video/H264', {framesReceived: 0, framesDecoded: 0}),
			),
			undefined,
			onDecodeFailure,
		);
		await vi.advanceTimersByTimeAsync(5000);
		expect(vi.getTimerCount()).toBe(0);
		await vi.advanceTimersByTimeAsync(5000);
		expect(onDecodeFailure).not.toHaveBeenCalled();
		expect(decodeAdvertisedFor('H264')).toBe(true);
	});

	it('withdraws the codec exactly once when a second sample confirms the stall', async () => {
		const withdrawn: Array<string> = [];
		scheduleScreenShareDecoderVerification(
			statsSequence(
				screenShareStats('video/H264', {framesReceived: 180, framesDecoded: 0}),
				screenShareStats('video/H264', {framesReceived: 240, framesDecoded: 0}),
			),
			undefined,
			(failure) => {
				if (markScreenShareDecodeFailure(failure.codec, 'screen-share-decode-stalled')) {
					withdrawn.push(failure.codec);
				}
			},
		);
		await vi.advanceTimersByTimeAsync(5000);
		expect(withdrawn).toEqual([]);
		expect(decodeAdvertisedFor('H264')).toBe(true);
		await vi.advanceTimersByTimeAsync(5000);
		expect(withdrawn).toEqual(['h264']);
		expect(decodeAdvertisedFor('H264')).toBe(false);
	});

	it('withdraws nothing when the second sample decoded a frame', async () => {
		const onDecodeFailure = vi.fn();
		scheduleScreenShareDecoderVerification(
			statsSequence(
				screenShareStats('video/H264', {framesReceived: 180, framesDecoded: 0}),
				screenShareStats('video/H264', {framesReceived: 240, framesDecoded: 12}),
			),
			undefined,
			onDecodeFailure,
		);
		await vi.advanceTimersByTimeAsync(10000);
		expect(onDecodeFailure).not.toHaveBeenCalled();
		expect(decodeAdvertisedFor('H264')).toBe(true);
	});

	it('withdraws nothing when no new frames arrived between the samples', async () => {
		const onDecodeFailure = vi.fn();
		scheduleScreenShareDecoderVerification(
			statsSequence(
				screenShareStats('video/H264', {framesReceived: 180, framesDecoded: 0}),
				screenShareStats('video/H264', {framesReceived: 180, framesDecoded: 0}),
			),
			undefined,
			onDecodeFailure,
		);
		await vi.advanceTimersByTimeAsync(10000);
		expect(onDecodeFailure).not.toHaveBeenCalled();
		expect(decodeAdvertisedFor('H264')).toBe(true);
	});

	it('withdraws nothing when the track goes away between the samples', async () => {
		const onDecodeFailure = vi.fn();
		const cancel = scheduleScreenShareDecoderVerification(
			statsSequence(
				screenShareStats('video/H264', {framesReceived: 180, framesDecoded: 0}),
				screenShareStats('video/H264', {framesReceived: 240, framesDecoded: 0}),
			),
			undefined,
			onDecodeFailure,
		);
		await vi.advanceTimersByTimeAsync(5000);
		expect(vi.getTimerCount()).toBe(1);
		cancel();
		expect(vi.getTimerCount()).toBe(0);
		await vi.advanceTimersByTimeAsync(5000);
		expect(onDecodeFailure).not.toHaveBeenCalled();
		expect(decodeAdvertisedFor('H264')).toBe(true);
	});
});
