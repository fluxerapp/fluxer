// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	confirmDecodeStall,
	findSoftwareVideoDecoder,
	findStalledVideoDecoder,
	isSoftwareVideoImplementation,
	type StalledVideoDecoderInfo,
} from './ScreenShareCodecDiagnostics';

function createStatsReport(entries: Array<Record<string, unknown>>): RTCStatsReport {
	return new Map(entries.map((entry) => [entry.id as string, entry])) as unknown as RTCStatsReport;
}

function createStallInfo(overrides: Partial<StalledVideoDecoderInfo> = {}): StalledVideoDecoderInfo {
	return {
		codec: 'h264',
		mimeType: 'video/H264',
		packetsReceived: 400,
		bytesReceived: 250_000,
		framesDecoded: 0,
		framesReceived: 20,
		...overrides,
	};
}

describe('isSoftwareVideoImplementation', () => {
	it('detects common software encoder and decoder implementations', () => {
		expect(isSoftwareVideoImplementation('libvpx')).toBe(true);
		expect(isSoftwareVideoImplementation('FFmpegVideoDecoder')).toBe(true);
		expect(isSoftwareVideoImplementation('Dav1dVideoDecoder')).toBe(true);
		expect(isSoftwareVideoImplementation('D3D11VideoDecoder')).toBe(false);
	});
});

describe('findSoftwareVideoDecoder', () => {
	it('finds a software decoder from the decoder implementation', () => {
		const stats = createStatsReport([
			{id: 'codec-1', type: 'codec', mimeType: 'video/AV1'},
			{
				id: 'inbound-1',
				type: 'inbound-rtp',
				kind: 'video',
				codecId: 'codec-1',
				decoderImplementation: 'Dav1dVideoDecoder',
				powerEfficientDecoder: false,
			},
		]);
		expect(findSoftwareVideoDecoder(stats)).toEqual({
			codec: 'AV1',
			implementation: 'Dav1dVideoDecoder',
			powerEfficientDecoder: false,
		});
	});
	it('finds a software decoder from power efficiency when implementation is hidden', () => {
		const stats = createStatsReport([
			{id: 'codec-1', type: 'codec', mimeType: 'video/H264'},
			{
				id: 'inbound-1',
				type: 'inbound-rtp',
				codecId: 'codec-1',
				powerEfficientDecoder: false,
			},
		]);
		expect(findSoftwareVideoDecoder(stats)).toEqual({
			codec: 'H264',
			implementation: 'software decoder',
			powerEfficientDecoder: false,
		});
	});
	it('does not flag a named hardware decoder only because power efficiency is false', () => {
		const stats = createStatsReport([
			{id: 'codec-1', type: 'codec', mimeType: 'video/H264'},
			{
				id: 'inbound-1',
				type: 'inbound-rtp',
				kind: 'video',
				codecId: 'codec-1',
				decoderImplementation: 'VideoToolboxVideoDecoder',
				powerEfficientDecoder: false,
			},
		]);
		expect(findSoftwareVideoDecoder(stats)).toBeNull();
	});
	it('ignores hardware and non-video inbound stats', () => {
		const stats = createStatsReport([
			{id: 'codec-1', type: 'codec', mimeType: 'video/H264'},
			{id: 'codec-2', type: 'codec', mimeType: 'audio/opus'},
			{
				id: 'inbound-1',
				type: 'inbound-rtp',
				kind: 'video',
				codecId: 'codec-1',
				decoderImplementation: 'D3D11VideoDecoder',
				powerEfficientDecoder: true,
			},
			{
				id: 'inbound-2',
				type: 'inbound-rtp',
				kind: 'audio',
				codecId: 'codec-2',
				decoderImplementation: 'FFmpegAudioDecoder',
				powerEfficientDecoder: false,
			},
		]);
		expect(findSoftwareVideoDecoder(stats)).toBeNull();
	});
});

describe('findStalledVideoDecoder', () => {
	it('detects received video packets that never decode into frames', () => {
		const stats = createStatsReport([
			{id: 'codec-1', type: 'codec', mimeType: 'video/VP9'},
			{
				id: 'inbound-1',
				type: 'inbound-rtp',
				kind: 'video',
				codecId: 'codec-1',
				packetsReceived: 42,
				bytesReceived: 32000,
				framesDecoded: 0,
				framesReceived: 8,
			},
		]);
		expect(findStalledVideoDecoder(stats)).toMatchObject({
			codec: 'vp9',
			mimeType: 'video/VP9',
			packetsReceived: 42,
			bytesReceived: 32000,
			framesDecoded: 0,
			framesReceived: 8,
		});
	});
	it('does not treat an idle track as a decoder stall', () => {
		const stats = createStatsReport([
			{id: 'codec-1', type: 'codec', mimeType: 'video/AV1'},
			{
				id: 'inbound-1',
				type: 'inbound-rtp',
				kind: 'video',
				codecId: 'codec-1',
				packetsReceived: 0,
				bytesReceived: 0,
				framesDecoded: 0,
			},
		]);
		expect(findStalledVideoDecoder(stats)).toBeNull();
	});
	it('does not treat a first keyframe that is still arriving as a decoder stall', () => {
		const stats = createStatsReport([
			{id: 'codec-1', type: 'codec', mimeType: 'video/H264'},
			{
				id: 'inbound-1',
				type: 'inbound-rtp',
				kind: 'video',
				codecId: 'codec-1',
				packetsReceived: 40,
				bytesReceived: 60_000,
				framesReceived: 0,
				framesDecoded: 0,
			},
		]);
		expect(findStalledVideoDecoder(stats)).toBeNull();
	});
	it('does not treat packets that carry no video frames as a decoder stall', () => {
		const stats = createStatsReport([
			{id: 'codec-1', type: 'codec', mimeType: 'video/H264'},
			{
				id: 'inbound-1',
				type: 'inbound-rtp',
				kind: 'video',
				codecId: 'codec-1',
				packetsReceived: 50,
				bytesReceived: 0,
				framesReceived: 0,
				framesDecoded: 0,
			},
		]);
		expect(findStalledVideoDecoder(stats)).toBeNull();
	});
	it('treats whole frames that never decode as a decoder stall', () => {
		const stats = createStatsReport([
			{id: 'codec-1', type: 'codec', mimeType: 'video/H264'},
			{
				id: 'inbound-1',
				type: 'inbound-rtp',
				kind: 'video',
				codecId: 'codec-1',
				packetsReceived: 40,
				bytesReceived: 60_000,
				framesReceived: 40,
				framesDecoded: 0,
			},
		]);
		expect(findStalledVideoDecoder(stats)).toMatchObject({codec: 'h264', framesReceived: 40, framesDecoded: 0});
	});
});

describe('confirmDecodeStall', () => {
	it('confirms a stall when more frames arrived and none of them decoded', () => {
		const second = createStallInfo({framesReceived: 45});
		expect(confirmDecodeStall(createStallInfo({framesReceived: 20}), second)).toBe(second);
	});
	it('does not confirm a stall when the second sample decoded a frame', () => {
		expect(confirmDecodeStall(createStallInfo(), createStallInfo({framesReceived: 45, framesDecoded: 3}))).toBeNull();
	});
	it('does not confirm a stall when the second sample is missing', () => {
		expect(confirmDecodeStall(createStallInfo(), null)).toBeNull();
	});
	it('does not confirm a stall when no new frames arrived between the samples', () => {
		expect(confirmDecodeStall(createStallInfo({framesReceived: 20}), createStallInfo({framesReceived: 20}))).toBeNull();
	});
	it('does not confirm a stall across two different codecs', () => {
		expect(
			confirmDecodeStall(
				createStallInfo({framesReceived: 20}),
				createStallInfo({codec: 'vp9', mimeType: 'video/VP9', framesReceived: 45}),
			),
		).toBeNull();
	});
});
