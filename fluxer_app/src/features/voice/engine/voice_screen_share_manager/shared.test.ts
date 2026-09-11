// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	findStalledVideoEncoder,
	scheduleScreenShareEncoderVerification,
	shouldTriggerSoftwareEncoderWarning,
} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import SoftwareEncoderWarning from '@app/features/voice/state/SoftwareEncoderWarning';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@app/features/voice/utils/VideoQualityEntitlement', () => ({hasHigherVideoQuality: () => false}));

let h264Acceleration = 'hardware';
const markSoftwareEncodeObserved = vi.fn();

vi.mock('@app/features/voice/utils/CodecCapabilityDetector', () => ({
	adjustScreenShareEncodingForCodec: (encoding: unknown) => encoding,
	getCodecCapabilityReport: () => ({
		vp8: {hardwareAccelerated: 'software'},
		vp9: {hardwareAccelerated: 'software'},
		h264: {hardwareAccelerated: h264Acceleration},
		h265: {hardwareAccelerated: 'software'},
		av1: {hardwareAccelerated: 'software'},
	}),
	markScreenShareCodecSoftwareEncodeObserved: (codec: string) => markSoftwareEncodeObserved(codec),
	resolveVideoPublishCodecPolicy: (requested: string) => ({
		allowed: [requested],
		requested,
		primary: requested,
		backupCodec: false,
	}),
}));

function makeStats(entries: ReadonlyArray<Record<string, unknown>>): RTCStatsReport {
	return new Map(entries.map((entry) => [entry.id as string, entry])) as unknown as RTCStatsReport;
}

const CODEC_REPORT = {type: 'codec', id: 'codec-h264', mimeType: 'video/H264'};
const SOURCE_REPORT = {type: 'media-source', id: 'source-1', kind: 'video', frames: 240, framesPerSecond: 30};

function outboundReport(overrides: Record<string, unknown>): Record<string, unknown> {
	return {
		type: 'outbound-rtp',
		id: 'outbound-1',
		kind: 'video',
		codecId: 'codec-h264',
		mediaSourceId: 'source-1',
		framesEncoded: 0,
		framesSent: 0,
		...overrides,
	};
}

describe('findStalledVideoEncoder', () => {
	test('reports a stall when the encoding is active and the source is producing frames', () => {
		const stats = makeStats([CODEC_REPORT, SOURCE_REPORT, outboundReport({active: true})]);
		expect(findStalledVideoEncoder(stats, 'h264')).toEqual({
			codec: 'h264',
			framesEncoded: 0,
			framesSent: 0,
			sourceFrames: 240,
			sourceFramesPerSecond: 30,
		});
	});

	test('ignores an encoding the SFU deactivated even though the source is producing frames', () => {
		const stats = makeStats([CODEC_REPORT, SOURCE_REPORT, outboundReport({active: false})]);
		expect(findStalledVideoEncoder(stats, 'h264')).toBeNull();
	});

	test('still reports a stall when active is absent, as on Firefox', () => {
		const stats = makeStats([CODEC_REPORT, SOURCE_REPORT, outboundReport({})]);
		expect(findStalledVideoEncoder(stats, 'h264')?.codec).toBe('h264');
	});

	test('skips a deactivated layer and reports the still-active sibling instead', () => {
		const stats = makeStats([
			CODEC_REPORT,
			SOURCE_REPORT,
			{type: 'media-source', id: 'source-low', kind: 'video', frames: 90, framesPerSecond: 15},
			outboundReport({id: 'outbound-low', mediaSourceId: 'source-low', active: false}),
			outboundReport({id: 'outbound-high', active: true}),
		]);
		expect(findStalledVideoEncoder(stats, 'h264')?.sourceFrames).toBe(240);
	});
});

describe('the screen share encoder verification', () => {
	function openH264Stats(): RTCStatsReport {
		return makeStats([
			CODEC_REPORT,
			SOURCE_REPORT,
			outboundReport({
				active: true,
				framesEncoded: 120,
				framesSent: 120,
				encoderImplementation: 'OpenH264',
				powerEfficientEncoder: false,
			}),
		]);
	}

	async function verifyOpenH264Share(): Promise<void> {
		scheduleScreenShareEncoderVerification(async () => openH264Stats(), 'h264');
		await vi.advanceTimersByTimeAsync(2500);
	}

	beforeEach(() => {
		vi.useFakeTimers();
		h264Acceleration = 'hardware';
		markSoftwareEncodeObserved.mockClear();
		SoftwareEncoderWarning.reset();
	});

	afterEach(() => {
		vi.useRealTimers();
		vi.restoreAllMocks();
		SoftwareEncoderWarning.reset();
	});

	test('records a software session verdict when the encoder turns out to be OpenH264', async () => {
		await verifyOpenH264Share();
		expect(markSoftwareEncodeObserved).toHaveBeenCalledWith('h264');
	});

	test('warns about the software encoder while the capability layer still claims hardware', async () => {
		await verifyOpenH264Share();
		expect(SoftwareEncoderWarning.showWarning).toBe(true);
	});

	test('does not warn once the session verdict says the codec encodes in software', async () => {
		h264Acceleration = 'software';
		await verifyOpenH264Share();
		expect(SoftwareEncoderWarning.showWarning).toBe(false);
		expect(markSoftwareEncodeObserved).toHaveBeenCalledWith('h264');
	});

	test('records no verdict when the user asked for the software encoder', async () => {
		vi.spyOn(VoiceSettings, 'getScreenShareEncoderMode').mockReturnValue('software');
		await verifyOpenH264Share();
		expect(markSoftwareEncodeObserved).not.toHaveBeenCalled();
	});
});

describe('the software encoder warning gate', () => {
	test('stays quiet once the session verdict says the codec encodes in software', () => {
		h264Acceleration = 'software';
		expect(shouldTriggerSoftwareEncoderWarning('h264')).toBe(false);
	});

	test('still speaks up while the capability layer claims hardware', () => {
		h264Acceleration = 'hardware';
		expect(shouldTriggerSoftwareEncoderWarning('h264')).toBe(true);
	});
});
