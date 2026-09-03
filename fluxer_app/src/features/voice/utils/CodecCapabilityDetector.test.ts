// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import type {HardwareEncodeReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {afterEach, beforeEach, describe, expect, it, type MockInstance, vi} from 'vitest';

let av1OptIn = false;
let hevcOptIn = false;
let desktop = true;
let gpuReport: HardwareEncodeReport | null = null;
let cameraPreference = 'auto';
let openH264Status: {enabled: boolean; downloaded: boolean} | null = null;
let codecCapabilityInfo: MockInstance<(...args: Array<unknown>) => void>;

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {
		getScreenShareAv1OptIn: () => av1OptIn,
		getScreenShareHevcOptIn: () => hevcOptIn,
		getPreferredVideoCodec: () => cameraPreference,
	},
}));

vi.mock('@app/features/devtools/utils/DesktopTroubleshootingUtils', () => ({
	getCachedDesktopTroubleshootingSettings: () => null,
}));

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	guessPlatform: () => 'windows',
	isChromiumBrowser: () => true,
	isDesktop: () => desktop,
	isFirefoxBrowser: () => false,
}));

vi.mock('@app/features/voice/utils/GpuEncoderCapabilities', () => ({
	getGpuEncoderReportSync: () => gpuReport,
}));

vi.mock('@app/features/voice/utils/NativeHardwareEncoderCapabilities', () => ({
	getNativeHardwareEncoderCapabilitiesSync: () => null,
	hasNativeHardwareEncoder: () => false,
	resetNativeHardwareEncoderCapabilities: () => undefined,
}));

vi.mock('@app/features/voice/utils/OpenH264Status', () => ({
	getOpenH264StatusSync: () => openH264Status,
	resetOpenH264Status: () => undefined,
}));

const ALL_SENDER_CODECS = ['video/VP8', 'video/VP9', 'video/H264', 'video/H265', 'video/AV1'];
let senderCodecs = ALL_SENDER_CODECS;

Object.defineProperty(globalThis, 'RTCRtpSender', {
	configurable: true,
	writable: true,
	value: {
		getCapabilities: () => ({codecs: senderCodecs.map((mimeType) => ({mimeType}))}),
	},
});

const {
	buildCameraPublishOptions,
	findVideoPublishCodecPolicyViolation,
	getAllowedVideoPublishCodecs,
	getCodecCapabilityReport,
	getRoomVideoPublishDefaults,
	isVideoCodecAllowedForPublish,
	markScreenShareCodecEncodeRuntimeFailure,
	resetCachedCodecCapabilities,
	resolveScreenShareEncoderVerificationAction,
	resolveVideoPublishCodecPolicy,
	selectAutomaticScreenShareCodec,
} = await import('./CodecCapabilityDetector');

const ALL_SOFTWARE: HardwareEncodeReport = {
	av1: 'software',
	h265: 'software',
	h264: 'software',
	vp9: 'software',
	vp8: 'software',
};

describe('screen-share AV1/HEVC opt-in gate', () => {
	afterEach(() => {
		try {
			expect(codecCapabilityInfo.mock.calls.length).toBeGreaterThan(0);
			for (const call of codecCapabilityInfo.mock.calls) {
				expect(call).toEqual([
					'Codec capabilities probed',
					{capabilities: {vp8: true, vp9: true, h264: true, h265: true, av1: true}},
				]);
			}
		} finally {
			codecCapabilityInfo.mockRestore();
		}
	});

	beforeEach(() => {
		codecCapabilityInfo = vi.spyOn(Logger.prototype, 'info').mockImplementation(() => undefined);
		av1OptIn = false;
		hevcOptIn = false;
		desktop = true;
		gpuReport = null;
		resetCachedCodecCapabilities();
	});

	it('reports AV1 as unsupported until the user opts in, even when the encoder is there', () => {
		expect(getCodecCapabilityReport().av1).toMatchObject({supported: false, reason: 'opt-in-required'});
		av1OptIn = true;
		resetCachedCodecCapabilities();
		expect(getCodecCapabilityReport().av1).toMatchObject({supported: true, reason: 'supported'});
	});

	it('reports HEVC as unsupported until the user opts in, even when the encoder is there', () => {
		expect(getCodecCapabilityReport().h265).toMatchObject({supported: false, reason: 'opt-in-required'});
		hevcOptIn = true;
		resetCachedCodecCapabilities();
		expect(getCodecCapabilityReport().h265).toMatchObject({supported: true, reason: 'supported'});
	});

	it('leaves the always-available codecs alone while AV1 and HEVC are gated', () => {
		const report = getCodecCapabilityReport();
		expect(report.vp9.supported).toBe(true);
		expect(report.h264.supported).toBe(true);
		expect(report.vp8.supported).toBe(true);
		expect(report.av1.supported).toBe(false);
		expect(report.h265.supported).toBe(false);
	});

	it('never selects AV1 for the forced-software path while the opt-in is off', () => {
		expect(selectAutomaticScreenShareCodec('software')).toEqual({codec: 'vp9', reason: 'software-vp9'});
		av1OptIn = true;
		resetCachedCodecCapabilities();
		expect(selectAutomaticScreenShareCodec('software')).toEqual({codec: 'av1', reason: 'software-av1'});
	});

	it('never selects AV1 for the desktop hardware path while the opt-in is off', () => {
		gpuReport = {...ALL_SOFTWARE, av1: 'hardware'};
		expect(selectAutomaticScreenShareCodec('auto').codec).not.toBe('av1');
		av1OptIn = true;
		resetCachedCodecCapabilities();
		expect(selectAutomaticScreenShareCodec('auto')).toEqual({codec: 'av1', reason: 'hardware-av1'});
	});

	it('never selects HEVC for the desktop hardware path while the opt-in is off', () => {
		gpuReport = {...ALL_SOFTWARE, h265: 'hardware'};
		expect(selectAutomaticScreenShareCodec('auto').codec).not.toBe('h265');
		hevcOptIn = true;
		resetCachedCodecCapabilities();
		expect(selectAutomaticScreenShareCodec('auto')).toEqual({codec: 'h265', reason: 'hardware-h265'});
	});

	it('never selects AV1 or HEVC for the software fallback path while the opt-ins are off', () => {
		desktop = false;
		gpuReport = ALL_SOFTWARE;
		const selection = selectAutomaticScreenShareCodec('auto').codec;
		expect(selection).not.toBe('av1');
		expect(selection).not.toBe('h265');
	});

	it('rebuilds the memoised report when either opt-in is flipped at runtime', () => {
		expect(getCodecCapabilityReport().av1.supported).toBe(false);
		expect(getCodecCapabilityReport().h265.supported).toBe(false);
		av1OptIn = true;
		expect(getCodecCapabilityReport().av1.supported).toBe(true);
		hevcOptIn = true;
		expect(getCodecCapabilityReport().h265.supported).toBe(true);
	});
});

describe('video publish codec policy', () => {
	beforeEach(() => {
		av1OptIn = false;
		hevcOptIn = false;
		desktop = true;
		gpuReport = null;
		cameraPreference = 'auto';
		openH264Status = null;
		senderCodecs = ALL_SENDER_CODECS;
		resetCachedCodecCapabilities();
	});

	it('keeps AV1 and HEVC out of the allowed set while the opt-ins are off', () => {
		expect(getAllowedVideoPublishCodecs()).toEqual(['vp8', 'h264', 'vp9']);
		expect(isVideoCodecAllowedForPublish('av1')).toBe(false);
		expect(isVideoCodecAllowedForPublish('h265')).toBe(false);
	});

	it('admits AV1 and HEVC only once their opt-ins are on', () => {
		av1OptIn = true;
		expect(getAllowedVideoPublishCodecs()).toEqual(['vp8', 'h264', 'vp9', 'av1']);
		hevcOptIn = true;
		expect(getAllowedVideoPublishCodecs()).toEqual(['vp8', 'h264', 'vp9', 'av1', 'h265']);
		expect(resolveVideoPublishCodecPolicy('av1').primary).toBe('av1');
		expect(resolveVideoPublishCodecPolicy('h265').primary).toBe('h265');
	});

	it('substitutes an opted-out request with the first compatible codec and never AV1 or HEVC', () => {
		expect(resolveVideoPublishCodecPolicy('av1')).toMatchObject({primary: 'h264', backupCodec: false});
		expect(resolveVideoPublishCodecPolicy('h265')).toMatchObject({primary: 'h264', backupCodec: false});
		expect(resolveVideoPublishCodecPolicy('vp9')).toMatchObject({primary: 'vp9', backupCodec: {codec: 'h264'}});
		expect(resolveVideoPublishCodecPolicy('h264')).toMatchObject({primary: 'h264', backupCodec: false});
	});

	it('drops H.264 from the allowed set when the sender cannot encode it even though OpenH264 claims it is ready', () => {
		senderCodecs = ALL_SENDER_CODECS.filter((mimeType) => mimeType !== 'video/H264');
		openH264Status = {enabled: true, downloaded: true};
		expect(getCodecCapabilityReport().h264.supported).toBe(true);
		expect(isVideoCodecAllowedForPublish('h264')).toBe(false);
		expect(resolveVideoPublishCodecPolicy('h264')).toMatchObject({primary: 'vp9', backupCodec: false});
		expect(getRoomVideoPublishDefaults()).toEqual({videoCodec: 'vp9', backupCodec: false});
	});

	it('drops a codec from the allowed set once it failed to encode at runtime', () => {
		expect(markScreenShareCodecEncodeRuntimeFailure('vp9', 'test')).toBe(true);
		expect(getAllowedVideoPublishCodecs()).toEqual(['vp8', 'h264']);
		expect(resolveVideoPublishCodecPolicy('vp9').primary).toBe('h264');
	});

	it('hands the camera an explicit allowed codec for every preference while the opt-ins are off', () => {
		expect(buildCameraPublishOptions()).toEqual({
			videoCodec: 'vp9',
			backupCodec: {codec: 'h264'},
			backupCodecPolicy: 1,
		});
		for (const preference of ['auto', 'vp8', 'vp9', 'h264', 'av1', 'h265']) {
			cameraPreference = preference;
			const options = buildCameraPublishOptions();
			expect(options.videoCodec).toBeDefined();
			expect(getAllowedVideoPublishCodecs()).toContain(options.videoCodec);
			expect(options.videoCodec).not.toBe('av1');
			expect(options.videoCodec).not.toBe('h265');
		}
		cameraPreference = 'av1';
		expect(buildCameraPublishOptions().videoCodec).toBe('h264');
		av1OptIn = true;
		expect(buildCameraPublishOptions().videoCodec).toBe('av1');
	});

	it('derives the room publish defaults from the same policy', () => {
		expect(getRoomVideoPublishDefaults()).toEqual({videoCodec: 'vp9', backupCodec: {codec: 'h264'}});
		cameraPreference = 'av1';
		expect(getRoomVideoPublishDefaults().videoCodec).toBe('h264');
	});

	it('flags a negotiated codec outside the policy with a bounded alternative', () => {
		expect(findVideoPublishCodecPolicyViolation('h264', 'av1')).toEqual({
			requested: 'h264',
			negotiated: 'av1',
			alternative: 'vp9',
		});
		expect(findVideoPublishCodecPolicyViolation('h264', 'h264')).toBeNull();
		expect(findVideoPublishCodecPolicyViolation('h264', 'vp9')).toBeNull();
		expect(findVideoPublishCodecPolicyViolation('h264', undefined)).toBeNull();
	});

	it('no longer blacklists the requested codec when the publisher negotiated a different one', () => {
		expect(
			resolveScreenShareEncoderVerificationAction({reason: 'codec-mismatch', codec: 'h264', activeCodecs: ['av1']}),
		).toEqual({kind: 'correct-negotiated', requested: 'h264', negotiated: ['av1'], alternative: 'vp9'});
		expect(isVideoCodecAllowedForPublish('h264')).toBe(true);
		expect(getCodecCapabilityReport().h264.supported).toBe(true);
		expect(
			resolveScreenShareEncoderVerificationAction({reason: 'codec-mismatch', codec: 'h264', activeCodecs: ['vp9']}),
		).toEqual({kind: 'accept-negotiated', requested: 'h264', negotiated: ['vp9']});
		expect(isVideoCodecAllowedForPublish('h264')).toBe(true);
	});

	it('still blacklists a codec whose encoder stalled, and only warns about it once', () => {
		expect(resolveScreenShareEncoderVerificationAction({reason: 'stalled', codec: 'h264'})).toEqual({
			kind: 'recover-stalled',
			codec: 'h264',
		});
		expect(isVideoCodecAllowedForPublish('h264')).toBe(false);
		expect(getCodecCapabilityReport().h264).toMatchObject({supported: false, reason: 'runtime-failed'});
		expect(resolveScreenShareEncoderVerificationAction({reason: 'stalled', codec: 'h264'})).toEqual({
			kind: 'ignore-repeated-stall',
			codec: 'h264',
		});
	});

	it('keeps recovering from a vp8 stall because vp8 is never blacklisted', () => {
		for (let attempt = 0; attempt < 2; attempt++) {
			expect(resolveScreenShareEncoderVerificationAction({reason: 'stalled', codec: 'vp8'})).toEqual({
				kind: 'recover-stalled',
				codec: 'vp8',
			});
			expect(isVideoCodecAllowedForPublish('vp8')).toBe(true);
		}
	});
});
