// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import type {HardwareEncodeReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {afterEach, beforeEach, describe, expect, it, type MockInstance, vi} from 'vitest';

let av1OptIn = false;
let hevcOptIn = false;
let desktop = true;
let gpuReport: HardwareEncodeReport | null = null;
let codecCapabilityInfo: MockInstance<(...args: Array<unknown>) => void>;

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {
		getScreenShareAv1OptIn: () => av1OptIn,
		getScreenShareHevcOptIn: () => hevcOptIn,
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
	getOpenH264StatusSync: () => null,
	resetOpenH264Status: () => undefined,
}));

Object.defineProperty(globalThis, 'RTCRtpSender', {
	configurable: true,
	writable: true,
	value: {
		getCapabilities: () => ({
			codecs: [
				{mimeType: 'video/VP8'},
				{mimeType: 'video/VP9'},
				{mimeType: 'video/H264'},
				{mimeType: 'video/H265'},
				{mimeType: 'video/AV1'},
			],
		}),
	},
});

const {getCodecCapabilityReport, resetCachedCodecCapabilities, selectAutomaticScreenShareCodec} = await import(
	'./CodecCapabilityDetector'
);

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
