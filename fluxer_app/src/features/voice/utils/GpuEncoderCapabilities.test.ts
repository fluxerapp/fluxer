// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HardwareEncodeReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {beforeEach, describe, expect, it, vi} from 'vitest';

let streamingMode = 'custom';
let screenshareResolution = 'high';
let videoFrameRate = 60;
let higherVideoQuality = true;
let shareSettingsUnavailable = false;

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {
		getStreamingMode: () => {
			if (shareSettingsUnavailable) throw new ReferenceError('Cannot access VoiceSettings before initialization');
			return streamingMode;
		},
		getScreenshareResolution: () => screenshareResolution,
		getVideoFrameRate: () => videoFrameRate,
	},
}));

vi.mock('@app/features/voice/utils/VideoQualityEntitlement', () => ({
	hasHigherVideoQuality: () => higherVideoQuality,
}));

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	getElectronAPI: () => null,
	getNativePlatformSync: () => 'windows',
	isDesktop: () => false,
}));

interface EncodingInfoConfig {
	type: string;
	video: {contentType: string; width: number; height: number; bitrate: number; framerate: number};
}

const encodingInfoConfigs: Array<EncodingInfoConfig> = [];
let encodingInfoAnswer: () => {supported?: boolean; powerEfficient?: boolean} = () => ({
	supported: true,
	powerEfficient: false,
});

Object.defineProperty(globalThis, 'navigator', {
	configurable: true,
	writable: true,
	value: {
		mediaCapabilities: {
			encodingInfo: async (config: EncodingInfoConfig) => {
				encodingInfoConfigs.push(config);
				return encodingInfoAnswer();
			},
		},
	},
});

const {
	AMD_RDNA3_PLUS,
	NEGOTIABLE_H264_PROBE_CONTENT_TYPE,
	NVIDIA_PRE_ADA,
	PCI_VENDOR_AMD,
	PCI_VENDOR_APPLE,
	PCI_VENDOR_NVIDIA,
	probeWebRtcEncodeEfficiency,
	reconcileHardwareEncodeReport,
} = await import('./GpuEncoderCapabilities');

const AMD_RDNA3_REPORT: HardwareEncodeReport = {...AMD_RDNA3_PLUS.caps, gpuFamily: AMD_RDNA3_PLUS.family};
const NVIDIA_REPORT: HardwareEncodeReport = {...NVIDIA_PRE_ADA.caps, gpuFamily: NVIDIA_PRE_ADA.family};
const PROBE_SAYS_SOFTWARE = {
	av1: 'software',
	h265: 'software',
	h264: 'software',
	vp9: 'software',
	vp8: 'software',
} as const;

function h264Probes(): Array<EncodingInfoConfig> {
	return encodingInfoConfigs.filter((config) => config.video.contentType.startsWith('video/H264'));
}

describe('the WebRTC encode probe', () => {
	beforeEach(() => {
		encodingInfoConfigs.length = 0;
		streamingMode = 'custom';
		screenshareResolution = 'high';
		videoFrameRate = 60;
		higherVideoQuality = true;
		shareSettingsUnavailable = false;
		encodingInfoAnswer = () => ({supported: true, powerEfficient: false});
	});

	it('asks about the single H.264 format the SFU can negotiate', async () => {
		await probeWebRtcEncodeEfficiency();
		expect(h264Probes().map((config) => config.video.contentType)).toEqual([
			'video/H264;level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f',
		]);
		expect(NEGOTIABLE_H264_PROBE_CONTENT_TYPE).toBe(
			'video/H264;level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f',
		);
	});

	it('probes at the configured share size and frame rate', async () => {
		await probeWebRtcEncodeEfficiency();
		expect(h264Probes()[0]?.video).toMatchObject({width: 1920, height: 1080, framerate: 60, bitrate: 6_000_000});
	});

	it('follows the share settings down when the user picks a smaller share', async () => {
		screenshareResolution = 'medium';
		videoFrameRate = 30;
		await probeWebRtcEncodeEfficiency();
		expect(h264Probes()[0]?.video).toMatchObject({width: 1280, height: 720, framerate: 30, bitrate: 3_000_000});
	});

	it('answers software for H.264 when the negotiated format is not power efficient', async () => {
		await expect(probeWebRtcEncodeEfficiency()).resolves.toMatchObject({h264: 'software'});
	});

	it('answers hardware for H.264 when the negotiated format is power efficient', async () => {
		encodingInfoAnswer = () => ({supported: true, powerEfficient: true});
		await expect(probeWebRtcEncodeEfficiency()).resolves.toMatchObject({h264: 'hardware'});
	});

	it('still answers at the largest share size when the share settings cannot be read', async () => {
		shareSettingsUnavailable = true;
		await expect(probeWebRtcEncodeEfficiency()).resolves.toMatchObject({h264: 'software'});
		expect(h264Probes()[0]?.video).toMatchObject({width: 1920, height: 1080, framerate: 60, bitrate: 6_000_000});
	});
});

describe('reconciling the GPU table against the probe', () => {
	it('demotes H.264 to software on Windows when the negotiated format is not power efficient', () => {
		const report = reconcileHardwareEncodeReport(AMD_RDNA3_REPORT, PROBE_SAYS_SOFTWARE, 'windows', PCI_VENDOR_AMD);
		expect(report.h264).toBe('software');
	});

	it('demotes H.264 to software on macOS when the negotiated format is not power efficient', () => {
		const report = reconcileHardwareEncodeReport(AMD_RDNA3_REPORT, PROBE_SAYS_SOFTWARE, 'macos', PCI_VENDOR_APPLE);
		expect(report.h264).toBe('software');
	});

	it('demotes H.264 to software when the probe could not answer at all', () => {
		expect(reconcileHardwareEncodeReport(AMD_RDNA3_REPORT, null, 'windows', PCI_VENDOR_AMD).h264).toBe('software');
	});

	it('keeps hardware H.264 on Windows when the probe reports a power efficient encoder', () => {
		const report = reconcileHardwareEncodeReport(
			AMD_RDNA3_REPORT,
			{...PROBE_SAYS_SOFTWARE, h264: 'hardware'},
			'windows',
			PCI_VENDOR_AMD,
		);
		expect(report.h264).toBe('hardware');
	});

	it('keeps hardware H.264 on Linux when the probe reports a power efficient encoder', () => {
		const report = reconcileHardwareEncodeReport(
			NVIDIA_REPORT,
			{...PROBE_SAYS_SOFTWARE, h264: 'hardware'},
			'linux',
			PCI_VENDOR_NVIDIA,
		);
		expect(report.h264).toBe('hardware');
	});

	it('leaves the other codecs on the table verdict outside Linux NVIDIA', () => {
		const report = reconcileHardwareEncodeReport(AMD_RDNA3_REPORT, PROBE_SAYS_SOFTWARE, 'windows', PCI_VENDOR_AMD);
		expect(report.av1).toBe('hardware');
		expect(report.h265).toBe('hardware');
	});
});
