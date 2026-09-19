// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScreenShareTarget} from '@app/features/voice/utils/ScreenShareOptions';
import type {GpuInfo} from '@app/types/electron.d';
import {type I18n, type MessageDescriptor, setupI18n} from '@lingui/core';
import type {VideoCodec} from 'livekit-client';
import {afterEach, describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: MessageDescriptor) => descriptor}));

vi.mock('@app/features/app/config/Config', () => ({
	default: {
		PUBLIC_BUILD_VERSION: 'test',
		PUBLIC_RELEASE_CHANNEL: 'canary',
		PUBLIC_BOOTSTRAP_API_ENDPOINT: 'https://example.invalid',
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: 'https://example.invalid',
	},
}));

const {
	formatScreenShareResolutionLabel,
	formatScreenShareTargetLabel,
	SCREEN_SHARE_STATUS_SOURCE_RESOLUTION_DESCRIPTOR,
} = await import('@app/features/voice/utils/VoiceMessageDescriptors');

const {
	ENCODE_PROBE_VIDEO_CONFIG,
	H264_ENCODE_PROBE_CONTENT_TYPES,
	H264_PROBE_PROFILE_LEVEL_IDS,
	WEBRTC_ENCODE_PROBE_CONTENT_TYPES,
	probeWebRtcEncodeEfficiency,
	reconcileHardwareEncodeReport,
	reportFromGpuInfo,
} = await import('@app/features/voice/utils/GpuEncoderCapabilities');

const icu = setupI18n({locale: 'en', messages: {en: {}}});

type Values = Record<string, unknown> | undefined;

const plainI18n = {
	locale: 'en',
	_: (descriptor: MessageDescriptor, values?: Values) => {
		const message = descriptor.message;
		if (typeof message !== 'string') {
			throw new Error(`descriptor ${JSON.stringify(descriptor)} has no message`);
		}
		return icu._({id: message, message}, values);
	},
} as unknown as I18n;

const FIXED_TARGET: ScreenShareTarget = {
	mode: 'screenshare',
	resolution: 'ultra',
	frameRate: 30,
	context: 'display',
	width: 2560,
	height: 1440,
	rung: 'ultra',
	maxBitrate: 4_000_000,
	contentHint: 'text',
	presetOwned: true,
	tierLimited: false,
	deviceMapped: false,
};

const SOURCE_BOX_TARGET: ScreenShareTarget = {
	...FIXED_TARGET,
	resolution: 'source',
	frameRate: 15,
	width: 3840,
	height: 2160,
	rung: 'source',
	maxBitrate: 4_500_000,
};

const SOURCE_FITTED_TARGET: ScreenShareTarget = {
	...SOURCE_BOX_TARGET,
	width: 1920,
	height: 1080,
	rung: 'high',
};

describe('formatScreenShareResolutionLabel', () => {
	it.each([
		[1920, 1080, '1080p'],
		[1280, 720, '720p'],
		[852, 480, '480p'],
		[854, 480, '480p'],
		[2560, 1440, '1440p'],
		[3840, 2160, '2160p'],
		[2714, 762, '2714×762'],
		[640, 480, '640×480'],
		[1080, 1920, '1080×1920'],
		[800, 600, '800×600'],
		[1600, 900, '1600×900'],
		[1280, 800, '1280×800'],
		[860, 480, '480p'],
		[864, 480, '864×480'],
		[1910, 1080, '1080p'],
		[1940, 1080, '1940×1080'],
		[1900, 1080, '1900×1080'],
	])('labels %ix%i as %s', (width, height, expected) => {
		expect(formatScreenShareResolutionLabel(width, height)).toBe(expected);
	});
});

describe('formatScreenShareTargetLabel', () => {
	it('calls a source target with no known source size Source', () => {
		expect(formatScreenShareTargetLabel(plainI18n, SOURCE_BOX_TARGET)).toBe('Source');
	});

	it('names the fitted size once a source target has source dimensions', () => {
		expect(formatScreenShareTargetLabel(plainI18n, SOURCE_FITTED_TARGET)).toBe('1080p');
	});

	it('names the size a fixed target asked for', () => {
		expect(formatScreenShareTargetLabel(plainI18n, FIXED_TARGET)).toBe('1440p');
	});

	it('names an odd fixed size by its dimensions', () => {
		expect(formatScreenShareTargetLabel(plainI18n, {...FIXED_TARGET, width: 2714, height: 762})).toBe('2714×762');
	});
});

describe('SCREEN_SHARE_STATUS_SOURCE_RESOLUTION_DESCRIPTOR', () => {
	it('reads Source, carries a translator comment and keeps the wording plain', () => {
		expect(SCREEN_SHARE_STATUS_SOURCE_RESOLUTION_DESCRIPTOR.message).toBe('Source');
		expect(SCREEN_SHARE_STATUS_SOURCE_RESOLUTION_DESCRIPTOR.comment).toEqual(expect.stringMatching(/\S/));
		expect(SCREEN_SHARE_STATUS_SOURCE_RESOLUTION_DESCRIPTOR.message).not.toMatch(/[;:—–]/);
	});
});

const RTX_5090_GPU_INFO: GpuInfo = {
	devices: [{active: true, vendorId: 0x10de, deviceId: 0x2b85, vendorName: 'NVIDIA', deviceString: 'RTX 5090'}],
	glRenderer: 'ANGLE (NVIDIA, NVIDIA GeForce RTX 5090 Direct3D11 vs_5_0 ps_5_0, D3D11)',
};

const CHROMIUM_HARDWARE_H264_PROFILES: ReadonlySet<string> = new Set(['6400', '4d00', '4200']);

const h264ProfileLevelId = (contentType: string): string =>
	/profile-level-id=([0-9a-f]{6})/.exec(contentType)?.[1] ?? '';

function stubChromiumEncodingInfo(hardwareCodecs: ReadonlySet<string>): Array<string> {
	const asked: Array<string> = [];
	vi.stubGlobal('navigator', {
		mediaCapabilities: {
			encodingInfo: async ({video}: {video: {contentType: string}}) => {
				asked.push(video.contentType);
				if (video.contentType.startsWith('video/H264')) {
					const profile = h264ProfileLevelId(video.contentType).slice(0, 4);
					return {
						supported: true,
						powerEfficient: hardwareCodecs.has('h264') && CHROMIUM_HARDWARE_H264_PROFILES.has(profile),
					};
				}
				const codec = video.contentType.slice('video/'.length).toLowerCase();
				if (codec === 'h265' && !hardwareCodecs.has('h265')) return {supported: false, powerEfficient: false};
				return {supported: true, powerEfficient: hardwareCodecs.has(codec)};
			},
		},
	});
	return asked;
}

afterEach(() => {
	vi.unstubAllGlobals();
});

describe('WebRTC hardware encode probe', () => {
	it('asks H.264 about the profiles a hardware encoder advertises, not only constrained baseline', () => {
		expect(H264_PROBE_PROFILE_LEVEL_IDS).toEqual(['640028', '4d0028', '420028', '42e028']);
		expect(H264_ENCODE_PROBE_CONTENT_TYPES.map((contentType) => h264ProfileLevelId(contentType).slice(0, 4))).toEqual([
			'6400',
			'4d00',
			'4200',
			'42e0',
		]);
		expect(WEBRTC_ENCODE_PROBE_CONTENT_TYPES.h264).toBe(H264_ENCODE_PROBE_CONTENT_TYPES);
	});

	it('keeps every probed H.264 level high enough for the resolution the probe asks about', () => {
		const macroblocks =
			Math.ceil(ENCODE_PROBE_VIDEO_CONFIG.width / 16) * Math.ceil(ENCODE_PROBE_VIDEO_CONFIG.height / 16);
		expect(macroblocks).toBeLessThanOrEqual(8192);
		expect(macroblocks * ENCODE_PROBE_VIDEO_CONFIG.framerate).toBeLessThanOrEqual(245_760);
		for (const contentType of H264_ENCODE_PROBE_CONTENT_TYPES) {
			expect(Number.parseInt(h264ProfileLevelId(contentType).slice(4), 16)).toBeGreaterThanOrEqual(0x28);
		}
	});

	it('leaves AV1 and H.265 without a level that could contradict the probed resolution', () => {
		expect(WEBRTC_ENCODE_PROBE_CONTENT_TYPES.av1).toEqual(['video/AV1']);
		expect(WEBRTC_ENCODE_PROBE_CONTENT_TYPES.h265).toEqual(['video/H265']);
	});

	it('reads H.264 as hardware on a Chromium that only accelerates the unconstrained profiles', async () => {
		const asked = stubChromiumEncodingInfo(new Set<string>(['h264', 'h265']));
		await expect(probeWebRtcEncodeEfficiency()).resolves.toEqual({
			av1: 'software',
			h265: 'hardware',
			h264: 'hardware',
			vp9: 'software',
			vp8: 'software',
		});
		expect(asked).toContain(H264_ENCODE_PROBE_CONTENT_TYPES[0]);
		expect(asked).not.toContain('video/H264;level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f');
	});

	it('reads a codec Chromium cannot encode at all as unknown, not as software', async () => {
		stubChromiumEncodingInfo(new Set<string>(['h264']));
		await expect(probeWebRtcEncodeEfficiency()).resolves.toMatchObject({h265: 'unknown'});
	});

	it('returns nothing when the browser has no encodingInfo to ask', async () => {
		vi.stubGlobal('navigator', {});
		await expect(probeWebRtcEncodeEfficiency()).resolves.toBeNull();
	});
});

describe('reconcileHardwareEncodeReport', () => {
	const table = (): ReturnType<typeof reportFromGpuInfo> => reportFromGpuInfo(RTX_5090_GPU_INFO);
	const probe = (overrides: Partial<Record<VideoCodec, 'hardware' | 'software' | 'unknown'>>) => ({
		av1: 'unknown' as const,
		h265: 'unknown' as const,
		h264: 'unknown' as const,
		vp9: 'unknown' as const,
		vp8: 'unknown' as const,
		...overrides,
	});

	it('classifies an RTX 5090 from its PCI id before any probe runs', () => {
		expect(table().gpuFamily).toBe('nvidia-ada-or-blackwell');
		expect(table().h264).toBe('hardware');
	});

	it('takes the probe answer over the GPU table for every codec', () => {
		expect(
			reconcileHardwareEncodeReport(
				table(),
				probe({av1: 'software', h265: 'software', h264: 'hardware', vp9: 'hardware'}),
			),
		).toMatchObject({av1: 'software', h265: 'software', h264: 'hardware', vp9: 'hardware'});
	});

	it('keeps the GPU table answer where the probe says unknown', () => {
		expect(reconcileHardwareEncodeReport(table(), probe({}))).toMatchObject({
			av1: 'hardware',
			h265: 'hardware',
			h264: 'hardware',
			vp9: 'software',
			vp8: 'software',
		});
	});

	it('keeps the GPU table when the probe could not run', () => {
		expect(reconcileHardwareEncodeReport(table(), null)).toEqual(table());
	});

	it('answers hardware H.264 end to end for an RTX 5090 behind a Chromium that refuses constrained baseline', async () => {
		stubChromiumEncodingInfo(new Set<string>(['h264', 'h265', 'av1']));
		const efficiency = await probeWebRtcEncodeEfficiency();
		expect(reconcileHardwareEncodeReport(table(), efficiency)).toMatchObject({
			av1: 'hardware',
			h265: 'hardware',
			h264: 'hardware',
		});
	});
});
