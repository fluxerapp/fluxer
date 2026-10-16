// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	describeHardwareVideoEncodeProfiles,
	type HardwareVideoEncodeProfile,
	isSoftwareVideoEncodeExpected,
	probeHardwareVideoEncodeProfiles,
	type VideoEncoderLike,
} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {describe, expect, it} from 'vitest';

const H264_HIGH: HardwareVideoEncodeProfile = {
	codec: 'h264',
	profile: 'high',
	maxWidth: 3840,
	maxHeight: 2160,
	maxFrameRate: 30,
};

interface ProbeConfig {
	codec: string;
	width: number;
	height: number;
	framerate?: number;
	hardwareAcceleration: string;
}

function fakeEncoder(supports: (config: ProbeConfig) => boolean): VideoEncoderLike {
	return {
		isConfigSupported: async (config) => ({supported: supports(config as ProbeConfig)}),
	};
}

describe('expected software video encoding', () => {
	it('is expected on Linux when the system has no hardware encoder at all', () => {
		expect(
			isSoftwareVideoEncodeExpected({platform: 'linux', h264HardwareProfileCount: 0, hardwareEncodeProfiles: []}),
		).toBe(true);
		expect(
			isSoftwareVideoEncodeExpected({platform: 'linux', h264HardwareProfileCount: null, hardwareEncodeProfiles: []}),
		).toBe(true);
		expect(
			isSoftwareVideoEncodeExpected({platform: 'linux', h264HardwareProfileCount: 0, hardwareEncodeProfiles: null}),
		).toBe(true);
	});

	it('is not expected on Linux when a hardware encoder exists', () => {
		expect(
			isSoftwareVideoEncodeExpected({
				platform: 'linux',
				h264HardwareProfileCount: 0,
				hardwareEncodeProfiles: [H264_HIGH],
			}),
		).toBe(false);
		expect(
			isSoftwareVideoEncodeExpected({platform: 'linux', h264HardwareProfileCount: 2, hardwareEncodeProfiles: []}),
		).toBe(false);
	});

	it('is not expected on Linux before anything has been probed', () => {
		expect(
			isSoftwareVideoEncodeExpected({platform: 'linux', h264HardwareProfileCount: null, hardwareEncodeProfiles: null}),
		).toBe(false);
	});

	it('is never expected on other platforms', () => {
		for (const platform of ['windows', 'macos', 'unknown'] as const) {
			expect(isSoftwareVideoEncodeExpected({platform, h264HardwareProfileCount: 0, hardwareEncodeProfiles: []})).toBe(
				false,
			);
		}
	});
});

describe('hardware video encode profiles', () => {
	it('reports nothing when WebCodecs is unavailable', async () => {
		await expect(probeHardwareVideoEncodeProfiles(null)).resolves.toBeNull();
		expect(describeHardwareVideoEncodeProfiles(null)).toMatch(/^unknown/);
	});

	it('reports an empty list when every hardware probe fails', async () => {
		const profiles = await probeHardwareVideoEncodeProfiles(fakeEncoder(() => false));
		expect(profiles).toEqual([]);
		expect(describeHardwareVideoEncodeProfiles(profiles)).toMatch(/^none/);
	});

	it('only asks for hardware encoders', async () => {
		const asked = new Set<string>();
		await probeHardwareVideoEncodeProfiles(
			fakeEncoder((config) => {
				asked.add(config.hardwareAcceleration);
				return false;
			}),
		);
		expect([...asked]).toEqual(['prefer-hardware']);
	});

	it('bounds each profile by the largest size and frame rate the encoder accepts', async () => {
		const bounded = await probeHardwareVideoEncodeProfiles(
			fakeEncoder(
				(config) =>
					config.codec === 'avc1.640033' &&
					config.width <= 2560 &&
					(config.framerate === undefined || config.framerate <= 60),
			),
		);
		expect(bounded).toEqual([{codec: 'h264', profile: 'high', maxWidth: 2560, maxHeight: 1440, maxFrameRate: 60}]);
		expect(describeHardwareVideoEncodeProfiles(bounded)).toEqual(['h264 high up to 2560x1440 at 60 fps']);
	});
});
