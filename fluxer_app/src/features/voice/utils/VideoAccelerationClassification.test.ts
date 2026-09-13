// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	classifyVideoDecoderAcceleration,
	classifyVideoEncoderAcceleration,
	isHardwareVideoImplementation,
	isSoftwareVideoImplementation,
} from '@app/features/voice/utils/VideoAccelerationClassification';
import {describe, expect, it} from 'vitest';

describe('video acceleration implementation classification', () => {
	it('detects common software implementations', () => {
		expect(isSoftwareVideoImplementation('FFmpegVideoDecoder')).toBe(true);
		expect(isSoftwareVideoImplementation('Dav1dVideoDecoder')).toBe(true);
		expect(isSoftwareVideoImplementation('libvpx')).toBe(true);
		expect(isSoftwareVideoImplementation('OpenH264')).toBe(true);
	});

	it('detects common hardware implementations', () => {
		expect(isHardwareVideoImplementation('D3D11VideoDecoder')).toBe(true);
		expect(isHardwareVideoImplementation('VideoToolboxVideoDecoder')).toBe(true);
		expect(isHardwareVideoImplementation('VaapiVideoDecoder')).toBe(true);
		expect(isHardwareVideoImplementation('NVENCVideoEncoder')).toBe(true);
		expect(isHardwareVideoImplementation('h264_qsv')).toBe(true);
	});
});

describe('video acceleration status classification', () => {
	it('prefers a known software implementation over power efficiency', () => {
		expect(classifyVideoDecoderAcceleration('FFmpegVideoDecoder', true)).toBe('software');
		expect(classifyVideoEncoderAcceleration('libaom', true)).toBe('software');
	});

	it('prefers a known hardware implementation over power efficiency', () => {
		expect(classifyVideoDecoderAcceleration('D3D11VideoDecoder', false)).toBe('hardware');
		expect(classifyVideoEncoderAcceleration('VideoToolboxVideoEncoder', false)).toBe('hardware');
	});

	it('falls back to WebRTC power efficiency flags when implementation is hidden', () => {
		expect(classifyVideoDecoderAcceleration(undefined, true)).toBe('hardware');
		expect(classifyVideoDecoderAcceleration(undefined, false)).toBe('software');
		expect(classifyVideoEncoderAcceleration(undefined, true)).toBe('hardware');
		expect(classifyVideoEncoderAcceleration(undefined, false)).toBe('software');
	});

	it('returns unknown when no useful signal is exposed', () => {
		expect(classifyVideoDecoderAcceleration(undefined, undefined)).toBe('unknown');
		expect(classifyVideoEncoderAcceleration('MysteryVideoEncoder', undefined)).toBe('unknown');
	});
});
