// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GENERATOR_AUDIO_CHUNK_DURATION_US,
	mixToStereoInterleaved,
	NativeAudioFrameChunker,
	resampleInterleavedStereo,
	shouldMixSelfWindowAudioIntoSystemCapture,
} from '@app/features/voice/utils/native_audio_capture_bridge/shared';
import {describe, expect, it} from 'vitest';

function expectSamples(actual: Float32Array, expected: ReadonlyArray<number>): void {
	expect(Array.from(actual)).toEqual(expected);
}

function audioMessage(samples: Float32Array, timestampUs: number, sampleRate = 48000, channels = 2) {
	return {
		sampleRate,
		channels,
		timestampUs,
		samples: samples.buffer.slice(samples.byteOffset, samples.byteOffset + samples.byteLength) as ArrayBuffer,
	};
}

describe('shouldMixSelfWindowAudioIntoSystemCapture', () => {
	it('keeps system capture independent of the Fluxer renderer audio tap', () => {
		expect(
			shouldMixSelfWindowAudioIntoSystemCapture({
				available: true,
				backend: 'windows-wasapi-loopback',
				capabilities: {
					process: true,
					system: true,
					systemExcludesSelf: true,
				},
			}),
		).toBe(false);
	});
});

describe('NativeAudioFrameChunker', () => {
	it('accumulates short native packets into stable 20 ms chunks', () => {
		const chunker = new NativeAudioFrameChunker(GENERATOR_AUDIO_CHUNK_DURATION_US);
		const firstHalf = new Float32Array(480 * 2).fill(0.25);
		const secondHalf = new Float32Array(480 * 2).fill(0.5);
		expect(chunker.push(audioMessage(firstHalf, 1234))).toEqual([]);
		const chunks = chunker.push(audioMessage(secondHalf, 11_234));
		expect(chunks).toHaveLength(1);
		expect(chunks[0].sampleRate).toBe(48000);
		expect(chunks[0].channels).toBe(2);
		expect(chunks[0].numberOfFrames).toBe(960);
		expect(chunks[0].durationUs).toBe(20_000);
		expect(chunks[0].timestampUs).toBe(1234);
		expect(chunks[0].samples[0]).toBe(0.25);
		expect(chunks[0].samples[960]).toBe(0.5);
	});

	it('splits large native packets and advances timestamps continuously', () => {
		const chunker = new NativeAudioFrameChunker(GENERATOR_AUDIO_CHUNK_DURATION_US);
		const samples = new Float32Array(1920 * 2).fill(1);
		const chunks = chunker.push(audioMessage(samples, 50_000));
		expect(chunks).toHaveLength(2);
		expect(chunks.map((chunk) => chunk.numberOfFrames)).toEqual([960, 960]);
		expect(chunks.map((chunk) => chunk.timestampUs)).toEqual([50_000, 70_000]);
	});

	it('resets partial buffered audio when the native format changes', () => {
		const chunker = new NativeAudioFrameChunker(GENERATOR_AUDIO_CHUNK_DURATION_US);
		expect(chunker.push(audioMessage(new Float32Array(480 * 2), 100_000))).toEqual([]);
		const chunks = chunker.push(audioMessage(new Float32Array(882 * 2).fill(0.75), 200_000, 44100, 2));
		expect(chunks).toHaveLength(1);
		expect(chunks[0].sampleRate).toBe(44100);
		expect(chunks[0].numberOfFrames).toBe(882);
		expect(chunks[0].timestampUs).toBe(200_000);
	});
});

describe('mixToStereoInterleaved', () => {
	it('duplicates mono samples into interleaved stereo frames', () => {
		expectSamples(mixToStereoInterleaved(new Float32Array([0.25, -0.5]), 1), [0.25, 0.25, -0.5, -0.5]);
	});
	it('keeps stereo samples as interleaved stereo frames', () => {
		expectSamples(mixToStereoInterleaved(new Float32Array([0.25, -0.5, 0.75, -1]), 2), [0.25, -0.5, 0.75, -1]);
	});
	it('downmixes channels after stereo into the matching side at half amplitude', () => {
		expectSamples(mixToStereoInterleaved(new Float32Array([0.25, 0.5, 0.25, 0.5]), 4), [0.375, 0.75]);
	});
	it('clamps output to [-1, 1] when surround channels push past full scale', () => {
		expectSamples(mixToStereoInterleaved(new Float32Array([1, 2, 0.5, 0.25, -1, -2, 4, -6]), 4), [1, 1, 1, -1]);
	});
});

describe('resampleInterleavedStereo', () => {
	it('returns the original samples when rates match', () => {
		const samples = new Float32Array([0.25, -0.5, 0.75, -1]);
		expect(resampleInterleavedStereo(samples, 48000, 48000)).toBe(samples);
	});
	it('linearly interpolates during downsampling', () => {
		expectSamples(resampleInterleavedStereo(new Float32Array([0, 10, 3, 13, 9, 19]), 48000, 32000), [0, 10, 6, 16]);
	});
	it('linearly interpolates during upsampling and clamps the final frame', () => {
		expectSamples(
			resampleInterleavedStereo(new Float32Array([0, 10, 8, 18]), 24000, 48000),
			[0, 10, 4, 14, 8, 18, 8, 18],
		);
	});
	it('returns empty output when resampling zero frames', () => {
		expectSamples(resampleInterleavedStereo(new Float32Array(0), 48000, 32000), []);
	});
	it('returns empty output for invalid rates', () => {
		expectSamples(resampleInterleavedStereo(new Float32Array([1, 2, 3, 4]), 0, 48000), []);
		expectSamples(resampleInterleavedStereo(new Float32Array([1, 2, 3, 4]), 48000, 0), []);
		expectSamples(resampleInterleavedStereo(new Float32Array([1, 2, 3, 4]), -1, 48000), []);
	});
	it('returns empty output for a single sample (less than one stereo frame)', () => {
		expectSamples(resampleInterleavedStereo(new Float32Array([1]), 48000, 44100), []);
	});
});
