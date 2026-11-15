// SPDX-License-Identifier: AGPL-3.0-or-later

import {clamp} from '@app/features/ui/components/slider/SliderMath';

export interface EncodeWavOptions {
	startSeconds: number;
	endSeconds: number;
	downmixToMono?: boolean;
}

const RIFF_HEADER_BYTES = 44;

export function encodeAudioBufferSliceToWav(buffer: AudioBuffer, options: EncodeWavOptions): Blob {
	const sampleRate = buffer.sampleRate;
	const totalDurationSeconds = buffer.duration;
	const startSeconds = clamp(options.startSeconds, 0, totalDurationSeconds);
	const endSeconds = clamp(options.endSeconds, startSeconds, totalDurationSeconds);
	const startFrame = Math.floor(startSeconds * sampleRate);
	const endFrame = Math.floor(endSeconds * sampleRate);
	const frameCount = Math.max(0, endFrame - startFrame);
	const sourceChannels = buffer.numberOfChannels;
	const outputChannels = options.downmixToMono || sourceChannels === 1 ? 1 : 2;
	const bytesPerSample = 2;
	const dataBytes = frameCount * outputChannels * bytesPerSample;
	const totalBytes = RIFF_HEADER_BYTES + dataBytes;
	const out = new ArrayBuffer(totalBytes);
	const view = new DataView(out);
	let offset = 0;
	const writeString = (value: string) => {
		for (let i = 0; i < value.length; i++) {
			view.setUint8(offset++, value.charCodeAt(i));
		}
	};
	writeString('RIFF');
	view.setUint32(offset, totalBytes - 8, true);
	offset += 4;
	writeString('WAVE');
	writeString('fmt ');
	view.setUint32(offset, 16, true);
	offset += 4;
	view.setUint16(offset, 1, true);
	offset += 2;
	view.setUint16(offset, outputChannels, true);
	offset += 2;
	view.setUint32(offset, sampleRate, true);
	offset += 4;
	view.setUint32(offset, sampleRate * outputChannels * bytesPerSample, true);
	offset += 4;
	view.setUint16(offset, outputChannels * bytesPerSample, true);
	offset += 2;
	view.setUint16(offset, bytesPerSample * 8, true);
	offset += 2;
	writeString('data');
	view.setUint32(offset, dataBytes, true);
	offset += 4;
	const channelBuffers: Array<Float32Array> = [];
	for (let c = 0; c < sourceChannels; c++) {
		channelBuffers.push(buffer.getChannelData(c));
	}
	for (let frame = 0; frame < frameCount; frame++) {
		const sourceIndex = startFrame + frame;
		if (outputChannels === 1) {
			let sum = 0;
			for (let c = 0; c < sourceChannels; c++) {
				sum += channelBuffers[c]![sourceIndex] ?? 0;
			}
			const sample = clamp(sum / sourceChannels, -1, 1);
			view.setInt16(offset, sample < 0 ? sample * 0x8000 : sample * 0x7fff, true);
			offset += 2;
		} else {
			for (let c = 0; c < outputChannels; c++) {
				const channel = channelBuffers[c] ?? channelBuffers[0]!;
				const sample = clamp(channel[sourceIndex] ?? 0, -1, 1);
				view.setInt16(offset, sample < 0 ? sample * 0x8000 : sample * 0x7fff, true);
				offset += 2;
			}
		}
	}
	return new Blob([out], {type: 'audio/wav'});
}
