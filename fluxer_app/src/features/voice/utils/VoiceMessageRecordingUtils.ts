// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {resolveAudioContextConstructor} from '@app/features/voice/engine/VoiceSharedAudioContext';
import {encodeAudioBufferSliceToWav} from '@app/features/voice/utils/AudioWavEncode';

export interface VoiceWaveformResult {
	duration: number;
	waveform: string;
}

export interface PreparedVoiceMessage {
	file: File;
	duration: number;
	waveform: string;
}

const logger = new Logger('VoiceMessageRecordingUtils');
const WAVEFORM_MAX_POINTS = 256;
const WAVEFORM_SAMPLE_INTERVAL_SECONDS = 0.1;

function buildWaveformBytes(
	channelData: Float32Array<ArrayBufferLike>,
	durationSeconds: number,
): Uint8Array<ArrayBuffer> {
	const pointCount = Math.min(
		WAVEFORM_MAX_POINTS,
		Math.max(1, Math.ceil(durationSeconds / WAVEFORM_SAMPLE_INTERVAL_SECONDS)),
	);
	const samplesPerPoint = Math.max(1, Math.floor(channelData.length / pointCount));
	const magnitudes = new Array<number>(pointCount).fill(0);
	let maxMagnitude = 0;
	for (let i = 0; i < pointCount; i++) {
		const start = i * samplesPerPoint;
		const end = Math.min(channelData.length, start + samplesPerPoint);
		let sumSquares = 0;
		let count = 0;
		for (let j = start; j < end; j++) {
			const sample = channelData[j];
			sumSquares += sample * sample;
			count++;
		}
		const rms = count > 0 ? Math.sqrt(sumSquares / count) : 0;
		magnitudes[i] = rms;
		if (rms > maxMagnitude) {
			maxMagnitude = rms;
		}
	}
	const bytes = new Uint8Array(pointCount);
	if (maxMagnitude <= 0) {
		return bytes;
	}
	for (let i = 0; i < pointCount; i++) {
		const normalised = Math.max(0, Math.min(1, magnitudes[i] / maxMagnitude));
		bytes[i] = Math.min(255, Math.round(Math.sqrt(normalised) * 255));
	}
	return bytes;
}

export async function prepareVoiceMessageWav(blob: Blob, filename: string): Promise<PreparedVoiceMessage> {
	const contextClass = resolveAudioContextConstructor();
	if (!contextClass) {
		throw new Error('AudioContext is unavailable; cannot prepare voice message');
	}
	const audioContext = new contextClass();
	try {
		const arrayBuffer = await blob.arrayBuffer();
		const audioBuffer = await audioContext.decodeAudioData(arrayBuffer);
		const wavBlob = encodeAudioBufferSliceToWav(audioBuffer, {
			startSeconds: 0,
			endSeconds: audioBuffer.duration,
			downmixToMono: true,
		});
		const {duration, waveform} = computeVoiceWaveformFromAudioBuffer(audioBuffer);
		return {file: new File([wavBlob], filename, {type: wavBlob.type}), duration, waveform};
	} catch (error) {
		logger.warn({error, size: blob.size, type: blob.type}, 'Unable to prepare voice message WAV');
		throw error;
	} finally {
		audioContext.close().catch(() => {});
	}
}

export function computeVoiceWaveformFromAudioBuffer(
	audioBuffer: AudioBuffer,
	startSeconds = 0,
	endSeconds: number = audioBuffer.duration,
): VoiceWaveformResult {
	const sampleRate = audioBuffer.sampleRate;
	const channelData = audioBuffer.getChannelData(0);
	const startSample = Math.max(0, Math.min(channelData.length, Math.floor(startSeconds * sampleRate)));
	const endSample = Math.max(startSample, Math.min(channelData.length, Math.ceil(endSeconds * sampleRate)));
	const slice = channelData.subarray(startSample, endSample);
	const sliceDurationSeconds = Math.max(0, endSeconds - startSeconds);
	const data = buildWaveformBytes(slice, sliceDurationSeconds);
	const binary = String.fromCharCode(...data);
	return {duration: Math.max(1, Math.round(sliceDurationSeconds)), waveform: btoa(binary)};
}
