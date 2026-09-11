// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	endBridgeStats,
	recordBridgeFrame,
	recordBridgeFrameDrops,
	startBridgeStats,
} from '@app/features/voice/utils/native_audio_capture_bridge/bridgeStats';
import {
	computeAudioLevels,
	getNativeAudioApi,
	isValidAudioFrameMessage,
	mixToStereoInterleaved,
	type NativeAudioBridgeHandle,
	resampleInterleavedStereo,
} from '@app/features/voice/utils/native_audio_capture_bridge/shared';

const logger = new Logger('NativeAudioCaptureBridge');

const MAX_PENDING_CHUNKS = 128;

const TRIM_FADE_FRAMES = 64;

class PendingStereoChunkRing {
	private readonly chunks: Array<Float32Array | null>;
	private head = 0;
	private tail = 0;
	private count = 0;
	private offsetSamples = 0;

	constructor() {
		assert.ok(MAX_PENDING_CHUNKS > 0, 'script processor pending chunk capacity must be positive');
		this.chunks = new Array(MAX_PENDING_CHUNKS).fill(null);
	}

	get length(): number {
		return this.count;
	}

	push(chunk: Float32Array): number {
		assert.equal(chunk.length % 2, 0, 'script processor chunks must be interleaved stereo');
		let dropped = 0;
		if (this.count >= MAX_PENDING_CHUNKS) {
			this.dropOldest();
			dropped = 1;
		}
		this.chunks[this.tail] = chunk;
		this.tail = (this.tail + 1) % MAX_PENDING_CHUNKS;
		this.count += 1;
		assert.ok(this.count <= MAX_PENDING_CHUNKS, 'script processor pending chunks exceeded capacity');
		if (dropped > 0) this.fadeInFirst();
		return dropped;
	}

	drainInto(left: Float32Array, right: Float32Array): void {
		assert.equal(left.length, right.length, 'script processor channel lengths must match');
		let outputIndex = 0;
		while (outputIndex < left.length && this.count > 0) {
			const chunk = this.chunks[this.head];
			assert.ok(chunk, 'script processor pending chunk must exist');
			const availableFrames = Math.floor((chunk.length - this.offsetSamples) / 2);
			const framesToCopy = Math.min(left.length - outputIndex, availableFrames);
			for (let frameIndex = 0; frameIndex < framesToCopy; frameIndex++) {
				left[outputIndex + frameIndex] = chunk[this.offsetSamples + frameIndex * 2] ?? 0;
				right[outputIndex + frameIndex] = chunk[this.offsetSamples + frameIndex * 2 + 1] ?? 0;
			}
			outputIndex += framesToCopy;
			this.offsetSamples += framesToCopy * 2;
			if (this.offsetSamples >= chunk.length) {
				this.dropOldest();
			}
		}
	}

	private dropOldest(): void {
		assert.ok(this.count > 0, 'script processor pending chunk count must be positive before drop');
		this.chunks[this.head] = null;
		this.head = (this.head + 1) % MAX_PENDING_CHUNKS;
		this.count -= 1;
		this.offsetSamples = 0;
	}

	private fadeInFirst(): void {
		if (this.count === 0) return;
		const first = this.chunks[this.head];
		assert.ok(first, 'script processor fade chunk must exist');
		const fadeFrames = Math.min(TRIM_FADE_FRAMES, Math.floor(first.length / 2));
		for (let i = 0; i < fadeFrames; i++) {
			const gain = i / fadeFrames;
			first[i * 2] *= gain;
			first[i * 2 + 1] *= gain;
		}
	}
}

export async function createScriptProcessorBridge(
	captureId: string,
	tryDelegateRemoteEnd: (captureId: string) => boolean,
): Promise<NativeAudioBridgeHandle> {
	const nativeAudioApi = getNativeAudioApi();
	if (!nativeAudioApi) {
		throw new Error('Native audio API unavailable');
	}
	const AudioContextCtor = window.AudioContext ?? window.webkitAudioContext;
	if (!AudioContextCtor) {
		throw new Error('AudioContext unavailable');
	}
	const audioContext = new AudioContextCtor({sampleRate: 48000});
	const destination = audioContext.createMediaStreamDestination();
	const processor = audioContext.createScriptProcessor(4096, 0, 2);
	const mutedSink = audioContext.createGain();
	startBridgeStats('script-processor', captureId);
	mutedSink.gain.value = 0;
	processor.connect(destination);
	processor.connect(mutedSink);
	mutedSink.connect(audioContext.destination);
	if (audioContext.state === 'suspended') {
		void audioContext.resume().catch(() => {});
	}
	const pendingChunks = new PendingStereoChunkRing();
	let cleanedUp = false;
	processor.onaudioprocess = (event) => {
		const left = event.outputBuffer.getChannelData(0);
		const right = event.outputBuffer.getChannelData(1);
		left.fill(0);
		right.fill(0);
		pendingChunks.drainInto(left, right);
	};
	const cleanup = async (stopRemote: boolean = true, endDetail?: string): Promise<void> => {
		if (cleanedUp) return;
		cleanedUp = true;
		endBridgeStats(captureId, 'cleanup', endDetail ?? (stopRemote ? 'caller-stopped' : 'remote-ended'));
		unsubscribeFrame();
		unsubscribeEnd();
		processor.onaudioprocess = null;
		processor.disconnect();
		mutedSink.disconnect();
		for (const track of destination.stream.getTracks()) {
			track.stop();
		}
		await audioContext.close().catch(() => {});
		if (stopRemote) {
			try {
				await nativeAudioApi.stop(captureId);
			} catch (error) {
				logger.warn('Failed to stop native audio capture after script-processor cleanup', {
					captureId,
					error,
				});
			}
		}
	};
	const unsubscribeFrame = nativeAudioApi.onFrame((message) => {
		if (message.captureId !== captureId || cleanedUp) return;
		if (!isValidAudioFrameMessage(message)) {
			logger.warn('Dropping invalid native audio frame', {
				captureId,
				sampleRate: message.sampleRate,
				channels: message.channels,
				byteLength: message.samples?.byteLength,
				timestampUs: message.timestampUs,
			});
			recordBridgeFrameDrops(captureId, 1);
			return;
		}
		const targetSampleRate = audioContext.sampleRate;
		const samples = new Float32Array(message.samples, 0, message.samples.byteLength / Float32Array.BYTES_PER_ELEMENT);
		recordBridgeFrame(captureId, computeAudioLevels(samples));
		let stereo: Float32Array;
		if (message.channels === 2 && message.sampleRate === targetSampleRate) {
			stereo = samples;
		} else {
			const interleavedStereo = message.channels === 2 ? samples : mixToStereoInterleaved(samples, message.channels);
			stereo =
				message.sampleRate === targetSampleRate
					? interleavedStereo
					: resampleInterleavedStereo(interleavedStereo, message.sampleRate, targetSampleRate);
		}
		recordBridgeFrameDrops(captureId, pendingChunks.push(stereo));
	});
	const unsubscribeEnd = nativeAudioApi.onEnd((message) => {
		if (message.captureId !== captureId) return;
		endBridgeStats(captureId, message.reason ?? 'ended', message.detail ?? null);
		if (!tryDelegateRemoteEnd(captureId)) {
			void cleanup(false);
		}
	});
	const track = destination.stream.getAudioTracks()[0];
	if (!track) {
		await cleanup(false);
		throw new Error('ScriptProcessor fallback produced no audio track');
	}
	return {
		track,
		cleanup,
	};
}
