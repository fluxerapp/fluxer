// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	endBridgeStats,
	recordBridgeFrame,
	recordBridgeFrameDrops,
	recordBridgePrebufferTarget,
	recordBridgeQueue,
	recordBridgeRebuffer,
	startBridgeStats,
} from '@app/features/voice/utils/native_audio_capture_bridge/bridgeStats';
import {
	type AudioDataFrame,
	computeAudioLevels,
	GENERATOR_AUDIO_CHUNK_DURATION_US,
	GENERATOR_AUDIO_MAX_PREBUFFER_US,
	GENERATOR_AUDIO_PREBUFFER_STEP_US,
	GENERATOR_AUDIO_PREBUFFER_TIMEOUT_MS,
	GENERATOR_AUDIO_PREBUFFER_US,
	GENERATOR_AUDIO_REBUFFER_GAP_MS,
	getAudioDataCtor,
	getGeneratorCtor,
	getNativeAudioApi,
	isValidAudioFrameMessage,
	MAX_GENERATOR_BUFFERED_AUDIO_US,
	MAX_GENERATOR_PENDING_AUDIO_FRAMES,
	type NativeAudioBridgeHandle,
	NativeAudioFrameChunker,
} from '@app/features/voice/utils/native_audio_capture_bridge/shared';

const logger = new Logger('NativeAudioCaptureBridge');

interface PendingAudioDataFrame {
	audioFrame: AudioDataFrame;
	durationUs: number;
}

const GENERATOR_AUDIO_QUEUE_CAPACITY = MAX_GENERATOR_PENDING_AUDIO_FRAMES + (MAX_GENERATOR_PENDING_AUDIO_FRAMES >> 1);

class PendingAudioFrameRing {
	private readonly frames: Array<PendingAudioDataFrame | null>;
	private head = 0;
	private tail = 0;
	private count = 0;

	constructor() {
		assert.ok(GENERATOR_AUDIO_QUEUE_CAPACITY > 0, 'native audio generator queue capacity must be positive');
		this.frames = new Array(GENERATOR_AUDIO_QUEUE_CAPACITY).fill(null);
	}

	get length(): number {
		return this.count;
	}

	push(frame: PendingAudioDataFrame): PendingAudioDataFrame | null {
		assert.ok(frame.audioFrame);
		let dropped: PendingAudioDataFrame | null = null;
		if (this.count >= GENERATOR_AUDIO_QUEUE_CAPACITY) {
			dropped = this.pop();
		}
		this.frames[this.tail] = frame;
		this.tail = (this.tail + 1) % GENERATOR_AUDIO_QUEUE_CAPACITY;
		this.count += 1;
		assert.ok(this.count <= GENERATOR_AUDIO_QUEUE_CAPACITY, 'native audio generator queue exceeded its cap');
		return dropped;
	}

	pop(): PendingAudioDataFrame | null {
		if (this.count === 0) return null;
		const frame = this.frames[this.head];
		this.frames[this.head] = null;
		this.head = (this.head + 1) % GENERATOR_AUDIO_QUEUE_CAPACITY;
		this.count -= 1;
		assert.ok(this.count >= 0, 'native audio generator queue count underflowed');
		return frame;
	}

	drain(closeAudioFrame: (audioFrame: AudioDataFrame) => void): void {
		assert.equal(typeof closeAudioFrame, 'function');
		let frame = this.pop();
		while (frame) {
			closeAudioFrame(frame.audioFrame);
			frame = this.pop();
		}
	}
}

export async function createGeneratorBridge(
	captureId: string,
	tryDelegateRemoteEnd: (captureId: string) => boolean,
): Promise<NativeAudioBridgeHandle> {
	const nativeAudioApi = getNativeAudioApi();
	const Generator = getGeneratorCtor();
	const AudioData = getAudioDataCtor();
	if (!nativeAudioApi || !Generator || !AudioData) {
		throw new Error('MediaStreamTrackGenerator path unavailable');
	}
	const generator = new Generator({kind: 'audio'});
	const writer = generator.writable.getWriter();
	let cleanedUp = false;
	let activeWrite: Promise<void> | null = null;
	let pendingBufferedDurationUs = 0;
	let prebufferTargetUs = GENERATOR_AUDIO_PREBUFFER_US;
	let prebuffered = false;
	let prebufferTimer: NodeJS.Timeout | null = null;
	let lastRendererFrameAt = 0;
	const pendingFrames = new PendingAudioFrameRing();
	const chunker = new NativeAudioFrameChunker(GENERATOR_AUDIO_CHUNK_DURATION_US);
	startBridgeStats('generator', captureId, {
		prebufferTargetUs: GENERATOR_AUDIO_PREBUFFER_US,
		frameDurationUs: GENERATOR_AUDIO_CHUNK_DURATION_US,
	});
	const closeAudioFrame = (audioFrame: AudioDataFrame): void => {
		try {
			audioFrame.close();
		} catch {}
	};
	const noteQueue = (): void => {
		recordBridgeQueue(captureId, pendingFrames.length, pendingBufferedDurationUs);
	};
	const clearPrebufferTimer = (): void => {
		if (prebufferTimer == null) return;
		clearTimeout(prebufferTimer);
		prebufferTimer = null;
	};
	const schedulePrebufferTimer = (): void => {
		if (prebuffered || prebufferTimer != null || pendingFrames.length === 0) return;
		prebufferTimer = setTimeout(() => {
			prebufferTimer = null;
			if (cleanedUp || prebuffered || pendingFrames.length === 0) return;
			prebuffered = true;
			pumpPendingFrames();
		}, GENERATOR_AUDIO_PREBUFFER_TIMEOUT_MS);
	};
	const pumpPendingFrames = (): void => {
		if (cleanedUp || activeWrite || pendingFrames.length === 0) return;
		if (!prebuffered) {
			schedulePrebufferTimer();
			if (pendingBufferedDurationUs < prebufferTargetUs) return;
			prebuffered = true;
		}
		clearPrebufferTimer();
		const pendingFrame = pendingFrames.pop();
		if (!pendingFrame) return;
		const {audioFrame} = pendingFrame;
		pendingBufferedDurationUs = Math.max(0, pendingBufferedDurationUs - pendingFrame.durationUs);
		noteQueue();
		try {
			activeWrite = writer
				.write(audioFrame)
				.catch((error) => {
					logger.warn('Failed to write native audio frame to MediaStreamTrackGenerator', {
						captureId,
						error,
					});
				})
				.finally(() => {
					closeAudioFrame(audioFrame);
					activeWrite = null;
					pumpPendingFrames();
				});
		} catch (error) {
			logger.warn('Failed to write native audio frame to MediaStreamTrackGenerator', {captureId, error});
			closeAudioFrame(audioFrame);
			activeWrite = null;
			pumpPendingFrames();
		}
	};
	const cleanup = async (stopRemote: boolean = true, endDetail?: string): Promise<void> => {
		if (cleanedUp) return;
		cleanedUp = true;
		endBridgeStats(captureId, 'cleanup', endDetail ?? (stopRemote ? 'caller-stopped' : 'remote-ended'));
		unsubscribeFrame();
		unsubscribeEnd();
		clearPrebufferTimer();
		pendingFrames.drain(closeAudioFrame);
		pendingBufferedDurationUs = 0;
		const stopRemotePromise = stopRemote
			? nativeAudioApi.stop(captureId).catch((error) => {
					logger.warn('Failed to stop native audio capture after generator cleanup', {captureId, error});
				})
			: null;
		if (activeWrite) {
			await activeWrite;
		}
		try {
			await writer.close();
		} catch {}
		try {
			generator.stop();
		} catch {}
		await stopRemotePromise;
	};
	const unsubscribeFrame = nativeAudioApi.onFrame((message) => {
		if (message.captureId !== captureId || cleanedUp) return;
		const arrivalAt = Date.now();
		if (lastRendererFrameAt > 0 && arrivalAt - lastRendererFrameAt > GENERATOR_AUDIO_REBUFFER_GAP_MS) {
			prebufferTargetUs = Math.min(
				GENERATOR_AUDIO_MAX_PREBUFFER_US,
				prebufferTargetUs + GENERATOR_AUDIO_PREBUFFER_STEP_US,
			);
			prebuffered = false;
			recordBridgePrebufferTarget(captureId, prebufferTargetUs);
			recordBridgeRebuffer(captureId);
		}
		lastRendererFrameAt = arrivalAt;
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
		const samples = new Float32Array(message.samples, 0, message.samples.byteLength / Float32Array.BYTES_PER_ELEMENT);
		const numberOfFrames = Math.floor(samples.length / message.channels);
		const durationUs = Math.round((numberOfFrames / message.sampleRate) * 1_000_000);
		const audioLevels = computeAudioLevels(samples);
		recordBridgeFrame(captureId, {
			timestampUs: message.timestampUs,
			durationUs,
			...audioLevels,
		});
		let dropped = 0;
		for (const chunk of chunker.push(message)) {
			let audioFrame: AudioDataFrame;
			try {
				audioFrame = new AudioData({
					data: chunk.samples,
					format: 'f32',
					sampleRate: chunk.sampleRate,
					numberOfFrames: chunk.numberOfFrames,
					numberOfChannels: chunk.channels,
					timestamp: chunk.timestampUs,
				});
			} catch (error) {
				logger.warn('Failed to construct native audio frame for MediaStreamTrackGenerator', {
					captureId,
					error,
				});
				recordBridgeFrameDrops(captureId, 1);
				continue;
			}
			const droppedFrame = pendingFrames.push({audioFrame, durationUs: chunk.durationUs});
			if (droppedFrame) {
				pendingBufferedDurationUs = Math.max(0, pendingBufferedDurationUs - droppedFrame.durationUs);
				closeAudioFrame(droppedFrame.audioFrame);
				dropped += 1;
			}
			pendingBufferedDurationUs += chunk.durationUs;
		}
		while (pendingBufferedDurationUs > MAX_GENERATOR_BUFFERED_AUDIO_US) {
			const droppedFrame = pendingFrames.pop();
			if (droppedFrame) {
				pendingBufferedDurationUs = Math.max(0, pendingBufferedDurationUs - droppedFrame.durationUs);
				closeAudioFrame(droppedFrame.audioFrame);
				dropped += 1;
			} else {
				break;
			}
		}
		recordBridgeFrameDrops(captureId, dropped);
		noteQueue();
		schedulePrebufferTimer();
		pumpPendingFrames();
	});
	const unsubscribeEnd = nativeAudioApi.onEnd((message) => {
		if (message.captureId !== captureId) return;
		endBridgeStats(captureId, message.reason ?? 'ended', message.detail ?? null);
		if (!tryDelegateRemoteEnd(captureId)) {
			void cleanup(false);
		}
	});
	return {
		track: generator,
		cleanup,
	};
}
