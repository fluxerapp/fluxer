// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {NativeAudioAvailability, NativeAudioStartOptions} from '@app/types/electron.d';

export const MAX_GENERATOR_PENDING_AUDIO_FRAMES = 64;
export const GENERATOR_AUDIO_CHUNK_DURATION_US = 20_000;
export const GENERATOR_AUDIO_PREBUFFER_US = 200_000;
export const GENERATOR_AUDIO_MAX_PREBUFFER_US = 1_000_000;
export const GENERATOR_AUDIO_PREBUFFER_STEP_US = 100_000;
export const GENERATOR_AUDIO_PREBUFFER_TIMEOUT_MS = 250;
export const GENERATOR_AUDIO_REBUFFER_GAP_MS = 160;
export const MAX_GENERATOR_BUFFERED_AUDIO_US = 1_500_000;
export const MIN_NATIVE_AUDIO_SAMPLE_RATE = 8000;
export const MAX_NATIVE_AUDIO_SAMPLE_RATE = 192000;
export const MAX_NATIVE_AUDIO_CHANNELS = 8;
export const MAX_NATIVE_AUDIO_FRAME_SECONDS = 1;

export type ArmedNativeAudioCapture =
	| {
			kind: 'capture';
			captureId: string;
			includeSelfWindowAudio?: boolean;
	  }
	| {
			kind: 'linux-routing';
			linuxRule: NonNullable<NativeAudioStartOptions['linuxRule']>;
			includeSelfWindowAudio?: boolean;
	  }
	| {
			kind: 'self-window-web-audio';
	  };

export interface NativeAudioBridgeHandle {
	track: MediaStreamTrack;
	cleanup: (stopRemote?: boolean) => Promise<void>;
}

export interface ActiveNativeAudioBridge {
	captureId: string;
	cleanup: (stopRemote?: boolean) => Promise<void>;
	linuxRule?: NativeAudioRoutingRule;
	includeSelfWindowAudio?: boolean;
}

export type NativeAudioRoutingRule = NonNullable<NativeAudioStartOptions['linuxRule']>;

function routingPatternsEqual(a: NativeAudioRoutingRule['include'], b: NativeAudioRoutingRule['include']): boolean {
	const left = a ?? [];
	const right = b ?? [];
	if (left.length !== right.length) return false;
	const serialize = (entry: Record<string, unknown>): string =>
		JSON.stringify(
			Object.entries(entry)
				.filter(([, value]) => typeof value === 'string')
				.sort(([x], [y]) => x.localeCompare(y)),
		);
	const serializedLeft = left.map(serialize).sort();
	const serializedRight = right.map(serialize).sort();
	return serializedLeft.every((entry, index) => entry === serializedRight[index]);
}

export function nativeAudioRoutingRulesEqual(
	a: NativeAudioRoutingRule | undefined,
	b: NativeAudioRoutingRule | undefined,
): boolean {
	if (a === b) return true;
	if (!a || !b) return false;
	if (!routingPatternsEqual(a.include, b.include)) return false;
	if (!routingPatternsEqual(a.exclude, b.exclude)) return false;
	return (
		Boolean(a.ignoreDevices) === Boolean(b.ignoreDevices) &&
		Boolean(a.ignoreInputMedia) === Boolean(b.ignoreInputMedia) &&
		Boolean(a.ignoreVirtual) === Boolean(b.ignoreVirtual) &&
		Boolean(a.workaround) === Boolean(b.workaround) &&
		Boolean(a.onlySpeakers) === Boolean(b.onlySpeakers) &&
		Boolean(a.onlyDefaultSpeakers) === Boolean(b.onlyDefaultSpeakers)
	);
}

export interface GeneratorAudioTrack extends MediaStreamTrack {
	writable: WritableStream<unknown>;
}

export interface AudioDataCtor {
	new (init: {
		data: BufferSource;
		format: string;
		sampleRate: number;
		numberOfFrames: number;
		numberOfChannels: number;
		timestamp: number;
	}): AudioDataFrame;
}

export interface GeneratorCtor {
	new (options: {kind: 'audio'}): GeneratorAudioTrack;
}

export interface AudioDataFrame {
	close: () => void;
}

export interface NativeAudioBridgeStats {
	active: boolean;
	bridgeMode: 'generator' | 'script-processor' | null;
	captureId: string | null;
	startedAt: number | null;
	lastFrameAt: number | null;
	lastFrameTimestampUs: number | null;
	framesReceived: number;
	framesDropped: number;
	lateFrameCount: number;
	rebufferCount: number;
	maxFrameArrivalGapMs: number;
	maxFrameTimestampGapMs: number;
	maxPendingFrames: number;
	maxBufferedDurationMs: number;
	lastFramePeak: number | null;
	lastFrameRms: number | null;
	maxFramePeak: number;
	maxFrameRms: number;
	nonSilentFrameCount: number;
	prebufferTargetMs: number | null;
	frameDurationMs: number | null;
	endReason: string | null;
	endDetail: string | null;
	endedAt: number | null;
}

export interface NativeAudioBridgeFrameMetrics {
	timestampUs?: number;
	durationUs?: number;
	peak?: number;
	rms?: number;
}

export const initialBridgeStats: NativeAudioBridgeStats = {
	active: false,
	bridgeMode: null,
	captureId: null,
	startedAt: null,
	lastFrameAt: null,
	lastFrameTimestampUs: null,
	framesReceived: 0,
	framesDropped: 0,
	lateFrameCount: 0,
	rebufferCount: 0,
	maxFrameArrivalGapMs: 0,
	maxFrameTimestampGapMs: 0,
	maxPendingFrames: 0,
	maxBufferedDurationMs: 0,
	lastFramePeak: null,
	lastFrameRms: null,
	maxFramePeak: 0,
	maxFrameRms: 0,
	nonSilentFrameCount: 0,
	prebufferTargetMs: null,
	frameDurationMs: null,
	endReason: null,
	endDetail: null,
	endedAt: null,
};

export function isNativeAudioDesktopPlatform(): boolean {
	const electronApi = getElectronAPI();
	return (
		Boolean(electronApi) &&
		(electronApi?.platform === 'darwin' || electronApi?.platform === 'win32' || electronApi?.platform === 'linux')
	);
}

export function getGeneratorCtor(): GeneratorCtor | undefined {
	return (
		window as typeof window & {
			MediaStreamTrackGenerator?: GeneratorCtor;
		}
	).MediaStreamTrackGenerator;
}

export function getAudioDataCtor(): AudioDataCtor | undefined {
	return (
		window as typeof window & {
			AudioData?: AudioDataCtor;
		}
	).AudioData;
}

export function getNativeAudioApi() {
	return getElectronAPI()?.nativeAudio ?? null;
}

export function isArrayBuffer(value: unknown): value is ArrayBuffer {
	return value instanceof ArrayBuffer || Object.prototype.toString.call(value) === '[object ArrayBuffer]';
}

export function isValidAudioFrameMessage(message: {
	sampleRate: number;
	channels: number;
	timestampUs: number;
	samples: ArrayBuffer;
}): boolean {
	const sampleCount = isArrayBuffer(message.samples) ? message.samples.byteLength / Float32Array.BYTES_PER_ELEMENT : 0;
	const frameCount = message.channels > 0 ? sampleCount / message.channels : 0;
	return (
		isArrayBuffer(message.samples) &&
		Number.isFinite(message.sampleRate) &&
		message.sampleRate >= MIN_NATIVE_AUDIO_SAMPLE_RATE &&
		message.sampleRate <= MAX_NATIVE_AUDIO_SAMPLE_RATE &&
		Number.isSafeInteger(message.channels) &&
		message.channels > 0 &&
		message.channels <= MAX_NATIVE_AUDIO_CHANNELS &&
		Number.isFinite(message.timestampUs) &&
		message.samples.byteLength % Float32Array.BYTES_PER_ELEMENT === 0 &&
		sampleCount >= message.channels &&
		sampleCount % message.channels === 0 &&
		frameCount <= Math.ceil(message.sampleRate * MAX_NATIVE_AUDIO_FRAME_SECONDS)
	);
}

export function computeAudioLevels(samples: Float32Array): Pick<NativeAudioBridgeFrameMetrics, 'peak' | 'rms'> {
	let peak = 0;
	let sumSquares = 0;
	for (const sample of samples) {
		if (!Number.isFinite(sample)) continue;
		const absoluteSample = Math.abs(sample);
		peak = Math.max(peak, absoluteSample);
		sumSquares += sample * sample;
	}
	const rms = samples.length === 0 ? 0 : Math.sqrt(sumSquares / samples.length);
	return {peak, rms};
}

export interface NativeAudioChunkerOutput {
	samples: Float32Array<ArrayBuffer>;
	sampleRate: number;
	channels: number;
	timestampUs: number;
	numberOfFrames: number;
	durationUs: number;
}

export class NativeAudioFrameChunker {
	private readonly targetChunkDurationUs: number;
	private sampleRate = 0;
	private channels = 0;
	private nextTimestampUs = 0;
	private pendingChunkSamples: Float32Array<ArrayBuffer> | null = null;
	private pendingChunkFrames = 0;

	constructor(targetChunkDurationUs: number = GENERATOR_AUDIO_CHUNK_DURATION_US) {
		assert.ok(targetChunkDurationUs > 0, 'native audio chunk duration must be positive');
		this.targetChunkDurationUs = targetChunkDurationUs;
	}

	push(message: {
		sampleRate: number;
		channels: number;
		timestampUs: number;
		samples: ArrayBuffer;
	}): Array<NativeAudioChunkerOutput> {
		const samples = new Float32Array(message.samples, 0, message.samples.byteLength / Float32Array.BYTES_PER_ELEMENT);
		const frameCount = Math.floor(samples.length / message.channels);
		if (frameCount <= 0) return [];
		assert.ok(message.sampleRate > 0, 'native audio chunker sample rate must be positive');
		assert.ok(message.channels > 0, 'native audio chunker channel count must be positive');
		assert.ok(
			frameCount <= Math.ceil(message.sampleRate * MAX_NATIVE_AUDIO_FRAME_SECONDS),
			'native audio packet too large',
		);
		if (message.sampleRate !== this.sampleRate || message.channels !== this.channels) {
			this.reset(message.sampleRate, message.channels, message.timestampUs);
		}
		const targetFrames = this.targetFramesPerChunk();
		const outputs: Array<NativeAudioChunkerOutput> = [];
		let sourceOffsetFrames = 0;
		while (sourceOffsetFrames < frameCount) {
			const pending = this.ensurePendingChunk(targetFrames);
			const writableFrames = targetFrames - this.pendingChunkFrames;
			const framesToCopy = Math.min(frameCount - sourceOffsetFrames, writableFrames);
			const sourceStart = sourceOffsetFrames * this.channels;
			const sourceEnd = sourceStart + framesToCopy * this.channels;
			pending.set(samples.subarray(sourceStart, sourceEnd), this.pendingChunkFrames * this.channels);
			this.pendingChunkFrames += framesToCopy;
			sourceOffsetFrames += framesToCopy;
			if (this.pendingChunkFrames >= targetFrames) {
				outputs.push(this.takePendingChunk(targetFrames));
			}
		}
		return outputs;
	}

	private reset(sampleRate: number, channels: number, timestampUs: number): void {
		this.sampleRate = sampleRate;
		this.channels = channels;
		this.nextTimestampUs = Math.max(0, Math.round(timestampUs));
		this.pendingChunkSamples = null;
		this.pendingChunkFrames = 0;
	}

	private targetFramesPerChunk(): number {
		return Math.max(1, Math.round((this.sampleRate * this.targetChunkDurationUs) / 1_000_000));
	}

	private ensurePendingChunk(targetFrames: number): Float32Array<ArrayBuffer> {
		assert.ok(targetFrames > 0, 'native audio chunk target must be positive');
		assert.ok(this.channels > 0, 'native audio chunk channels must be positive');
		if (!this.pendingChunkSamples) {
			this.pendingChunkSamples = new Float32Array(targetFrames * this.channels);
		}
		return this.pendingChunkSamples;
	}

	private takePendingChunk(targetFrames: number): NativeAudioChunkerOutput {
		const samples = this.pendingChunkSamples;
		assert.ok(samples, 'native audio chunk samples must exist');
		assert.equal(this.pendingChunkFrames, targetFrames, 'native audio chunk must be full before output');
		const durationUs = Math.round((targetFrames / this.sampleRate) * 1_000_000);
		const timestampUs = this.nextTimestampUs;
		this.nextTimestampUs += durationUs;
		this.pendingChunkSamples = null;
		this.pendingChunkFrames = 0;
		return {
			samples,
			sampleRate: this.sampleRate,
			channels: this.channels,
			timestampUs,
			numberOfFrames: targetFrames,
			durationUs,
		};
	}
}

export async function replaceStreamAudioTrack(stream: MediaStream, nextTrack: MediaStreamTrack): Promise<void> {
	for (const existingAudioTrack of stream.getAudioTracks()) {
		stream.removeTrack(existingAudioTrack);
		try {
			existingAudioTrack.stop();
		} catch {}
	}
	stream.addTrack(nextTrack);
}

export function stopStreamTracks(stream: MediaStream): void {
	for (const track of stream.getTracks()) {
		try {
			track.stop();
		} catch {}
	}
}

export function restoreTrackStop(track: MediaStreamTrack, originalStop: MediaStreamTrack['stop']): void {
	try {
		Object.defineProperty(track, 'stop', {
			value: originalStop,
			configurable: true,
			writable: true,
		});
	} catch {
		try {
			(track as MediaStreamTrack & {stop: MediaStreamTrack['stop']}).stop = originalStop;
		} catch {}
	}
}

export function patchTrackStopForCleanup(track: MediaStreamTrack, cleanup: () => void): () => void {
	const originalStop = track.stop;
	let restored = false;
	const patchedStop = function patchedNativeAudioTrackStop(this: MediaStreamTrack): void {
		cleanup();
		originalStop.call(this);
	};
	try {
		Object.defineProperty(track, 'stop', {
			value: patchedStop,
			configurable: true,
			writable: true,
		});
	} catch {
		try {
			(track as MediaStreamTrack & {stop: MediaStreamTrack['stop']}).stop = patchedStop;
		} catch {
			return () => undefined;
		}
	}
	return () => {
		if (restored) return;
		restored = true;
		restoreTrackStop(track, originalStop);
	};
}

export function getArmedCaptureId(capture: ArmedNativeAudioCapture | null): string | null {
	return capture?.kind === 'capture' ? capture.captureId : null;
}

export function unsupportedPlatformAvailability(): NativeAudioAvailability {
	return {available: false, reason: 'unsupported-platform'};
}

export function shouldMixSelfWindowAudioIntoSystemCapture(_availability: NativeAudioAvailability): boolean {
	return false;
}

export function mixToStereoInterleaved(samples: Float32Array, channels: number): Float32Array {
	const frameCount = Math.floor(samples.length / channels);
	const out = new Float32Array(frameCount * 2);
	if (channels === 1) {
		for (let i = 0; i < frameCount; i++) {
			out[i * 2] = samples[i]!;
			out[i * 2 + 1] = samples[i]!;
		}
	} else {
		for (let i = 0; i < frameCount; i++) {
			let left = samples[i * channels]!;
			let right = samples[i * channels + 1]!;
			for (let ch = 2; ch < channels; ch++) {
				const s = samples[i * channels + ch]! * 0.5;
				if (ch % 2 === 0) {
					left += s;
				} else {
					right += s;
				}
			}
			out[i * 2] = Math.max(-1, Math.min(1, left));
			out[i * 2 + 1] = Math.max(-1, Math.min(1, right));
		}
	}
	return out;
}

export function resampleInterleavedStereo(samples: Float32Array, srcRate: number, dstRate: number): Float32Array {
	if (srcRate === dstRate) return samples;
	if (srcRate <= 0 || dstRate <= 0 || samples.length < 2) return new Float32Array(0);
	const srcFrames = Math.floor(samples.length / 2);
	const dstFrames = Math.round((srcFrames * dstRate) / srcRate);
	if (dstFrames === 0) return new Float32Array(0);
	const out = new Float32Array(dstFrames * 2);
	const ratio = srcRate / dstRate;
	for (let i = 0; i < dstFrames; i++) {
		const srcPos = i * ratio;
		const idx = Math.floor(srcPos);
		const frac = srcPos - idx;
		const idx0 = idx * 2;
		const idx1 = Math.min(idx + 1, srcFrames - 1) * 2;
		out[i * 2] = samples[idx0]! * (1 - frac) + samples[idx1]! * frac;
		out[i * 2 + 1] = samples[idx0 + 1]! * (1 - frac) + samples[idx1 + 1]! * frac;
	}
	return out;
}
