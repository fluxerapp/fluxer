// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {randomUuid} from '@app/features/platform/utils/RandomUuid';
import type {NativeAudioFramePump} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {computeAudioLevels} from '@app/features/voice/utils/native_audio_capture_bridge/shared';

const logger = new Logger('NativeEngineAudioTrackPump');

const SCRIPT_PROCESSOR_BUFFER_SIZE = 2048;
const CAPTURE_SAMPLE_RATE = 48000;
const AUDIO_WORKLET_PROCESSOR_NAME = 'fluxer-native-engine-audio-track-pump';
const AUDIO_WORKLET_BUFFER_POOL_SIZE = 32;
const AUDIO_WORKLET_BUFFER_FRAMES = SCRIPT_PROCESSOR_BUFFER_SIZE;
const AUDIO_WORKLET_SOURCE = `
const BUFFER_POOL_SIZE = ${AUDIO_WORKLET_BUFFER_POOL_SIZE};
const BUFFER_FRAMES = ${AUDIO_WORKLET_BUFFER_FRAMES};

class FluxerNativeEngineAudioTrackPumpProcessor extends AudioWorkletProcessor {
	constructor() {
		super();
		this.stopped = false;
		this.currentBuffer = null;
		this.currentLength = 0;
		this.freeBuffers = [];
		for (let index = 0; index < BUFFER_POOL_SIZE; index++) {
			this.freeBuffers.push(new Float32Array(BUFFER_FRAMES));
		}
		this.port.onmessage = (event) => {
			if (event.data && event.data.type === 'stop') {
				this.flushCurrentBuffer();
				this.stopped = true;
				return;
			}
			if (
				event.data &&
				event.data.type === 'return-buffer' &&
				event.data.buffer instanceof ArrayBuffer &&
				event.data.buffer.byteLength === BUFFER_FRAMES * Float32Array.BYTES_PER_ELEMENT &&
				this.freeBuffers.length < BUFFER_POOL_SIZE
			) {
				this.freeBuffers.push(new Float32Array(event.data.buffer));
			}
		};
	}

	flushCurrentBuffer() {
		const samples = this.currentBuffer;
		if (samples === null) return;
		const length = this.currentLength;
		this.currentBuffer = null;
		this.currentLength = 0;
		if (length === 0) {
			this.freeBuffers.push(samples);
			return;
		}
		this.port.postMessage(
			{
				type: 'frame',
				sampleRate,
				channels: 1,
				samples: samples.subarray(0, length),
			},
			[samples.buffer],
		);
	}

	mixQuantumIntoCurrentBuffer(input, firstChannel) {
		const samples = this.currentBuffer;
		const offset = this.currentLength;
		if (input.length === 1) {
			samples.set(firstChannel, offset);
		} else {
			for (let index = 0; index < firstChannel.length; index++) {
				let value = 0;
				for (const channel of input) {
					value += channel[index] || 0;
				}
				samples[offset + index] = value / input.length;
			}
		}
		this.currentLength = offset + firstChannel.length;
	}

	process(inputs, outputs) {
		const output = outputs[0];
		if (output) {
			for (const channel of output) {
				channel.fill(0);
			}
		}
		if (this.stopped) return false;
		const input = inputs[0];
		const firstChannel = input && input[0];
		if (!firstChannel || firstChannel.length === 0) return true;
		if (firstChannel.length > BUFFER_FRAMES) return true;
		if (this.currentBuffer !== null && this.currentLength + firstChannel.length > BUFFER_FRAMES) {
			this.flushCurrentBuffer();
		}
		if (this.currentBuffer === null) {
			const samples = this.freeBuffers.pop();
			if (!samples) return true;
			this.currentBuffer = samples;
			this.currentLength = 0;
		}
		this.mixQuantumIntoCurrentBuffer(input, firstChannel);
		if (this.currentLength === BUFFER_FRAMES) {
			this.flushCurrentBuffer();
		}
		return true;
	}
}

registerProcessor('${AUDIO_WORKLET_PROCESSOR_NAME}', FluxerNativeEngineAudioTrackPumpProcessor);
`;

export interface NativeEngineAudioTrackFrame {
	sampleRate: number;
	channels: number;
	samples: Float32Array;
	release?: () => void;
}

export interface NativeEngineAudioTrackEndMessage {
	captureId: string;
	reason?: string;
	detail?: string;
}

export interface NativeEngineAudioTrackPumpStats {
	active: boolean;
	captureId: string | null;
	startedAt: number | null;
	lastFrameAt: number | null;
	framesForwarded: number;
	lastFramePeak: number | null;
	lastFrameRms: number | null;
	maxFramePeak: number;
	maxFrameRms: number;
	nonSilentFrameCount: number;
	endReason: string | null;
	endDetail: string | null;
	endedAt: number | null;
}

interface AudioWorkletFrameMessage {
	type: 'frame';
	sampleRate: number;
	channels: number;
	samples: Float32Array;
}

const NON_SILENT_PEAK_THRESHOLD = 0.0005;
const NON_SILENT_RMS_THRESHOLD = 0.0001;

const initialPumpStats: NativeEngineAudioTrackPumpStats = {
	active: false,
	captureId: null,
	startedAt: null,
	lastFrameAt: null,
	framesForwarded: 0,
	lastFramePeak: null,
	lastFrameRms: null,
	maxFramePeak: 0,
	maxFrameRms: 0,
	nonSilentFrameCount: 0,
	endReason: null,
	endDetail: null,
	endedAt: null,
};

let pumpStats: NativeEngineAudioTrackPumpStats = {...initialPumpStats};

function createCaptureId(): string {
	return `media-track-screen-audio:${randomUuid()}`;
}

function roundLevel(value: number | undefined): number | null {
	if (typeof value !== 'number' || !Number.isFinite(value)) return null;
	return Math.round(Math.max(0, value) * 1_000_000) / 1_000_000;
}

function startPumpStats(captureId: string): void {
	pumpStats = {
		...initialPumpStats,
		active: true,
		captureId,
		startedAt: Date.now(),
	};
}

function recordPumpFrame(captureId: string, samples: Float32Array): void {
	if (pumpStats.captureId !== captureId) return;
	const levels = computeAudioLevels(samples);
	const peak = roundLevel(levels.peak);
	const rms = roundLevel(levels.rms);
	pumpStats = {
		...pumpStats,
		active: true,
		lastFrameAt: Date.now(),
		framesForwarded: pumpStats.framesForwarded + 1,
		lastFramePeak: peak,
		lastFrameRms: rms,
		maxFramePeak: Math.max(pumpStats.maxFramePeak, peak ?? 0),
		maxFrameRms: Math.max(pumpStats.maxFrameRms, rms ?? 0),
		nonSilentFrameCount:
			(peak ?? 0) >= NON_SILENT_PEAK_THRESHOLD || (rms ?? 0) >= NON_SILENT_RMS_THRESHOLD
				? pumpStats.nonSilentFrameCount + 1
				: pumpStats.nonSilentFrameCount,
	};
}

function endPumpStats(captureId: string, reason: string, detail?: string, options: {overwrite?: boolean} = {}): void {
	if (pumpStats.captureId !== captureId) return;
	if (pumpStats.endedAt != null && !options.overwrite) return;
	pumpStats = {
		...pumpStats,
		active: false,
		endReason: reason,
		endDetail: detail ?? null,
		endedAt: Date.now(),
	};
}

export function getNativeEngineAudioTrackPumpStats(): NativeEngineAudioTrackPumpStats {
	return {...pumpStats};
}

function isAudioWorkletFrameMessage(message: unknown): message is AudioWorkletFrameMessage {
	if (!message || typeof message !== 'object') return false;
	const record = message as Record<string, unknown>;
	return (
		record.type === 'frame' &&
		typeof record.sampleRate === 'number' &&
		typeof record.channels === 'number' &&
		record.samples instanceof Float32Array
	);
}

async function installAudioWorkletModule(audioContext: AudioContext): Promise<void> {
	const module = new Blob([AUDIO_WORKLET_SOURCE], {type: 'text/javascript'});
	const moduleUrl = URL.createObjectURL(module);
	try {
		await audioContext.audioWorklet.addModule(moduleUrl);
	} finally {
		URL.revokeObjectURL(moduleUrl);
	}
}

export async function startNativeEngineAudioTrackFramePump(
	track: MediaStreamTrack,
	onFrame: (frame: NativeEngineAudioTrackFrame) => void,
	onEnd?: (message: NativeEngineAudioTrackEndMessage) => void,
): Promise<NativeAudioFramePump | null> {
	if (typeof window === 'undefined') return null;
	if (track.readyState === 'ended') return null;
	const AudioContextCtor = window.AudioContext ?? window.webkitAudioContext;
	if (!AudioContextCtor) {
		logger.warn('Cannot publish native-engine screen-share audio from track: AudioContext unavailable');
		return null;
	}

	const captureId = createCaptureId();
	startPumpStats(captureId);
	let audioContext: AudioContext | null = null;
	let source: MediaStreamAudioSourceNode | null = null;
	let workletNode: AudioWorkletNode | null = null;
	let scriptProcessor: ScriptProcessorNode | null = null;
	let mutedSink: GainNode | null = null;
	let cleanedUp = false;

	const cleanup = async (stopRemote: boolean = true): Promise<void> => {
		if (cleanedUp) return;
		cleanedUp = true;
		endPumpStats(captureId, stopRemote ? 'cleanup-stop-track' : 'cleanup');
		track.removeEventListener('ended', handleTrackEnded);
		if (workletNode) {
			workletNode.port.postMessage({type: 'stop'});
			workletNode.port.close();
			workletNode.disconnect();
		}
		if (scriptProcessor) {
			scriptProcessor.onaudioprocess = null;
			scriptProcessor.disconnect();
		}
		source?.disconnect();
		mutedSink?.disconnect();
		if (stopRemote) {
			try {
				track.stop();
			} catch (error) {
				logger.warn('Failed to stop native-engine screen-share audio track', {captureId, error});
			}
		}
		await audioContext?.close().catch((error) => {
			logger.warn('Failed to close native-engine screen-share audio context', {captureId, error});
		});
	};

	const forwardFrame = (frame: NativeEngineAudioTrackFrame): void => {
		recordPumpFrame(captureId, frame.samples);
		try {
			onFrame(frame);
		} catch (error) {
			frame.release?.();
			logger.warn('Failed to forward native-engine screen-share audio track frame', {captureId, error});
		}
	};

	const handleTrackEnded = (): void => {
		endPumpStats(captureId, 'track-ended');
		onEnd?.({captureId, reason: 'track-ended'});
		void cleanup(false);
	};

	try {
		audioContext = new AudioContextCtor({sampleRate: CAPTURE_SAMPLE_RATE});
		const stream = new MediaStream([track]);
		source = audioContext.createMediaStreamSource(stream);
		mutedSink = audioContext.createGain();
		mutedSink.gain.value = 0;
		const AudioWorkletNodeCtor = globalThis.AudioWorkletNode;
		if (audioContext.audioWorklet && AudioWorkletNodeCtor) {
			try {
				await installAudioWorkletModule(audioContext);
				workletNode = new AudioWorkletNodeCtor(audioContext, AUDIO_WORKLET_PROCESSOR_NAME, {
					numberOfInputs: 1,
					numberOfOutputs: 1,
					outputChannelCount: [1],
				});
				workletNode.port.onmessage = (event: MessageEvent<unknown>) => {
					if (cleanedUp || !isAudioWorkletFrameMessage(event.data)) return;
					const samples = event.data.samples;
					let released = false;
					const release = (): void => {
						if (released) return;
						released = true;
						try {
							workletNode?.port.postMessage({type: 'return-buffer', buffer: samples.buffer}, [samples.buffer]);
						} catch (error) {
							logger.debug('Failed to return native-engine screen-share audio worklet buffer', {captureId, error});
						}
					};
					forwardFrame({
						sampleRate: event.data.sampleRate || audioContext?.sampleRate || CAPTURE_SAMPLE_RATE,
						channels: event.data.channels,
						samples,
						release,
					});
				};
				workletNode.onprocessorerror = (event) => {
					logger.warn('Native-engine screen-share audio worklet failed', {captureId, event});
					endPumpStats(captureId, 'worklet-error');
					onEnd?.({captureId, reason: 'worklet-error'});
					void cleanup(false);
				};
				source.connect(workletNode);
				workletNode.connect(mutedSink);
			} catch (error) {
				logger.warn('Native-engine screen-share audio worklet unavailable; falling back to ScriptProcessor', {
					captureId,
					error,
				});
				workletNode?.disconnect();
				workletNode = null;
			}
		}
		if (!workletNode) {
			scriptProcessor = audioContext.createScriptProcessor(SCRIPT_PROCESSOR_BUFFER_SIZE, 1, 1);
			scriptProcessor.onaudioprocess = (event) => {
				if (cleanedUp) return;
				event.outputBuffer.getChannelData(0).fill(0);
				forwardFrame({
					sampleRate: event.inputBuffer.sampleRate || audioContext?.sampleRate || CAPTURE_SAMPLE_RATE,
					channels: 1,
					samples: event.inputBuffer.getChannelData(0),
				});
			};
			source.connect(scriptProcessor);
			scriptProcessor.connect(mutedSink);
		}
		mutedSink.connect(audioContext.destination);
		track.addEventListener('ended', handleTrackEnded);
		if (audioContext.state === 'suspended') {
			await audioContext.resume().catch((error) => {
				logger.warn('Failed to resume native-engine screen-share audio context', {captureId, error});
			});
		}
		return {captureId, sampleRate: audioContext.sampleRate || CAPTURE_SAMPLE_RATE, channels: 1, cleanup};
	} catch (error) {
		logger.warn('Cannot publish native-engine screen-share audio from track', {captureId, error});
		await cleanup(false);
		endPumpStats(captureId, 'start-failed', error instanceof Error ? error.message : String(error), {overwrite: true});
		return null;
	}
}
