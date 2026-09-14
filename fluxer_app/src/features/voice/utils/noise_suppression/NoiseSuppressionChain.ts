// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {createVoiceAudioContext} from '@app/features/voice/engine/VoiceSharedAudioContext';
import {
	detectWasmSimdSupport,
	resolveNoiseSuppressionContextSampleRate,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {resolveNoiseGateTuning} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionGateTuning';
import type * as NoiseSuppressionWorkletAssets from '@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletAssets';
import {
	NOISE_SUPPRESSION_WORKLET_PROCESSOR_NAMES,
	type NoiseSuppressionWorkletBackend,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletTypes';

const logger = new Logger('NoiseSuppressionChain');
export const NOISE_SUPPRESSION_STARTUP_TIMEOUT_MS = 8000;

export interface NoiseSuppressionWorkletChain {
	inputDestination: MediaStreamAudioDestinationNode;
	processedTrack: MediaStreamTrack;
	dispose: () => Promise<void>;
}

interface WorkletSignal {
	type: 'ready' | 'error';
	message?: string;
}

export interface NoiseSuppressionWorkletOptions {
	audioContext: AudioContext;
	backend: NoiseSuppressionWorkletBackend;
	suppressionStrength: number;
	signal?: AbortSignal;
	onRuntimeFailure?: (error: Error) => void;
}

interface WorkletResources {
	inputDestination: MediaStreamAudioDestinationNode;
	feedTrack: MediaStreamTrack;
	context: AudioContext;
	node?: AudioWorkletNode;
	source?: MediaStreamAudioSourceNode;
	output?: MediaStreamAudioDestinationNode;
	processedTrack?: MediaStreamTrack;
	disposal?: Promise<void>;
}

function isWorkletSignal(value: unknown): value is WorkletSignal {
	if (!value || typeof value !== 'object') return false;
	const record = value as Record<string, unknown>;
	if (record.message !== undefined && typeof record.message !== 'string') return false;
	return record.type === 'ready' || record.type === 'error';
}

const wasmBinaries = new Map<string, ArrayBuffer>();

async function fetchWasmBinary(url: string, signal: AbortSignal): Promise<ArrayBuffer> {
	const existing = wasmBinaries.get(url);
	if (existing) return existing;
	const response = await fetch(url, {credentials: 'same-origin', signal});
	if (!response.ok) {
		throw new Error(`Noise suppression asset request failed with status ${response.status}`);
	}
	const binary = await response.arrayBuffer();
	signal.throwIfAborted();
	wasmBinaries.set(url, binary);
	return binary;
}

function resolveWasmUrl(
	assets: typeof NoiseSuppressionWorkletAssets,
	backend: NoiseSuppressionWorkletBackend,
): string | null {
	switch (backend) {
		case 'gate':
			return null;
		case 'speex':
			return assets.NOISE_SUPPRESSION_WASM_URLS.speex;
		case 'gtcrn':
			return assets.NOISE_SUPPRESSION_WASM_URLS.gtcrn;
		case 'rnnoise':
			return detectWasmSimdSupport()
				? assets.NOISE_SUPPRESSION_WASM_URLS.rnnoiseSimd
				: assets.NOISE_SUPPRESSION_WASM_URLS.rnnoise;
	}
}

function buildProcessorOptions(
	backend: NoiseSuppressionWorkletBackend,
	suppressionStrength: number,
	wasmBinary: ArrayBuffer | null,
): Record<string, unknown> {
	if (backend === 'gate') {
		const tuning = resolveNoiseGateTuning(suppressionStrength);
		return {
			openThreshold: tuning.openThreshold,
			closeThreshold: tuning.closeThreshold,
			holdMs: tuning.holdMs,
			maxChannels: 1,
		};
	}
	return {maxChannels: 1, wasmBinary};
}

function safeDisconnect(node: AudioNode | null | undefined): void {
	if (!node) return;
	try {
		node.disconnect();
	} catch {}
}

function safeStopTrack(track: MediaStreamTrack | null | undefined): void {
	if (!track) return;
	try {
		track.stop();
	} catch {}
}

export function resolveNoiseSuppressionWorkletContext(
	backend: NoiseSuppressionWorkletBackend,
	captureContext: AudioContext,
	feedTrack: MediaStreamTrack,
): AudioContext | null {
	const targetSampleRate = resolveNoiseSuppressionContextSampleRate(backend, captureContext.sampleRate);
	if (targetSampleRate === captureContext.sampleRate) {
		return createVoiceAudioContext({latencyHint: 'interactive', sampleRate: captureContext.sampleRate});
	}
	const bridged = createVoiceAudioContext({latencyHint: 'interactive', sampleRate: targetSampleRate});
	if (!bridged) return null;
	if (bridged.sampleRate !== targetSampleRate) {
		void bridged.close().catch(() => undefined);
		return null;
	}
	try {
		bridged.createMediaStreamSource(new MediaStream([feedTrack])).disconnect();
		return bridged;
	} catch (error) {
		logger.info('Noise suppression cannot read the capture graph at the model rate', {
			backend,
			targetSampleRate,
			captureSampleRate: captureContext.sampleRate,
			error,
		});
	}
	void bridged.close().catch(() => undefined);
	return null;
}

function awaitStartupStep<T>(pending: Promise<T>, signal: AbortSignal): Promise<T> {
	return new Promise((resolve, reject) => {
		const onAbort = () => reject(signal.reason);
		if (signal.aborted) {
			reject(signal.reason);
		} else {
			signal.addEventListener('abort', onAbort, {once: true});
		}
		pending.then(
			(value) => {
				signal.removeEventListener('abort', onAbort);
				resolve(value);
			},
			(error: unknown) => {
				signal.removeEventListener('abort', onAbort);
				reject(error);
			},
		);
	});
}

function awaitWorkletReady(
	node: AudioWorkletNode,
	backend: NoiseSuppressionWorkletBackend,
	signal: AbortSignal,
): Promise<void> {
	return new Promise((resolve, reject) => {
		let settled = false;
		const finish = (error?: unknown) => {
			if (settled) return;
			settled = true;
			node.port.onmessage = null;
			node.onprocessorerror = null;
			signal.removeEventListener('abort', onAbort);
			if (error !== undefined) reject(error);
			else resolve();
		};
		const onAbort = () => finish(signal.reason);
		if (signal.aborted) {
			onAbort();
			return;
		}
		signal.addEventListener('abort', onAbort, {once: true});
		node.port.onmessage = (event: MessageEvent) => {
			if (!isWorkletSignal(event.data)) return;
			if (event.data.type === 'ready') {
				finish();
				return;
			}
			finish(new Error(`Noise suppression worklet "${backend}" failed: ${event.data.message ?? 'unknown error'}`));
		};
		node.onprocessorerror = () => finish(new Error(`Noise suppression worklet "${backend}" raised a processor error`));
		node.port.start();
	});
}

function createWorkletResources(audioContext: AudioContext, backend: NoiseSuppressionWorkletBackend): WorkletResources {
	const inputDestination = audioContext.createMediaStreamDestination();
	inputDestination.channelCount = 1;
	inputDestination.channelCountMode = 'explicit';
	inputDestination.channelInterpretation = 'speakers';
	const feedTrack = inputDestination.stream.getAudioTracks()[0];
	if (!feedTrack) {
		safeDisconnect(inputDestination);
		throw new Error('buildNoiseSuppressionWorkletChain: missing capture feed track');
	}
	const workletContext = resolveNoiseSuppressionWorkletContext(backend, audioContext, feedTrack);
	if (!workletContext) {
		safeDisconnect(inputDestination);
		safeStopTrack(feedTrack);
		throw new Error(`buildNoiseSuppressionWorkletChain: no usable audio context for "${backend}"`);
	}
	return {inputDestination, feedTrack, context: workletContext};
}

async function releaseWorkletResources(resources: WorkletResources): Promise<void> {
	const {node, source, output, inputDestination, feedTrack, processedTrack, context} = resources;
	if (node) {
		node.port.onmessage = null;
		node.onprocessorerror = null;
		try {
			node.port.postMessage('destroy');
			node.port.close();
		} catch (error) {
			logger.debug('Failed to close noise suppression worklet port', error);
		}
	}
	safeDisconnect(node);
	safeDisconnect(source);
	safeDisconnect(output);
	safeDisconnect(inputDestination);
	safeStopTrack(processedTrack);
	safeStopTrack(feedTrack);
	try {
		await context.close();
	} catch (error) {
		logger.debug('Failed to close noise suppression audio context', error);
	}
}

function disposeWorkletResources(resources: WorkletResources): Promise<void> {
	resources.disposal ??= releaseWorkletResources(resources);
	return resources.disposal;
}

async function installWorklet(
	resources: WorkletResources,
	options: NoiseSuppressionWorkletOptions,
	signal: AbortSignal,
): Promise<void> {
	const {backend, suppressionStrength} = options;
	const assets = await awaitStartupStep(
		import('@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletAssets'),
		signal,
	);
	signal.throwIfAborted();
	const wasmUrl = resolveWasmUrl(assets, backend);
	const [, wasmBinary] = await awaitStartupStep(
		Promise.all([
			resources.context.audioWorklet.addModule(assets.NOISE_SUPPRESSION_WORKLET_MODULE_URLS[backend]),
			wasmUrl === null ? Promise.resolve(null) : fetchWasmBinary(wasmUrl, signal),
		]),
		signal,
	);
	signal.throwIfAborted();
	resources.source = resources.context.createMediaStreamSource(new MediaStream([resources.feedTrack]));
	const node = new AudioWorkletNode(resources.context, NOISE_SUPPRESSION_WORKLET_PROCESSOR_NAMES[backend], {
		numberOfInputs: 1,
		numberOfOutputs: 1,
		outputChannelCount: [1],
		channelCount: 1,
		channelCountMode: 'explicit',
		channelInterpretation: 'speakers',
		processorOptions: buildProcessorOptions(backend, suppressionStrength, wasmBinary),
	});
	resources.node = node;
	await awaitWorkletReady(node, backend, signal);
	signal.throwIfAborted();
	const output = resources.context.createMediaStreamDestination();
	resources.output = output;
	resources.processedTrack = output.stream.getAudioTracks()[0];
	if (!resources.processedTrack) {
		throw new Error('buildNoiseSuppressionWorkletChain: missing processed output track');
	}
	output.channelCount = 1;
	output.channelCountMode = 'explicit';
	output.channelInterpretation = 'speakers';
	resources.source.connect(node);
	node.connect(output);
	let failed = false;
	const reportFailure = (error: Error) => {
		if (failed || resources.disposal) return;
		failed = true;
		options.onRuntimeFailure?.(error);
	};
	node.port.onmessage = (event: MessageEvent) => {
		if (!isWorkletSignal(event.data) || event.data.type !== 'error') return;
		reportFailure(new Error(`Noise suppression worklet "${backend}" failed: ${event.data.message ?? 'unknown error'}`));
	};
	node.onprocessorerror = () =>
		reportFailure(new Error(`Noise suppression worklet "${backend}" raised a processor error`));
}

export async function buildNoiseSuppressionWorkletChain(
	options: NoiseSuppressionWorkletOptions,
): Promise<NoiseSuppressionWorkletChain> {
	options.signal?.throwIfAborted();
	const controller = new AbortController();
	const onAbort = () => controller.abort(options.signal?.reason);
	options.signal?.addEventListener('abort', onAbort, {once: true});
	const timeoutId = window.setTimeout(() => {
		controller.abort(new Error(`Noise suppression worklet "${options.backend}" startup timed out`));
	}, NOISE_SUPPRESSION_STARTUP_TIMEOUT_MS);
	let resources: WorkletResources | undefined;
	try {
		resources = createWorkletResources(options.audioContext, options.backend);
		await installWorklet(resources, options, controller.signal);
		const {inputDestination, processedTrack} = resources;
		if (!processedTrack) {
			throw new Error('buildNoiseSuppressionWorkletChain: missing processed output track');
		}
		const readyResources = resources;
		return {
			inputDestination,
			processedTrack,
			dispose: () => disposeWorkletResources(readyResources),
		};
	} catch (error) {
		controller.abort(error);
		if (resources) void disposeWorkletResources(resources);
		throw error;
	} finally {
		window.clearTimeout(timeoutId);
		options.signal?.removeEventListener('abort', onAbort);
	}
}
