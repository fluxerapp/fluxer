// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {createVoiceAudioContext} from '@app/features/voice/engine/VoiceSharedAudioContext';
import {
	detectWasmSimdSupport,
	resolveNoiseSuppressionContextSampleRate,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {resolveNoiseGateTuning} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionGateTuning';
import {
	NOISE_SUPPRESSION_WORKLET_PROCESSOR_NAMES,
	type NoiseSuppressionWorkletBackend,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletTypes';

const logger = new Logger('NoiseSuppressionChain');
const WORKLET_READY_TIMEOUT_MS = 8000;

export interface NoiseSuppressionWorkletChain {
	inputDestination: MediaStreamAudioDestinationNode;
	processedTrack: MediaStreamTrack;
	dispose: () => Promise<void>;
}

interface WorkletSignal {
	type: 'ready' | 'error';
	message?: string;
}

function isWorkletSignal(value: unknown): value is WorkletSignal {
	if (!value || typeof value !== 'object') return false;
	const record = value as Record<string, unknown>;
	return record.type === 'ready' || record.type === 'error';
}

const registeredModules = new WeakMap<BaseAudioContext, Map<string, Promise<void>>>();

function ensureWorkletModule(audioContext: BaseAudioContext, moduleUrl: string): Promise<void> {
	let perContext = registeredModules.get(audioContext);
	if (!perContext) {
		perContext = new Map();
		registeredModules.set(audioContext, perContext);
	}
	const existing = perContext.get(moduleUrl);
	if (existing) return existing;
	const pending = audioContext.audioWorklet.addModule(moduleUrl).catch((error: unknown) => {
		perContext?.delete(moduleUrl);
		throw error;
	});
	perContext.set(moduleUrl, pending);
	return pending;
}

const wasmBinaries = new Map<string, Promise<ArrayBuffer>>();

function fetchWasmBinary(url: string): Promise<ArrayBuffer> {
	const existing = wasmBinaries.get(url);
	if (existing) return existing;
	const pending = fetch(url, {credentials: 'same-origin'})
		.then((response) => {
			if (!response.ok) {
				throw new Error(`Noise suppression asset request failed with status ${response.status}`);
			}
			return response.arrayBuffer();
		})
		.catch((error: unknown) => {
			wasmBinaries.delete(url);
			throw error;
		});
	wasmBinaries.set(url, pending);
	return pending;
}

type NoiseSuppressionAssetModule =
	typeof import('@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletAssets');

function resolveWasmUrl(assets: NoiseSuppressionAssetModule, backend: NoiseSuppressionWorkletBackend): string | null {
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

function awaitWorkletReady(node: AudioWorkletNode, backend: NoiseSuppressionWorkletBackend): Promise<void> {
	return new Promise((resolve, reject) => {
		let settled = false;
		const finish = (error: Error | null) => {
			if (settled) return;
			settled = true;
			window.clearTimeout(timeoutId);
			if (error) reject(error);
			else resolve();
		};
		const timeoutId = window.setTimeout(() => {
			finish(new Error(`Noise suppression worklet "${backend}" did not report ready in time`));
		}, WORKLET_READY_TIMEOUT_MS);
		node.port.onmessage = (event: MessageEvent) => {
			if (!isWorkletSignal(event.data)) return;
			if (event.data.type === 'ready') {
				finish(null);
				return;
			}
			finish(new Error(`Noise suppression worklet "${backend}" failed: ${event.data.message ?? 'unknown error'}`));
		};
		node.port.start();
	});
}

export async function buildNoiseSuppressionWorkletChain(opts: {
	audioContext: AudioContext;
	backend: NoiseSuppressionWorkletBackend;
	suppressionStrength: number;
	onRuntimeFailure?: (error: Error) => void;
}): Promise<NoiseSuppressionWorkletChain> {
	const {audioContext, backend, suppressionStrength} = opts;
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
	const teardown = async (
		node?: AudioWorkletNode,
		source?: AudioNode,
		output?: AudioNode,
		track?: MediaStreamTrack,
	) => {
		if (node) {
			try {
				node.port.postMessage('destroy');
			} catch {}
			node.port.onmessage = null;
			node.onprocessorerror = null;
		}
		safeDisconnect(node);
		safeDisconnect(source);
		safeDisconnect(output);
		safeDisconnect(inputDestination);
		safeStopTrack(track);
		safeStopTrack(feedTrack);
		if (workletContext !== audioContext) {
			await workletContext.close().catch((error: unknown) => {
				logger.debug('Failed to close noise suppression audio context', error);
			});
		}
	};
	let node: AudioWorkletNode | null = null;
	let sourceNode: MediaStreamAudioSourceNode | null = null;
	let outputDestination: MediaStreamAudioDestinationNode | null = null;
	try {
		const assets = await import('@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletAssets');
		const wasmUrl = resolveWasmUrl(assets, backend);
		const [, wasmBinary] = await Promise.all([
			ensureWorkletModule(workletContext, assets.NOISE_SUPPRESSION_WORKLET_MODULE_URLS[backend]),
			wasmUrl == null ? Promise.resolve(null) : fetchWasmBinary(wasmUrl),
		]);
		sourceNode = workletContext.createMediaStreamSource(new MediaStream([feedTrack]));
		node = new AudioWorkletNode(workletContext, NOISE_SUPPRESSION_WORKLET_PROCESSOR_NAMES[backend], {
			numberOfInputs: 1,
			numberOfOutputs: 1,
			outputChannelCount: [1],
			channelCount: 1,
			channelCountMode: 'explicit',
			channelInterpretation: 'speakers',
			processorOptions: buildProcessorOptions(backend, suppressionStrength, wasmBinary),
		});
		await awaitWorkletReady(node, backend);
		outputDestination = workletContext.createMediaStreamDestination();
		outputDestination.channelCount = 1;
		outputDestination.channelCountMode = 'explicit';
		outputDestination.channelInterpretation = 'speakers';
		sourceNode.connect(node);
		node.connect(outputDestination);
		const processedTrack = outputDestination.stream.getAudioTracks()[0];
		if (!processedTrack) {
			throw new Error('buildNoiseSuppressionWorkletChain: missing processed output track');
		}
		const readyNode = node;
		let disposed = false;
		readyNode.port.onmessage = (event: MessageEvent) => {
			if (!isWorkletSignal(event.data) || event.data.type !== 'error' || disposed) return;
			opts.onRuntimeFailure?.(
				new Error(`Noise suppression worklet "${backend}" failed: ${event.data.message ?? 'unknown error'}`),
			);
		};
		readyNode.onprocessorerror = () => {
			if (disposed) return;
			opts.onRuntimeFailure?.(new Error(`Noise suppression worklet "${backend}" raised a processor error`));
		};
		return {
			inputDestination,
			processedTrack,
			dispose: async () => {
				if (disposed) return;
				disposed = true;
				await teardown(readyNode, sourceNode ?? undefined, outputDestination ?? undefined, processedTrack);
			},
		};
	} catch (error) {
		await teardown(node ?? undefined, sourceNode ?? undefined, outputDestination ?? undefined);
		throw error;
	}
}
