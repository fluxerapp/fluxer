// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {detectWasmSimdSupport} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import type * as NoiseSuppressionWorkletAssets from '@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletAssets';
import {
	NOISE_SUPPRESSION_WORKLET_PROCESSOR_NAMES,
	type NoiseSuppressionWorkletBackend,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletTypes';

const logger = new Logger('NoiseSuppressionChain');
const NOISE_GATE_OPEN_THRESHOLD_DB = -41.6;
const NOISE_GATE_CLOSE_THRESHOLD_DB = -47.6;
const NOISE_GATE_HOLD_MS = 180;
const NOISE_SUPPRESSION_STARTUP_TIMEOUT_MS = 8000;

export interface NoiseSuppressionWorkletNode {
	node: AudioWorkletNode;
	dispose: () => void;
}

interface WorkletSignal {
	type: 'ready' | 'error';
	message?: string;
}

export interface NoiseSuppressionWorkletNodeOptions {
	signal: AbortSignal;
	onRuntimeFailure: (error: Error) => void;
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
	wasmBinary: ArrayBuffer | null,
): Record<string, unknown> {
	if (backend === 'gate') {
		return {
			openThreshold: NOISE_GATE_OPEN_THRESHOLD_DB,
			closeThreshold: NOISE_GATE_CLOSE_THRESHOLD_DB,
			holdMs: NOISE_GATE_HOLD_MS,
			maxChannels: 1,
		};
	}
	return {maxChannels: 1, wasmBinary};
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

const workletModules = new WeakMap<BaseAudioContext, Map<NoiseSuppressionWorkletBackend, Promise<void>>>();

function addWorkletModule(
	context: BaseAudioContext,
	backend: NoiseSuppressionWorkletBackend,
	url: string,
): Promise<void> {
	const modules = workletModules.get(context) ?? new Map<NoiseSuppressionWorkletBackend, Promise<void>>();
	workletModules.set(context, modules);
	let pending = modules.get(backend);
	if (!pending) {
		pending = context.audioWorklet.addModule(url);
		modules.set(backend, pending);
		const added = pending;
		added.catch(() => {
			if (modules.get(backend) === added) modules.delete(backend);
		});
	}
	return pending;
}

function closeWorkletNode(node: AudioWorkletNode): void {
	node.port.onmessage = null;
	node.onprocessorerror = null;
	try {
		node.port.postMessage('destroy');
		node.port.close();
	} catch (error) {
		logger.debug('Failed to close noise suppression worklet port', error);
	}
	node.disconnect();
}

async function startWorkletNode(
	context: AudioContext,
	backend: NoiseSuppressionWorkletBackend,
	signal: AbortSignal,
): Promise<AudioWorkletNode> {
	const assets = await awaitStartupStep(
		import('@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletAssets'),
		signal,
	);
	const wasmUrl = resolveWasmUrl(assets, backend);
	const [, wasmBinary] = await awaitStartupStep(
		Promise.all([
			addWorkletModule(context, backend, assets.NOISE_SUPPRESSION_WORKLET_MODULE_URLS[backend]),
			wasmUrl === null ? Promise.resolve(null) : fetchWasmBinary(wasmUrl, signal),
		]),
		signal,
	);
	signal.throwIfAborted();
	const node = new AudioWorkletNode(context, NOISE_SUPPRESSION_WORKLET_PROCESSOR_NAMES[backend], {
		numberOfInputs: 1,
		numberOfOutputs: 1,
		outputChannelCount: [1],
		channelCount: 1,
		channelCountMode: 'explicit',
		channelInterpretation: 'speakers',
		processorOptions: buildProcessorOptions(backend, wasmBinary),
	});
	try {
		await awaitWorkletReady(node, backend, signal);
	} catch (error) {
		closeWorkletNode(node);
		throw error;
	}
	return node;
}

export async function createNoiseSuppressionWorkletNode(
	context: AudioContext,
	backend: NoiseSuppressionWorkletBackend,
	options: NoiseSuppressionWorkletNodeOptions,
): Promise<NoiseSuppressionWorkletNode> {
	options.signal.throwIfAborted();
	const controller = new AbortController();
	const onAbort = () => controller.abort(options.signal.reason);
	options.signal.addEventListener('abort', onAbort, {once: true});
	const timeoutId = window.setTimeout(() => {
		controller.abort(new Error(`Noise suppression worklet "${backend}" startup timed out`));
	}, NOISE_SUPPRESSION_STARTUP_TIMEOUT_MS);
	let node: AudioWorkletNode;
	try {
		node = await startWorkletNode(context, backend, controller.signal);
	} finally {
		window.clearTimeout(timeoutId);
		options.signal.removeEventListener('abort', onAbort);
	}
	let disposed = false;
	const reportFailure = (error: Error) => {
		if (disposed) return;
		options.onRuntimeFailure(error);
	};
	node.port.onmessage = (event: MessageEvent) => {
		if (!isWorkletSignal(event.data) || event.data.type !== 'error') return;
		reportFailure(new Error(`Noise suppression worklet "${backend}" failed: ${event.data.message ?? 'unknown error'}`));
	};
	node.onprocessorerror = () =>
		reportFailure(new Error(`Noise suppression worklet "${backend}" raised a processor error`));
	return {
		node,
		dispose: () => {
			if (disposed) return;
			disposed = true;
			closeWorkletNode(node);
		},
	};
}
