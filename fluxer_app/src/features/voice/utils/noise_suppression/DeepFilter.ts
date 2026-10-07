// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {acquireIdleVoiceInputContext, isVoiceInputSourceLive} from '@app/features/voice/engine/VoiceInputAudioContext';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import NoiseSuppressionAvailability, {
	type NoiseSuppressionFailureReason,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionAvailability';
import {getNoiseSuppressionBackendDescriptor} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {readRequestedNoiseSuppressionBackend} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionRuntime';
import {getActiveVoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';

const logger = new Logger('DeepFilter');

const DEEP_FILTER_WASM_URL = new URL('./deepfilternet3/df_bg.wasm', import.meta.url).href;
const DEEP_FILTER_MODEL_URL = new URL('./deepfilternet3/DeepFilterNet3_onnx.tar.gz', import.meta.url).href;
const DEEP_FILTER_PROCESSOR_NAME = 'fluxer-deep-filter';
const DEEP_FILTER_ASSET_IDLE_MS = 20_000;
const DEEP_FILTER_ASSET_TOTAL_MS = 10 * 60_000;
const DEEP_FILTER_BUILD_TIMEOUT_MS = 8000;
const DEEP_FILTER_ATTEN_LIM_DB = 30;
const DEEP_FILTER_INPUT_GAIN = 10;
const DEEP_FILTER_OUTPUT_GAIN = 0.1;
const DEEP_FILTER_HIGH_PASS_HZ = 60;
const DEEP_FILTER_PREFETCH_IDLE_TIMEOUT_MS = 20_000;
const DEEP_FILTER_PRIMED_TIMEOUT_MS = 500;
const DEEP_FILTER_IDLE_DISPOSE_MS = 60_000;
const DEEP_FILTER_JOIN_WAIT_MS = 2000;
const LEVEL_FAULT_MIN_INPUT_RMS = 10 ** (-50 / 20);
const LEVEL_FAULT_REPORTS = 4;
const LIVENESS_CHECK_INTERVAL_MS = 250;
const LIVENESS_MAX_CHECK_DELAY_MS = 2000;
const LIVENESS_MAX_REPORT_GAP_SECONDS = 2;
const GZIP_MAGIC = [0x1f, 0x8b];
const TAR_MAGIC = [0x75, 0x73, 0x74, 0x61, 0x72];
const TAR_MAGIC_OFFSET = 257;

interface DeepFilterAssets {
	module: WebAssembly.Module;
	modelBytes: ArrayBuffer;
}

interface DeepFilterNodeHandle {
	node: AudioWorkletNode;
	flushAndAwaitPrimed: (signal: AbortSignal) => Promise<void>;
	watch: (onFault: (error: Error) => void) => () => void;
	dispose: () => void;
}

export interface DeepFilterNodeLease {
	node: AudioWorkletNode;
	builtOverLiveSource: boolean;
	primed: (signal: AbortSignal) => Promise<void>;
	release: () => void;
}

type DeepFilterWorkletMessage =
	| {type: 'ready'; frameLength: number}
	| {type: 'primed'}
	| {type: 'error'; message: string}
	| {type: 'health'; inputRms: number; outputRms: number; nonFinite: boolean; frame: number; contextTime: number};

class DeepFilterUnavailableError extends Error {
	constructor(
		readonly reason: NoiseSuppressionFailureReason,
		message: string,
	) {
		super(message);
		this.name = 'DeepFilterUnavailableError';
	}
}

function isDeepFilterWorkletMessage(value: unknown): value is DeepFilterWorkletMessage {
	if (!value || typeof value !== 'object') return false;
	const type = (value as {type?: unknown}).type;
	return type === 'ready' || type === 'primed' || type === 'error' || type === 'health';
}

function markDeepFilterFailed(reason: NoiseSuppressionFailureReason, error: unknown): void {
	logger.warn('DeepFilterNet is unavailable; using browser noise suppression', {reason, error});
	NoiseSuppressionAvailability.markBackendFailed('deep_filter', reason, Date.now());
}

function toDeepFilterBuildFailure(error: unknown): DeepFilterUnavailableError {
	if (error instanceof DeepFilterUnavailableError) return error;
	return new DeepFilterUnavailableError('build', error instanceof Error ? error.message : String(error));
}

function raceAbort<T>(pending: Promise<T>, signal: AbortSignal): Promise<T> {
	if (signal.aborted) return Promise.reject(signal.reason);
	return new Promise((resolve, reject) => {
		const onAbort = () => reject(signal.reason);
		signal.addEventListener('abort', onAbort, {once: true});
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

async function fetchAssetBody(url: string, controller: AbortController): Promise<ReadableStream<Uint8Array>> {
	let idleTimer: ReturnType<typeof setTimeout> | undefined;
	const stopIdleTimer = () => clearTimeout(idleTimer);
	const restartIdleTimer = () => {
		stopIdleTimer();
		idleTimer = setTimeout(() => {
			controller.abort(
				new DeepFilterUnavailableError('assets', `No data from ${url} for ${DEEP_FILTER_ASSET_IDLE_MS} ms`),
			);
		}, DEEP_FILTER_ASSET_IDLE_MS);
	};
	controller.signal.addEventListener('abort', stopIdleTimer, {once: true});
	restartIdleTimer();
	try {
		const response = await fetch(url, {credentials: 'same-origin', signal: controller.signal});
		if (!response.ok || !response.body) {
			throw new DeepFilterUnavailableError('assets', `${url} returned status ${response.status}`);
		}
		restartIdleTimer();
		return response.body.pipeThrough(
			new TransformStream<Uint8Array, Uint8Array>({
				transform(chunk, stream) {
					restartIdleTimer();
					stream.enqueue(chunk);
				},
				flush: stopIdleTimer,
			}),
		);
	} catch (error) {
		stopIdleTimer();
		throw error;
	}
}

async function compileDeepFilterWasm(body: ReadableStream<Uint8Array>): Promise<WebAssembly.Module> {
	const response = new Response(body, {headers: {'content-type': 'application/wasm'}});
	if (typeof WebAssembly.compileStreaming === 'function') return WebAssembly.compileStreaming(response);
	return WebAssembly.compile(await response.arrayBuffer());
}

function hasBytesAt(bytes: Uint8Array, offset: number, expected: ReadonlyArray<number>): boolean {
	return expected.every((value, index) => bytes[offset + index] === value);
}

async function readDeepFilterModel(body: ReadableStream<Uint8Array>): Promise<ArrayBuffer> {
	const buffer = await new Response(body).arrayBuffer();
	const bytes = new Uint8Array(buffer);
	if (hasBytesAt(bytes, 0, GZIP_MAGIC)) return buffer;
	if (hasBytesAt(bytes, TAR_MAGIC_OFFSET, TAR_MAGIC)) {
		throw new DeepFilterUnavailableError('model_encoding', 'DeepFilterNet model arrived decompressed');
	}
	throw new DeepFilterUnavailableError('assets', 'DeepFilterNet model is not a gzip archive');
}

async function fetchDeepFilterAssets(): Promise<DeepFilterAssets> {
	const controller = new AbortController();
	const totalTimer = setTimeout(() => {
		controller.abort(new DeepFilterUnavailableError('assets', 'DeepFilterNet assets took too long to download'));
	}, DEEP_FILTER_ASSET_TOTAL_MS);
	try {
		const [module, modelBytes] = await Promise.all([
			fetchAssetBody(DEEP_FILTER_WASM_URL, controller).then(compileDeepFilterWasm),
			fetchAssetBody(DEEP_FILTER_MODEL_URL, controller).then(readDeepFilterModel),
		]);
		return {module, modelBytes};
	} catch (error) {
		controller.abort(error);
		const cause: unknown = controller.signal.reason;
		if (cause instanceof DeepFilterUnavailableError) throw cause;
		throw new DeepFilterUnavailableError('assets', cause instanceof Error ? cause.message : String(cause));
	} finally {
		clearTimeout(totalTimer);
	}
}

let assetsLoad: Promise<DeepFilterAssets> | null = null;

function loadDeepFilterAssets(signal?: AbortSignal): Promise<DeepFilterAssets> {
	if (!assetsLoad) {
		const load = fetchDeepFilterAssets();
		assetsLoad = load;
		NoiseSuppressionAvailability.setDeepFilterAssetState('loading');
		load.then(
			() => {
				if (assetsLoad === load) NoiseSuppressionAvailability.setDeepFilterAssetState('ready');
			},
			(error: unknown) => {
				if (assetsLoad !== load) return;
				assetsLoad = null;
				markDeepFilterFailed(error instanceof DeepFilterUnavailableError ? error.reason : 'assets', error);
				NoiseSuppressionAvailability.setDeepFilterAssetState('idle');
			},
		);
	}
	return signal ? raceAbort(assetsLoad, signal) : assetsLoad;
}

function isDeepFilterRequested(): boolean {
	return (
		getActiveVoiceProcessingMode(VoiceSettings) !== 'studio' && readRequestedNoiseSuppressionBackend() === 'deep_filter'
	);
}

export function prefetchDeepFilterAssets(): void {
	if (!isDeepFilterRequested()) return;
	if (NoiseSuppressionAvailability.failures.get('deep_filter')?.active) return;
	if (NoiseSuppressionAvailability.deepFilterAssets !== 'idle') return;
	void loadDeepFilterAssets().catch(() => {});
}

export function scheduleDeepFilterPrefetch(): () => void {
	if (typeof window.requestIdleCallback === 'function') {
		const handle = window.requestIdleCallback(prefetchDeepFilterAssets, {
			timeout: DEEP_FILTER_PREFETCH_IDLE_TIMEOUT_MS,
		});
		return () => window.cancelIdleCallback(handle);
	}
	const handle = window.setTimeout(prefetchDeepFilterAssets, DEEP_FILTER_PREFETCH_IDLE_TIMEOUT_MS);
	return () => window.clearTimeout(handle);
}

const workletModules = new WeakMap<BaseAudioContext, Promise<void>>();

function addDeepFilterWorkletModule(context: BaseAudioContext): Promise<void> {
	let pending = workletModules.get(context);
	if (!pending) {
		pending = import('@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletAssets').then((assets) =>
			context.audioWorklet.addModule(assets.DEEP_FILTER_WORKLET_URL),
		);
		workletModules.set(context, pending);
		pending.catch(() => {
			if (workletModules.get(context) === pending) workletModules.delete(context);
		});
	}
	return pending;
}

type DeepFilterMessageListener = (message: DeepFilterWorkletMessage) => void;

function listenToDeepFilterNode(node: AudioWorkletNode): (listener: DeepFilterMessageListener) => () => void {
	const listeners = new Set<DeepFilterMessageListener>();
	node.port.onmessage = (event: MessageEvent) => {
		if (!isDeepFilterWorkletMessage(event.data)) return;
		for (const listener of [...listeners]) listener(event.data);
	};
	return (listener) => {
		listeners.add(listener);
		return () => {
			listeners.delete(listener);
		};
	};
}

function awaitDeepFilterReady(
	node: AudioWorkletNode,
	listen: (listener: DeepFilterMessageListener) => () => void,
	signal: AbortSignal,
): Promise<void> {
	return new Promise((resolve, reject) => {
		const finish = (error?: unknown) => {
			stopListening();
			node.onprocessorerror = null;
			signal.removeEventListener('abort', onAbort);
			if (error === undefined) resolve();
			else reject(error);
		};
		const onAbort = () => finish(signal.reason);
		signal.addEventListener('abort', onAbort, {once: true});
		const stopListening = listen((message) => {
			if (message.type === 'ready') finish();
			else if (message.type === 'error') {
				finish(new DeepFilterUnavailableError('build', `DeepFilterNet worklet failed: ${message.message}`));
			}
		});
		node.onprocessorerror = () =>
			finish(new DeepFilterUnavailableError('build', 'DeepFilterNet worklet raised an error'));
	});
}

const voiceInputLevelTimes = new WeakMap<BaseAudioContext, number>();

export function reportVoiceInputLevel(context: BaseAudioContext, contextTime: number): void {
	if (!Number.isFinite(contextTime)) return;
	voiceInputLevelTimes.set(context, contextTime);
}

function watchDeepFilterNode(
	context: BaseAudioContext,
	node: AudioWorkletNode,
	listen: (listener: DeepFilterMessageListener) => () => void,
	onFault: (error: Error) => void,
): () => void {
	let lastReportContextTime = context.currentTime;
	let levelFaultReports = 0;
	let lastCheckAtMs = performance.now();
	const checkLiveness = () => {
		const now = performance.now();
		const checkWasDelayed = now - lastCheckAtMs > LIVENESS_MAX_CHECK_DELAY_MS;
		lastCheckAtMs = now;
		if (checkWasDelayed) return;
		if (context.state !== 'running') {
			lastReportContextTime = context.currentTime;
			levelFaultReports = 0;
			return;
		}
		const levelTime = voiceInputLevelTimes.get(context);
		if (
			context.currentTime - lastReportContextTime > LIVENESS_MAX_REPORT_GAP_SECONDS &&
			levelTime !== undefined &&
			levelTime - lastReportContextTime > LIVENESS_MAX_REPORT_GAP_SECONDS &&
			context.currentTime - levelTime < LIVENESS_MAX_REPORT_GAP_SECONDS
		) {
			onFault(new Error('DeepFilterNet stopped processing audio'));
		}
	};
	const livenessTimer = setInterval(checkLiveness, LIVENESS_CHECK_INTERVAL_MS);
	const stopListening = listen((message) => {
		if (message.type === 'error') {
			onFault(new Error(`DeepFilterNet worklet failed: ${message.message}`));
			return;
		}
		if (message.type !== 'health') return;
		lastReportContextTime = message.contextTime;
		NoiseSuppressionAvailability.setDeepFilterLevels(context, message.inputRms, message.outputRms, message.contextTime);
		const silentOutput = message.outputRms === 0 || message.nonFinite;
		levelFaultReports = message.inputRms > LEVEL_FAULT_MIN_INPUT_RMS && silentOutput ? levelFaultReports + 1 : 0;
		if (levelFaultReports >= LEVEL_FAULT_REPORTS) onFault(new Error('DeepFilterNet output went silent'));
	});
	node.onprocessorerror = () => onFault(new Error('DeepFilterNet worklet raised an error'));
	return () => {
		clearInterval(livenessTimer);
		stopListening();
		node.onprocessorerror = null;
	};
}

function awaitDeepFilterPrimed(
	listen: (listener: DeepFilterMessageListener) => () => void,
	signal: AbortSignal,
): Promise<void> {
	return new Promise((resolve) => {
		const finish = () => {
			clearTimeout(timer);
			stopListening();
			signal.removeEventListener('abort', finish);
			resolve();
		};
		const timer = setTimeout(finish, DEEP_FILTER_PRIMED_TIMEOUT_MS);
		signal.addEventListener('abort', finish, {once: true});
		const stopListening = listen((message) => {
			if (message.type === 'primed') finish();
		});
	});
}

async function createDeepFilterNode(context: AudioContext): Promise<DeepFilterNodeHandle> {
	const isClosed = () => context.state === 'closed';
	if (isClosed()) throw new DOMException('Voice input AudioContext closed', 'AbortError');
	const supportedSampleRates = getNoiseSuppressionBackendDescriptor('deep_filter').supportedSampleRates ?? [];
	if (!supportedSampleRates.includes(context.sampleRate)) {
		const error = new DeepFilterUnavailableError('sample_rate', `DeepFilterNet cannot run at ${context.sampleRate} Hz`);
		markDeepFilterFailed(error.reason, error);
		throw error;
	}
	const assets = await loadDeepFilterAssets();
	const controller = new AbortController();
	const onStateChange = () => {
		if (isClosed()) {
			controller.abort(new DOMException('Voice input AudioContext closed', 'AbortError'));
		}
	};
	context.addEventListener('statechange', onStateChange);
	onStateChange();
	let runningMs = 0;
	let previousTick = performance.now();
	const deadline = setInterval(() => {
		const now = performance.now();
		if (context.state === 'running') runningMs += now - previousTick;
		previousTick = now;
		if (runningMs >= DEEP_FILTER_BUILD_TIMEOUT_MS) {
			controller.abort(new DeepFilterUnavailableError('build', 'DeepFilterNet did not start in time'));
		}
	}, 100);
	let node: AudioWorkletNode | null = null;
	let listen: ((listener: DeepFilterMessageListener) => () => void) | null = null;
	try {
		await raceAbort(addDeepFilterWorkletModule(context), controller.signal);
		node = new AudioWorkletNode(context, DEEP_FILTER_PROCESSOR_NAME, {
			numberOfInputs: 1,
			numberOfOutputs: 1,
			outputChannelCount: [1],
			channelCount: 1,
			channelCountMode: 'explicit',
			channelInterpretation: 'speakers',
			processorOptions: {
				wasmModule: assets.module,
				modelBytes: assets.modelBytes,
				attenLimDb: DEEP_FILTER_ATTEN_LIM_DB,
				inputGain: DEEP_FILTER_INPUT_GAIN,
				outputGain: DEEP_FILTER_OUTPUT_GAIN,
				highPassHz: DEEP_FILTER_HIGH_PASS_HZ,
			},
		});
		listen = listenToDeepFilterNode(node);
		await awaitDeepFilterReady(node, listen, controller.signal);
	} catch (error) {
		if (node) {
			node.port.postMessage({type: 'dispose'});
			node.port.close();
			node.disconnect();
		}
		if (isClosed()) throw new DOMException('Voice input AudioContext closed', 'AbortError');
		const failure = toDeepFilterBuildFailure(error);
		markDeepFilterFailed(failure.reason, failure);
		throw failure;
	} finally {
		clearInterval(deadline);
		context.removeEventListener('statechange', onStateChange);
	}
	NoiseSuppressionAvailability.clearBackendFailure('deep_filter');
	const readyNode = node;
	const readyListen = listen;
	return {
		node: readyNode,
		flushAndAwaitPrimed: (signal) => {
			const primed = awaitDeepFilterPrimed(readyListen, signal);
			readyNode.port.postMessage({type: 'flush'});
			return primed;
		},
		watch: (onFault) => watchDeepFilterNode(context, readyNode, readyListen, onFault),
		dispose: () => {
			readyNode.disconnect();
			readyNode.port.postMessage({type: 'dispose'});
			readyNode.port.close();
		},
	};
}

interface DeepFilterPoolEntry {
	readonly build: Promise<DeepFilterNodeHandle>;
	readonly stopWatchingContext: () => void;
	holder: symbol | null;
	idleTimer: ReturnType<typeof setTimeout> | undefined;
	faulted: boolean;
	disposed: boolean;
	builtOverLiveSource: boolean;
}

const deepFilterPools = new WeakMap<BaseAudioContext, Set<DeepFilterPoolEntry>>();

function getDeepFilterPool(context: BaseAudioContext): Set<DeepFilterPoolEntry> {
	const pool = deepFilterPools.get(context) ?? new Set<DeepFilterPoolEntry>();
	deepFilterPools.set(context, pool);
	return pool;
}

function disposePoolEntry(pool: Set<DeepFilterPoolEntry>, entry: DeepFilterPoolEntry): void {
	if (entry.disposed) return;
	entry.disposed = true;
	entry.faulted = true;
	entry.stopWatchingContext();
	clearTimeout(entry.idleTimer);
	pool.delete(entry);
	void entry.build.then(
		(handle) => handle.dispose(),
		() => {},
	);
}

function createPoolEntry(context: AudioContext, pool: Set<DeepFilterPoolEntry>): DeepFilterPoolEntry {
	const onStateChange = () => {
		if (context.state === 'closed') disposePoolEntry(pool, entry);
	};
	const entry: DeepFilterPoolEntry = {
		build: createDeepFilterNode(context),
		stopWatchingContext: () => context.removeEventListener('statechange', onStateChange),
		holder: null,
		idleTimer: undefined,
		faulted: false,
		disposed: false,
		builtOverLiveSource: false,
	};
	pool.add(entry);
	context.addEventListener('statechange', onStateChange);
	onStateChange();
	entry.build.then(
		() => {
			entry.builtOverLiveSource = isVoiceInputSourceLive();
		},
		() => {
			disposePoolEntry(pool, entry);
		},
	);
	return entry;
}

function releasePoolEntry(pool: Set<DeepFilterPoolEntry>, entry: DeepFilterPoolEntry, token: symbol): void {
	if (entry.holder !== token) return;
	entry.holder = null;
	if (entry.faulted) {
		disposePoolEntry(pool, entry);
		return;
	}
	entry.idleTimer = setTimeout(() => disposePoolEntry(pool, entry), DEEP_FILTER_IDLE_DISPOSE_MS);
}

export function hasIdleDeepFilterNode(context: BaseAudioContext): boolean {
	for (const entry of getDeepFilterPool(context)) {
		if (entry.holder === null && !entry.faulted) return true;
	}
	return false;
}

export async function acquireDeepFilterNode(
	context: AudioContext,
	signal: AbortSignal,
	onFault: () => void,
): Promise<DeepFilterNodeLease> {
	await loadDeepFilterAssets(signal);
	signal.throwIfAborted();
	const pool = getDeepFilterPool(context);
	const idle = [...pool].find((candidate) => candidate.holder === null && !candidate.faulted);
	const entry = idle ?? createPoolEntry(context, pool);
	const token = Symbol('deep-filter-lease');
	entry.holder = token;
	clearTimeout(entry.idleTimer);
	let handle: DeepFilterNodeHandle;
	try {
		handle = await raceAbort(entry.build, signal);
	} catch (error) {
		releasePoolEntry(pool, entry, token);
		throw error;
	}
	const builtOverLiveSource = entry.builtOverLiveSource;
	entry.builtOverLiveSource = false;
	const stopWatching = handle.watch((error) => {
		if (entry.holder !== token || entry.faulted) return;
		entry.faulted = true;
		pool.delete(entry);
		markDeepFilterFailed('runtime', error);
		onFault();
	});
	return {
		node: handle.node,
		builtOverLiveSource,
		primed: (primedSignal) => handle.flushAndAwaitPrimed(primedSignal),
		release: () => {
			if (entry.holder !== token) return;
			stopWatching();
			releasePoolEntry(pool, entry, token);
		},
	};
}

let microphoneSessionWarmup: Promise<void> | null = null;

async function buildIdleDeepFilterNode(context: AudioContext, signal?: AbortSignal): Promise<void> {
	const joinWait = new AbortController();
	const joinDeadline = setTimeout(() => joinWait.abort(), DEEP_FILTER_JOIN_WAIT_MS);
	const onAbort = () => joinWait.abort(signal?.reason);
	signal?.addEventListener('abort', onAbort, {once: true});
	if (signal?.aborted) onAbort();
	try {
		await loadDeepFilterAssets(joinWait.signal);
		if (isVoiceInputSourceLive()) return;
		const lease = await acquireDeepFilterNode(context, joinWait.signal, () => {});
		lease.release();
	} finally {
		clearTimeout(joinDeadline);
		signal?.removeEventListener('abort', onAbort);
	}
}

async function warmDeepFilterNode(signal?: AbortSignal): Promise<void> {
	const lease = acquireIdleVoiceInputContext();
	if (!lease) return;
	await buildIdleDeepFilterNode(lease.context, signal)
		.catch(() => {})
		.finally(() => lease.release());
}

export function beginMicrophoneSession(signal?: AbortSignal): void {
	NoiseSuppressionAvailability.releaseExpiredFailures(Date.now());
	prefetchDeepFilterAssets();
	if (!isDeepFilterRequested() || NoiseSuppressionAvailability.failures.get('deep_filter')?.active) return;
	const warmup = warmDeepFilterNode(signal);
	microphoneSessionWarmup = warmup;
	void warmup.finally(() => {
		if (microphoneSessionWarmup === warmup) microphoneSessionWarmup = null;
	});
}

export function awaitMicrophoneSessionWarmup(): Promise<void> {
	return microphoneSessionWarmup ?? Promise.resolve();
}
