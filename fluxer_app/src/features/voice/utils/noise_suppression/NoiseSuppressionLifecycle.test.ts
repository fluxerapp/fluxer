import {
	buildNoiseSuppressionWorkletChain,
	NOISE_SUPPRESSION_STARTUP_TIMEOUT_MS,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionChain';
import {removeVoiceInputProcessor, syncVoiceInputProcessor} from '@app/features/voice/utils/VoiceInputProcessor';
import type {LocalAudioTrack, Track, TrackProcessor} from 'livekit-client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const settings = vi.hoisted(() => ({configVersion: 0, inputVolume: 150, warn: vi.fn()}));

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug() {}
		info() {}
		warn = settings.warn;
	},
}));
vi.mock('@app/features/input/state/InputKeybind', () => ({default: {transmitMode: 'push_to_talk'}}));
vi.mock('@app/features/voice/state/VoiceSettings', () => ({default: {getInputVolume: () => settings.inputVolume}}));
vi.mock('@app/features/voice/utils/VoiceProcessingProfile', () => ({
	getActiveInputDeviceLabel: () => 'microphone',
	resolveVoiceProcessingFromStateForDeviceLabel: () => ({
		mode: 'custom',
		echoCancellation: false,
		browserNoiseSuppression: false,
		autoGainControl: false,
		deepFilter: false,
		deepFilterNoiseReductionLevel: 0,
		contentHint: '',
		noiseSuppressionBackend: 'none',
		stereoCapture: false,
	}),
}));
vi.mock('@app/features/voice/utils/noise_suppression/NoiseSuppressionRuntime', () => ({
	readEffectiveNoiseSuppression: () => ({
		rolloutApplied: true,
		backend: 'gate',
		requestedBackend: 'gate',
		source: 'global',
		suppressionStrength: 80,
		stereoEnabled: false,
		configVersion: settings.configVersion,
	}),
}));
vi.mock('@app/features/voice/engine/v2/VoiceEngineV2AppMicrophoneTransaction', () => ({
	computeSpeakingDetectorRms: () => 0,
}));
vi.mock('@app/features/voice/utils/DeepFilterNoiseProcessor', () => ({
	buildDeepFilterAudioChain: () => {
		throw new Error('DeepFilter is outside this worklet lifecycle test');
	},
}));
vi.mock('@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletAssets', () => ({
	NOISE_SUPPRESSION_WORKLET_MODULE_URLS: {
		gate: '/gate.js',
		speex: '/speex.js',
		rnnoise: '/rnnoise.js',
		gtcrn: '/gtcrn.js',
	},
	NOISE_SUPPRESSION_WASM_URLS: {
		speex: '/speex.wasm',
		rnnoise: '/rnnoise.wasm',
		rnnoiseSimd: '/rnnoise_simd.wasm',
		gtcrn: '/gtcrn.wasm',
	},
}));

interface Deferred<T> {
	promise: Promise<T>;
	resolve: (value: T) => void;
	reject: (error: unknown) => void;
}

function deferred<T>(): Deferred<T> {
	let resolve!: (value: T) => void;
	let reject!: (error: unknown) => void;
	const promise = new Promise<T>((onResolve, onReject) => {
		resolve = onResolve;
		reject = onReject;
	});
	return {promise, resolve, reject};
}

class FakeTrack {
	readyState: MediaStreamTrackState = 'live';
	enabled = true;
	stop = vi.fn(() => {
		this.readyState = 'ended';
	});
}

class FakeStream {
	constructor(private readonly tracks: Array<FakeTrack>) {}
	getAudioTracks(): Array<FakeTrack> {
		return this.tracks;
	}
}

class FakeNode {
	channelCount = 2;
	channelCountMode = 'max';
	channelInterpretation = 'speakers';
	gain = {value: 1};
	connect = vi.fn();
	disconnect = vi.fn();
}

class FakeDestination extends FakeNode {
	readonly track = new FakeTrack();
	readonly stream = new FakeStream([this.track]);
}

let moduleRequested: Deferred<void>;
let moduleResult: Promise<void>;
let nodeCreated: Deferred<FakeWorklet>;
let failOutputConnection: boolean;
const contexts: Array<FakeContext> = [];
const worklets: Array<FakeWorklet> = [];

class FakeContext {
	readonly sampleRate = 48000;
	readonly destinations: Array<FakeDestination> = [];
	readonly sources: Array<FakeNode> = [];
	readonly close = vi.fn(async () => {});
	readonly audioWorklet = {
		addModule: vi.fn(() => {
			moduleRequested.resolve(undefined);
			return moduleResult;
		}),
	};
	constructor() {
		contexts.push(this);
	}
	createMediaStreamDestination(): FakeDestination {
		const destination = new FakeDestination();
		this.destinations.push(destination);
		return destination;
	}
	createMediaStreamSource(): FakeNode {
		const source = new FakeNode();
		this.sources.push(source);
		return source;
	}
	createGain(): FakeNode {
		return new FakeNode();
	}
}

class FakeWorklet extends FakeNode {
	onprocessorerror: (() => void) | null = null;
	readonly port = {
		onmessage: null as ((event: {data: unknown}) => void) | null,
		start: vi.fn(),
		postMessage: vi.fn(),
		close: vi.fn(),
	};
	constructor() {
		super();
		if (failOutputConnection) {
			this.connect.mockImplementation(() => {
				throw new Error('Output connection failed');
			});
		}
		worklets.push(this);
		nodeCreated.resolve(this);
	}
	signal(data: unknown): void {
		this.port.onmessage?.({data});
	}
}

class FakeLocalTrack {
	readonly rawTrack = new FakeTrack();
	readonly context = new FakeContext();
	processor?: TrackProcessor<Track.Kind.Audio>;
	installCount = 0;
	initializationBarrier?: Promise<void>;
	readonly initializationStarted = deferred<void>();
	private operation: Promise<void> = Promise.resolve();

	private exclusive<T>(operation: () => Promise<T>): Promise<T> {
		const result = this.operation.then(operation);
		this.operation = result.then(
			() => {},
			() => {},
		);
		return result;
	}

	setProcessor(processor: TrackProcessor<Track.Kind.Audio>): Promise<void> {
		return this.exclusive(async () => {
			this.installCount++;
			this.initializationStarted.resolve(undefined);
			if (this.initializationBarrier) await this.initializationBarrier;
			await processor.init({
				kind: 'audio' as Track.Kind.Audio,
				track: this.rawTrack as unknown as MediaStreamTrack,
				audioContext: this.context as unknown as AudioContext,
			});
			this.processor = processor;
		});
	}

	stopProcessorIfCurrent(processor: TrackProcessor<Track.Kind.Audio>): Promise<boolean> {
		return this.exclusive(async () => {
			if (this.processor !== processor) return false;
			this.processor = undefined;
			await processor.destroy();
			return true;
		});
	}

	asLocalAudioTrack(): LocalAudioTrack {
		return this as unknown as LocalAudioTrack;
	}
}

function captureContext(): AudioContext {
	return new FakeContext() as unknown as AudioContext;
}

beforeEach(() => {
	vi.useFakeTimers();
	settings.configVersion++;
	settings.warn.mockClear();
	contexts.length = 0;
	worklets.length = 0;
	moduleRequested = deferred<void>();
	moduleResult = Promise.resolve();
	nodeCreated = deferred<FakeWorklet>();
	failOutputConnection = false;
	vi.stubGlobal('window', {AudioContext: FakeContext, setTimeout, clearTimeout, setInterval, clearInterval});
	vi.stubGlobal('AudioWorkletNode', FakeWorklet);
	vi.stubGlobal('MediaStream', FakeStream);
	vi.stubGlobal(
		'fetch',
		vi.fn(() => Promise.reject(new Error('Unexpected asset request'))),
	);
});

afterEach(async () => {
	await removeVoiceInputProcessor();
	expect(vi.getTimerCount()).toBe(0);
	vi.unstubAllGlobals();
	vi.useRealTimers();
});

describe('worklet startup ownership', () => {
	it('does not allocate resources for an already-cancelled startup', async () => {
		const audioContext = captureContext();
		const controller = new AbortController();
		controller.abort(new Error('Cancelled before setup'));
		await expect(
			buildNoiseSuppressionWorkletChain({
				audioContext,
				backend: 'gate',
				suppressionStrength: 80,
				signal: controller.signal,
			}),
		).rejects.toThrow('Cancelled before setup');
		expect(contexts).toHaveLength(1);
		expect(contexts[0].destinations).toHaveLength(0);
	});

	it('bounds module loading and ignores its late completion after timeout', async () => {
		const loading = deferred<void>();
		moduleResult = loading.promise;
		const build = buildNoiseSuppressionWorkletChain({
			audioContext: captureContext(),
			backend: 'gate',
			suppressionStrength: 80,
		});
		const failure = expect(build).rejects.toThrow('startup timed out');
		await moduleRequested.promise;
		await vi.advanceTimersByTimeAsync(NOISE_SUPPRESSION_STARTUP_TIMEOUT_MS);
		await failure;
		expect(contexts[1].close).toHaveBeenCalledTimes(1);
		expect(contexts[0].destinations[0].track.readyState).toBe('ended');
		loading.resolve(undefined);
		await loading.promise;
		expect(worklets).toHaveLength(0);
	});

	it('releases a timed-out startup without waiting for audio-context closure', async () => {
		moduleResult = deferred<void>().promise;
		const closing = deferred<void>();
		const build = buildNoiseSuppressionWorkletChain({
			audioContext: captureContext(),
			backend: 'gate',
			suppressionStrength: 80,
		});
		const failure = expect(build).rejects.toThrow('startup timed out');
		await moduleRequested.promise;
		contexts[1].close.mockImplementation(() => closing.promise);
		await vi.advanceTimersByTimeAsync(NOISE_SUPPRESSION_STARTUP_TIMEOUT_MS);
		await failure;
		expect(contexts[0].destinations[0].track.readyState).toBe('ended');
		expect(contexts[1].close).toHaveBeenCalledTimes(1);
		closing.resolve(undefined);
		await closing.promise;
	});

	it('aborts a stalled WASM download and does not cache its late response', async () => {
		const download = deferred<Response>();
		const requested = deferred<AbortSignal>();
		vi.mocked(fetch).mockImplementation((_url, options) => {
			requested.resolve(options!.signal as AbortSignal);
			return download.promise;
		});
		const build = buildNoiseSuppressionWorkletChain({
			audioContext: captureContext(),
			backend: 'rnnoise',
			suppressionStrength: 80,
		});
		const failure = expect(build).rejects.toThrow('startup timed out');
		const signal = await requested.promise;
		await vi.advanceTimersByTimeAsync(NOISE_SUPPRESSION_STARTUP_TIMEOUT_MS);
		await failure;
		expect(signal.aborted).toBe(true);
		download.resolve(new Response(new Uint8Array([1])));
		await download.promise;
		await vi.advanceTimersByTimeAsync(0);
		expect(worklets).toHaveLength(0);
		vi.mocked(fetch).mockResolvedValueOnce(new Response(new Uint8Array([2])));
		const retry = buildNoiseSuppressionWorkletChain({
			audioContext: captureContext(),
			backend: 'rnnoise',
			suppressionStrength: 80,
		});
		(await nodeCreated.promise).signal({type: 'ready'});
		const chain = await retry;
		expect(fetch).toHaveBeenCalledTimes(2);
		await chain.dispose();
	});

	it.each([
		'abort',
		'processor-error',
	] as const)('releases startup resources on %s before readiness', async (failureKind) => {
		const controller = new AbortController();
		const build = buildNoiseSuppressionWorkletChain({
			audioContext: captureContext(),
			backend: 'gate',
			suppressionStrength: 80,
			signal: controller.signal,
		});
		const failure = expect(build).rejects.toThrow(failureKind === 'abort' ? 'Cancelled' : 'processor error');
		const node = await nodeCreated.promise;
		if (failureKind === 'abort') controller.abort(new Error('Cancelled'));
		else node.onprocessorerror?.();
		await failure;
		expect(node.port.onmessage).toBeNull();
		expect(node.onprocessorerror).toBeNull();
		expect(node.port.close).toHaveBeenCalledTimes(1);
		expect(contexts[1].close).toHaveBeenCalledTimes(1);
		expect(contexts[0].destinations[0].track.readyState).toBe('ended');
	});

	it('stops an output track when connecting the completed graph fails', async () => {
		failOutputConnection = true;
		const build = buildNoiseSuppressionWorkletChain({
			audioContext: captureContext(),
			backend: 'gate',
			suppressionStrength: 80,
		});
		const failure = expect(build).rejects.toThrow('Output connection failed');
		(await nodeCreated.promise).signal({type: 'ready'});
		await failure;
		expect(contexts[1].destinations[0].track.readyState).toBe('ended');
		expect(contexts[1].close).toHaveBeenCalledTimes(1);
	});

	it('reports a runtime failure once and shares one disposal operation', async () => {
		const onRuntimeFailure = vi.fn();
		const build = buildNoiseSuppressionWorkletChain({
			audioContext: captureContext(),
			backend: 'gate',
			suppressionStrength: 80,
			onRuntimeFailure,
		});
		const node = await nodeCreated.promise;
		node.signal({type: 'ready'});
		const chain = await build;
		const oldMessageHandler = node.port.onmessage!;
		node.signal({type: 'error', message: 'Failed during processing'});
		node.onprocessorerror?.();
		expect(onRuntimeFailure).toHaveBeenCalledTimes(1);
		const disposal = chain.dispose();
		expect(chain.dispose()).toBe(disposal);
		await disposal;
		oldMessageHandler({data: {type: 'error', message: 'Late error'}});
		expect(onRuntimeFailure).toHaveBeenCalledTimes(1);
		expect(chain.processedTrack.readyState).toBe('ended');
		expect(contexts[0].close).not.toHaveBeenCalled();
		expect(contexts[1].close).toHaveBeenCalledTimes(1);
	});
});

describe('voice input processor lifecycle', () => {
	it('cancels an installing processor before the track has published it', async () => {
		const loading = deferred<void>();
		moduleResult = loading.promise;
		const track = new FakeLocalTrack();
		const install = syncVoiceInputProcessor(track.asLocalAudioTrack());
		await moduleRequested.promise;
		await Promise.all([removeVoiceInputProcessor(track.asLocalAudioTrack()), install]);
		expect(track.processor).toBeUndefined();
		expect(contexts[1].close).toHaveBeenCalledTimes(1);
		loading.resolve(undefined);
		await loading.promise;
		expect(worklets).toHaveLength(0);
		expect(settings.warn).not.toHaveBeenCalled();
	});

	it('replaces a pending track without letting its late failure blacklist the backend', async () => {
		const loading = deferred<void>();
		moduleResult = loading.promise;
		const first = new FakeLocalTrack();
		const installFirst = syncVoiceInputProcessor(first.asLocalAudioTrack());
		await moduleRequested.promise;
		moduleResult = Promise.resolve();
		const second = new FakeLocalTrack();
		const installSecond = syncVoiceInputProcessor(second.asLocalAudioTrack());
		loading.reject(new Error('Obsolete asset failure'));
		(await nodeCreated.promise).signal({type: 'ready'});
		await Promise.all([installFirst, installSecond]);
		expect(first.processor).toBeUndefined();
		expect(second.processor?.processedTrack?.readyState).toBe('live');
		await removeVoiceInputProcessor(first.asLocalAudioTrack());
		expect(second.processor).toBeDefined();
		expect(settings.warn).not.toHaveBeenCalled();
	});

	it('coalesces overlapping requests before creating the processor', async () => {
		const track = new FakeLocalTrack();
		const first = syncVoiceInputProcessor(track.asLocalAudioTrack());
		const second = syncVoiceInputProcessor(track.asLocalAudioTrack());
		(await nodeCreated.promise).signal({type: 'ready'});
		await Promise.all([first, second]);
		expect(track.installCount).toBe(1);
		expect(worklets).toHaveLength(1);
	});

	it('cancels a candidate while it is waiting to enter initialization', async () => {
		const track = new FakeLocalTrack();
		const waiting = deferred<void>();
		track.initializationBarrier = waiting.promise;
		const install = syncVoiceInputProcessor(track.asLocalAudioTrack());
		await track.initializationStarted.promise;
		const removal = removeVoiceInputProcessor(track.asLocalAudioTrack());
		waiting.resolve(undefined);
		await Promise.all([install, removal]);
		expect(track.processor).toBeUndefined();
		expect(contexts).toHaveLength(1);
		expect(worklets).toHaveLength(0);
		expect(settings.warn).not.toHaveBeenCalled();
	});

	it('retains the capture context for LiveKit restarts that omit it', async () => {
		const track = new FakeLocalTrack();
		const install = syncVoiceInputProcessor(track.asLocalAudioTrack());
		(await nodeCreated.promise).signal({type: 'ready'});
		await install;
		const previousOutput = track.processor!.processedTrack!;
		nodeCreated = deferred<FakeWorklet>();
		const restart = track.processor!.restart({
			kind: 'audio' as Track.Kind.Audio,
			track: track.rawTrack as unknown as MediaStreamTrack,
		});
		(await nodeCreated.promise).signal({type: 'ready'});
		await restart;
		expect(previousOutput.readyState).toBe('ended');
		expect(track.processor!.processedTrack!.readyState).toBe('live');
		expect(contexts[0].close).not.toHaveBeenCalled();
	});

	it('does not reinstall an old track when a runtime failure overlaps a track change', async () => {
		const first = new FakeLocalTrack();
		const installFirst = syncVoiceInputProcessor(first.asLocalAudioTrack());
		const failedNode = await nodeCreated.promise;
		failedNode.signal({type: 'ready'});
		await installFirst;
		failedNode.signal({type: 'error', message: 'Processing failed'});
		const second = new FakeLocalTrack();
		await syncVoiceInputProcessor(second.asLocalAudioTrack());
		expect(first.processor).toBeUndefined();
		expect(first.installCount).toBe(1);
		expect(second.installCount).toBe(1);
		expect(second.processor!.processedTrack!.readyState).toBe('live');
	});
});
