// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/app/state/RuntimeConfig', () => ({
	default: {staticCdnEndpoint: 'https://cdn.test'},
}));

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {
		getDeepFilterNoiseSuppression: () => true,
		getDeepFilterNoiseSuppressionLevel: () => 80,
	},
}));

const deepFilterHarness = vi.hoisted(() => ({
	instances: [] as Array<{
		audioContext: AudioContext | null;
		processedTrack: MediaStreamTrack | null;
		sampleRateAtInit: number | null;
	}>,
}));

vi.mock('deepfilternet3-noise-filter', () => ({
	DeepFilterNoiseFilterProcessor: class {
		audioContext: AudioContext | null = null;
		processedTrack: MediaStreamTrack | null = null;
		sampleRateAtInit: number | null = null;

		constructor() {
			deepFilterHarness.instances.push(this);
		}

		async init(): Promise<void> {
			this.sampleRateAtInit = this.audioContext?.sampleRate ?? null;
			this.processedTrack = {} as MediaStreamTrack;
		}

		async destroy(): Promise<void> {}
	},
}));

const MODEL_SAMPLE_RATE = 48000;

interface ContextHarness {
	requestedRates: Array<number | undefined>;
	closedRates: Array<number>;
}

function installAudioGraph(config: {hardwareSampleRate: number; canBridgeSampleRates: boolean}): ContextHarness {
	const harness: ContextHarness = {requestedRates: [], closedRates: []};

	class FakeAudioContext {
		state: AudioContextState = 'running';
		sampleRate: number;

		constructor(options?: AudioContextOptions) {
			harness.requestedRates.push(options?.sampleRate);
			this.sampleRate = options?.sampleRate ?? config.hardwareSampleRate;
		}

		createMediaStreamSource(): {disconnect: () => void} {
			if (!config.canBridgeSampleRates && this.sampleRate !== config.hardwareSampleRate) {
				throw new DOMException(
					'Connecting AudioNodes from AudioContexts with different sample-rate is currently not supported.',
					'NotSupportedError',
				);
			}
			return {disconnect: (): void => undefined};
		}

		async close(): Promise<void> {
			harness.closedRates.push(this.sampleRate);
			this.state = 'closed';
		}
	}

	vi.stubGlobal('window', {AudioContext: FakeAudioContext});
	vi.stubGlobal('MediaStream', class {});
	return harness;
}

async function loadModule() {
	vi.resetModules();
	return await import('@app/features/voice/utils/DeepFilterNoiseProcessor');
}

function createSourceContext(sampleRate: number): AudioContext {
	return {sampleRate} as unknown as AudioContext;
}

const feedTrack = {} as MediaStreamTrack;

describe('resolveDeepFilterAudioContext', () => {
	afterEach(() => {
		vi.unstubAllGlobals();
	});

	it('runs the worklet at the model rate when the capture graph already runs there', async () => {
		const harness = installAudioGraph({hardwareSampleRate: MODEL_SAMPLE_RATE, canBridgeSampleRates: false});
		const {resolveDeepFilterAudioContext} = await loadModule();

		const context = resolveDeepFilterAudioContext(createSourceContext(MODEL_SAMPLE_RATE), feedTrack);

		expect(context?.sampleRate).toBe(MODEL_SAMPLE_RATE);
		expect(harness.requestedRates).toEqual([MODEL_SAMPLE_RATE]);
		expect(harness.closedRates).toEqual([]);
	});

	it('keeps the model rate on browsers that resample across graphs', async () => {
		const harness = installAudioGraph({hardwareSampleRate: 44100, canBridgeSampleRates: true});
		const {resolveDeepFilterAudioContext} = await loadModule();

		const context = resolveDeepFilterAudioContext(createSourceContext(44100), feedTrack);

		expect(context?.sampleRate).toBe(MODEL_SAMPLE_RATE);
		expect(harness.requestedRates).toEqual([MODEL_SAMPLE_RATE]);
		expect(harness.closedRates).toEqual([]);
	});

	it('falls back to the capture rate when the browser refuses to bridge sample rates', async () => {
		const harness = installAudioGraph({hardwareSampleRate: 44100, canBridgeSampleRates: false});
		const {resolveDeepFilterAudioContext} = await loadModule();

		const context = resolveDeepFilterAudioContext(createSourceContext(44100), feedTrack);

		expect(context?.sampleRate).toBe(44100);
		expect(harness.requestedRates).toEqual([MODEL_SAMPLE_RATE, 44100]);
		expect(harness.closedRates).toEqual([MODEL_SAMPLE_RATE]);
	});

	it('falls back to the capture rate for a bluetooth headset running well below the model rate', async () => {
		installAudioGraph({hardwareSampleRate: 16000, canBridgeSampleRates: false});
		const {resolveDeepFilterAudioContext} = await loadModule();

		const context = resolveDeepFilterAudioContext(createSourceContext(16000), feedTrack);

		expect(context?.sampleRate).toBe(16000);
	});
});

function createCaptureContext(sampleRate: number): AudioContext {
	const track = {stop: (): void => undefined} as unknown as MediaStreamTrack;
	const node = {
		connect: (next: unknown): unknown => next,
		disconnect: (): void => undefined,
		stream: {getAudioTracks: (): Array<MediaStreamTrack> => [track]},
		type: '',
		frequency: {value: 0},
		Q: {value: 0},
		threshold: {value: 0},
		knee: {value: 0},
		ratio: {value: 0},
		attack: {value: 0},
		release: {value: 0},
	};
	return {
		sampleRate,
		createMediaStreamDestination: () => node,
		createMediaStreamSource: () => node,
		createBiquadFilter: () => node,
		createDynamicsCompressor: () => node,
	} as unknown as AudioContext;
}

describe('buildDeepFilterAudioChain', () => {
	beforeEach(() => {
		deepFilterHarness.instances.length = 0;
	});

	afterEach(() => {
		vi.unstubAllGlobals();
	});

	it('gives the processor its audio context before the processor builds its own graph', async () => {
		installAudioGraph({hardwareSampleRate: MODEL_SAMPLE_RATE, canBridgeSampleRates: false});
		const {buildDeepFilterAudioChain} = await loadModule();

		await buildDeepFilterAudioChain({audioContext: createCaptureContext(MODEL_SAMPLE_RATE)});

		expect(deepFilterHarness.instances[0]?.sampleRateAtInit).toBe(MODEL_SAMPLE_RATE);
	});

	it('gives the processor a capture-rate context when the browser refuses to bridge sample rates', async () => {
		installAudioGraph({hardwareSampleRate: 44100, canBridgeSampleRates: false});
		const {buildDeepFilterAudioChain} = await loadModule();

		await buildDeepFilterAudioChain({audioContext: createCaptureContext(44100)});

		expect(deepFilterHarness.instances[0]?.sampleRateAtInit).toBe(44100);
	});
});
