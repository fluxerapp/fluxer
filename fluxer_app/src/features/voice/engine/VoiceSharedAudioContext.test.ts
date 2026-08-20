// SPDX-License-Identifier: AGPL-3.0-or-later

import {VOICE_TRACK_MAX_TOTAL_GAIN} from '@app/features/voice/utils/VoiceVolumeUtils';
import {afterEach, describe, expect, it, vi} from 'vitest';

interface FakeAudioContext {
	state: AudioContextState;
	sampleRate: number;
	resumeCount: number;
}

interface FakeAudioNode {
	kind: string;
	connections: Array<FakeAudioNode>;
	connect: (target: FakeAudioNode) => FakeAudioNode;
}

interface FakeGainNode extends FakeAudioNode {
	gain: {value: number};
}

interface FakeWaveShaperNode extends FakeAudioNode {
	curve: Float32Array | null;
	oversample: OverSampleType;
}

interface FakeGraphContext {
	state: AudioContextState;
	sampleRate: number;
	hardwareDestination: FakeAudioNode;
	readonly destination: FakeAudioNode;
}

interface FakeGraphHarness {
	contexts: Array<FakeGraphContext>;
	gains: Array<FakeGainNode>;
	waveShapers: Array<FakeWaveShaperNode>;
}

interface FakeWindowHarness {
	requestedOptions: Array<AudioContextOptions | undefined>;
	contexts: Array<FakeAudioContext>;
	bodyListeners: Array<string>;
}

const HARDWARE_SAMPLE_RATE = 44100;

function installFakeWindow(
	config: {
		throwOn?: (options: AudioContextOptions | undefined) => boolean;
		initialState?: AudioContextState;
		honourSampleRate?: boolean;
	} = {},
): FakeWindowHarness {
	const harness: FakeWindowHarness = {requestedOptions: [], contexts: [], bodyListeners: []};
	const throwOn = config.throwOn ?? ((): boolean => false);
	const honourSampleRate = config.honourSampleRate ?? true;

	class FakeAudioContextImpl {
		state: AudioContextState = config.initialState ?? 'running';
		sampleRate: number;
		resumeCount = 0;

		constructor(options?: AudioContextOptions) {
			harness.requestedOptions.push(options);
			if (throwOn(options)) {
				throw new DOMException('sample rate not supported', 'NotSupportedError');
			}
			this.sampleRate = (honourSampleRate ? options?.sampleRate : undefined) ?? HARDWARE_SAMPLE_RATE;
			harness.contexts.push(this as unknown as FakeAudioContext);
		}

		async resume(): Promise<void> {
			this.resumeCount += 1;
			this.state = 'running';
		}

		addEventListener(): void {}
		removeEventListener(): void {}
	}

	vi.stubGlobal('window', {
		AudioContext: FakeAudioContextImpl,
		document: {
			body: {
				addEventListener: (type: string): void => {
					harness.bodyListeners.push(type);
				},
				removeEventListener: (): void => {},
			},
		},
	});
	return harness;
}

function createFakeAudioNode(kind: string): FakeAudioNode {
	const node: FakeAudioNode = {
		kind,
		connections: [],
		connect: (target: FakeAudioNode): FakeAudioNode => {
			node.connections.push(target);
			return target;
		},
	};
	return node;
}

function installFakeGraphWindow(config: {withWaveShaper?: boolean} = {}): FakeGraphHarness {
	const harness: FakeGraphHarness = {contexts: [], gains: [], waveShapers: []};
	const withWaveShaper = config.withWaveShaper ?? true;

	class FakeGraphAudioContextImpl {
		state: AudioContextState = 'running';
		sampleRate = HARDWARE_SAMPLE_RATE;
		hardwareDestination: FakeAudioNode;

		constructor() {
			this.hardwareDestination = createFakeAudioNode('destination');
			harness.contexts.push(this as unknown as FakeGraphContext);
		}

		get destination(): FakeAudioNode {
			return this.hardwareDestination;
		}

		createGain(): FakeGainNode {
			const node = createFakeAudioNode('gain') as FakeGainNode;
			node.gain = {value: 1};
			harness.gains.push(node);
			return node;
		}

		async resume(): Promise<void> {
			this.state = 'running';
		}

		addEventListener(): void {}
		removeEventListener(): void {}
	}

	class FakeGraphAudioContextWithWaveShaperImpl extends FakeGraphAudioContextImpl {
		createWaveShaper(): FakeWaveShaperNode {
			const node = createFakeAudioNode('waveShaper') as FakeWaveShaperNode;
			node.curve = null;
			node.oversample = '4x';
			harness.waveShapers.push(node);
			return node;
		}
	}

	vi.stubGlobal('window', {
		AudioContext: withWaveShaper ? FakeGraphAudioContextWithWaveShaperImpl : FakeGraphAudioContextImpl,
		document: {
			body: {
				addEventListener: (): void => {},
				removeEventListener: (): void => {},
			},
		},
	});
	return harness;
}

function evaluateSoftClipCurve(curve: Float32Array, input: number): number {
	const scaled = ((curve.length - 1) / 2) * (input + 1);
	if (scaled <= 0) return curve[0]!;
	if (scaled >= curve.length - 1) return curve[curve.length - 1]!;
	const index = Math.floor(scaled);
	const fraction = scaled - index;
	return (1 - fraction) * curve[index]! + fraction * curve[index + 1]!;
}

function expectCurveIsMonotoneOddAndBounded(curve: Float32Array): void {
	let maxAbsolute = 0;
	let maxOddError = 0;
	let maxBackwardsStep = 0;
	for (let i = 0; i < curve.length; i++) {
		maxAbsolute = Math.max(maxAbsolute, Math.abs(curve[i]!));
		maxOddError = Math.max(maxOddError, Math.abs(curve[i]! + curve[curve.length - 1 - i]!));
		if (i < curve.length - 1) {
			maxBackwardsStep = Math.max(maxBackwardsStep, curve[i]! - curve[i + 1]!);
		}
	}
	expect(maxBackwardsStep).toBe(0);
	expect(maxOddError).toBeLessThanOrEqual(1e-6);
	expect(maxAbsolute).toBeLessThanOrEqual(1);
}

function measureWorstTransparencyError(curve: Float32Array, maxAmplitude: number): number {
	let worst = 0;
	for (let step = -2000; step <= 2000; step++) {
		const amplitude = (step / 2000) * maxAmplitude;
		const shaped = evaluateSoftClipCurve(curve, amplitude / VOICE_TRACK_MAX_TOTAL_GAIN);
		worst = Math.max(worst, Math.abs(shaped - amplitude));
	}
	return worst;
}

async function loadModule() {
	vi.resetModules();
	return await import('@app/features/voice/engine/VoiceSharedAudioContext');
}

describe('VoiceSharedAudioContext', () => {
	afterEach(() => {
		vi.unstubAllGlobals();
	});

	it('leaves the shared context on the hardware rate so it shares a graph with capture and playback', async () => {
		const harness = installFakeWindow();
		const {getSharedVoiceAudioContext} = await loadModule();

		const context = getSharedVoiceAudioContext();

		expect(harness.requestedOptions).toEqual([{latencyHint: 'interactive'}]);
		expect(context).toBe(harness.contexts[0] as unknown as AudioContext);
		expect(context?.sampleRate).toBe(HARDWARE_SAMPLE_RATE);
	});

	it('returns null without throwing when the construction fails', async () => {
		const harness = installFakeWindow({throwOn: () => true});
		const {getSharedVoiceAudioContext} = await loadModule();

		let context: AudioContext | null | undefined;
		expect(() => {
			context = getSharedVoiceAudioContext();
		}).not.toThrow();

		expect(context).toBeNull();
		expect(harness.contexts).toHaveLength(0);
		expect(harness.requestedOptions).toHaveLength(1);
	});

	it('caches the context across calls and rebuilds it once it has closed', async () => {
		const harness = installFakeWindow();
		const {getSharedVoiceAudioContext} = await loadModule();

		const first = getSharedVoiceAudioContext();
		expect(getSharedVoiceAudioContext()).toBe(first);
		expect(harness.requestedOptions).toHaveLength(1);

		harness.contexts[0].state = 'closed';
		const second = getSharedVoiceAudioContext();

		expect(second).not.toBe(first);
		expect(harness.contexts).toHaveLength(2);
		expect(harness.requestedOptions).toEqual([{latencyHint: 'interactive'}, {latencyHint: 'interactive'}]);
	});

	it('resumes a suspended context and arms the user-gesture resume listener', async () => {
		const harness = installFakeWindow({initialState: 'suspended'});
		const {getSharedVoiceAudioContext} = await loadModule();

		const context = getSharedVoiceAudioContext();

		expect(context).not.toBeNull();
		expect(harness.contexts[0].resumeCount).toBe(1);
		expect(harness.bodyListeners).toEqual(['click']);
	});

	it('createVoiceAudioContext reports the rate the browser actually resolved, not the requested one', async () => {
		installFakeWindow({honourSampleRate: false});
		const {createVoiceAudioContext} = await loadModule();

		const context = createVoiceAudioContext({latencyHint: 'interactive', sampleRate: 48000});

		expect(context?.sampleRate).toBe(HARDWARE_SAMPLE_RATE);
	});

	it('createVoiceAudioContext returns null instead of throwing when the rate is rejected', async () => {
		installFakeWindow({throwOn: (options) => options?.sampleRate !== undefined});
		const {createVoiceAudioContext} = await loadModule();

		expect(createVoiceAudioContext({latencyHint: 'interactive', sampleRate: 48000})).toBeNull();
	});

	it('leaves the context untouched when the browser has no WaveShaper', async () => {
		installFakeWindow();
		const {getSharedVoiceAudioContext} = await loadModule();

		const context = getSharedVoiceAudioContext();

		expect(context).not.toBeNull();
		expect(Object.getOwnPropertyDescriptor(context as object, 'destination')).toBeUndefined();
	});

	it('leaves the hardware destination in place when the context cannot build a WaveShaper', async () => {
		const harness = installFakeGraphWindow({withWaveShaper: false});
		const {getSharedVoiceAudioContext} = await loadModule();

		const context = getSharedVoiceAudioContext() as unknown as FakeGraphContext;

		expect(Object.getOwnPropertyDescriptor(context as object, 'destination')).toBeUndefined();
		expect(context.destination).toBe(harness.contexts[0]!.hardwareDestination);
		expect(harness.gains).toHaveLength(0);
	});
});

describe('VoiceSharedAudioContext master soft clip', () => {
	afterEach(() => {
		vi.unstubAllGlobals();
	});

	it('routes LiveKit track gain through the master soft clipper', async () => {
		const harness = installFakeGraphWindow();
		const {getSharedVoiceAudioContext} = await loadModule();

		const context = getSharedVoiceAudioContext() as unknown as FakeGraphContext;
		const masterInput = harness.gains[0]!;
		const waveShaper = harness.waveShapers[0]!;

		expect(context.destination).toBe(masterInput);
		expect(context.destination).not.toBe(context.hardwareDestination);
		expect(masterInput.connections).toEqual([waveShaper]);
		expect(waveShaper.connections).toEqual([context.hardwareDestination]);
	});

	it('pre-gain opens a 12x window', async () => {
		const harness = installFakeGraphWindow();
		const {getSharedVoiceAudioContext} = await loadModule();

		getSharedVoiceAudioContext();

		expect(harness.gains).toHaveLength(1);
		expect(harness.gains[0]!.gain.value).toBe(1 / VOICE_TRACK_MAX_TOTAL_GAIN);
	});

	it('oversample stays none so the limiter adds zero latency', async () => {
		const harness = installFakeGraphWindow();
		const {getSharedVoiceAudioContext} = await loadModule();

		getSharedVoiceAudioContext();

		expect(harness.waveShapers[0]!.oversample).toBe('none');
	});

	it('installs the shared curve, which is monotone, odd and never exceeds full scale', async () => {
		const harness = installFakeGraphWindow();
		const {getSharedVoiceAudioContext, createVoiceSoftClipCurve} = await loadModule();

		getSharedVoiceAudioContext();
		const curve = harness.waveShapers[0]!.curve;

		expect(curve).toEqual(createVoiceSoftClipCurve());
		expectCurveIsMonotoneOddAndBounded(curve as Float32Array);
	});

	it('is transparent below the knee so normal playback is unchanged', async () => {
		const harness = installFakeGraphWindow();
		const {getSharedVoiceAudioContext} = await loadModule();

		getSharedVoiceAudioContext();
		const curve = harness.waveShapers[0]!.curve as Float32Array;

		for (const amplitude of [0, 0.4, 0.79]) {
			expect(evaluateSoftClipCurve(curve, amplitude / VOICE_TRACK_MAX_TOTAL_GAIN)).toBeCloseTo(amplitude, 6);
		}
		expect(measureWorstTransparencyError(curve, 0.8)).toBeLessThan(1e-6);
	});

	it('reuses the shared context without stacking a second limiter', async () => {
		const harness = installFakeGraphWindow();
		const {getSharedVoiceAudioContext} = await loadModule();

		const first = getSharedVoiceAudioContext();

		expect(getSharedVoiceAudioContext()).toBe(first);
		expect(harness.gains).toHaveLength(1);
		expect(harness.waveShapers).toHaveLength(1);
	});
});

describe('createVoiceSoftClipCurve', () => {
	afterEach(() => {
		vi.unstubAllGlobals();
	});

	it('spans the full 12x window and lands exactly on full scale at the edges', async () => {
		installFakeWindow();
		const {createVoiceSoftClipCurve} = await loadModule();

		const curve = createVoiceSoftClipCurve();

		expect(curve).toHaveLength(8192);
		expect(curve[0]).toBe(-1);
		expect(curve[curve.length - 1]).toBe(1);
	});

	it('is monotone, odd-symmetric and bounded by full scale', async () => {
		installFakeWindow();
		const {createVoiceSoftClipCurve} = await loadModule();

		expectCurveIsMonotoneOddAndBounded(createVoiceSoftClipCurve());
	});

	it('passes everything below the knee through untouched', async () => {
		installFakeWindow();
		const {createVoiceSoftClipCurve} = await loadModule();

		const curve = createVoiceSoftClipCurve();

		expect(measureWorstTransparencyError(curve, 0.8)).toBeLessThan(1e-6);
	});

	it('shaves full scale by 0.42 dB and hard limits far above it', async () => {
		installFakeWindow();
		const {createVoiceSoftClipCurve} = await loadModule();

		const curve = createVoiceSoftClipCurve();
		const atFullScale = evaluateSoftClipCurve(curve, 1 / VOICE_TRACK_MAX_TOTAL_GAIN);

		expect(atFullScale).toBeCloseTo(0.95232, 5);
		expect(20 * Math.log10(atFullScale)).toBeCloseTo(-0.42, 2);
		expect(evaluateSoftClipCurve(curve, 3 / VOICE_TRACK_MAX_TOTAL_GAIN)).toBeCloseTo(1, 6);
		expect(evaluateSoftClipCurve(curve, 5)).toBe(1);
		expect(evaluateSoftClipCurve(curve, -5)).toBe(-1);
	});
});
