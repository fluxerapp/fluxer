// SPDX-License-Identifier: AGPL-3.0-or-later

import vm from 'node:vm';
import gateWorkletSource from '@app/features/voice/utils/voiceGateProcessor.worklet.js?raw';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const SAMPLE_RATE = 48000;
const QUANTUM = 128;
const TICK_SAMPLES = SAMPLE_RATE / 20;
const ANALYSER_WINDOW = 512;

const audio = vi.hoisted(() => ({window: new Float32Array(512), state: 'running'}));

vi.mock('@app/features/voice/state/VoiceSettings', () => ({default: {getVadThreshold: () => 50}}));

vi.mock('@app/features/voice/engine/VoiceSharedAudioContext', () => ({
	getSharedVoiceAudioContext: () => ({
		get state() {
			return audio.state;
		},
		createMediaStreamSource: () => ({connect: () => undefined, disconnect: () => undefined}),
		createAnalyser: () => ({
			fftSize: 0,
			smoothingTimeConstant: 0,
			disconnect: () => undefined,
			getFloatTimeDomainData: (target: Float32Array) => target.set(audio.window),
		}),
	}),
}));

const {LOCAL_AUTO_MIN_RMS, LOCAL_MAX_RMS, SPEAKING_LOCAL_RELEASE_MS, getLocalSpeakingThresholdRms} = await import(
	'@app/features/voice/engine/VoiceSpeakingThreshold'
);
const {computeTimeDomainRms, readSpeakingThresholdRms, VoiceSpeakingMeter} = await import(
	'@app/features/voice/engine/VoiceSpeakingDetection'
);
const {createVoiceRemoteSpeakingSnapshot, transitionVoiceRemoteSpeakingSnapshot, clearVoiceRemoteSpeakingCommands} =
	await import('@app/features/voice/engine/VoiceParticipantStateMachine');

interface GateProcessor {
	speaking: boolean;
	process(inputs: Array<Array<Float32Array>>, outputs: Array<Array<Float32Array>>): boolean;
}

type GateConstructor = new (options: {processorOptions: Record<string, unknown>}) => GateProcessor;

interface GateMode {
	apply: boolean;
	auto: boolean;
	slider?: number;
}

function loadGate(mode: GateMode): GateProcessor {
	let Processor: GateConstructor | null = null;
	const sandbox = vm.createContext({
		sampleRate: SAMPLE_RATE,
		currentTime: 0,
		AudioWorkletProcessor: class {
			port = {postMessage: () => undefined, onmessage: null};
		},
		registerProcessor: (_name: string, processor: GateConstructor) => {
			Processor = processor;
		},
	});
	vm.runInContext(gateWorkletSource, sandbox);
	if (!Processor) throw new Error('voice gate worklet did not register');
	return new (Processor as GateConstructor)({
		processorOptions: {
			apply: mode.apply,
			auto: mode.auto,
			manualThresholdRms: getLocalSpeakingThresholdRms(mode.slider ?? 50),
			channelCount: 1,
			minRms: LOCAL_AUTO_MIN_RMS,
			maxRms: LOCAL_MAX_RMS,
			releaseMs: SPEAKING_LOCAL_RELEASE_MS,
		},
	});
}

function createRandom(seed: number): () => number {
	let state = seed >>> 0;
	return () => {
		state = (state + 0x6d2b79f5) >>> 0;
		let value = state;
		value = Math.imul(value ^ (value >>> 15), value | 1);
		value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
		return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
	};
}

const UNIFORM_RMS_TO_PEAK = Math.sqrt(3);

function noise(random: () => number, rms: number): number {
	return (random() * 2 - 1) * rms * UNIFORM_RMS_TO_PEAK;
}

function syllable(index: number, periodSamples: number, onSamples: number): boolean {
	return index % periodSamples < onSamples;
}

function tone(index: number, rms: number): number {
	return Math.sin((2 * Math.PI * 180 * index) / SAMPLE_RATE) * rms * Math.SQRT2;
}

interface Timeline {
	gateSpeaking: boolean;
	localSpeaking: Array<boolean>;
	remoteSpeaking: Array<boolean>;
}

function runPipeline(mode: GateMode, seconds: number, signal: (index: number) => number, transmit = true): Timeline {
	const gate = loadGate(mode);
	const track = {readyState: 'live'} as MediaStreamTrack;
	const meter = new VoiceSpeakingMeter(
		() => track,
		() => undefined,
	);
	let remote = transitionVoiceRemoteSpeakingSnapshot(createVoiceRemoteSpeakingSnapshot(), {
		type: 'remote.attach',
		identity: 'peer',
		track: 'track',
	});
	const history = new Float32Array(ANALYSER_WINDOW);
	const timeline: Timeline = {gateSpeaking: false, localSpeaking: [], remoteSpeaking: []};
	const seedEnd = 0.3 * SAMPLE_RATE;
	const total = Math.floor(seconds * SAMPLE_RATE);
	let nextTick = TICK_SAMPLES;
	for (let start = 0; start < total; start += QUANTUM) {
		const input = new Float32Array(QUANTUM);
		for (let offset = 0; offset < QUANTUM; offset++) input[offset] = signal(start + offset);
		const output = new Float32Array(QUANTUM);
		gate.process([[input]], [[output]]);
		if (start >= seedEnd && gate.speaking) timeline.gateSpeaking = true;
		history.copyWithin(0, QUANTUM);
		history.set(transmit ? output : new Float32Array(QUANTUM), ANALYSER_WINDOW - QUANTUM);
		const end = start + QUANTUM;
		if (end < nextTick) continue;
		nextTick += TICK_SAMPLES;
		const nowMs = (end / SAMPLE_RATE) * 1000;
		audio.window.set(history);
		meter.tick(nowMs);
		remote = transitionVoiceRemoteSpeakingSnapshot(remote, {
			type: 'remote.tick',
			identity: 'peer',
			rms: computeTimeDomainRms(history),
			threshold: readSpeakingThresholdRms(),
			nowMs,
		});
		remote = clearVoiceRemoteSpeakingCommands(remote);
		timeline.localSpeaking.push(meter.speaking);
		timeline.remoteSpeaking.push(remote.context.analysers.get('peer')?.speaking ?? false);
	}
	meter.dispose();
	return timeline;
}

const VAD_AUTO: GateMode = {apply: true, auto: true};
const VAD_MANUAL: GateMode = {apply: true, auto: false, slider: 50};
const VAD_MANUAL_SENSITIVE: GateMode = {apply: true, auto: false, slider: 10};
const PUSH_TO_TALK: GateMode = {apply: false, auto: true};
const MODES: Array<[string, GateMode]> = [
	['voice activity with auto sensitivity', VAD_AUTO],
	['voice activity with a manual threshold', VAD_MANUAL],
	['voice activity with a sensitive manual threshold', VAD_MANUAL_SENSITIVE],
	['push to talk', PUSH_TO_TALK],
];

function suppressionResidue(seed: number): (index: number) => number {
	const random = createRandom(seed);
	return (index) => {
		const click = syllable(index, 0.3 * SAMPLE_RATE, 0.02 * SAMPLE_RATE);
		return noise(random, click ? 0.001 : 0.00003);
	};
}

function hardwareMutedWhilePeersTalk(seed: number): (index: number) => number {
	const random = createRandom(seed);
	return (index) => {
		if (index < SAMPLE_RATE) return 0;
		const peerTalking = syllable(index, 0.25 * SAMPLE_RATE, 0.15 * SAMPLE_RATE);
		return peerTalking ? tone(index, 0.00004) + noise(random, 0.00001) : 0;
	};
}

function speechInRoomNoise(seed: number): (index: number) => number {
	const random = createRandom(seed);
	return (index) => {
		const room = noise(random, 0.0003);
		if (index < SAMPLE_RATE) return room;
		return syllable(index, 0.2 * SAMPLE_RATE, 0.15 * SAMPLE_RATE) ? room + tone(index, 0.05) : room;
	};
}

describe('local speaking indicator', () => {
	beforeEach(() => {
		vi.stubGlobal('MediaStream', class {});
		audio.window.fill(0);
		audio.state = 'running';
	});

	afterEach(() => {
		vi.unstubAllGlobals();
	});

	it.each(MODES)('stays dark on noise left behind by noise suppression in %s', (_name, mode) => {
		const timeline = runPipeline(mode, 3, suppressionResidue(1));
		expect(timeline.localSpeaking).not.toContain(true);
		expect(timeline.remoteSpeaking).not.toContain(true);
	});

	it.each(MODES)('stays dark when a hardware-muted mic only carries peer bleed in %s', (_name, mode) => {
		const timeline = runPipeline(mode, 4, hardwareMutedWhilePeersTalk(2));
		expect(timeline.localSpeaking).not.toContain(true);
		expect(timeline.remoteSpeaking).not.toContain(true);
	});

	it.each(MODES)('lights for real speech in %s', (_name, mode) => {
		const timeline = runPipeline(mode, 3, speechInRoomNoise(3));
		expect(timeline.localSpeaking).toContain(true);
		expect(timeline.localSpeaking.slice(0, 18)).not.toContain(true);
	});

	it.each(MODES)('agrees tick for tick with how peers judge the same audio in %s', (_name, mode) => {
		for (const signal of [suppressionResidue(4), hardwareMutedWhilePeersTalk(5), speechInRoomNoise(6)]) {
			const timeline = runPipeline(mode, 3, signal);
			expect(timeline.localSpeaking).toEqual(timeline.remoteSpeaking);
		}
	});

	it('never lights while the outgoing track is muted, even for loud speech', () => {
		for (const [, mode] of MODES) {
			const timeline = runPipeline(mode, 3, speechInRoomNoise(7), false);
			expect(timeline.localSpeaking).not.toContain(true);
		}
	});

	it('keeps the transmit gate opening on those inaudible inputs, so the gate cannot drive the indicator', () => {
		expect(runPipeline(VAD_AUTO, 3, suppressionResidue(1)).gateSpeaking).toBe(true);
		expect(runPipeline(VAD_AUTO, 4, hardwareMutedWhilePeersTalk(2)).gateSpeaking).toBe(true);
	});

	it('does not hold a stale reading while the analysing context is not running', () => {
		const track = {readyState: 'live'} as MediaStreamTrack;
		const meter = new VoiceSpeakingMeter(
			() => track,
			() => undefined,
		);
		audio.window.fill(0.1);
		meter.tick(0);
		meter.tick(50);
		expect(meter.speaking).toBe(true);
		audio.state = 'suspended';
		for (let nowMs = 100; nowMs <= 400; nowMs += 50) meter.tick(nowMs);
		expect(meter.speaking).toBe(false);
		meter.dispose();
	});
});
