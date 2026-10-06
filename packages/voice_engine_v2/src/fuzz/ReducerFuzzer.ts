// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2Snapshot} from '@fluxer/voice_engine_v2/src/core/state';
import {FuzzPrng} from '@fluxer/voice_engine_v2/src/fuzz/FuzzPrng';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2DisconnectReason,
	VoiceEngineV2LifecycleReason,
} from '@fluxer/voice_engine_v2/src/protocol/types';
import {createVoiceEngineV2MemoryEventLogSpillSink} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';
import {createVoiceEngineV2DeterministicClockPort} from '@fluxer/voice_engine_v2/src/runtime/platformPort';
import {assertEventLogInvariants, VoiceEngineV2Runtime} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Runtime';
import {hashSnapshot} from '@fluxer/voice_engine_v2/src/simulation/Simulator';
import {
	type SimulatorDriverFaultPolicy,
	VoiceEngineV2SimulatorDriver,
} from '@fluxer/voice_engine_v2/src/simulation/SimulatorPorts';
import {VoiceEngineV2TestImplementation} from '@fluxer/voice_engine_v2/src/testing/VoiceEngineV2TestImplementation';

const FUZZ_ITERATIONS_MAX = 1024;

export const FUZZ_REDUCER_POSITIVE_ITERATIONS = 1000;

export const FUZZ_REDUCER_NEGATIVE_ITERATIONS = 256;

export const FUZZ_REDUCER_ARBITRARY_ITERATIONS = 200;

const FUZZ_REDUCER_FLUSH_MICROTASKS_MAX = 64;

export const FUZZ_REDUCER_EXTERNAL_CONNECTION_ITERATIONS = 512;

const FUZZ_EXTERNAL_CONNECTION_WEIGHT = 6;

interface ReducerFuzzerFailure {
	seed: number;
	iteration: number;
	mode: 'positive' | 'negative' | 'qualitative' | 'arbitraryOrder' | 'externalConnection';
	reason: string;
	event: VoiceEngineV2Event | null;
}

interface ReducerFuzzerReport {
	seed: number;
	mode: 'positive' | 'negative' | 'qualitative' | 'arbitraryOrder' | 'externalConnection';
	iterations: number;
	dispatched: number;
	rejected: number;
	failures: ReadonlyArray<ReducerFuzzerFailure>;
	snapshotHash?: string;
}

const POSITIVE_EVENT_KINDS: ReadonlyArray<VoiceEngineV2Event['type']> = [
	'implementation.prewarmRequested',
	'connection.connectRequested',
	'connection.disconnectRequested',
	'microphone.publishRequested',
	'microphone.unpublishRequested',
	'microphone.setEnabledRequested',
	'camera.publishRequested',
	'camera.unpublishRequested',
	'screen.publishRequested',
	'screen.unpublishRequested',
	'screenAudio.publishRequested',
	'screenAudio.unpublishRequested',
	'outputDevice.setRequested',
	'participantVolume.setRequested',
	'remoteTrackSubscription.setRequested',
	'data.publishRequested',
	'stats.collectRequested',
	'devices.enumerateRequested',
	'devices.selectAudioInputRequested',
	'devices.selectAudioOutputRequested',
	'devices.selectCameraRequested',
	'permissions.checkRequested',
	'permissions.requestRequested',
	'capabilities.hardwareEncoderQueryRequested',
	'localAudio.muteRequested',
	'localAudio.deafenRequested',
	'lifecycle.teardownRequested',
	'connection.reconnectRequested',
	'gateway.voiceStateClearRequested',
	'gateway.desiredVoiceStateChanged',
	'gateway.voiceStateReconcileRequested',
	'gateway.voiceStateUpdated',
];

export const EXTERNAL_CONNECTION_EVENT_KINDS: ReadonlyArray<VoiceEngineV2Event['type']> = [
	'connection.externallyEstablished',
	'connection.remoteDisconnected',
];

const ARBITRARY_EVENT_KINDS: ReadonlyArray<VoiceEngineV2Event['type']> = [
	...POSITIVE_EVENT_KINDS,
	...EXTERNAL_CONNECTION_EVENT_KINDS,
];

const EXTERNAL_CONNECTION_CYCLE_KIND_POOL: ReadonlyArray<VoiceEngineV2Event['type']> = buildWeightedKindPool();

function buildWeightedKindPool(): ReadonlyArray<VoiceEngineV2Event['type']> {
	assert.ok(FUZZ_EXTERNAL_CONNECTION_WEIGHT >= 1, 'external connection weight must be >= 1');
	assert.ok(FUZZ_EXTERNAL_CONNECTION_WEIGHT <= 16, 'external connection weight must stay bounded');
	const pool: Array<VoiceEngineV2Event['type']> = [...POSITIVE_EVENT_KINDS];
	for (let repeat = 0; repeat < FUZZ_EXTERNAL_CONNECTION_WEIGHT; repeat += 1) {
		for (const kind of EXTERNAL_CONNECTION_EVENT_KINDS) {
			pool.push(kind);
		}
	}
	assert.equal(
		pool.length,
		POSITIVE_EVENT_KINDS.length + EXTERNAL_CONNECTION_EVENT_KINDS.length * FUZZ_EXTERNAL_CONNECTION_WEIGHT,
		'weighted kind pool must contain every positive kind plus the weighted external kinds',
	);
	return pool;
}

const DISCONNECT_REASONS: ReadonlyArray<VoiceEngineV2DisconnectReason> = [
	'user',
	'server',
	'network',
	'replaced',
	'shutdown',
];

const LIFECYCLE_REASONS: ReadonlyArray<VoiceEngineV2LifecycleReason> = [
	'rendererDisposed',
	'windowClosed',
	'appShutdown',
	'sessionReplaced',
	'logout',
	'test',
];

export class ReducerFuzzer {
	private readonly seed: number;

	constructor(seed: number) {
		assert.equal(typeof seed, 'number', 'seed must be a number');
		assert.ok(Number.isInteger(seed), 'seed must be an integer');
		assert.ok(seed >= 0, 'seed must be non-negative');
		this.seed = seed;
	}

	fuzzPositive(iterations: number = FUZZ_REDUCER_POSITIVE_ITERATIONS): ReducerFuzzerReport {
		assert.ok(Number.isInteger(iterations), 'iterations must be an integer');
		assert.ok(iterations >= 1, 'iterations must be >= 1');
		assert.ok(iterations <= FUZZ_ITERATIONS_MAX, 'iterations must respect FUZZ_ITERATIONS_MAX');
		const prng = new FuzzPrng({seed: this.seed});
		const runtime = freshRuntime();
		const failures: Array<ReducerFuzzerFailure> = [];
		let dispatched = 0;
		for (let iteration = 0; iteration < iterations; iteration += 1) {
			const event = generatePositiveEvent(prng);
			const before = runtime.snapshot;
			const failure = tryDispatch(runtime, event, this.seed, iteration, 'positive');
			if (failure !== null) {
				failures.push(failure);
				continue;
			}
			dispatched += 1;
			assertSnapshotShapePreserved(before, runtime.snapshot, event, this.seed, failures, iteration, 'positive');
		}
		runtime.dispose();
		return {seed: this.seed, mode: 'positive', iterations, dispatched, rejected: 0, failures};
	}

	fuzzNegative(iterations: number = FUZZ_REDUCER_NEGATIVE_ITERATIONS): ReducerFuzzerReport {
		assert.ok(Number.isInteger(iterations), 'iterations must be an integer');
		assert.ok(iterations >= 1, 'iterations must be >= 1');
		assert.ok(iterations <= FUZZ_ITERATIONS_MAX, 'iterations must respect FUZZ_ITERATIONS_MAX');
		const prng = new FuzzPrng({seed: this.seed});
		const runtime = freshRuntime();
		const failures: Array<ReducerFuzzerFailure> = [];
		let dispatched = 0;
		let rejected = 0;
		for (let iteration = 0; iteration < iterations; iteration += 1) {
			const event = generateNegativeEvent(prng);
			const before = runtime.snapshot;
			const failure = tryDispatch(runtime, event, this.seed, iteration, 'negative');
			if (failure !== null) {
				rejected += 1;
				continue;
			}
			dispatched += 1;
			assertSnapshotShapePreserved(before, runtime.snapshot, event, this.seed, failures, iteration, 'negative');
		}
		runtime.dispose();
		return {seed: this.seed, mode: 'negative', iterations, dispatched, rejected, failures};
	}

	async fuzzQualitative(): Promise<ReducerFuzzerReport> {
		const prng = new FuzzPrng({seed: this.seed});
		const driver = buildPermissivePolicyDriver();
		const runtime = freshRuntimeWith(driver);
		const failures: Array<ReducerFuzzerFailure> = [];
		const script = buildIdealisedScript(prng);
		assert.ok(script.length >= 1, 'qualitative script must contain at least one event');
		assert.ok(script.length <= FUZZ_ITERATIONS_MAX, 'qualitative script must respect cap');
		let dispatched = 0;
		for (let iteration = 0; iteration < script.length; iteration += 1) {
			const event = script[iteration];
			assert.ok(event !== undefined, 'qualitative script holes are not permitted');
			const failure = tryDispatch(runtime, event, this.seed, iteration, 'qualitative');
			if (failure !== null) failures.push(failure);
			else dispatched += 1;
			await flushMicrotasks(FUZZ_REDUCER_FLUSH_MICROTASKS_MAX);
		}
		await flushMicrotasks(FUZZ_REDUCER_FLUSH_MICROTASKS_MAX);
		assertQualitativeAcceptance(runtime, this.seed, failures);
		runtime.dispose();
		return {seed: this.seed, mode: 'qualitative', iterations: script.length, dispatched, rejected: 0, failures};
	}

	fuzzArbitraryOrder(iterations: number = FUZZ_REDUCER_ARBITRARY_ITERATIONS): ReducerFuzzerReport {
		assert.ok(Number.isInteger(iterations), 'iterations must be an integer');
		assert.ok(iterations >= 1, 'iterations must be >= 1');
		assert.ok(iterations <= FUZZ_ITERATIONS_MAX, 'iterations must respect FUZZ_ITERATIONS_MAX');
		const prng = new FuzzPrng({seed: this.seed});
		const runtime = freshRuntime();
		const failures: Array<ReducerFuzzerFailure> = [];
		let dispatched = 0;
		let rejected = 0;
		for (let iteration = 0; iteration < iterations; iteration += 1) {
			const event = generateArbitraryEvent(prng);
			const failure = tryDispatch(runtime, event, this.seed, iteration, 'arbitraryOrder');
			if (failure !== null) {
				rejected += 1;
				continue;
			}
			dispatched += 1;
			const guardFailure = guardRuntimeInvariants(runtime, this.seed, iteration, 'arbitraryOrder', event);
			if (guardFailure !== null) failures.push(guardFailure);
		}
		runtime.dispose();
		return {seed: this.seed, mode: 'arbitraryOrder', iterations, dispatched, rejected, failures};
	}

	fuzzExternalConnectionCycles(iterations: number = FUZZ_REDUCER_EXTERNAL_CONNECTION_ITERATIONS): ReducerFuzzerReport {
		assert.ok(Number.isInteger(iterations), 'iterations must be an integer');
		assert.ok(iterations >= 1, 'iterations must be >= 1');
		assert.ok(iterations <= FUZZ_ITERATIONS_MAX, 'iterations must respect FUZZ_ITERATIONS_MAX');
		const prng = new FuzzPrng({seed: this.seed});
		const runtime = freshRuntime();
		const failures: Array<ReducerFuzzerFailure> = [];
		let dispatched = 0;
		let rejected = 0;
		for (let iteration = 0; iteration < iterations; iteration += 1) {
			const event = generateExternalConnectionCycleEvent(prng);
			const failure = tryDispatch(runtime, event, this.seed, iteration, 'externalConnection');
			if (failure !== null) {
				rejected += 1;
				continue;
			}
			dispatched += 1;
			const guardFailure = guardRuntimeInvariants(runtime, this.seed, iteration, 'externalConnection', event);
			if (guardFailure !== null) failures.push(guardFailure);
		}
		const snapshotHash = hashSnapshot(runtime.snapshot);
		runtime.dispose();
		return {seed: this.seed, mode: 'externalConnection', iterations, dispatched, rejected, failures, snapshotHash};
	}
}

function freshRuntime(): VoiceEngineV2Runtime {
	const driver = new VoiceEngineV2SimulatorDriver({
		policy: permissivePolicy(),
		inventory: {audioInputs: ['mic-1'], audioOutputs: ['speaker-1'], cameras: ['cam-1']},
	});
	return freshRuntimeWith(driver);
}

function freshRuntimeWith(driver: VoiceEngineV2SimulatorDriver): VoiceEngineV2Runtime {
	const implementation = new VoiceEngineV2TestImplementation(driver);
	const clock = createVoiceEngineV2DeterministicClockPort(0, 1);
	return new VoiceEngineV2Runtime(implementation, {
		clock,
		eventLogSpillSink: createVoiceEngineV2MemoryEventLogSpillSink(),
	});
}

function permissivePolicy(): SimulatorDriverFaultPolicy {
	return {
		shouldDropConnect: () => false,
		shouldDropDisconnect: () => false,
		shouldFailMicrophonePublish: () => false,
		shouldFailCameraPublish: () => false,
		shouldFailScreenPublish: () => false,
		shouldFailNativeCaptureStart: () => false,
		shouldEmitDeviceLoss: () => false,
	};
}

function buildPermissivePolicyDriver(): VoiceEngineV2SimulatorDriver {
	return new VoiceEngineV2SimulatorDriver({
		policy: permissivePolicy(),
		inventory: {audioInputs: ['mic-1'], audioOutputs: ['speaker-1'], cameras: ['cam-1']},
	});
}

function generatePositiveEvent(prng: FuzzPrng): VoiceEngineV2Event {
	assert.ok(POSITIVE_EVENT_KINDS.length >= 1, 'positive event kinds must not be empty');
	const kind = prng.nextChoice(POSITIVE_EVENT_KINDS);
	return materialiseEvent(kind, prng, false);
}

function generateNegativeEvent(prng: FuzzPrng): VoiceEngineV2Event {
	assert.ok(POSITIVE_EVENT_KINDS.length >= 1, 'negative event kinds must not be empty');
	const kind = prng.nextChoice(POSITIVE_EVENT_KINDS);
	return materialiseEvent(kind, prng, true);
}

function generateArbitraryEvent(prng: FuzzPrng): VoiceEngineV2Event {
	const distort = prng.nextBool(0.4);
	const kind = prng.nextChoice(ARBITRARY_EVENT_KINDS);
	return materialiseEvent(kind, prng, distort);
}

function generateExternalConnectionCycleEvent(prng: FuzzPrng): VoiceEngineV2Event {
	assert.ok(EXTERNAL_CONNECTION_CYCLE_KIND_POOL.length >= 1, 'external connection kind pool must not be empty');
	const kind = prng.nextChoice(EXTERNAL_CONNECTION_CYCLE_KIND_POOL);
	return materialiseEvent(kind, prng, false);
}

interface MaterialiseContext {
	operationId: number;
	identity: string;
	captureId: string;
	enabled: boolean;
	negativeSpace: boolean;
}

function materialiseEvent(
	kind: VoiceEngineV2Event['type'],
	prng: FuzzPrng,
	negativeSpace: boolean,
): VoiceEngineV2Event {
	assert.equal(typeof kind, 'string', 'event kind must be a string');
	const ctx: MaterialiseContext = {
		operationId: (prng.nextU32() % 1024) + 1,
		identity: `identity-${prng.nextU32() % 64}`,
		captureId: negativeSpace && prng.nextBool(0.3) ? '' : `capture-${prng.nextU32() % 32}`,
		enabled: prng.nextBool(0.5),
		negativeSpace,
	};
	const connection = tryMaterialiseConnectionEvent(kind, prng, ctx);
	if (connection !== null) return connection;
	const mediaEvent = tryMaterialiseMediaEvent(kind, ctx);
	if (mediaEvent !== null) return mediaEvent;
	const controlEvent = tryMaterialiseControlEvent(kind, prng, ctx);
	if (controlEvent !== null) return controlEvent;
	return {type: 'implementation.prewarmRequested'};
}

function tryMaterialiseConnectionEvent(
	kind: VoiceEngineV2Event['type'],
	prng: FuzzPrng,
	ctx: MaterialiseContext,
): VoiceEngineV2Event | null {
	assert.equal(typeof kind, 'string', 'event kind must be a string');
	assert.ok(ctx !== null, 'materialise context must not be null');
	if (kind === 'implementation.prewarmRequested') return {type: 'implementation.prewarmRequested'};
	if (kind === 'connection.connectRequested') {
		return {
			type: 'connection.connectRequested',
			options: {url: `wss://voice.example.test/${ctx.operationId}`, token: `tok-${ctx.operationId}`},
		};
	}
	if (kind === 'connection.disconnectRequested') {
		return {type: 'connection.disconnectRequested', reason: prng.nextChoice(DISCONNECT_REASONS)};
	}
	if (kind === 'connection.reconnectRequested') return {type: 'connection.reconnectRequested'};
	if (kind === 'connection.externallyEstablished') {
		const endpointId = prng.nextU32() % 8;
		return {
			type: 'connection.externallyEstablished',
			options: {
				url: ctx.negativeSpace && prng.nextBool(0.3) ? '' : `wss://voice.example.test/external-${endpointId}`,
				token: ctx.negativeSpace && prng.nextBool(0.3) ? '' : `tok-ext-${endpointId}`,
			},
		};
	}
	if (kind === 'connection.remoteDisconnected') {
		const reason = prng.nextChoice(DISCONNECT_REASONS);
		if (prng.nextBool(0.25)) {
			return {
				type: 'connection.remoteDisconnected',
				reason,
				error: {code: 'liveKitError', message: `simulated remote drop (${reason})`},
			};
		}
		return {type: 'connection.remoteDisconnected', reason};
	}
	if (kind === 'gateway.voiceStateClearRequested') {
		return {
			type: 'gateway.voiceStateClearRequested',
			guildId: ctx.negativeSpace ? null : `guild-${ctx.operationId}`,
		};
	}
	if (kind === 'gateway.desiredVoiceStateChanged') {
		return {
			type: 'gateway.desiredVoiceStateChanged',
			desired: {
				guildId: ctx.negativeSpace ? null : `guild-${ctx.operationId}`,
				channelId: ctx.negativeSpace && prng.nextBool(0.3) ? null : `channel-${ctx.operationId}`,
				selfMute: ctx.enabled,
				selfDeaf: prng.nextBool(0.5),
				selfVideo: prng.nextBool(0.5),
				selfStream: prng.nextBool(0.5),
			},
		};
	}
	if (kind === 'gateway.voiceStateReconcileRequested') {
		return {type: 'gateway.voiceStateReconcileRequested'};
	}
	if (kind === 'gateway.voiceStateUpdated') {
		if (ctx.negativeSpace && prng.nextBool(0.3)) return {type: 'gateway.voiceStateUpdated', voiceState: null};
		return {
			type: 'gateway.voiceStateUpdated',
			voiceState: {
				guildId: `guild-${ctx.operationId}`,
				channelId: `channel-${ctx.operationId}`,
				userId: ctx.identity,
				sessionId: `session-${ctx.operationId}`,
				selfMute: ctx.enabled,
				selfDeaf: prng.nextBool(0.5),
				selfVideo: prng.nextBool(0.5),
				selfStream: prng.nextBool(0.5),
				suppress: false,
				requestToSpeakTimestamp: null,
			},
		};
	}
	return null;
}

function tryMaterialiseMediaEvent(
	kind: VoiceEngineV2Event['type'],
	ctx: MaterialiseContext,
): VoiceEngineV2Event | null {
	assert.equal(typeof kind, 'string', 'event kind must be a string');
	assert.ok(ctx !== null, 'materialise context must not be null');
	if (kind === 'microphone.publishRequested') {
		return {type: 'microphone.publishRequested', options: {deviceId: `mic-${ctx.operationId}`}};
	}
	if (kind === 'microphone.unpublishRequested') return {type: 'microphone.unpublishRequested'};
	if (kind === 'microphone.setEnabledRequested') return {type: 'microphone.setEnabledRequested', enabled: ctx.enabled};
	if (kind === 'camera.publishRequested') {
		return {type: 'camera.publishRequested', options: {deviceId: `cam-${ctx.operationId}`}};
	}
	if (kind === 'camera.unpublishRequested') return {type: 'camera.unpublishRequested'};
	if (kind === 'screen.publishRequested') {
		return {
			type: 'screen.publishRequested',
			options: {captureId: ctx.captureId, width: ctx.negativeSpace ? 0 : 1280, height: ctx.negativeSpace ? 0 : 720},
		};
	}
	if (kind === 'screen.unpublishRequested') return {type: 'screen.unpublishRequested'};
	if (kind === 'screenAudio.publishRequested') {
		return {
			type: 'screenAudio.publishRequested',
			options: {sampleRate: ctx.negativeSpace ? 0 : 48_000, numChannels: ctx.negativeSpace ? 0 : 2},
		};
	}
	if (kind === 'screenAudio.unpublishRequested') return {type: 'screenAudio.unpublishRequested'};
	return null;
}

function tryMaterialiseControlEvent(
	kind: VoiceEngineV2Event['type'],
	prng: FuzzPrng,
	ctx: MaterialiseContext,
): VoiceEngineV2Event | null {
	assert.equal(typeof kind, 'string', 'event kind must be a string');
	assert.ok(ctx !== null, 'materialise context must not be null');
	if (kind === 'outputDevice.setRequested') {
		return {type: 'outputDevice.setRequested', options: {deviceId: `out-${ctx.operationId}`}};
	}
	if (kind === 'participantVolume.setRequested') {
		return {
			type: 'participantVolume.setRequested',
			options: {participantIdentity: ctx.identity, volume: ctx.negativeSpace ? Number.NaN : 1},
		};
	}
	if (kind === 'remoteTrackSubscription.setRequested') {
		return {
			type: 'remoteTrackSubscription.setRequested',
			options: {participantIdentity: ctx.identity, source: 'microphone', subscribed: ctx.enabled},
		};
	}
	if (kind === 'data.publishRequested') return {type: 'data.publishRequested', options: {payload: new ArrayBuffer(0)}};
	if (kind === 'stats.collectRequested') return {type: 'stats.collectRequested'};
	if (kind === 'devices.enumerateRequested') return {type: 'devices.enumerateRequested'};
	if (kind === 'devices.selectAudioInputRequested') {
		return {type: 'devices.selectAudioInputRequested', deviceId: ctx.identity};
	}
	if (kind === 'devices.selectAudioOutputRequested') {
		return {type: 'devices.selectAudioOutputRequested', deviceId: ctx.identity};
	}
	if (kind === 'devices.selectCameraRequested') return {type: 'devices.selectCameraRequested', deviceId: ctx.identity};
	if (kind === 'permissions.checkRequested') return {type: 'permissions.checkRequested', name: 'microphone'};
	if (kind === 'permissions.requestRequested') return {type: 'permissions.requestRequested', name: 'microphone'};
	if (kind === 'capabilities.hardwareEncoderQueryRequested')
		return {type: 'capabilities.hardwareEncoderQueryRequested'};
	if (kind === 'localAudio.muteRequested') return {type: 'localAudio.muteRequested', muted: ctx.enabled};
	if (kind === 'localAudio.deafenRequested') return {type: 'localAudio.deafenRequested', deafened: ctx.enabled};
	if (kind === 'lifecycle.teardownRequested') {
		return {type: 'lifecycle.teardownRequested', reason: prng.nextChoice(LIFECYCLE_REASONS)};
	}
	return null;
}

function buildIdealisedScript(prng: FuzzPrng): Array<VoiceEngineV2Event> {
	assert.ok(prng !== undefined, 'qualitative script generator requires a PRNG');
	const tokenSalt = prng.nextU32() % 0xffff;
	const script: Array<VoiceEngineV2Event> = [];
	script.push({
		type: 'connection.connectRequested',
		options: {url: `wss://voice.example.test/qualitative-${tokenSalt}`, token: `tok-${tokenSalt}`},
	});
	script.push({type: 'microphone.publishRequested', options: {deviceId: 'mic-1'}});
	script.push({type: 'camera.publishRequested', options: {deviceId: 'cam-1'}});
	for (let participantIndex = 1; participantIndex <= 3; participantIndex += 1) {
		script.push({
			type: 'room.participantJoined',
			participant: {
				sid: `sid-${participantIndex}`,
				identity: `peer-${participantIndex}`,
				name: `Peer ${participantIndex}`,
			},
		});
	}
	script.push({
		type: 'participantVolume.setRequested',
		options: {participantIdentity: 'peer-1', volume: 0.75},
	});
	script.push({type: 'camera.unpublishRequested'});
	script.push({type: 'microphone.unpublishRequested'});
	script.push({type: 'connection.disconnectRequested', reason: 'user'});
	return script;
}

function tryDispatch(
	runtime: VoiceEngineV2Runtime,
	event: VoiceEngineV2Event,
	seed: number,
	iteration: number,
	mode: ReducerFuzzerFailure['mode'],
): ReducerFuzzerFailure | null {
	assert.ok(runtime instanceof VoiceEngineV2Runtime, 'runtime must be a VoiceEngineV2Runtime');
	assert.ok(event !== null, 'event must not be null');
	try {
		runtime.dispatch(event);
		return null;
	} catch (error) {
		return {
			seed,
			iteration,
			mode,
			reason: error instanceof Error ? error.message : String(error),
			event,
		};
	}
}

function guardRuntimeInvariants(
	runtime: VoiceEngineV2Runtime,
	seed: number,
	iteration: number,
	mode: ReducerFuzzerFailure['mode'],
	event: VoiceEngineV2Event,
): ReducerFuzzerFailure | null {
	try {
		assertEventLogInvariants(runtime);
		const snapshot = runtime.snapshot;
		assert.ok(snapshot !== null, 'snapshot must be non-null after dispatch');
		assert.equal(typeof snapshot.nextOperationId, 'number', 'nextOperationId must remain a number');
		assert.ok(snapshot.nextOperationId >= 1, 'nextOperationId must remain >= 1');
		return null;
	} catch (error) {
		return {
			seed,
			iteration,
			mode,
			reason: error instanceof Error ? error.message : String(error),
			event,
		};
	}
}

function assertSnapshotShapePreserved(
	before: VoiceEngineV2Snapshot | unknown,
	after: unknown,
	event: VoiceEngineV2Event,
	seed: number,
	failures: Array<ReducerFuzzerFailure>,
	iteration: number,
	mode: ReducerFuzzerFailure['mode'],
): void {
	assert.ok(before !== undefined, 'before snapshot must be defined');
	assert.ok(after !== undefined, 'after snapshot must be defined');
	const afterSnapshot = after as {nextOperationId: number; connection: {status: string}};
	if (typeof afterSnapshot.nextOperationId !== 'number') {
		failures.push({seed, iteration, mode, reason: 'nextOperationId lost number type', event});
		return;
	}
	if (typeof afterSnapshot.connection?.status !== 'string') {
		failures.push({seed, iteration, mode, reason: 'connection.status lost string type', event});
	}
}

function assertQualitativeAcceptance(
	runtime: VoiceEngineV2Runtime,
	seed: number,
	failures: Array<ReducerFuzzerFailure>,
): void {
	const snapshot = runtime.snapshot;
	assert.ok(snapshot !== null, 'qualitative snapshot must be non-null');
	const log = runtime.eventLog;
	assert.ok(Array.isArray(log), 'event log must be an array');
	assert.ok(log.length >= 1, 'qualitative event log must hold at least one entry');
	let lastSequence = 0;
	for (const entry of log) {
		assert.ok(entry.sequence > lastSequence, 'event log sequence must increase strictly');
		lastSequence = entry.sequence;
	}
	if (runtime.droppedEventsCount > 0) {
		failures.push({
			seed,
			iteration: -1,
			mode: 'qualitative',
			reason: `event log dropped ${runtime.droppedEventsCount} entries`,
			event: null,
		});
	}
}

async function flushMicrotasks(maxIterations: number): Promise<void> {
	assert.equal(typeof maxIterations, 'number', 'maxIterations must be a number');
	assert.ok(maxIterations >= 1, 'maxIterations must be >= 1');
	for (let index = 0; index < maxIterations; index += 1) {
		await Promise.resolve();
	}
}
