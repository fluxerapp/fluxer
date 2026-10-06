// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVoiceEngineV2EmptyFaultPlan,
	createVoiceEngineV2FaultPlan,
} from '@fluxer/voice_engine_v2/src/simulation/FaultInjector';
import {
	STABLE_STRINGIFY_DEPTH_MAX,
	stableStringify,
	VoiceEngineV2Simulator,
} from '@fluxer/voice_engine_v2/src/simulation/Simulator';
import {
	createVoiceEngineV2FiveParticipantConferenceWorkload,
	createVoiceEngineV2OneOnOneCallWorkload,
	createVoiceEngineV2ScreenShareWorkload,
	VoiceEngineV2WorkloadBuilder,
} from '@fluxer/voice_engine_v2/src/simulation/Workload';
import {describe, expect, it} from 'vitest';

describe('VoiceEngineV2Simulator determinism', () => {
	it('produces identical snapshot hashes for identical inputs', async () => {
		const workload = createVoiceEngineV2OneOnOneCallWorkload();
		const faults = createVoiceEngineV2EmptyFaultPlan();
		const first = await new VoiceEngineV2Simulator({seed: 1, workload, faults, mode: 'safety'}).run();
		const second = await new VoiceEngineV2Simulator({seed: 1, workload, faults, mode: 'safety'}).run();
		expect(first.snapshotHash).toBe(second.snapshotHash);
		expect(first.finalTick).toBe(second.finalTick);
	});

	it('produces different event logs for different seeds with packet-loss faults', async () => {
		const builder = new VoiceEngineV2WorkloadBuilder('seed-divergence');
		for (let i = 0; i < 16; i++) {
			builder.at(i * 2).connect({url: 'wss://voice.example.test', token: `tok-${i}`});
		}
		const workload = builder.build();
		const faults = createVoiceEngineV2FaultPlan([
			{kind: 'packetLoss', rate: 0.5, fromTick: 0, untilTick: workload.tickCount},
		]);
		const seedOne = await new VoiceEngineV2Simulator({seed: 1, workload, faults, mode: 'safety'}).run();
		const seedTwo = await new VoiceEngineV2Simulator({seed: 7, workload, faults, mode: 'safety'}).run();
		expect(seedOne.eventLog.length).toBeGreaterThan(0);
		expect(seedTwo.eventLog.length).toBeGreaterThan(0);
		expect(seedOne.snapshotHash).not.toBe(seedTwo.snapshotHash);
	});
});

describe('VoiceEngineV2Simulator safety mode', () => {
	it('reports no safety violations under an empty fault plan', async () => {
		const workload = createVoiceEngineV2OneOnOneCallWorkload();
		const result = await new VoiceEngineV2Simulator({
			seed: 42,
			workload,
			faults: createVoiceEngineV2EmptyFaultPlan(),
			mode: 'safety',
		}).run();
		expect(result.violations).toEqual([]);
	});

	it('reports no safety violations under sustained packet-loss faults', async () => {
		const workload = createVoiceEngineV2OneOnOneCallWorkload();
		const faults = createVoiceEngineV2FaultPlan([
			{kind: 'packetLoss', rate: 0.5, fromTick: 0, untilTick: workload.tickCount},
		]);
		const result = await new VoiceEngineV2Simulator({seed: 3, workload, faults, mode: 'safety'}).run();
		expect(result.violations).toEqual([]);
	});

	it('preserves event-log sequence under a high event-rate workload', async () => {
		const builder = new VoiceEngineV2WorkloadBuilder('high-rate');
		builder.at(0).connect({url: 'wss://voice.example.test', token: 'tok'});
		builder.advance(1).emit({type: 'connection.connectSucceeded', operationId: 1});
		for (let i = 0; i < 64; i++) {
			builder.advance(1).joinParticipant({
				sid: `sid-${i}`,
				identity: `peer-${i}`,
				name: `Peer ${i}`,
			});
		}
		const workload = builder.build();
		const result = await new VoiceEngineV2Simulator({
			seed: 9,
			workload,
			faults: createVoiceEngineV2EmptyFaultPlan(),
			mode: 'safety',
		}).run();
		expect(result.violations).toEqual([]);
		for (let i = 0; i < result.eventLog.length; i++) {
			expect(result.eventLog[i].sequence).toBe(i + 1);
		}
	});

	it('respects deterministic fault ordering across runs', async () => {
		const workload = createVoiceEngineV2ScreenShareWorkload();
		const faults = createVoiceEngineV2FaultPlan([
			{kind: 'gpuDeviceLost', atTick: 3},
			{kind: 'encoderFailed', captureId: 'cap-1', atTick: 5},
			{kind: 'deviceDisconnect', deviceId: 'default-mic', atTick: 7},
		]);
		const first = await new VoiceEngineV2Simulator({seed: 11, workload, faults, mode: 'safety'}).run();
		const second = await new VoiceEngineV2Simulator({seed: 11, workload, faults, mode: 'safety'}).run();
		expect(first.snapshotHash).toBe(second.snapshotHash);
		expect(first.eventLog.length).toBe(second.eventLog.length);
	});

	it('runs the screen-share workload without invariant violations', async () => {
		const workload = createVoiceEngineV2ScreenShareWorkload();
		const result = await new VoiceEngineV2Simulator({
			seed: 17,
			workload,
			faults: createVoiceEngineV2EmptyFaultPlan(),
			mode: 'safety',
		}).run();
		expect(result.violations).toEqual([]);
	});

	it('runs the five-party conference workload safely', async () => {
		const workload = createVoiceEngineV2FiveParticipantConferenceWorkload();
		const result = await new VoiceEngineV2Simulator({
			seed: 23,
			workload,
			faults: createVoiceEngineV2EmptyFaultPlan(),
			mode: 'safety',
		}).run();
		expect(result.violations).toEqual([]);
	});
});

describe('stableStringify', () => {
	it('matches JSON.stringify for a nested fixture whose keys are already sorted', () => {
		const sortedKeyFixture = {
			alpha: {inner: {deep: [{}, [], null, 'text'], list: [1, 2.5, -3]}},
			beta: [true, false, {a: 'x', b: 'y'}],
			gamma: 'value',
		};
		expect(stableStringify(sortedKeyFixture)).toBe(JSON.stringify(sortedKeyFixture));
	});

	it('sorts object keys so insertion order does not affect the output', () => {
		const insertionOrderOne = {gamma: 'value', alpha: {b: 2, a: 1}, beta: [{z: true, a: false}]};
		const insertionOrderTwo = {alpha: {a: 1, b: 2}, beta: [{a: false, z: true}], gamma: 'value'};
		expect(stableStringify(insertionOrderOne)).toBe(stableStringify(insertionOrderTwo));
		expect(stableStringify(insertionOrderOne)).toBe(JSON.stringify(insertionOrderTwo));
	});

	it('serializes non-finite numbers and undefined as null like before', () => {
		expect(stableStringify({a: Number.NaN, b: Number.POSITIVE_INFINITY, c: undefined})).toBe(
			'{"a":null,"b":null,"c":null}',
		);
	});

	it('handles nesting up to the named depth bound without recursion', () => {
		let nested: unknown = 'leaf';
		for (let i = 0; i < STABLE_STRINGIFY_DEPTH_MAX - 1; i += 1) {
			nested = [nested];
		}
		expect(stableStringify(nested)).toBe(JSON.stringify(nested));
	});

	it('crashes loudly when the depth bound is exceeded', () => {
		let nested: unknown = 'leaf';
		for (let i = 0; i < STABLE_STRINGIFY_DEPTH_MAX + 1; i += 1) {
			nested = [nested];
		}
		expect(() => stableStringify(nested)).toThrow('stableStringify depth budget exceeded');
	});
});

describe('VoiceEngineV2Simulator liveness mode', () => {
	it('recovers after a device disconnect by republishing the microphone', async () => {
		const builder = new VoiceEngineV2WorkloadBuilder('liveness-device');
		builder.at(0).connect({url: 'wss://voice.example.test', token: 'tok-live'});
		builder.advance(1).emit({type: 'connection.connectSucceeded', operationId: 1});
		builder.advance(1).publishMicrophone();
		builder
			.advance(10)
			.emit({type: 'room.participantJoined', participant: {sid: 'sid-r', identity: 'remote', name: 'Remote'}});
		builder.advance(20).publishMicrophone({deviceId: 'fallback-mic'});
		const workload = builder.build();
		const faults = createVoiceEngineV2FaultPlan([
			{kind: 'asymmetricPartition', peerIds: ['ghost-peer'], fromTick: 5},
			{kind: 'deviceDisconnect', deviceId: 'default-mic', atTick: 6},
		]);
		const result = await new VoiceEngineV2Simulator({seed: 29, workload, faults, mode: 'liveness'}).run();
		expect(result.partitionedPeers).toContain('ghost-peer');
		expect(result.livenessRecovered).toBe(true);
	});

	it('recovers after asymmetric partition by completing further publishes', async () => {
		const builder = new VoiceEngineV2WorkloadBuilder('liveness-partition');
		builder.at(0).connect({url: 'wss://voice.example.test', token: 'tok-live-2'});
		builder.advance(1).emit({type: 'connection.connectSucceeded', operationId: 1});
		builder.advance(2).publishMicrophone();
		builder.advance(15).publishMicrophone({deviceId: 'alt-mic'});
		const workload = builder.build();
		const faults = createVoiceEngineV2FaultPlan([{kind: 'asymmetricPartition', peerIds: ['ghost'], fromTick: 4}]);
		const result = await new VoiceEngineV2Simulator({seed: 31, workload, faults, mode: 'liveness'}).run();
		expect(result.partitionedPeers).toEqual(['ghost']);
		expect(result.livenessRecovered).toBe(true);
	});
});
