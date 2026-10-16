// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVoiceEngineV2EmptyFaultPlan,
	createVoiceEngineV2FaultPlan,
} from '@fluxer/voice_engine_v2/src/simulation/FaultInjector';
import {VoiceEngineV2Simulator} from '@fluxer/voice_engine_v2/src/simulation/Simulator';
import {
	createVoiceEngineV2FiveParticipantConferenceWorkload,
	createVoiceEngineV2OneOnOneCallWorkload,
	createVoiceEngineV2ScreenShareWorkload,
	VoiceEngineV2WorkloadBuilder,
} from '@fluxer/voice_engine_v2/src/simulation/Workload';
import {describe, expect, it} from 'vitest';

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
