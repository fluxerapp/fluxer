// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVoiceEngineV2EmptyFaultPlan,
	createVoiceEngineV2FaultPlan,
} from '@fluxer/voice_engine_v2/src/simulation/FaultInjector';
import {VoiceEngineV2Simulator} from '@fluxer/voice_engine_v2/src/simulation/Simulator';
import {
	createVoiceEngineV2ExternalEstablishmentCycleWorkload,
	SIMULATOR_EXTERNAL_ESTABLISH_CYCLES,
	VoiceEngineV2WorkloadBuilder,
} from '@fluxer/voice_engine_v2/src/simulation/Workload';
import {describe, expect, it} from 'vitest';

describe('external establishment workload shape', () => {
	it('emits an establish and a remote disconnect for every cycle', () => {
		const workload = createVoiceEngineV2ExternalEstablishmentCycleWorkload();
		const establishCount = workload.steps.filter(
			(step) => step.event.type === 'connection.externallyEstablished',
		).length;
		const remoteDisconnectCount = workload.steps.filter(
			(step) => step.event.type === 'connection.remoteDisconnected',
		).length;
		expect(establishCount).toBe(SIMULATOR_EXTERNAL_ESTABLISH_CYCLES + 1);
		expect(remoteDisconnectCount).toBe(SIMULATOR_EXTERNAL_ESTABLISH_CYCLES);
	});

	it('keeps step ticks monotonic', () => {
		const workload = createVoiceEngineV2ExternalEstablishmentCycleWorkload();
		let previousTick = 0;
		for (const step of workload.steps) {
			expect(step.tick).toBeGreaterThanOrEqual(previousTick);
			previousTick = step.tick;
		}
		expect(workload.tickCount).toBeGreaterThan(0);
	});

	it('supports the builder helpers for externally-driven connection events', () => {
		const workload = new VoiceEngineV2WorkloadBuilder('external-helpers')
			.at(0)
			.externallyEstablish({url: 'wss://voice.example.test', token: 'tok-1'})
			.advance(1)
			.remoteDisconnect('network')
			.build();
		expect(workload.steps.map((step) => step.event.type)).toEqual([
			'connection.externallyEstablished',
			'connection.remoteDisconnected',
		]);
	});

	it('asserts on malformed externally-established options', () => {
		const builder = new VoiceEngineV2WorkloadBuilder('external-invalid');
		expect(() => builder.externallyEstablish({url: 1 as unknown as string, token: 'tok'})).toThrow(
			'externallyEstablish url must be a string',
		);
	});
});

describe('external establishment simulation safety', () => {
	it('reports no structural safety violations across establish/disconnect cycles', async () => {
		const workload = createVoiceEngineV2ExternalEstablishmentCycleWorkload();
		const result = await new VoiceEngineV2Simulator({
			seed: 11,
			workload,
			faults: createVoiceEngineV2EmptyFaultPlan(),
			mode: 'safety',
		}).run();
		expect(result.violations).toEqual([]);
		expect(result.eventLog.length).toBeGreaterThan(0);
	});

	it('produces identical snapshot hashes for identical seeds (determinism)', async () => {
		const workload = createVoiceEngineV2ExternalEstablishmentCycleWorkload();
		const faults = createVoiceEngineV2EmptyFaultPlan();
		const first = await new VoiceEngineV2Simulator({seed: 5, workload, faults, mode: 'safety'}).run();
		const second = await new VoiceEngineV2Simulator({seed: 5, workload, faults, mode: 'safety'}).run();
		expect(first.snapshotHash).toBe(second.snapshotHash);
		expect(first.eventLog.length).toBe(second.eventLog.length);
	});

	it('keeps the event log sequence strictly increasing through the cycles', async () => {
		const workload = createVoiceEngineV2ExternalEstablishmentCycleWorkload();
		const result = await new VoiceEngineV2Simulator({
			seed: 23,
			workload,
			faults: createVoiceEngineV2EmptyFaultPlan(),
			mode: 'safety',
		}).run();
		let previousSequence = 0;
		for (const entry of result.eventLog) {
			expect(entry.sequence).toBeGreaterThan(previousSequence);
			previousSequence = entry.sequence;
		}
	});

	it('stays violation-free under sustained packet loss across cycles', async () => {
		const workload = createVoiceEngineV2ExternalEstablishmentCycleWorkload();
		const faults = createVoiceEngineV2FaultPlan([
			{kind: 'packetLoss', rate: 0.5, fromTick: 0, untilTick: workload.tickCount},
		]);
		const result = await new VoiceEngineV2Simulator({seed: 3, workload, faults, mode: 'safety'}).run();
		expect(result.violations).toEqual([]);
	});

	it('records every externally-driven connection event in the durable log', async () => {
		const workload = createVoiceEngineV2ExternalEstablishmentCycleWorkload();
		const result = await new VoiceEngineV2Simulator({
			seed: 17,
			workload,
			faults: createVoiceEngineV2EmptyFaultPlan(),
			mode: 'safety',
		}).run();
		const establishEntries = result.eventLog.filter((entry) => entry.event.type === 'connection.externallyEstablished');
		const remoteDisconnectEntries = result.eventLog.filter(
			(entry) => entry.event.type === 'connection.remoteDisconnected',
		);
		expect(establishEntries.length).toBe(SIMULATOR_EXTERNAL_ESTABLISH_CYCLES + 1);
		expect(remoteDisconnectEntries.length).toBe(SIMULATOR_EXTERNAL_ESTABLISH_CYCLES);
	});
});
