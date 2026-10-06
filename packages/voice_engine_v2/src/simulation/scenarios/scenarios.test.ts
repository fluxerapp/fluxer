// SPDX-License-Identifier: AGPL-3.0-or-later

import {SIMULATOR_TICK_MAX, VoiceEngineV2Simulator} from '@fluxer/voice_engine_v2/src/simulation/Simulator';
import {
	defineAsymmetricNatAfterConnectScenario,
	defineCaptureDeviceDisconnectScenario,
	defineEncoderFailUnderLoadScenario,
	defineGpuTdrMidFrameScenario,
	defineNetworkPartitionDuringScreenShareScenario,
	type VoiceEngineV2SimulationScenario,
} from '@fluxer/voice_engine_v2/src/simulation/scenarios/index';
import {describe, expect, it} from 'vitest';

const SCENARIO_FACTORIES: ReadonlyArray<(seed: number) => VoiceEngineV2SimulationScenario> = [
	defineNetworkPartitionDuringScreenShareScenario,
	defineGpuTdrMidFrameScenario,
	defineCaptureDeviceDisconnectScenario,
	defineAsymmetricNatAfterConnectScenario,
	defineEncoderFailUnderLoadScenario,
];

async function runScenario(scenario: VoiceEngineV2SimulationScenario, seed: number) {
	return new VoiceEngineV2Simulator({
		seed,
		workload: scenario.workload,
		faults: scenario.faultPlan,
		mode: scenario.mode,
	}).run();
}

describe('scenario: networkPartitionDuringScreenShare', () => {
	it('passes acceptance after running the simulator', async () => {
		const scenario = defineNetworkPartitionDuringScreenShareScenario(101);
		const result = await runScenario(scenario, 101);
		const verdict = scenario.acceptance(result);
		expect(verdict.reasons).toEqual([]);
		expect(verdict.passed).toBe(true);
	});
});

describe('scenario: gpuTdrMidFrame', () => {
	it('passes acceptance after running the simulator', async () => {
		const scenario = defineGpuTdrMidFrameScenario(202);
		const result = await runScenario(scenario, 202);
		const verdict = scenario.acceptance(result);
		expect(verdict.reasons).toEqual([]);
		expect(verdict.passed).toBe(true);
	});
});

describe('scenario: captureDeviceDisconnect', () => {
	it('passes acceptance after running the simulator', async () => {
		const scenario = defineCaptureDeviceDisconnectScenario(303);
		const result = await runScenario(scenario, 303);
		const verdict = scenario.acceptance(result);
		expect(verdict.reasons).toEqual([]);
		expect(verdict.passed).toBe(true);
	});
});

describe('scenario: asymmetricNatAfterConnect', () => {
	it('passes acceptance after running the simulator', async () => {
		const scenario = defineAsymmetricNatAfterConnectScenario(404);
		const result = await runScenario(scenario, 404);
		const verdict = scenario.acceptance(result);
		expect(verdict.reasons).toEqual([]);
		expect(verdict.passed).toBe(true);
	});
});

describe('scenario: encoderFailUnderLoad', () => {
	it('passes acceptance after running the simulator', async () => {
		const scenario = defineEncoderFailUnderLoadScenario(505);
		const result = await runScenario(scenario, 505);
		const verdict = scenario.acceptance(result);
		expect(verdict.reasons).toEqual([]);
		expect(verdict.passed).toBe(true);
	});
});

describe('scenarios: cross-scenario determinism', () => {
	it('produces identical snapshot hashes across repeated runs of every scenario', async () => {
		const seed = 4242;
		for (const factory of SCENARIO_FACTORIES) {
			const scenario = factory(seed);
			const first = await runScenario(scenario, seed);
			const second = await runScenario(scenario, seed);
			expect(second.snapshotHash).toBe(first.snapshotHash);
			expect(second.finalTick).toBe(first.finalTick);
			expect(second.eventLog.length).toBe(first.eventLog.length);
		}
	});
});

describe('scenarios: tick budget', () => {
	it('each scenario completes within SIMULATOR_TICK_MAX', async () => {
		for (const factory of SCENARIO_FACTORIES) {
			const scenario = factory(7);
			expect(scenario.workload.tickCount).toBeLessThanOrEqual(SIMULATOR_TICK_MAX);
			const result = await runScenario(scenario, 7);
			expect(result.finalTick).toBeLessThanOrEqual(SIMULATOR_TICK_MAX);
		}
	});
});
