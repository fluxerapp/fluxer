// SPDX-License-Identifier: AGPL-3.0-or-later

import {VoiceEngineV2Simulator} from '@fluxer/voice_engine_v2/src/simulation/Simulator';
import {
	defineAsymmetricNatAfterConnectScenario,
	defineCaptureDeviceDisconnectScenario,
	defineEncoderFailUnderLoadScenario,
	defineGpuTdrMidFrameScenario,
	defineNetworkPartitionDuringScreenShareScenario,
	type VoiceEngineV2SimulationScenario,
} from '@fluxer/voice_engine_v2/src/simulation/scenarios/index';
import {describe, expect, it} from 'vitest';

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
