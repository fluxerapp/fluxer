// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2FaultPlan} from '@fluxer/voice_engine_v2/src/simulation/FaultInjector';
import type {
	VoiceEngineV2SimulatorMode,
	VoiceEngineV2SimulatorResult,
} from '@fluxer/voice_engine_v2/src/simulation/Simulator';
import type {VoiceEngineV2Workload} from '@fluxer/voice_engine_v2/src/simulation/Workload';

const SCENARIO_SEED_MAX = 0x7fffffff;

export interface VoiceEngineV2AcceptanceVerdict {
	readonly passed: boolean;
	readonly reasons: ReadonlyArray<string>;
}

export type VoiceEngineV2AcceptanceCriteria = (result: VoiceEngineV2SimulatorResult) => VoiceEngineV2AcceptanceVerdict;

export interface VoiceEngineV2SimulationScenario {
	readonly name: string;
	readonly mode: VoiceEngineV2SimulatorMode;
	readonly workload: VoiceEngineV2Workload;
	readonly faultPlan: VoiceEngineV2FaultPlan;
	readonly acceptance: VoiceEngineV2AcceptanceCriteria;
}

export function passVerdict(): VoiceEngineV2AcceptanceVerdict {
	return {passed: true, reasons: []};
}

export function failVerdict(reasons: ReadonlyArray<string>): VoiceEngineV2AcceptanceVerdict {
	assert.ok(Array.isArray(reasons), 'failVerdict requires a reasons array');
	assert.ok(reasons.length > 0, 'failVerdict requires at least one reason');
	return {passed: false, reasons: [...reasons]};
}

export function combineVerdicts(
	verdicts: ReadonlyArray<VoiceEngineV2AcceptanceVerdict>,
): VoiceEngineV2AcceptanceVerdict {
	assert.ok(Array.isArray(verdicts), 'combineVerdicts requires an array');
	assert.ok(verdicts.length > 0, 'combineVerdicts requires at least one verdict');
	const reasons: Array<string> = [];
	for (const verdict of verdicts) {
		for (const reason of verdict.reasons) reasons.push(reason);
	}
	return reasons.length === 0 ? passVerdict() : failVerdict(reasons);
}

export function assertSeedWellFormed(seed: number): void {
	assert.ok(Number.isInteger(seed), 'scenario seed must be an integer');
	assert.ok(seed >= 0, 'scenario seed must be non-negative');
	assert.ok(seed <= SCENARIO_SEED_MAX, 'scenario seed exceeds SCENARIO_SEED_MAX');
}

export {defineAsymmetricNatAfterConnectScenario} from '@fluxer/voice_engine_v2/src/simulation/scenarios/asymmetricNatAfterConnect';
export {defineCaptureDeviceDisconnectScenario} from '@fluxer/voice_engine_v2/src/simulation/scenarios/captureDeviceDisconnect';
export {defineEncoderFailUnderLoadScenario} from '@fluxer/voice_engine_v2/src/simulation/scenarios/encoderFailUnderLoad';
export {defineGpuTdrMidFrameScenario} from '@fluxer/voice_engine_v2/src/simulation/scenarios/gpuTdrMidFrame';
export {defineNetworkPartitionDuringScreenShareScenario} from '@fluxer/voice_engine_v2/src/simulation/scenarios/networkPartitionDuringScreenShare';
