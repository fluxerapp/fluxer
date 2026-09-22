// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createVoiceEngineV2FaultPlan} from '@fluxer/voice_engine_v2/src/simulation/FaultInjector';
import type {VoiceEngineV2SimulatorResult} from '@fluxer/voice_engine_v2/src/simulation/Simulator';
import {
	assertSeedWellFormed,
	combineVerdicts,
	failVerdict,
	passVerdict,
	type VoiceEngineV2AcceptanceVerdict,
	type VoiceEngineV2SimulationScenario,
} from '@fluxer/voice_engine_v2/src/simulation/scenarios/index';
import type {VoiceEngineV2Workload} from '@fluxer/voice_engine_v2/src/simulation/Workload';
import {VoiceEngineV2WorkloadBuilder} from '@fluxer/voice_engine_v2/src/simulation/Workload';

const DEVICE_DISCONNECT_TICK = 100;
const DEVICE_DISCONNECT_DEVICE_ID = 'mic-1';
const DEVICE_DISCONNECT_RECOVERY_TICK = 140;

export function defineCaptureDeviceDisconnectScenario(seed: number): VoiceEngineV2SimulationScenario {
	assertSeedWellFormed(seed);
	const workload = buildWorkload();
	assert.ok(workload.tickCount <= 512, 'workload tick count exceeds the simulator budget');
	const faultPlan = createVoiceEngineV2FaultPlan([
		{kind: 'deviceDisconnect', deviceId: DEVICE_DISCONNECT_DEVICE_ID, atTick: DEVICE_DISCONNECT_TICK},
	]);
	return {
		name: 'capture-device-disconnect',
		mode: 'safety',
		workload,
		faultPlan,
		acceptance: checkAcceptance,
	};
}

function buildWorkload(): VoiceEngineV2Workload {
	const builder = new VoiceEngineV2WorkloadBuilder('capture-device-disconnect');
	builder.at(0).connect({url: 'wss://voice.example.test', token: 'tok-cap'});
	builder.advance(2).emit({type: 'connection.connectSucceeded', operationId: 1});
	builder.advance(1).publishMicrophone({deviceId: DEVICE_DISCONNECT_DEVICE_ID});
	builder.advance(1).publishCamera({deviceId: 'cam-1'});
	builder.advance(2).joinParticipant({sid: 'sid-r', identity: 'remote', name: 'Remote'});
	builder.at(DEVICE_DISCONNECT_TICK).publishMicrophone({deviceId: 'mic-1-retry'});
	builder.at(DEVICE_DISCONNECT_RECOVERY_TICK).publishMicrophone({deviceId: 'mic-2'});
	return builder.build();
}

function checkAcceptance(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	assert.ok(result, 'acceptance requires a simulator result');
	assert.ok(Array.isArray(result.eventLog), 'acceptance requires an event log array');
	const verdicts = [
		verifyNoViolations(result),
		verifyMicrophoneFailed(result),
		verifyCameraUnaffected(result),
		verifyRecovery(result),
	];
	return combineVerdicts(verdicts);
}

function verifyNoViolations(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	if (result.violations.length === 0) return passVerdict();
	return failVerdict(result.violations.map((violation) => `safety violation: ${violation.code}`));
}

function verifyMicrophoneFailed(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	let microphoneFailureSeen = false;
	for (const entry of result.eventLog) {
		if (entry.event.type === 'microphone.publishFailed') microphoneFailureSeen = true;
	}
	if (!microphoneFailureSeen) {
		return failVerdict(['expected microphone.publishFailed after device disconnect']);
	}
	return passVerdict();
}

function verifyCameraUnaffected(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	let cameraSucceeded = false;
	let cameraFailed = false;
	for (const entry of result.eventLog) {
		if (entry.event.type === 'camera.publishSucceeded') cameraSucceeded = true;
		if (entry.event.type === 'camera.publishFailed') cameraFailed = true;
	}
	if (!cameraSucceeded) {
		return failVerdict(['expected camera.publishSucceeded before disconnect window']);
	}
	if (cameraFailed) {
		return failVerdict(['unexpected camera.publishFailed during microphone disconnect window']);
	}
	return passVerdict();
}

function verifyRecovery(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	if (result.finalTick < DEVICE_DISCONNECT_RECOVERY_TICK) {
		return failVerdict([`finalTick ${result.finalTick} ended before recovery deadline`]);
	}
	let microphoneSucceededCount = 0;
	for (const entry of result.eventLog) {
		if (entry.event.type === 'microphone.publishSucceeded') microphoneSucceededCount += 1;
	}
	if (microphoneSucceededCount < 1) {
		return failVerdict(['expected at least one microphone.publishSucceeded across run']);
	}
	return passVerdict();
}
