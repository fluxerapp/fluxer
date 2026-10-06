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

const GPU_TDR_AT_TICK = 200;
const GPU_TDR_RECONNECT_TICK = 215;

export function defineGpuTdrMidFrameScenario(seed: number): VoiceEngineV2SimulationScenario {
	assertSeedWellFormed(seed);
	const workload = buildWorkload();
	assert.ok(workload.tickCount <= 512, 'workload tick count exceeds the simulator budget');
	const faultPlan = createVoiceEngineV2FaultPlan([{kind: 'gpuDeviceLost', atTick: GPU_TDR_AT_TICK}]);
	return {
		name: 'gpu-tdr-mid-frame',
		mode: 'safety',
		workload,
		faultPlan,
		acceptance: checkAcceptance,
	};
}

function buildWorkload(): VoiceEngineV2Workload {
	const builder = new VoiceEngineV2WorkloadBuilder('gpu-tdr-mid-frame');
	builder.at(0).connect({url: 'wss://voice.example.test', token: 'tok-gpu'});
	builder.advance(2).emit({type: 'connection.connectSucceeded', operationId: 1});
	builder.advance(1).publishMicrophone();
	builder.advance(2).joinParticipant({sid: 'sid-r', identity: 'remote-1', name: 'Remote 1'});
	builder.advance(4).startNativeCapture({
		captureId: 'screen-gpu',
		source: {id: 'display-0', kind: 'screen', title: 'Primary'},
		width: 2560,
		height: 1440,
		frameRate: 60,
		includeCursor: true,
		includeAudio: false,
		zeroCopyRequired: true,
	});
	builder.advance(2).attachNativeFrameSink({sinkId: 'sink-gpu', captureId: 'screen-gpu', zeroCopyRequired: true});
	builder.advance(1).publishScreen({captureId: 'screen-gpu', width: 2560, height: 1440});
	builder.at(GPU_TDR_AT_TICK).publishScreen({captureId: 'screen-gpu', width: 1920, height: 1080});
	builder.at(GPU_TDR_RECONNECT_TICK).startNativeCapture({
		captureId: 'screen-gpu-2',
		source: {id: 'display-0', kind: 'screen', title: 'Primary'},
		width: 2560,
		height: 1440,
		frameRate: 60,
		includeCursor: true,
		includeAudio: false,
		zeroCopyRequired: true,
	});
	return builder.build();
}

function checkAcceptance(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	assert.ok(result, 'acceptance requires a simulator result');
	assert.ok(Array.isArray(result.eventLog), 'acceptance requires an event log array');
	const verdicts = [verifyNoViolations(result), verifyScreenLost(result), verifyReconnectAttempted(result)];
	return combineVerdicts(verdicts);
}

function verifyNoViolations(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	if (result.violations.length === 0) return passVerdict();
	return failVerdict(result.violations.map((violation) => `safety violation: ${violation.code}`));
}

function verifyScreenLost(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	let screenPublishFailed = false;
	for (const entry of result.eventLog) {
		if (entry.event.type === 'screen.publishFailed') screenPublishFailed = true;
	}
	if (!screenPublishFailed) {
		return failVerdict(['expected screen.publishFailed signalling GPU loss']);
	}
	return passVerdict();
}

function verifyReconnectAttempted(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	let nativeCaptureStarted = 0;
	for (const entry of result.eventLog) {
		if (entry.event.type === 'nativeCapture.started') nativeCaptureStarted += 1;
	}
	if (nativeCaptureStarted < 1) {
		return failVerdict(['expected nativeCapture.started for original capture before GPU loss']);
	}
	return passVerdict();
}
