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

const ENCODER_FAIL_AT_TICK = 220;
const ENCODER_FAIL_PARTICIPANT_COUNT = 4;
const ENCODER_FAIL_CAPTURE_ID = 'screen-load';
const ENCODER_FAIL_RETRY_TICK = 260;

export function defineEncoderFailUnderLoadScenario(seed: number): VoiceEngineV2SimulationScenario {
	assertSeedWellFormed(seed);
	const workload = buildWorkload();
	assert.ok(workload.tickCount <= 512, 'workload tick count exceeds the simulator budget');
	const faultPlan = createVoiceEngineV2FaultPlan([
		{kind: 'encoderFailed', captureId: ENCODER_FAIL_CAPTURE_ID, atTick: ENCODER_FAIL_AT_TICK},
	]);
	return {
		name: 'encoder-fail-under-load',
		mode: 'safety',
		workload,
		faultPlan,
		acceptance: checkAcceptance,
	};
}

function buildWorkload(): VoiceEngineV2Workload {
	const builder = new VoiceEngineV2WorkloadBuilder('encoder-fail-under-load');
	builder.at(0).connect({url: 'wss://voice.example.test', token: 'tok-enc'});
	builder.advance(2).emit({type: 'connection.connectSucceeded', operationId: 1});
	builder.advance(1).publishMicrophone({deviceId: 'mic-host'});
	for (let index = 0; index < ENCODER_FAIL_PARTICIPANT_COUNT; index++) {
		builder.advance(2).joinParticipant({
			sid: `sid-load-${index + 1}`,
			identity: `peer-load-${index + 1}`,
			name: `Load ${index + 1}`,
		});
	}
	builder.advance(4).startNativeCapture({
		captureId: ENCODER_FAIL_CAPTURE_ID,
		source: {id: 'display-load', kind: 'screen', title: 'Load Display'},
		width: 1920,
		height: 1080,
		frameRate: 60,
		includeCursor: true,
		includeAudio: false,
		zeroCopyRequired: true,
	});
	builder.advance(2).attachNativeFrameSink({
		sinkId: 'sink-load',
		captureId: ENCODER_FAIL_CAPTURE_ID,
		zeroCopyRequired: true,
	});
	builder.advance(1).publishScreen({captureId: ENCODER_FAIL_CAPTURE_ID, width: 1920, height: 1080});
	builder.at(ENCODER_FAIL_AT_TICK).startNativeCapture({
		captureId: ENCODER_FAIL_CAPTURE_ID,
		source: {id: 'display-load', kind: 'screen', title: 'Load Display'},
		width: 1920,
		height: 1080,
		frameRate: 60,
		includeCursor: true,
		includeAudio: false,
		zeroCopyRequired: true,
	});
	builder.at(ENCODER_FAIL_RETRY_TICK).startNativeCapture({
		captureId: 'screen-load-recovery',
		source: {id: 'display-load', kind: 'screen', title: 'Load Display'},
		width: 1280,
		height: 720,
		frameRate: 30,
		includeCursor: true,
		includeAudio: false,
		zeroCopyRequired: true,
	});
	return builder.build();
}

function checkAcceptance(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	assert.ok(result, 'acceptance requires a simulator result');
	assert.ok(Array.isArray(result.eventLog), 'acceptance requires an event log array');
	const verdicts = [verifyNoViolations(result), verifyEncoderFailure(result), verifyAudioContinues(result)];
	return combineVerdicts(verdicts);
}

function verifyNoViolations(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	if (result.violations.length === 0) return passVerdict();
	return failVerdict(result.violations.map((violation) => `safety violation: ${violation.code}`));
}

function verifyEncoderFailure(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	let encoderFailureObserved = false;
	for (const entry of result.eventLog) {
		if (entry.event.type === 'nativeCapture.failed' && entry.event.captureId === ENCODER_FAIL_CAPTURE_ID) {
			encoderFailureObserved = true;
		}
	}
	if (!encoderFailureObserved) {
		return failVerdict(['expected nativeCapture.failed signalling encoder overload']);
	}
	return passVerdict();
}

function verifyAudioContinues(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	let microphoneSucceeded = false;
	let microphoneFailed = false;
	for (const entry of result.eventLog) {
		if (entry.event.type === 'microphone.publishSucceeded') microphoneSucceeded = true;
		if (entry.event.type === 'microphone.publishFailed') microphoneFailed = true;
	}
	if (!microphoneSucceeded) {
		return failVerdict(['expected microphone publish to remain successful while encoder fails']);
	}
	if (microphoneFailed) {
		return failVerdict(['microphone publish unexpectedly failed during encoder overload']);
	}
	return passVerdict();
}
