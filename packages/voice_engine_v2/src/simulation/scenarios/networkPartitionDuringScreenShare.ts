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

const PARTITION_FROM_TICK = 150;
const PARTITION_UNTIL_TICK = 350;
const PARTITION_RECOVERY_DEADLINE_TICK = 400;
const PARTITION_PARTICIPANT_COUNT = 4;
const PARTITION_RECONNECT_PROBE_TICK = 175;

export function defineNetworkPartitionDuringScreenShareScenario(seed: number): VoiceEngineV2SimulationScenario {
	assertSeedWellFormed(seed);
	const workload = buildWorkload();
	assert.ok(workload.tickCount <= 512, 'workload tick count exceeds the simulator budget');
	const faultPlan = createVoiceEngineV2FaultPlan([
		{kind: 'networkPartition', fromTick: PARTITION_FROM_TICK, untilTick: PARTITION_UNTIL_TICK},
	]);
	return {
		name: 'network-partition-during-screen-share',
		mode: 'safety',
		workload,
		faultPlan,
		acceptance: checkAcceptance,
	};
}

function buildWorkload(): VoiceEngineV2Workload {
	const builder = new VoiceEngineV2WorkloadBuilder('network-partition-during-screen-share');
	builder.at(0).connect({url: 'wss://voice.example.test', token: 'tok-a-1'});
	builder.advance(2).emit({type: 'connection.connectSucceeded', operationId: 1});
	builder.advance(1).publishMicrophone({deviceId: 'mic-A'});
	for (let index = 0; index < PARTITION_PARTICIPANT_COUNT; index++) {
		builder.advance(2).joinParticipant({
			sid: `sid-peer-${index + 1}`,
			identity: `peer-${index + 1}`,
			name: `Peer ${index + 1}`,
		});
	}
	builder.at(100).startNativeCapture({
		captureId: 'screen-A',
		source: {id: 'display-A', kind: 'screen', title: 'Display A'},
		width: 1920,
		height: 1080,
		frameRate: 30,
		includeCursor: true,
		includeAudio: false,
		zeroCopyRequired: true,
	});
	builder.advance(2).attachNativeFrameSink({sinkId: 'sink-A', captureId: 'screen-A', zeroCopyRequired: true});
	builder.advance(1).publishScreen({captureId: 'screen-A', width: 1920, height: 1080});
	builder.at(PARTITION_RECONNECT_PROBE_TICK).connect({url: 'wss://voice.example.test', token: 'tok-a-2'});
	builder.at(PARTITION_RECOVERY_DEADLINE_TICK).connect({url: 'wss://voice.example.test', token: 'tok-a-3'});
	return builder.build();
}

function checkAcceptance(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	assert.ok(result, 'acceptance requires a simulator result');
	assert.ok(Array.isArray(result.eventLog), 'acceptance requires an event log array');
	const verdicts = [verifyNoViolations(result), verifyPartitionDetected(result), verifyRecoveryAfterPartition(result)];
	return combineVerdicts(verdicts);
}

function verifyNoViolations(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	if (result.violations.length === 0) return passVerdict();
	return failVerdict(result.violations.map((violation) => `safety violation: ${violation.code}`));
}

function verifyPartitionDetected(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	let connectFailureSeen = false;
	for (const entry of result.eventLog) {
		if (entry.event.type === 'connection.connectFailed') connectFailureSeen = true;
	}
	if (!connectFailureSeen) {
		return failVerdict(['expected at least one connection.connectFailed during partition window']);
	}
	return passVerdict();
}

function verifyRecoveryAfterPartition(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	if (result.finalTick < PARTITION_RECOVERY_DEADLINE_TICK) {
		return failVerdict([`finalTick ${result.finalTick} ended before recovery deadline`]);
	}
	let recoverySucceeded = 0;
	for (const entry of result.eventLog) {
		if (entry.event.type === 'connection.connectSucceeded') recoverySucceeded += 1;
	}
	if (recoverySucceeded < 2) {
		return failVerdict(['expected connection.connectSucceeded for recovery operation after partition cleared']);
	}
	return passVerdict();
}
