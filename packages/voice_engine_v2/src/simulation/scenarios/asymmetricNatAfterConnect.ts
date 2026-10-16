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

const ASYM_PARTITION_FROM_TICK = 200;
const ASYM_PARTITION_PEER_ID = 'B';
const ASYM_FINAL_PROBE_TICK = 240;

export function defineAsymmetricNatAfterConnectScenario(seed: number): VoiceEngineV2SimulationScenario {
	assertSeedWellFormed(seed);
	const workload = buildWorkload();
	assert.ok(workload.tickCount <= 512, 'workload tick count exceeds the simulator budget');
	const faultPlan = createVoiceEngineV2FaultPlan([
		{kind: 'asymmetricPartition', peerIds: [ASYM_PARTITION_PEER_ID], fromTick: ASYM_PARTITION_FROM_TICK},
	]);
	return {
		name: 'asymmetric-nat-after-connect',
		mode: 'safety',
		workload,
		faultPlan,
		acceptance: checkAcceptance,
	};
}

function buildWorkload(): VoiceEngineV2Workload {
	const builder = new VoiceEngineV2WorkloadBuilder('asymmetric-nat-after-connect');
	builder.at(0).connect({url: 'wss://voice.example.test', token: 'tok-asym'});
	builder.advance(2).emit({type: 'connection.connectSucceeded', operationId: 1});
	builder.advance(1).publishMicrophone({deviceId: 'mic-A'});
	builder.advance(2).joinParticipant({sid: 'sid-A', identity: 'A', name: 'Alice'});
	builder.advance(2).joinParticipant({sid: 'sid-B', identity: 'B', name: 'Bob'});
	builder.advance(2).joinParticipant({sid: 'sid-C', identity: 'C', name: 'Carol'});
	builder.at(ASYM_PARTITION_FROM_TICK).leaveParticipant('B', 'sid-B');
	builder.at(ASYM_FINAL_PROBE_TICK).publishMicrophone({deviceId: 'mic-A-2'});
	return builder.build();
}

function checkAcceptance(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	assert.ok(result, 'acceptance requires a simulator result');
	assert.ok(Array.isArray(result.eventLog), 'acceptance requires an event log array');
	const verdicts = [
		verifyNoViolations(result),
		verifyPartitionTracked(result),
		verifyPeerBLeft(result),
		verifyAandCContinue(result),
	];
	return combineVerdicts(verdicts);
}

function verifyNoViolations(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	if (result.violations.length === 0) return passVerdict();
	return failVerdict(result.violations.map((violation) => `safety violation: ${violation.code}`));
}

function verifyPartitionTracked(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	if (!result.partitionedPeers.includes(ASYM_PARTITION_PEER_ID)) {
		return failVerdict([`expected partitionedPeers to include ${ASYM_PARTITION_PEER_ID}`]);
	}
	return passVerdict();
}

function verifyPeerBLeft(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	let peerLeft = false;
	let peerRejoined = false;
	for (const entry of result.eventLog) {
		if (entry.event.type === 'room.participantLeft' && entry.event.participantIdentity === ASYM_PARTITION_PEER_ID) {
			peerLeft = true;
		}
		if (entry.event.type === 'room.participantJoined' && peerLeft) {
			if (entry.event.participant.identity === ASYM_PARTITION_PEER_ID) peerRejoined = true;
		}
	}
	if (!peerLeft) return failVerdict([`expected room.participantLeft for ${ASYM_PARTITION_PEER_ID}`]);
	if (peerRejoined) return failVerdict([`peer ${ASYM_PARTITION_PEER_ID} re-joined before partition lifts`]);
	return passVerdict();
}

function verifyAandCContinue(result: VoiceEngineV2SimulatorResult): VoiceEngineV2AcceptanceVerdict {
	let aPresent = false;
	let cPresent = false;
	let microphoneOps = 0;
	for (const entry of result.eventLog) {
		if (entry.event.type === 'room.participantJoined') {
			if (entry.event.participant.identity === 'A') aPresent = true;
			if (entry.event.participant.identity === 'C') cPresent = true;
		}
		if (entry.event.type === 'microphone.publishRequested') microphoneOps += 1;
	}
	if (!aPresent) return failVerdict(['expected peer A to remain joined']);
	if (!cPresent) return failVerdict(['expected peer C to remain joined']);
	if (microphoneOps < 2) return failVerdict(['expected microphone publish attempts on both sides of partition']);
	return passVerdict();
}
