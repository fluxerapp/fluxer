// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	createSimulatorRandom,
	type VoiceEngineV2SimulatorRandom,
} from '@fluxer/voice_engine_v2/src/simulation/SimulatorPorts';

const SIMULATOR_FAULTS_MAX = 128;
const SIMULATOR_PEERS_PER_FAULT_MAX = 16;

export type VoiceEngineV2Fault =
	| {kind: 'packetLoss'; rate: number; fromTick: number; untilTick: number}
	| {kind: 'packetJitter'; jitterTicks: number; fromTick: number; untilTick: number}
	| {kind: 'packetReorder'; rate: number; fromTick: number; untilTick: number}
	| {kind: 'deviceDisconnect'; deviceId: string; atTick: number}
	| {kind: 'gpuDeviceLost'; atTick: number}
	| {kind: 'encoderFailed'; captureId: string; atTick: number}
	| {kind: 'networkPartition'; fromTick: number; untilTick: number}
	| {kind: 'asymmetricPartition'; peerIds: ReadonlyArray<string>; fromTick: number};

export interface VoiceEngineV2FaultPlan {
	readonly faults: ReadonlyArray<VoiceEngineV2Fault>;
}

export interface VoiceEngineV2FaultDecision {
	connectShouldDrop: boolean;
	disconnectShouldDrop: boolean;
	microphonePublishShouldFail: boolean;
	cameraPublishShouldFail: boolean;
	screenPublishShouldFail: boolean;
	failingCaptureIds: ReadonlyArray<string>;
	deviceLossDeviceIds: ReadonlyArray<string>;
	gpuLost: boolean;
	asymmetricallyPartitionedPeers: ReadonlyArray<string>;
	networkPartitionActive: boolean;
}

interface MutableVoiceEngineV2FaultDecision {
	connectShouldDrop: boolean;
	disconnectShouldDrop: boolean;
	microphonePublishShouldFail: boolean;
	cameraPublishShouldFail: boolean;
	screenPublishShouldFail: boolean;
	failingCaptureIds: Array<string>;
	deviceLossDeviceIds: Array<string>;
	gpuLost: boolean;
	asymmetricallyPartitionedPeers: Array<string>;
	networkPartitionActive: boolean;
}

export function createVoiceEngineV2EmptyFaultPlan(): VoiceEngineV2FaultPlan {
	return {faults: []};
}

export function createVoiceEngineV2FaultPlan(faults: ReadonlyArray<VoiceEngineV2Fault>): VoiceEngineV2FaultPlan {
	assert.ok(Array.isArray(faults), 'faults must be an array');
	assert.ok(faults.length <= SIMULATOR_FAULTS_MAX, 'fault plan exceeds SIMULATOR_FAULTS_MAX cap');
	for (const fault of faults) assertFaultWellFormed(fault);
	return {faults: [...faults]};
}

export class VoiceEngineV2FaultInjector {
	private readonly random: VoiceEngineV2SimulatorRandom;
	private readonly faults: ReadonlyArray<VoiceEngineV2Fault>;

	constructor(seed: number, plan: VoiceEngineV2FaultPlan) {
		assert.ok(Number.isInteger(seed), 'fault injector seed must be an integer');
		assert.ok(plan, 'fault injector requires a plan');
		assert.ok(plan.faults.length <= SIMULATOR_FAULTS_MAX, 'plan exceeds the SIMULATOR_FAULTS_MAX cap');
		this.random = createSimulatorRandom(seed ^ 0x1357acef);
		this.faults = plan.faults;
	}

	decide(tick: number): VoiceEngineV2FaultDecision {
		assert.ok(Number.isInteger(tick), 'fault decision tick must be an integer');
		assert.ok(tick >= 0, 'fault decision tick must be non-negative');
		const decision: MutableVoiceEngineV2FaultDecision = blankDecision();
		for (const fault of this.faults) this.applyFault(decision, fault, tick);
		return decision;
	}

	private applyFault(decision: MutableVoiceEngineV2FaultDecision, fault: VoiceEngineV2Fault, tick: number): void {
		switch (fault.kind) {
			case 'packetLoss':
				this.applyPacketLoss(decision, fault, tick);
				return;
			case 'packetJitter':
			case 'packetReorder':
				return;
			case 'deviceDisconnect':
				this.applyDeviceDisconnect(decision, fault, tick);
				return;
			case 'gpuDeviceLost':
				this.applyGpuDeviceLost(decision, fault, tick);
				return;
			case 'encoderFailed':
				this.applyEncoderFailed(decision, fault, tick);
				return;
			case 'networkPartition':
				this.applyNetworkPartition(decision, fault, tick);
				return;
			case 'asymmetricPartition':
				this.applyAsymmetricPartition(decision, fault, tick);
				return;
		}
	}

	private applyPacketLoss(
		decision: MutableVoiceEngineV2FaultDecision,
		fault: VoiceEngineV2Fault & {kind: 'packetLoss'},
		tick: number,
	): void {
		if (tick < fault.fromTick) return;
		if (tick > fault.untilTick) return;
		if (this.random.nextBool(fault.rate)) decision.connectShouldDrop = true;
		if (this.random.nextBool(fault.rate / 2)) decision.microphonePublishShouldFail = true;
	}

	private applyDeviceDisconnect(
		decision: MutableVoiceEngineV2FaultDecision,
		fault: VoiceEngineV2Fault & {kind: 'deviceDisconnect'},
		tick: number,
	): void {
		if (tick !== fault.atTick) return;
		decision.deviceLossDeviceIds.push(fault.deviceId);
		decision.microphonePublishShouldFail = true;
	}

	private applyGpuDeviceLost(
		decision: MutableVoiceEngineV2FaultDecision,
		fault: VoiceEngineV2Fault & {kind: 'gpuDeviceLost'},
		tick: number,
	): void {
		if (tick !== fault.atTick) return;
		decision.gpuLost = true;
		decision.screenPublishShouldFail = true;
	}

	private applyEncoderFailed(
		decision: MutableVoiceEngineV2FaultDecision,
		fault: VoiceEngineV2Fault & {kind: 'encoderFailed'},
		tick: number,
	): void {
		if (tick !== fault.atTick) return;
		decision.failingCaptureIds.push(fault.captureId);
	}

	private applyNetworkPartition(
		decision: MutableVoiceEngineV2FaultDecision,
		fault: VoiceEngineV2Fault & {kind: 'networkPartition'},
		tick: number,
	): void {
		if (tick < fault.fromTick) return;
		if (tick > fault.untilTick) return;
		decision.networkPartitionActive = true;
		decision.connectShouldDrop = true;
		decision.disconnectShouldDrop = false;
	}

	private applyAsymmetricPartition(
		decision: MutableVoiceEngineV2FaultDecision,
		fault: VoiceEngineV2Fault & {kind: 'asymmetricPartition'},
		tick: number,
	): void {
		if (tick < fault.fromTick) return;
		for (const peer of fault.peerIds) decision.asymmetricallyPartitionedPeers.push(peer);
	}
}

function blankDecision(): MutableVoiceEngineV2FaultDecision {
	return {
		connectShouldDrop: false,
		disconnectShouldDrop: false,
		microphonePublishShouldFail: false,
		cameraPublishShouldFail: false,
		screenPublishShouldFail: false,
		failingCaptureIds: [],
		deviceLossDeviceIds: [],
		gpuLost: false,
		asymmetricallyPartitionedPeers: [],
		networkPartitionActive: false,
	};
}

function assertFaultWellFormed(fault: VoiceEngineV2Fault): void {
	assert.ok(fault, 'fault must be defined');
	switch (fault.kind) {
		case 'packetLoss':
			assert.ok(fault.rate >= 0 && fault.rate <= 1, 'packetLoss rate out of bounds');
			assert.ok(fault.fromTick <= fault.untilTick, 'packetLoss range is reversed');
			return;
		case 'packetJitter':
			assert.ok(fault.jitterTicks >= 0, 'packetJitter jitterTicks must be non-negative');
			assert.ok(fault.fromTick <= fault.untilTick, 'packetJitter range is reversed');
			return;
		case 'packetReorder':
			assert.ok(fault.rate >= 0 && fault.rate <= 1, 'packetReorder rate out of bounds');
			assert.ok(fault.fromTick <= fault.untilTick, 'packetReorder range is reversed');
			return;
		case 'deviceDisconnect':
			assert.ok(typeof fault.deviceId === 'string' && fault.deviceId.length > 0, 'deviceDisconnect deviceId empty');
			assert.ok(fault.atTick >= 0, 'deviceDisconnect atTick must be non-negative');
			return;
		case 'gpuDeviceLost':
			assert.ok(fault.atTick >= 0, 'gpuDeviceLost atTick must be non-negative');
			return;
		case 'encoderFailed':
			assert.ok(typeof fault.captureId === 'string' && fault.captureId.length > 0, 'encoderFailed captureId empty');
			assert.ok(fault.atTick >= 0, 'encoderFailed atTick must be non-negative');
			return;
		case 'networkPartition':
			assert.ok(fault.fromTick <= fault.untilTick, 'networkPartition range is reversed');
			return;
		case 'asymmetricPartition':
			assert.ok(fault.peerIds.length > 0, 'asymmetricPartition requires at least one peer');
			assert.ok(fault.peerIds.length <= SIMULATOR_PEERS_PER_FAULT_MAX, 'asymmetricPartition exceeds peer cap');
			assert.ok(fault.fromTick >= 0, 'asymmetricPartition fromTick must be non-negative');
			return;
	}
}
