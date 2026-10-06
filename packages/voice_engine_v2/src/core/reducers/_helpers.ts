// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {
	VoiceEngineV2OperationState,
	VoiceEngineV2OperationStatus,
	VoiceEngineV2Snapshot,
	VoiceEngineV2Transition,
} from '@fluxer/voice_engine_v2/src/core/state';
import {
	getVoiceEngineV2CommandResourceKey,
	type VoiceEngineV2Command,
} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {
	VoiceEngineV2DataOptions,
	VoiceEngineV2DiagnosticEntry,
	VoiceEngineV2Error,
	VoiceEngineV2OperationId,
	VoiceEngineV2OutputDeviceOptions,
	VoiceEngineV2ParticipantVolumeOptions,
	VoiceEngineV2RemoteTrackSubscriptionOptions,
	VoiceEngineV2ResourceKey,
} from '@fluxer/voice_engine_v2/src/protocol/types';

export const VOICE_ENGINE_V2_TERMINAL_OPERATIONS_KEPT_MAX = 64;
const VOICE_ENGINE_V2_DIAGNOSTICS_KEPT_MAX = 200;

const VOICE_ENGINE_V2_TERMINAL_OPERATION_STATUSES: ReadonlySet<VoiceEngineV2OperationStatus> = new Set([
	'succeeded',
	'failed',
	'cancelled',
	'stale',
]);

interface OperationAllocation {
	snapshot: VoiceEngineV2Snapshot;
	operationId: VoiceEngineV2OperationId;
}

function isTerminalVoiceEngineV2OperationStatus(status: VoiceEngineV2OperationStatus): boolean {
	assert.equal(typeof status, 'string', 'isTerminalVoiceEngineV2OperationStatus status must be a string');
	assert.ok(status.length > 0, 'isTerminalVoiceEngineV2OperationStatus status must not be empty');
	return VOICE_ENGINE_V2_TERMINAL_OPERATION_STATUSES.has(status);
}

function pruneTerminalOperations(
	operations: Record<string, VoiceEngineV2OperationState>,
): Record<string, VoiceEngineV2OperationState> {
	assert.ok(operations != null, 'pruneTerminalOperations operations must not be null');
	const terminalOperationIds: Array<number> = [];
	for (const operation of Object.values(operations)) {
		if (isTerminalVoiceEngineV2OperationStatus(operation.status)) {
			terminalOperationIds.push(operation.operationId);
		}
	}
	if (terminalOperationIds.length <= VOICE_ENGINE_V2_TERMINAL_OPERATIONS_KEPT_MAX) return operations;
	terminalOperationIds.sort((a, b) => a - b);
	const dropCount = terminalOperationIds.length - VOICE_ENGINE_V2_TERMINAL_OPERATIONS_KEPT_MAX;
	assert.ok(dropCount >= 1, 'pruneTerminalOperations over cap must drop at least one entry');
	const pruned = {...operations};
	for (let index = 0; index < dropCount; index += 1) {
		const operationId = terminalOperationIds[index];
		assert.ok(operationId !== undefined, 'pruneTerminalOperations ids must not contain holes');
		delete pruned[String(operationId)];
	}
	assert.ok(
		Object.keys(pruned).length === Object.keys(operations).length - dropCount,
		'pruneTerminalOperations must remove exactly the dropped entries',
	);
	return pruned;
}

function assertVoiceEngineV2Transition(result: VoiceEngineV2Transition): void {
	assert.ok(result != null, 'transition result must not be null');
	assert.ok(result.snapshot != null, 'transition snapshot must not be null');
	assert.ok(Array.isArray(result.commands), 'transition commands must be an array');
}

export function implementationError(message: string, capability?: string): VoiceEngineV2Error {
	assert.equal(typeof message, 'string', 'implementationError message must be a string');
	assert.ok(message.length > 0, 'implementationError message must not be empty');
	return {
		code: 'implementationError',
		message,
		...(capability ? {capability} : {}),
	};
}

export function invalidArgument(message: string, capability?: string): VoiceEngineV2Error {
	assert.equal(typeof message, 'string', 'invalidArgument message must be a string');
	assert.ok(message.length > 0, 'invalidArgument message must not be empty');
	return {
		code: 'invalidArgument',
		message,
		...(capability ? {capability} : {}),
	};
}

export function unsupportedCapability(capability: string): VoiceEngineV2Error {
	assert.equal(typeof capability, 'string', 'unsupportedCapability capability must be a string');
	assert.ok(capability.length > 0, 'unsupportedCapability capability must not be empty');
	return {
		code: 'unsupportedCapability',
		capability,
		message: `Voice engine v2 implementation does not support ${capability}`,
	};
}

export function allocateOperation(snapshot: VoiceEngineV2Snapshot): OperationAllocation {
	assert.ok(snapshot != null, 'allocateOperation snapshot must not be null');
	assert.ok(Number.isInteger(snapshot.nextOperationId), 'nextOperationId must be an integer');
	assert.ok(snapshot.nextOperationId >= 1, 'nextOperationId must be positive');
	assert.ok(snapshot.nextOperationId <= Number.MAX_SAFE_INTEGER, 'nextOperationId past safe-integer cap');
	return {
		operationId: snapshot.nextOperationId,
		snapshot: {
			...snapshot,
			nextOperationId: snapshot.nextOperationId + 1,
		},
	};
}

export function recordQueuedCommand(
	snapshot: VoiceEngineV2Snapshot,
	command: VoiceEngineV2Command,
): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'recordQueuedCommand snapshot must not be null');
	assert.ok(command != null, 'recordQueuedCommand command must not be null');
	assert.ok(Number.isInteger(command.operationId), 'command.operationId must be an integer');
	return {
		...snapshot,
		operations: {
			...snapshot.operations,
			[String(command.operationId)]: {
				operationId: command.operationId,
				commandType: command.type,
				resourceKey: getVoiceEngineV2CommandResourceKey(command),
				status: 'queued',
				error: null,
			},
		},
	};
}

export function queueCommand(snapshot: VoiceEngineV2Snapshot, command: VoiceEngineV2Command): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'queueCommand snapshot must not be null');
	assert.ok(command != null, 'queueCommand command must not be null');
	const result: VoiceEngineV2Transition = {
		snapshot: recordQueuedCommand(snapshot, command),
		commands: [command],
	};
	assertVoiceEngineV2Transition(result);
	return result;
}

export function markOperation(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	status: VoiceEngineV2OperationStatus,
	error: VoiceEngineV2Error | null = null,
): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'markOperation snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'markOperation operationId must be an integer');
	assert.equal(typeof status, 'string', 'markOperation status must be a string');
	const operation = snapshot.operations[String(operationId)];
	if (!operation) return snapshot;
	const operations: Record<string, VoiceEngineV2OperationState> = {
		...snapshot.operations,
		[String(operationId)]: {
			...operation,
			status,
			error,
		},
	};
	return {
		...snapshot,
		operations: isTerminalVoiceEngineV2OperationStatus(status) ? pruneTerminalOperations(operations) : operations,
	};
}

export function isConnected(snapshot: VoiceEngineV2Snapshot): boolean {
	assert.ok(snapshot != null, 'isConnected snapshot must not be null');
	assert.ok(snapshot.connection != null, 'isConnected snapshot.connection must not be null');
	return snapshot.connection.status === 'connected';
}

export function appendTransition(
	base: VoiceEngineV2Transition,
	next: VoiceEngineV2Transition,
): VoiceEngineV2Transition {
	assertVoiceEngineV2Transition(base);
	assertVoiceEngineV2Transition(next);
	const result: VoiceEngineV2Transition = {
		snapshot: next.snapshot,
		commands: [...base.commands, ...next.commands],
	};
	assertVoiceEngineV2Transition(result);
	return result;
}

export function appendDiagnostic(
	diagnostics: Array<VoiceEngineV2DiagnosticEntry>,
	entry: VoiceEngineV2DiagnosticEntry,
): Array<VoiceEngineV2DiagnosticEntry> {
	assert.ok(Array.isArray(diagnostics), 'appendDiagnostic diagnostics must be an array');
	assert.ok(entry != null, 'appendDiagnostic entry must not be null');
	return [...diagnostics, entry].slice(-VOICE_ENGINE_V2_DIAGNOSTICS_KEPT_MAX);
}

export function recordFailure(snapshot: VoiceEngineV2Snapshot, error: VoiceEngineV2Error): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'recordFailure snapshot must not be null');
	assert.ok(error != null, 'recordFailure error must not be null');
	const result: VoiceEngineV2Transition = {
		snapshot: {
			...snapshot,
			lastFailure: error,
		},
		commands: [],
	};
	assertVoiceEngineV2Transition(result);
	return result;
}

export function clearOperationForResource(
	snapshot: VoiceEngineV2Snapshot,
	resourceKey: VoiceEngineV2ResourceKey,
	operationId: VoiceEngineV2OperationId,
): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'clearOperationForResource snapshot must not be null');
	assert.equal(typeof resourceKey, 'string', 'clearOperationForResource resourceKey must be a string');
	assert.ok(Number.isInteger(operationId), 'clearOperationForResource operationId must be an integer');
	if (resourceKey === 'connection' && snapshot.connection.operationId === operationId) {
		return {...snapshot, connection: {...snapshot.connection, operationId: null}};
	}
	if (resourceKey === 'microphone' && snapshot.microphone.operationId === operationId) {
		return {...snapshot, microphone: {...snapshot.microphone, operationId: null}};
	}
	if (resourceKey === 'camera' && snapshot.camera.operationId === operationId) {
		return {...snapshot, camera: {...snapshot.camera, operationId: null}};
	}
	if (resourceKey === 'screen' && snapshot.screen.operationId === operationId) {
		return {...snapshot, screen: {...snapshot.screen, operationId: null}};
	}
	if (resourceKey === 'screenAudio' && snapshot.screenAudio.operationId === operationId) {
		return {...snapshot, screenAudio: {...snapshot.screenAudio, operationId: null}};
	}
	if (resourceKey === 'outputDevice' && snapshot.outputDevice.operationId === operationId) {
		return {...snapshot, outputDevice: {...snapshot.outputDevice, operationId: null}};
	}
	if (resourceKey === 'stats' && snapshot.statsOperationId === operationId) {
		return {...snapshot, statsOperationId: null};
	}
	if (resourceKey === 'capabilities' && snapshot.hardwareEncoder.operationId === operationId) {
		return {...snapshot, hardwareEncoder: {...snapshot.hardwareEncoder, operationId: null}};
	}
	if (resourceKey === 'gateway' && snapshot.gateway.operationId === operationId) {
		return {...snapshot, gateway: {...snapshot.gateway, operationId: null}};
	}
	if (resourceKey === 'devices' && snapshot.devices.operationId === operationId) {
		return {...snapshot, devices: {...snapshot.devices, operationId: null}};
	}
	if (resourceKey === 'e2ee' && snapshot.e2ee.operationId === operationId) {
		return {...snapshot, e2ee: {...snapshot.e2ee, operationId: null}};
	}
	if (resourceKey === 'lifecycle' && snapshot.lifecycle.operationId === operationId) {
		return {...snapshot, lifecycle: {...snapshot.lifecycle, operationId: null}};
	}
	return snapshot;
}

type ImmediateCommandDraft =
	| {type: 'outputDevice.set'; options: VoiceEngineV2OutputDeviceOptions}
	| {type: 'participantVolume.set'; options: VoiceEngineV2ParticipantVolumeOptions}
	| {type: 'remoteTrackSubscription.set'; options: VoiceEngineV2RemoteTrackSubscriptionOptions}
	| {type: 'data.publish'; options: VoiceEngineV2DataOptions}
	| {type: 'stats.collect'};

export function commandIfConnected(
	snapshot: VoiceEngineV2Snapshot,
	capability: keyof VoiceEngineV2Snapshot['capabilities'],
	draft: ImmediateCommandDraft,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'commandIfConnected snapshot must not be null');
	assert.ok(draft != null, 'commandIfConnected draft must not be null');
	assert.equal(typeof draft.type, 'string', 'commandIfConnected draft.type must be a string');
	if (!snapshot.capabilities[capability]) {
		const error = unsupportedCapability(capability);
		return {snapshot: {...snapshot, lastFailure: error}, commands: []};
	}
	if (!isConnected(snapshot)) return {snapshot, commands: []};
	const allocated = allocateOperation(snapshot);
	const command = {...draft, operationId: allocated.operationId} as VoiceEngineV2Command;
	if (draft.type === 'outputDevice.set') {
		return {
			snapshot: {
				...allocated.snapshot,
				outputDevice: {...allocated.snapshot.outputDevice, operationId: allocated.operationId, failure: null},
			},
			commands: [command],
		};
	}
	if (draft.type === 'stats.collect') {
		return {
			snapshot: {
				...allocated.snapshot,
				statsOperationId: allocated.operationId,
				statsFailure: null,
			},
			commands: [command],
		};
	}
	return {snapshot: allocated.snapshot, commands: [command]};
}

type UnpublishableMediaKey = 'microphone' | 'camera' | 'screen' | 'screenAudio';
type UnpublishCommandType = 'microphone.unpublish' | 'camera.unpublish' | 'screen.unpublish' | 'screenAudio.unpublish';

export function beginUnpublish(
	snapshot: VoiceEngineV2Snapshot,
	key: UnpublishableMediaKey,
	commandType: UnpublishCommandType,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'beginUnpublish snapshot must not be null');
	assert.equal(typeof key, 'string', 'beginUnpublish key must be a string');
	assert.equal(typeof commandType, 'string', 'beginUnpublish commandType must be a string');
	const state = snapshot[key];
	const base = {
		...snapshot,
		[key]: {
			...state,
			desired: null,
		},
	};
	if (!isConnected(base) || (state.status === 'idle' && state.published == null)) return {snapshot: base, commands: []};
	const allocated = allocateOperation(base);
	return {
		snapshot: {
			...allocated.snapshot,
			[key]: {
				...allocated.snapshot[key],
				status: 'unpublishing',
				operationId: allocated.operationId,
				failure: null,
			},
		},
		commands: [{type: commandType, operationId: allocated.operationId} as VoiceEngineV2Command],
	};
}

export function failUnpublish(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
	key: UnpublishableMediaKey,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'failUnpublish snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'failUnpublish operationId must be an integer');
	assert.ok(error != null, 'failUnpublish error must not be null');
	assert.equal(typeof key, 'string', 'failUnpublish key must be a string');
	const state = snapshot[key];
	if (state.operationId !== operationId) return {snapshot, commands: []};
	return {
		snapshot: {
			...snapshot,
			[key]: {
				...state,
				status: 'failed',
				operationId: null,
				failure: error,
			},
			lastFailure: error,
		},
		commands: [],
	};
}
