// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {allocateOperation, markOperation, queueCommand} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import {applyMicrophonePermissionResult} from '@fluxer/voice_engine_v2/src/core/reducers/microphone';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2Error,
	VoiceEngineV2OperationId,
	VoiceEngineV2PermissionName,
	VoiceEngineV2PermissionResult,
} from '@fluxer/voice_engine_v2/src/protocol/types';

type VoiceEngineV2PermissionsEvent = Extract<VoiceEngineV2Event, {type: `permissions.${string}`}>;

function beginPermissionOperation(
	snapshot: VoiceEngineV2Snapshot,
	commandType: 'permissions.check' | 'permissions.request',
	name: VoiceEngineV2PermissionName,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'beginPermissionOperation snapshot must not be null');
	assert.equal(typeof commandType, 'string', 'beginPermissionOperation commandType must be a string');
	assert.equal(typeof name, 'string', 'beginPermissionOperation name must be a string');
	const allocated = allocateOperation(snapshot);
	return queueCommand(
		{
			...allocated.snapshot,
			permissions: {
				...allocated.snapshot.permissions,
				operationIds: {
					...allocated.snapshot.permissions.operationIds,
					[String(name)]: allocated.operationId,
				},
				failure: null,
			},
		},
		{type: commandType, operationId: allocated.operationId, name},
	);
}

export function transitionPermissions(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2PermissionsEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionPermissions snapshot must not be null');
	assert.ok(event != null, 'transitionPermissions event must not be null');
	assert.equal(typeof event.type, 'string', 'permissions event type must be a string');
	assert.ok(event.type.startsWith('permissions.'), 'permissions reducer received unrelated event');
	switch (event.type) {
		case 'permissions.checkRequested':
			return beginPermissionOperation(snapshot, 'permissions.check', event.name);
		case 'permissions.requestRequested':
			return beginPermissionOperation(snapshot, 'permissions.request', event.name);
		case 'permissions.result':
			return onResult(snapshot, event.operationId, event.result);
		case 'permissions.failed':
			return onFailed(snapshot, event.operationId, event.name, event.error);
	}
}

function onResult(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId | null,
	result: VoiceEngineV2PermissionResult,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onResult snapshot must not be null');
	assert.ok(result != null, 'onResult result must not be null');
	assert.equal(typeof result.name, 'string', 'result.name must be a string');
	if (operationId != null && snapshot.permissions.operationIds[String(result.name)] !== operationId) {
		return {snapshot, commands: []};
	}
	const operationIds = {...snapshot.permissions.operationIds};
	if (operationId != null) delete operationIds[String(result.name)];
	return applyMicrophonePermissionResult(
		{
			...(operationId != null ? markOperation(snapshot, operationId, 'succeeded') : snapshot),
			permissions: {
				results: {
					...snapshot.permissions.results,
					[String(result.name)]: result,
				},
				operationIds,
				failure: null,
			},
		},
		result,
	);
}

function onFailed(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	name: VoiceEngineV2PermissionName,
	error: VoiceEngineV2Error,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onFailed snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onFailed operationId must be an integer');
	assert.ok(error != null, 'onFailed error must not be null');
	if (snapshot.permissions.operationIds[String(name)] !== operationId) return {snapshot, commands: []};
	const operationIds = {...snapshot.permissions.operationIds};
	delete operationIds[String(name)];
	return {
		snapshot: {
			...markOperation(snapshot, operationId, 'failed', error),
			permissions: {...snapshot.permissions, operationIds, failure: error},
			lastFailure: error,
		},
		commands: [],
	};
}
