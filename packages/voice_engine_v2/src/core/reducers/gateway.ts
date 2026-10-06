// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	allocateOperation,
	isConnected,
	markOperation,
	queueCommand,
} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import {
	beginGatewayVoiceStateClear,
	planPendingConnectionTeardown,
} from '@fluxer/voice_engine_v2/src/core/reducers/connection';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2Error,
	VoiceEngineV2GatewayDesiredVoiceState,
	VoiceEngineV2GatewayVoiceState,
	VoiceEngineV2GatewayVoiceStateWrite,
	VoiceEngineV2OperationId,
} from '@fluxer/voice_engine_v2/src/protocol/types';

type VoiceEngineV2GatewayEvent = Extract<VoiceEngineV2Event, {type: `gateway.${string}` | 'livekit.roomStateChanged'}>;

export function transitionGateway(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2GatewayEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionGateway snapshot must not be null');
	assert.ok(event != null, 'transitionGateway event must not be null');
	assert.equal(typeof event.type, 'string', 'gateway event type must be a string');
	assert.ok(
		event.type.startsWith('gateway.') || event.type === 'livekit.roomStateChanged',
		'gateway reducer received unrelated event',
	);
	switch (event.type) {
		case 'gateway.desiredVoiceStateChanged':
			return onDesiredVoiceStateChanged(snapshot, event.desired);
		case 'gateway.voiceStateReconcileRequested':
			return reconcileGatewayVoiceState(snapshot);
		case 'gateway.voiceStateWriteRequested':
			return onWriteRequested(snapshot, event.options);
		case 'gateway.voiceStateWriteSucceeded':
			return onWriteSucceeded(snapshot, event.operationId);
		case 'gateway.voiceStateWriteFailed':
			return onWriteFailed(snapshot, event.operationId, event.error);
		case 'gateway.voiceStateClearRequested':
			return beginGatewayVoiceStateClear(snapshot, event.guildId);
		case 'gateway.voiceStateClearSucceeded':
			return onClearSucceeded(snapshot, event.operationId);
		case 'gateway.voiceStateClearFailed':
			return onClearFailed(snapshot, event.operationId, event.error);
		case 'gateway.voiceStateUpdated':
			return onVoiceStateUpdated(snapshot, event.voiceState);
		case 'gateway.voiceServerUpdated':
			return {
				snapshot: {...snapshot, gateway: {...snapshot.gateway, voiceServer: event.voiceServer}},
				commands: [],
			};
		case 'livekit.roomStateChanged':
			return {
				snapshot: {
					...snapshot,
					liveKit: {...snapshot.liveKit, ...event.room, failure: null},
				},
				commands: [],
			};
	}
}

function onWriteRequested(
	snapshot: VoiceEngineV2Snapshot,
	options: VoiceEngineV2GatewayVoiceStateWrite,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onWriteRequested snapshot must not be null');
	assert.ok(options != null, 'onWriteRequested options must not be null');
	const allocated = allocateOperation(snapshot);
	return queueCommand(
		{
			...allocated.snapshot,
			gateway: {
				...allocated.snapshot.gateway,
				desiredVoiceStateWrite: options,
				operationId: allocated.operationId,
				failure: null,
			},
		},
		{
			type: 'gateway.voiceState.write',
			operationId: allocated.operationId,
			options,
		},
	);
}

function onWriteSucceeded(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onWriteSucceeded snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onWriteSucceeded operationId must be an integer');
	if (snapshot.gateway.operationId !== operationId) return {snapshot, commands: []};
	return planPendingConnectionTeardown({
		...markOperation(snapshot, operationId, 'succeeded'),
		gateway: {...snapshot.gateway, operationId: null, failure: null},
	});
}

function onWriteFailed(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onWriteFailed snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onWriteFailed operationId must be an integer');
	assert.ok(error != null, 'onWriteFailed error must not be null');
	if (snapshot.gateway.operationId !== operationId) return {snapshot, commands: []};
	return {
		snapshot: {
			...markOperation(snapshot, operationId, 'failed', error),
			gateway: {...snapshot.gateway, desiredVoiceStateWrite: null, operationId: null, failure: error},
			lastFailure: error,
		},
		commands: [],
	};
}

function onClearSucceeded(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onClearSucceeded snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onClearSucceeded operationId must be an integer');
	if (snapshot.gateway.operationId !== operationId) return {snapshot, commands: []};
	const cleared: VoiceEngineV2Snapshot = {
		...markOperation(snapshot, operationId, 'succeeded'),
		gateway: {
			...snapshot.gateway,
			desiredVoiceStateWrite: null,
			selfVoiceState: null,
			operationId: null,
			failure: null,
		},
	};
	return planPendingConnectionTeardown(cleared);
}

function onClearFailed(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onClearFailed snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onClearFailed operationId must be an integer');
	assert.ok(error != null, 'onClearFailed error must not be null');
	if (snapshot.gateway.operationId !== operationId) return {snapshot, commands: []};
	return {
		snapshot: {
			...markOperation(snapshot, operationId, 'failed', error),
			gateway: {...snapshot.gateway, operationId: null, failure: error},
			lastFailure: error,
		},
		commands: [],
	};
}

function onVoiceStateUpdated(
	snapshot: VoiceEngineV2Snapshot,
	voiceState: VoiceEngineV2GatewayVoiceState | null,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onVoiceStateUpdated snapshot must not be null');
	assert.ok(snapshot.gateway != null, 'onVoiceStateUpdated snapshot.gateway must not be null');
	const pending = snapshot.gateway.desiredVoiceStateWrite;
	const converged = voiceState != null && pending != null && gatewayVoiceStateWriteMatchesReported(pending, voiceState);
	if (converged) {
		assert.ok(pending != null, 'onVoiceStateUpdated converged requires a pending write');
	}
	return {
		snapshot: {
			...snapshot,
			gateway: {
				...snapshot.gateway,
				selfVoiceState: voiceState,
				desiredVoiceStateWrite: converged ? null : pending,
			},
		},
		commands: [],
	};
}

function onDesiredVoiceStateChanged(
	snapshot: VoiceEngineV2Snapshot,
	desired: VoiceEngineV2GatewayDesiredVoiceState,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onDesiredVoiceStateChanged snapshot must not be null');
	assert.ok(desired != null, 'onDesiredVoiceStateChanged desired must not be null');
	assert.equal(typeof desired.selfMute, 'boolean', 'onDesiredVoiceStateChanged selfMute must be a boolean');
	return reconcileGatewayVoiceState({
		...snapshot,
		gateway: {...snapshot.gateway, desiredVoiceState: desired},
	});
}

export function deriveVoiceEngineV2DesiredGatewayVoiceState(
	snapshot: VoiceEngineV2Snapshot,
): VoiceEngineV2GatewayVoiceStateWrite | null {
	assert.ok(snapshot != null, 'deriveVoiceEngineV2DesiredGatewayVoiceState snapshot must not be null');
	assert.ok(snapshot.gateway != null, 'deriveVoiceEngineV2DesiredGatewayVoiceState snapshot.gateway must not be null');
	const desired = snapshot.gateway.desiredVoiceState;
	if (desired == null) return null;
	if (desired.channelId == null) return null;
	return {
		guildId: desired.guildId,
		channelId: desired.channelId,
		selfMute: desired.selfMute,
		selfDeaf: desired.selfDeaf,
		selfVideo: desired.selfVideo,
		selfStream: desired.selfStream,
	};
}

export function shouldApplyGatewayVoiceStateEcho(
	snapshot: VoiceEngineV2Snapshot,
	echo: VoiceEngineV2GatewayVoiceState,
): boolean {
	assert.ok(snapshot != null, 'shouldApplyGatewayVoiceStateEcho snapshot must not be null');
	assert.ok(echo != null, 'shouldApplyGatewayVoiceStateEcho echo must not be null');
	const pending = snapshot.gateway.desiredVoiceStateWrite;
	if (pending == null) return true;
	return gatewayVoiceStateWriteMatchesReported(pending, echo);
}

export function gatewayVoiceStateWriteMatchesReported(
	write: VoiceEngineV2GatewayVoiceStateWrite,
	reported: VoiceEngineV2GatewayVoiceState,
): boolean {
	assert.ok(write != null, 'gatewayVoiceStateWriteMatchesReported write must not be null');
	assert.ok(reported != null, 'gatewayVoiceStateWriteMatchesReported reported must not be null');
	if (write.selfMute !== reported.selfMute) return false;
	if (write.selfDeaf !== reported.selfDeaf) return false;
	if ((write.selfVideo ?? false) !== reported.selfVideo) return false;
	if ((write.selfStream ?? false) !== reported.selfStream) return false;
	return true;
}

function gatewayVoiceStateWriteValuesEqual(
	a: VoiceEngineV2GatewayVoiceStateWrite,
	b: VoiceEngineV2GatewayVoiceStateWrite,
): boolean {
	assert.ok(a != null, 'gatewayVoiceStateWriteValuesEqual a must not be null');
	assert.ok(b != null, 'gatewayVoiceStateWriteValuesEqual b must not be null');
	if (a.guildId !== b.guildId) return false;
	if (a.channelId !== b.channelId) return false;
	if (a.selfMute !== b.selfMute) return false;
	if (a.selfDeaf !== b.selfDeaf) return false;
	if ((a.selfVideo ?? false) !== (b.selfVideo ?? false)) return false;
	if ((a.selfStream ?? false) !== (b.selfStream ?? false)) return false;
	return true;
}

export function reconcileGatewayVoiceState(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'reconcileGatewayVoiceState snapshot must not be null');
	assert.ok(snapshot.gateway != null, 'reconcileGatewayVoiceState snapshot.gateway must not be null');
	if (!isConnected(snapshot)) return {snapshot, commands: []};
	const desired = deriveVoiceEngineV2DesiredGatewayVoiceState(snapshot);
	if (desired == null) return {snapshot, commands: []};
	const reported = snapshot.gateway.selfVoiceState;
	if (reported != null && gatewayVoiceStateWriteMatchesReported(desired, reported)) return {snapshot, commands: []};
	if (snapshot.gateway.operationId != null) return {snapshot, commands: []};
	const pending = snapshot.gateway.desiredVoiceStateWrite;
	if (pending != null && gatewayVoiceStateWriteValuesEqual(pending, desired)) return {snapshot, commands: []};
	return onWriteRequested(snapshot, desired);
}
