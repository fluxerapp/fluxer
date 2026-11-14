// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	allocateOperation,
	markOperation,
	queueCommand,
	recordQueuedCommand,
	unsupportedCapability,
} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import {resetPublishedMedia} from '@fluxer/voice_engine_v2/src/core/reducers/_media';
import {
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2Snapshot,
	type VoiceEngineV2Transition,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2ConnectOptions,
	VoiceEngineV2DisconnectReason,
	VoiceEngineV2Error,
	VoiceEngineV2LifecycleReason,
	VoiceEngineV2OperationId,
} from '@fluxer/voice_engine_v2/src/protocol/types';

type VoiceEngineV2ConnectionEvent = Extract<VoiceEngineV2Event, {type: `connection.${string}`}>;

function sameConnectOptions(a: VoiceEngineV2ConnectOptions | null, b: VoiceEngineV2ConnectOptions | null): boolean {
	assert.ok(a !== undefined, 'sameConnectOptions a must not be undefined');
	assert.ok(b !== undefined, 'sameConnectOptions b must not be undefined');
	return a?.url === b?.url && a?.token === b?.token && a?.e2eeKey === b?.e2eeKey;
}

export function resetAfterDisconnect(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'resetAfterDisconnect snapshot must not be null');
	assert.ok(snapshot.capabilities != null, 'resetAfterDisconnect snapshot.capabilities must not be null');
	const initial = createVoiceEngineV2InitialSnapshot(snapshot.capabilities);
	return {
		...initial,
		nextOperationId: snapshot.nextOperationId,
		connection: {
			...initial.connection,
			desired: null,
		},
	};
}

function staleCompletion(snapshot: VoiceEngineV2Snapshot, operationId: VoiceEngineV2OperationId | null): boolean {
	assert.ok(snapshot != null, 'staleCompletion snapshot must not be null');
	assert.ok(snapshot.connection != null, 'staleCompletion snapshot.connection must not be null');
	return operationId == null || operationId !== snapshot.connection.operationId;
}

function hasGatewayVoiceStateToClear(snapshot: VoiceEngineV2Snapshot): boolean {
	assert.ok(snapshot != null, 'hasGatewayVoiceStateToClear snapshot must not be null');
	assert.ok(snapshot.gateway != null, 'hasGatewayVoiceStateToClear snapshot.gateway must not be null');
	return (
		snapshot.gateway.selfVoiceState?.channelId != null || snapshot.gateway.desiredVoiceStateWrite?.channelId != null
	);
}

function gatewayVoiceStateGuildId(snapshot: VoiceEngineV2Snapshot): string | null {
	assert.ok(snapshot != null, 'gatewayVoiceStateGuildId snapshot must not be null');
	assert.ok(snapshot.gateway != null, 'gatewayVoiceStateGuildId snapshot.gateway must not be null');
	return snapshot.gateway.selfVoiceState?.guildId ?? snapshot.gateway.desiredVoiceStateWrite?.guildId ?? null;
}

function beginConnectionConnect(
	snapshot: VoiceEngineV2Snapshot,
	options: VoiceEngineV2ConnectOptions,
	status: 'connecting' | 'reconnecting' = 'connecting',
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'beginConnectionConnect snapshot must not be null');
	assert.ok(options != null, 'beginConnectionConnect options must not be null');
	assert.equal(typeof options.url, 'string', 'beginConnectionConnect options.url must be a string');
	if (!snapshot.capabilities.connect) {
		const error = unsupportedCapability('connect');
		return {
			snapshot: {
				...snapshot,
				connection: {...snapshot.connection, status: 'failed', failure: error},
				lastFailure: error,
			},
			commands: [],
		};
	}
	const allocated = allocateOperation(resetPublishedMedia(snapshot));
	const command: VoiceEngineV2Command = {type: 'connection.connect', operationId: allocated.operationId, options};
	return {
		snapshot: recordQueuedCommand(
			{
				...allocated.snapshot,
				connection: {
					...allocated.snapshot.connection,
					status,
					desired: options,
					operationId: allocated.operationId,
					disconnectReason: null,
					failure: null,
				},
			},
			command,
		),
		commands: [command],
	};
}

function beginConnectionDisconnect(
	snapshot: VoiceEngineV2Snapshot,
	reason: VoiceEngineV2DisconnectReason,
	desiredAfterDisconnect: VoiceEngineV2ConnectOptions | null,
	status: 'disconnecting' | 'reconnecting' = desiredAfterDisconnect ? 'reconnecting' : 'disconnecting',
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'beginConnectionDisconnect snapshot must not be null');
	assert.equal(typeof reason, 'string', 'beginConnectionDisconnect reason must be a string');
	const allocated = allocateOperation(resetPublishedMedia(snapshot));
	const command: VoiceEngineV2Command = {
		type: 'connection.disconnect',
		operationId: allocated.operationId,
		reason,
	};
	return {
		snapshot: recordQueuedCommand(
			{
				...allocated.snapshot,
				connection: {
					...allocated.snapshot.connection,
					status,
					desired: desiredAfterDisconnect,
					operationId: allocated.operationId,
					disconnectReason: reason,
					failure: null,
				},
			},
			command,
		),
		commands: [command],
	};
}

export function beginGatewayVoiceStateClear(
	snapshot: VoiceEngineV2Snapshot,
	guildId: string | null,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'beginGatewayVoiceStateClear snapshot must not be null');
	assert.ok(snapshot.gateway != null, 'beginGatewayVoiceStateClear snapshot.gateway must not be null');
	if (snapshot.gateway.operationId != null) return {snapshot, commands: []};
	const allocated = allocateOperation(snapshot);
	return queueCommand(
		{
			...allocated.snapshot,
			gateway: {
				...allocated.snapshot.gateway,
				desiredVoiceStateWrite: null,
				operationId: allocated.operationId,
				failure: null,
			},
		},
		{type: 'gateway.voiceState.clear', operationId: allocated.operationId, guildId},
	);
}

function beginLifecycleTeardownCommand(
	snapshot: VoiceEngineV2Snapshot,
	reason: VoiceEngineV2LifecycleReason,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'beginLifecycleTeardownCommand snapshot must not be null');
	assert.equal(typeof reason, 'string', 'beginLifecycleTeardownCommand reason must be a string');
	const allocated = allocateOperation(snapshot);
	return queueCommand(
		{
			...allocated.snapshot,
			lifecycle: {
				tearingDown: true,
				reason,
				operationId: allocated.operationId,
				failure: null,
			},
		},
		{type: 'lifecycle.teardown', operationId: allocated.operationId, reason},
	);
}

function clearConnectionAfterDisconnect(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'clearConnectionAfterDisconnect snapshot must not be null');
	assert.ok(snapshot.connection != null, 'clearConnectionAfterDisconnect snapshot.connection must not be null');
	return {
		...resetPublishedMedia(snapshot),
		connection: {
			...snapshot.connection,
			status: 'idle',
			active: null,
			desired: null,
			operationId: null,
			disconnectReason: null,
			failure: null,
		},
		liveKit: {
			...snapshot.liveKit,
			connectionState: 'disconnected',
			roomSid: null,
			roomName: null,
			serverRegion: null,
		},
	};
}

export function planPendingConnectionTeardown(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'planPendingConnectionTeardown snapshot must not be null');
	assert.ok(snapshot.lifecycle != null, 'planPendingConnectionTeardown snapshot.lifecycle must not be null');
	if (snapshot.lifecycle.tearingDown) return planLifecycleTeardown(snapshot);
	if (
		(snapshot.connection.status === 'disconnecting' || snapshot.connection.status === 'reconnecting') &&
		snapshot.connection.operationId == null
	) {
		return planPendingDisconnect(snapshot);
	}
	return {snapshot, commands: []};
}

function planLifecycleTeardown(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'planLifecycleTeardown snapshot must not be null');
	assert.ok(snapshot.lifecycle != null, 'planLifecycleTeardown snapshot.lifecycle must not be null');
	if (
		snapshot.lifecycle.operationId != null ||
		snapshot.gateway.operationId != null ||
		snapshot.connection.operationId != null
	) {
		return {snapshot, commands: []};
	}
	if (hasGatewayVoiceStateToClear(snapshot)) {
		return beginGatewayVoiceStateClear(snapshot, gatewayVoiceStateGuildId(snapshot));
	}
	if (
		snapshot.connection.status !== 'idle' ||
		snapshot.connection.active != null ||
		snapshot.connection.desired != null
	) {
		return beginConnectionDisconnect(
			{
				...snapshot,
				connection: {
					...snapshot.connection,
					desired: null,
					disconnectReason: 'shutdown',
				},
			},
			'shutdown',
			null,
			'disconnecting',
		);
	}
	return beginLifecycleTeardownCommand(snapshot, snapshot.lifecycle.reason ?? 'appShutdown');
}

function planPendingDisconnect(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'planPendingDisconnect snapshot must not be null');
	assert.ok(snapshot.connection != null, 'planPendingDisconnect snapshot.connection must not be null');
	if (snapshot.connection.active == null && snapshot.connection.desired == null) {
		return {snapshot: resetAfterDisconnect(snapshot), commands: []};
	}
	return beginConnectionDisconnect(
		snapshot,
		snapshot.connection.disconnectReason ?? (snapshot.connection.desired ? 'replaced' : 'user'),
		snapshot.connection.desired,
		snapshot.connection.desired ? 'reconnecting' : 'disconnecting',
	);
}

export function transitionConnection(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2ConnectionEvent,
	planAfterConnect: (snapshot: VoiceEngineV2Snapshot) => VoiceEngineV2Transition,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionConnection snapshot must not be null');
	assert.ok(event != null, 'transitionConnection event must not be null');
	assert.equal(typeof event.type, 'string', 'connection event type must be a string');
	assert.ok(event.type.startsWith('connection.'), 'connection reducer received unrelated event');
	assert.equal(typeof planAfterConnect, 'function', 'transitionConnection planAfterConnect must be a function');
	switch (event.type) {
		case 'connection.connectRequested':
			return onConnectRequested(snapshot, event.options);
		case 'connection.connectSucceeded':
			return onConnectSucceeded(snapshot, event.operationId, planAfterConnect);
		case 'connection.connectFailed':
			return onConnectFailed(snapshot, event.operationId, event.error);
		case 'connection.disconnectRequested':
			return onDisconnectRequested(snapshot, event.reason);
		case 'connection.disconnectSucceeded':
			return onDisconnectSucceeded(snapshot, event.operationId);
		case 'connection.disconnectFailed':
			return onDisconnectFailed(snapshot, event.operationId, event.error);
		case 'connection.remoteDisconnected':
			return onRemoteDisconnected(snapshot, event.reason, event.error);
		case 'connection.reconnectRequested':
			return onReconnectRequested(snapshot);
		case 'connection.externallyEstablished':
			return onExternallyEstablished(snapshot, event.options, planAfterConnect);
	}
}

function onExternallyEstablished(
	snapshot: VoiceEngineV2Snapshot,
	options: VoiceEngineV2ConnectOptions,
	planAfterConnect: (snapshot: VoiceEngineV2Snapshot) => VoiceEngineV2Transition,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onExternallyEstablished snapshot must not be null');
	assert.ok(options != null, 'onExternallyEstablished options must not be null');
	assert.equal(typeof options.url, 'string', 'onExternallyEstablished options.url must be a string');
	assert.equal(typeof options.token, 'string', 'onExternallyEstablished options.token must be a string');
	if (snapshot.connection.status === 'connected' && sameConnectOptions(snapshot.connection.active, options)) {
		return {snapshot, commands: []};
	}
	const connected: VoiceEngineV2Snapshot = {
		...snapshot,
		connection: {
			...snapshot.connection,
			status: 'connected',
			active: options,
			operationId: null,
			disconnectReason: null,
			failure: null,
		},
	};
	assert.equal(connected.connection.status, 'connected', 'onExternallyEstablished must mark snapshot connected');
	if (connected.lifecycle.tearingDown) return planPendingConnectionTeardown(connected);
	return planAfterConnect(connected);
}

function onConnectRequested(
	snapshot: VoiceEngineV2Snapshot,
	options: VoiceEngineV2ConnectOptions,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onConnectRequested snapshot must not be null');
	assert.ok(options != null, 'onConnectRequested options must not be null');
	if (
		(snapshot.connection.status === 'connected' ||
			snapshot.connection.status === 'connecting' ||
			snapshot.connection.status === 'reconnecting') &&
		sameConnectOptions(snapshot.connection.desired ?? snapshot.connection.active, options)
	) {
		return {snapshot, commands: []};
	}
	if (snapshot.connection.status === 'connected' || snapshot.connection.active != null) {
		return beginConnectionDisconnect(snapshot, 'replaced', options, 'reconnecting');
	}
	if (snapshot.connection.status === 'disconnecting') {
		return {
			snapshot: {
				...snapshot,
				connection: {
					...snapshot.connection,
					status: 'reconnecting',
					desired: options,
					disconnectReason: 'replaced',
					failure: null,
				},
			},
			commands: [],
		};
	}
	return beginConnectionConnect(snapshot, options);
}

function onConnectSucceeded(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	planAfterConnect: (snapshot: VoiceEngineV2Snapshot) => VoiceEngineV2Transition,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onConnectSucceeded snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onConnectSucceeded operationId must be an integer');
	if (staleCompletion(snapshot, operationId)) return {snapshot, commands: []};
	const connected: VoiceEngineV2Snapshot = {
		...markOperation(snapshot, operationId, 'succeeded'),
		connection: {
			...snapshot.connection,
			status: 'connected',
			active: snapshot.connection.desired,
			operationId: null,
			disconnectReason: null,
			failure: null,
		},
	};
	if (connected.lifecycle.tearingDown) return planPendingConnectionTeardown(connected);
	return planAfterConnect(connected);
}

function onConnectFailed(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onConnectFailed snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onConnectFailed operationId must be an integer');
	assert.ok(error != null, 'onConnectFailed error must not be null');
	if (staleCompletion(snapshot, operationId)) return {snapshot, commands: []};
	return {
		snapshot: {
			...markOperation(snapshot, operationId, 'failed', error),
			connection: {
				...snapshot.connection,
				status: 'failed',
				operationId: null,
				failure: error,
			},
			lastFailure: error,
		},
		commands: [],
	};
}

function onDisconnectRequested(
	snapshot: VoiceEngineV2Snapshot,
	reason: VoiceEngineV2DisconnectReason,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onDisconnectRequested snapshot must not be null');
	assert.equal(typeof reason, 'string', 'onDisconnectRequested reason must be a string');
	const base = {
		...resetPublishedMedia(snapshot),
		connection: {
			...snapshot.connection,
			status: 'disconnecting' as const,
			desired: null,
			operationId: null,
			disconnectReason: reason,
			failure: null,
		},
	};
	if (hasGatewayVoiceStateToClear(base)) {
		return beginGatewayVoiceStateClear(base, gatewayVoiceStateGuildId(base));
	}
	if (
		snapshot.connection.status === 'idle' &&
		snapshot.connection.active == null &&
		snapshot.connection.operationId == null
	) {
		return {snapshot: resetAfterDisconnect(base), commands: []};
	}
	return beginConnectionDisconnect(base, reason, null, 'disconnecting');
}

function onDisconnectSucceeded(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onDisconnectSucceeded snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onDisconnectSucceeded operationId must be an integer');
	if (staleCompletion(snapshot, operationId)) return {snapshot, commands: []};
	const completed = markOperation(snapshot, operationId, 'succeeded');
	if (completed.lifecycle.tearingDown) {
		return planPendingConnectionTeardown(clearConnectionAfterDisconnect(completed));
	}
	if (completed.connection.desired) {
		return beginConnectionConnect(
			{
				...completed,
				connection: {
					...completed.connection,
					active: null,
					operationId: null,
					disconnectReason: null,
				},
			},
			completed.connection.desired,
			'reconnecting',
		);
	}
	return {snapshot: resetAfterDisconnect(completed), commands: []};
}

function onDisconnectFailed(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onDisconnectFailed snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onDisconnectFailed operationId must be an integer');
	assert.ok(error != null, 'onDisconnectFailed error must not be null');
	if (staleCompletion(snapshot, operationId)) return {snapshot, commands: []};
	return {
		snapshot: {
			...markOperation(snapshot, operationId, 'failed', error),
			connection: {
				...snapshot.connection,
				status: 'failed',
				operationId: null,
				failure: error,
			},
			lastFailure: error,
		},
		commands: [],
	};
}

function onRemoteDisconnected(
	snapshot: VoiceEngineV2Snapshot,
	reason: VoiceEngineV2DisconnectReason,
	error: VoiceEngineV2Error | undefined,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onRemoteDisconnected snapshot must not be null');
	assert.equal(typeof reason, 'string', 'onRemoteDisconnected reason must be a string');
	const disconnected = resetAfterDisconnect(snapshot);
	return {
		snapshot: {
			...disconnected,
			connection: {
				...disconnected.connection,
				disconnectReason: reason,
				failure: error ?? null,
			},
			lastFailure: error ?? snapshot.lastFailure,
		},
		commands: [],
	};
}

function onReconnectRequested(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onReconnectRequested snapshot must not be null');
	assert.ok(snapshot.connection != null, 'onReconnectRequested snapshot.connection must not be null');
	const options = snapshot.connection.desired ?? snapshot.connection.active;
	if (!options) return {snapshot, commands: []};
	if (snapshot.connection.status === 'connecting' || snapshot.connection.status === 'reconnecting') {
		return {snapshot, commands: []};
	}
	if (snapshot.connection.status === 'connected' || snapshot.connection.active != null) {
		return beginConnectionDisconnect(snapshot, 'network', options, 'reconnecting');
	}
	return beginConnectionConnect(snapshot, options, 'reconnecting');
}
