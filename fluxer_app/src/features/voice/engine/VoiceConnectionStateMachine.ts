// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type VoiceConnectionDisconnectReason = 'user' | 'error' | 'server';
export type VoiceConnectionLocalDisconnectReason =
	| VoiceConnectionDisconnectReason
	| 'channelMove'
	| 'abort'
	| 'replaced'
	| null;
export type VoiceConnectionFailureReason = 'connectTimeout' | 'gatewayTimeout' | 'transport' | null;

export const VoiceEngineConnectionState = Object.freeze({
	Disconnected: 'disconnected',
	Connecting: 'connecting',
	Connected: 'connected',
	Reconnecting: 'reconnecting',
	SignalReconnecting: 'signalReconnecting',
	Failed: 'failed',
});

export type VoiceEngineConnectionState = (typeof VoiceEngineConnectionState)[keyof typeof VoiceEngineConnectionState];

export function asVoiceEngineConnectionState(value: unknown): VoiceEngineConnectionState {
	switch (value) {
		case VoiceEngineConnectionState.Connecting:
			return VoiceEngineConnectionState.Connecting;
		case VoiceEngineConnectionState.Connected:
			return VoiceEngineConnectionState.Connected;
		case VoiceEngineConnectionState.Reconnecting:
			return VoiceEngineConnectionState.Reconnecting;
		case VoiceEngineConnectionState.SignalReconnecting:
			return VoiceEngineConnectionState.SignalReconnecting;
		case VoiceEngineConnectionState.Failed:
			return VoiceEngineConnectionState.Failed;
		default:
			return VoiceEngineConnectionState.Disconnected;
	}
}

export interface VoiceConnectionServerUpdatePayload {
	token: string | null;
	endpoint: string | null;
	connection_id: string | null;
	guild_id?: string | null;
	channel_id?: string | null;
	e2ee_key?: string | null;
}

export interface VoiceConnectionHotSwapContext {
	pendingRoom: unknown | null;
	previousRoom: unknown | null;
	inProgress: boolean;
	abortRequested: boolean;
	queuedOperationCount: number;
}

export interface VoiceConnectionMachineContext {
	room: unknown | null;
	guildId: string | null;
	channelId: string | null;
	connecting: boolean;
	connected: boolean;
	reconnecting: boolean;
	voiceServerEndpoint: string | null;
	connectionId: string | null;
	connectionAttemptId: number;
	voiceServerTimeoutScheduled: boolean;
	localDisconnectReason: VoiceConnectionLocalDisconnectReason;
	failureReason: VoiceConnectionFailureReason;
	failedGuildId: string | null;
	failedChannelId: string | null;
	expectedRecovery: boolean;
	hotSwap: VoiceConnectionHotSwapContext;
}

export type VoiceConnectionEvent =
	| {type: 'connection.start'; guildId: string | null; channelId: string}
	| {type: 'connection.recoverExpectation'; guildId: string | null; channelId: string}
	| {
			type: 'voiceServer.accepted';
			guildId: string | null;
			channelId: string;
			endpoint: string | null;
			connectionId: string | null;
			isChannelMove: boolean;
	  }
	| {type: 'voiceServer.timeout'; guildId: string | null; channelId: string}
	| {type: 'connection.roomReady'; room: unknown; attemptId: number}
	| {type: 'connection.failed'; reason: 'error'}
	| {type: 'connection.recover'; guildId: string | null; channelId: string}
	| {type: 'connection.connected'}
	| {type: 'connection.disconnected'; reason: VoiceConnectionDisconnectReason}
	| {type: 'connection.reconnecting'}
	| {type: 'connection.reconnected'}
	| {type: 'connection.disconnectForChannelMove'}
	| {type: 'connection.updateChannel'; channelId: string}
	| {type: 'connection.acceptServerChannelChange'; channelId: string}
	| {type: 'connection.abort'}
	| {type: 'connection.reset'}
	| {type: 'connection.cleanup'}
	| {type: 'hotSwap.start'; pendingRoom: unknown; previousRoom: unknown}
	| {type: 'hotSwap.complete'; room: unknown; endpoint: string | null; connectionId: string | null}
	| {type: 'hotSwap.abort'}
	| {type: 'hotSwap.reset'}
	| {type: 'hotSwap.queueOperation'}
	| {type: 'hotSwap.drainQueue'}
	| {type: 'hotSwap.clearQueue'};

export type VoiceConnectionServerUpdateDecision =
	| {
			type: 'ignore';
			reason: 'guild-or-channel-mismatch' | 'stale-channel-update' | 'stale-attempt';
			expectedGuildId: string | null;
			incomingGuildId: string | null;
			expectedChannelId: string | null;
			incomingChannelId: string | null;
			attemptId: number;
	  }
	| {
			type: 'accept';
			attemptId: number;
			guildId: string | null;
			resolvedChannelId: string;
			incomingChannelId: string | null;
			endpoint: string | null;
			token: string | null;
			connectionId: string | null;
			isChannelMove: boolean;
			isRegionChange: boolean;
			currentRoom: unknown | null;
			currentEndpoint: string | null;
	  };

const initialHotSwapContext: VoiceConnectionHotSwapContext = {
	pendingRoom: null,
	previousRoom: null,
	inProgress: false,
	abortRequested: false,
	queuedOperationCount: 0,
};

export const initialVoiceConnectionContext: VoiceConnectionMachineContext = {
	room: null,
	guildId: null,
	channelId: null,
	connecting: false,
	connected: false,
	reconnecting: false,
	voiceServerEndpoint: null,
	connectionId: null,
	connectionAttemptId: 0,
	voiceServerTimeoutScheduled: false,
	localDisconnectReason: null,
	failureReason: null,
	failedGuildId: null,
	failedChannelId: null,
	expectedRecovery: false,
	hotSwap: initialHotSwapContext,
};

function startConnection(
	context: VoiceConnectionMachineContext,
	guildId: string | null,
	channelId: string,
	expectedRecovery: boolean,
): VoiceConnectionMachineContext {
	return {
		...context,
		guildId,
		channelId,
		connecting: true,
		connected: false,
		reconnecting: false,
		connectionId: null,
		connectionAttemptId: context.connectionAttemptId + 1,
		voiceServerTimeoutScheduled: true,
		localDisconnectReason: null,
		failureReason: null,
		failedGuildId: null,
		failedChannelId: null,
		expectedRecovery,
	};
}

function disconnectConnection(
	context: VoiceConnectionMachineContext,
	reason: VoiceConnectionDisconnectReason,
): VoiceConnectionMachineContext {
	const connectionAttemptId = context.connectionAttemptId;
	const connectionId = reason === 'user' ? null : context.connectionId;
	return {
		...initialVoiceConnectionContext,
		connectionAttemptId,
		connectionId,
		localDisconnectReason: reason,
		expectedRecovery: reason !== 'user',
	};
}

function failConnection(
	context: VoiceConnectionMachineContext,
	failureReason: NonNullable<VoiceConnectionFailureReason>,
): VoiceConnectionMachineContext {
	return {
		...initialVoiceConnectionContext,
		connectionAttemptId: context.connectionAttemptId,
		connectionId: context.connectionId,
		localDisconnectReason: 'error',
		failureReason,
		failedGuildId: context.guildId,
		failedChannelId: context.channelId,
		expectedRecovery: false,
	};
}

function abortConnection(context: VoiceConnectionMachineContext): VoiceConnectionMachineContext {
	return {
		...initialVoiceConnectionContext,
		connectionAttemptId: context.connectionAttemptId,
		localDisconnectReason: 'abort',
		expectedRecovery: false,
	};
}

function disconnectForChannelMove(context: VoiceConnectionMachineContext): VoiceConnectionMachineContext {
	return {
		...initialVoiceConnectionContext,
		connectionAttemptId: context.connectionAttemptId,
		connectionId: context.connectionId,
		localDisconnectReason: 'channelMove',
		expectedRecovery: true,
	};
}

function resetConnection(context: VoiceConnectionMachineContext): VoiceConnectionMachineContext {
	return {
		...initialVoiceConnectionContext,
		connectionAttemptId: context.connectionAttemptId,
	};
}

function clearHotSwap(context: VoiceConnectionMachineContext, abortRequested: boolean): VoiceConnectionMachineContext {
	return {
		...context,
		hotSwap: {
			...initialHotSwapContext,
			abortRequested,
		},
	};
}

export const voiceConnectionStateMachine = setup({
	types: {} as {
		context: VoiceConnectionMachineContext;
		events: VoiceConnectionEvent;
	},
	guards: {
		isLatestRoomAttempt: ({context, event}) =>
			event.type === 'connection.roomReady' && event.attemptId === context.connectionAttemptId,
		shouldApplyVoiceServerTimeout: ({context, event}) =>
			event.type === 'voiceServer.timeout' && shouldApplyVoiceServerTimeout(context, event.guildId, event.channelId),
		isErrorDisconnect: ({event}) => event.type === 'connection.disconnected' && event.reason === 'error',
	},
	actions: {
		start: assign(({context, event}) =>
			event.type === 'connection.start' ? startConnection(context, event.guildId, event.channelId, false) : context,
		),
		recoverExpectation: assign(({context, event}) =>
			event.type === 'connection.recoverExpectation'
				? startConnection(context, event.guildId, event.channelId, true)
				: context,
		),
		acceptVoiceServer: assign(({context, event}) => {
			if (event.type !== 'voiceServer.accepted') return context;
			return {
				...context,
				room: null,
				connecting: true,
				connected: false,
				reconnecting: false,
				guildId: event.guildId,
				channelId: event.channelId,
				voiceServerEndpoint: event.endpoint,
				connectionId: event.connectionId ?? context.connectionId,
				voiceServerTimeoutScheduled: false,
				localDisconnectReason: context.connected ? ('replaced' as const) : null,
			};
		}),
		timeout: assign(({context}) => failConnection(context, 'connectTimeout')),
		failTransport: assign(({context}) => failConnection(context, 'transport')),
		recover: assign(({context, event}) =>
			event.type === 'connection.recover' ? startConnection(context, event.guildId, event.channelId, false) : context,
		),
		roomReady: assign(({context, event}) =>
			event.type === 'connection.roomReady' ? {...context, room: event.room} : context,
		),
		connected: assign(({context}) => ({
			...context,
			connected: true,
			connecting: false,
			reconnecting: false,
			voiceServerTimeoutScheduled: false,
			localDisconnectReason: null,
			expectedRecovery: false,
		})),
		disconnected: assign(({context, event}) =>
			event.type === 'connection.disconnected' ? disconnectConnection(context, event.reason) : context,
		),
		reconnecting: assign(({context}) => ({
			...context,
			connecting: true,
			connected: false,
			reconnecting: true,
			expectedRecovery: true,
		})),
		reconnected: assign(({context}) => ({
			...context,
			connecting: false,
			connected: true,
			reconnecting: false,
			expectedRecovery: false,
		})),
		disconnectForChannelMove: assign(({context}) => disconnectForChannelMove(context)),
		updateChannel: assign(({context, event}) =>
			event.type === 'connection.updateChannel'
				? {
						...context,
						channelId: event.channelId,
						connected: false,
						connecting: true,
						reconnecting: false,
						room: null,
						expectedRecovery: true,
					}
				: context,
		),
		acceptServerChannelChange: assign(({context, event}) =>
			event.type === 'connection.acceptServerChannelChange' ? {...context, channelId: event.channelId} : context,
		),
		abort: assign(({context}) => abortConnection(context)),
		reset: assign(({context}) => resetConnection(context)),
		cleanup: assign(() => initialVoiceConnectionContext),
		startHotSwap: assign(({context, event}) =>
			event.type === 'hotSwap.start'
				? {
						...context,
						hotSwap: {
							pendingRoom: event.pendingRoom,
							previousRoom: event.previousRoom,
							inProgress: true,
							abortRequested: false,
							queuedOperationCount: 0,
						},
					}
				: context,
		),
		completeHotSwap: assign(({context, event}) => {
			if (event.type !== 'hotSwap.complete') return context;
			if (!context.hotSwap.inProgress) return context;
			const restoreConnected = context.reconnecting;
			return {
				...context,
				room: event.room,
				connected: restoreConnected ? true : context.connected,
				connecting: restoreConnected ? false : context.connecting,
				reconnecting: restoreConnected ? false : context.reconnecting,
				voiceServerEndpoint: event.endpoint,
				connectionId: event.connectionId ?? context.connectionId,
				hotSwap: {
					...initialHotSwapContext,
					queuedOperationCount: context.hotSwap.queuedOperationCount,
				},
			};
		}),
		abortHotSwap: assign(({context}) => clearHotSwap(context, true)),
		resetHotSwap: assign(({context}) => clearHotSwap(context, false)),
		queueHotSwapOperation: assign(({context}) => ({
			...context,
			hotSwap: {
				...context.hotSwap,
				queuedOperationCount: context.hotSwap.inProgress
					? context.hotSwap.queuedOperationCount + 1
					: context.hotSwap.queuedOperationCount,
			},
		})),
		clearHotSwapQueue: assign(({context}) => ({
			...context,
			hotSwap: {
				...context.hotSwap,
				queuedOperationCount: 0,
			},
		})),
	},
}).createMachine({
	id: 'voiceConnection',
	context: () => initialVoiceConnectionContext,
	initial: 'disconnected',
	states: {
		disconnected: {
			on: {
				'connection.start': {target: 'connecting', actions: 'start'},
				'connection.recoverExpectation': {target: 'connecting', actions: 'recoverExpectation'},
				'voiceServer.accepted': [
					{target: 'channelMove', guard: ({event}) => event.isChannelMove, actions: 'acceptVoiceServer'},
					{target: 'connecting', actions: 'acceptVoiceServer'},
				],
				'connection.disconnected': {actions: 'disconnected'},
				'connection.updateChannel': {target: 'connecting', actions: 'updateChannel'},
				'connection.cleanup': {actions: 'cleanup'},
				'connection.reset': {actions: 'reset'},
				'connection.abort': {actions: 'abort'},
			},
		},
		connecting: {
			on: {
				'connection.start': {actions: 'start'},
				'connection.recoverExpectation': {actions: 'recoverExpectation'},
				'voiceServer.accepted': [
					{target: 'channelMove', guard: ({event}) => event.isChannelMove, actions: 'acceptVoiceServer'},
					{actions: 'acceptVoiceServer'},
				],
				'voiceServer.timeout': {target: 'failed', guard: 'shouldApplyVoiceServerTimeout', actions: 'timeout'},
				'connection.roomReady': {guard: 'isLatestRoomAttempt', actions: 'roomReady'},
				'connection.failed': {target: 'failed', actions: 'failTransport'},
				'connection.connected': {target: 'connected', actions: 'connected'},
				'connection.reconnecting': {target: 'reconnecting', actions: 'reconnecting'},
				'connection.reconnected': {target: 'connected', actions: 'reconnected'},
				'connection.disconnected': [
					{target: 'failed', guard: 'isErrorDisconnect', actions: 'failTransport'},
					{target: 'disconnected', actions: 'disconnected'},
				],
				'connection.disconnectForChannelMove': {target: 'channelMove', actions: 'disconnectForChannelMove'},
				'connection.updateChannel': {actions: 'updateChannel'},
				'connection.abort': {target: 'disconnected', actions: 'abort'},
				'connection.reset': {target: 'disconnected', actions: 'reset'},
				'connection.cleanup': {target: 'disconnected', actions: 'cleanup'},
			},
		},
		connected: {
			on: {
				'connection.start': {target: 'connecting', actions: 'start'},
				'connection.recoverExpectation': {target: 'connecting', actions: 'recoverExpectation'},
				'voiceServer.accepted': [
					{target: 'channelMove', guard: ({event}) => event.isChannelMove, actions: 'acceptVoiceServer'},
					{target: 'connecting', actions: 'acceptVoiceServer'},
				],
				'connection.roomReady': {guard: 'isLatestRoomAttempt', actions: 'roomReady'},
				'connection.reconnecting': {target: 'reconnecting', actions: 'reconnecting'},
				'connection.disconnected': {target: 'disconnected', actions: 'disconnected'},
				'connection.disconnectForChannelMove': {target: 'channelMove', actions: 'disconnectForChannelMove'},
				'connection.acceptServerChannelChange': {actions: 'acceptServerChannelChange'},
				'connection.updateChannel': {target: 'connecting', actions: 'updateChannel'},
				'connection.abort': {target: 'disconnected', actions: 'abort'},
				'connection.reset': {target: 'disconnected', actions: 'reset'},
				'connection.cleanup': {target: 'disconnected', actions: 'cleanup'},
			},
		},
		reconnecting: {
			on: {
				'hotSwap.complete': {
					target: 'connected',
					guard: ({context}) => context.hotSwap.inProgress,
					actions: 'completeHotSwap',
				},
				'connection.reconnected': {target: 'connected', actions: 'reconnected'},
				'connection.connected': {target: 'connected', actions: 'connected'},
				'connection.disconnected': {target: 'disconnected', actions: 'disconnected'},
				'connection.start': {target: 'connecting', actions: 'start'},
				'connection.recoverExpectation': {target: 'connecting', actions: 'recoverExpectation'},
				'voiceServer.accepted': [
					{target: 'channelMove', guard: ({event}) => event.isChannelMove, actions: 'acceptVoiceServer'},
					{target: 'connecting', actions: 'acceptVoiceServer'},
				],
				'connection.abort': {target: 'disconnected', actions: 'abort'},
				'connection.reset': {target: 'disconnected', actions: 'reset'},
				'connection.cleanup': {target: 'disconnected', actions: 'cleanup'},
			},
		},
		channelMove: {
			on: {
				'connection.start': {target: 'connecting', actions: 'start'},
				'connection.recoverExpectation': {target: 'connecting', actions: 'recoverExpectation'},
				'voiceServer.accepted': {actions: 'acceptVoiceServer'},
				'connection.roomReady': {guard: 'isLatestRoomAttempt', actions: 'roomReady'},
				'connection.connected': {target: 'connected', actions: 'connected'},
				'connection.failed': {target: 'failed', actions: 'failTransport'},
				'connection.disconnected': [
					{target: 'failed', guard: 'isErrorDisconnect', actions: 'failTransport'},
					{target: 'disconnected', actions: 'disconnected'},
				],
				'connection.abort': {target: 'disconnected', actions: 'abort'},
				'connection.reset': {target: 'disconnected', actions: 'reset'},
				'connection.cleanup': {target: 'disconnected', actions: 'cleanup'},
			},
		},
		failed: {
			on: {
				'connection.start': {target: 'connecting', actions: 'start'},
				'connection.recover': {target: 'connecting', actions: 'recover'},
				'connection.recoverExpectation': {target: 'connecting', actions: 'recoverExpectation'},
				'voiceServer.accepted': [
					{target: 'channelMove', guard: ({event}) => event.isChannelMove, actions: 'acceptVoiceServer'},
					{target: 'connecting', actions: 'acceptVoiceServer'},
				],
				'connection.disconnected': [{guard: 'isErrorDisconnect'}, {target: 'disconnected', actions: 'disconnected'}],
				'connection.abort': {target: 'disconnected', actions: 'abort'},
				'connection.reset': {target: 'disconnected', actions: 'reset'},
				'connection.cleanup': {target: 'disconnected', actions: 'cleanup'},
			},
		},
	},
	on: {
		'hotSwap.start': {actions: 'startHotSwap'},
		'hotSwap.complete': {actions: 'completeHotSwap'},
		'hotSwap.abort': {actions: 'abortHotSwap'},
		'hotSwap.reset': {actions: 'resetHotSwap'},
		'hotSwap.queueOperation': {actions: 'queueHotSwapOperation'},
		'hotSwap.drainQueue': {actions: 'clearHotSwapQueue'},
		'hotSwap.clearQueue': {actions: 'clearHotSwapQueue'},
	},
});

export type VoiceConnectionSnapshot = SnapshotFrom<typeof voiceConnectionStateMachine>;
export type VoiceConnectionStateValue =
	| 'disconnected'
	| 'connecting'
	| 'connected'
	| 'reconnecting'
	| 'channelMove'
	| 'failed';

export function createVoiceConnectionSnapshot(): VoiceConnectionSnapshot {
	return initialTransition(voiceConnectionStateMachine)[0];
}

export function transitionVoiceConnectionSnapshot(
	snapshot: VoiceConnectionSnapshot,
	event: VoiceConnectionEvent,
): VoiceConnectionSnapshot {
	return transition(voiceConnectionStateMachine, snapshot, event)[0] as VoiceConnectionSnapshot;
}

export function getVoiceConnectionStateValue(snapshot: VoiceConnectionSnapshot): VoiceConnectionStateValue {
	return snapshot.value as VoiceConnectionStateValue;
}

export function isLatestVoiceConnectionAttempt(snapshot: VoiceConnectionSnapshot, attemptId: number): boolean {
	return snapshot.context.connectionAttemptId === attemptId;
}

export function isVoiceConnectionFailed(snapshot: VoiceConnectionSnapshot): boolean {
	return snapshot.value === 'failed';
}

export function getVoiceConnectionFailureReason(snapshot: VoiceConnectionSnapshot): VoiceConnectionFailureReason {
	return snapshot.context.failureReason;
}

export function getVoiceConnectionFailedTarget(
	snapshot: VoiceConnectionSnapshot,
): {guildId: string | null; channelId: string} | null {
	const {failedChannelId, failedGuildId} = snapshot.context;
	if (!failedChannelId) return null;
	return {guildId: failedGuildId, channelId: failedChannelId};
}

export function shouldApplyVoiceServerTimeout(
	context: VoiceConnectionMachineContext,
	guildId: string | null,
	channelId: string,
): boolean {
	return context.guildId === guildId && context.channelId === channelId && !context.connected;
}

export function selectVoiceConnectionServerUpdateDecision(
	snapshot: VoiceConnectionSnapshot,
	raw: VoiceConnectionServerUpdatePayload,
	attemptId = snapshot.context.connectionAttemptId,
): VoiceConnectionServerUpdateDecision {
	const context = snapshot.context;
	const guildId = raw.guild_id ?? null;
	const endpoint = raw.endpoint ?? null;
	const token = raw.token ?? null;
	const connectionId = raw.connection_id ?? null;
	const incomingChannelId = raw.channel_id ?? null;
	const expectedGuildId = context.guildId;
	const expectedChannelId = context.channelId;
	if (expectedGuildId !== guildId || expectedChannelId == null) {
		return {
			type: 'ignore',
			reason: 'guild-or-channel-mismatch',
			expectedGuildId,
			incomingGuildId: guildId,
			expectedChannelId,
			incomingChannelId,
			attemptId,
		};
	}
	const isChannelMove = !!(incomingChannelId && incomingChannelId !== expectedChannelId);
	if (isChannelMove && !context.connected) {
		return {
			type: 'ignore',
			reason: 'stale-channel-update',
			expectedGuildId,
			incomingGuildId: guildId,
			expectedChannelId,
			incomingChannelId,
			attemptId,
		};
	}
	if (!isLatestVoiceConnectionAttempt(snapshot, attemptId)) {
		return {
			type: 'ignore',
			reason: 'stale-attempt',
			expectedGuildId,
			incomingGuildId: guildId,
			expectedChannelId,
			incomingChannelId,
			attemptId,
		};
	}
	const isRegionChange =
		context.connected &&
		!!context.room &&
		!isChannelMove &&
		!!endpoint &&
		!!token &&
		!!context.voiceServerEndpoint &&
		endpoint !== context.voiceServerEndpoint &&
		!raw.e2ee_key;
	return {
		type: 'accept',
		attemptId,
		guildId,
		resolvedChannelId: incomingChannelId ?? expectedChannelId,
		incomingChannelId,
		endpoint,
		token,
		connectionId,
		isChannelMove,
		isRegionChange,
		currentRoom: context.room,
		currentEndpoint: context.voiceServerEndpoint,
	};
}
