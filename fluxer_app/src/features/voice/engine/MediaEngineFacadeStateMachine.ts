// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayErrorCode} from '@fluxer/constants/src/GatewayConstants';
import {GatewayErrorCodes} from '@fluxer/constants/src/GatewayConstants';
import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export interface MediaEngineFacadeConnectionTarget {
	guildId: string | null;
	channelId: string;
}

export interface MediaEngineFacadePendingSessionRestore extends MediaEngineFacadeConnectionTarget {
	restoreVideo: boolean;
	restoreStream: boolean;
}

export type MediaEngineFacadeLocalCleanupReason = 'disconnect' | 'channelMove' | 'logout' | 'cleanup' | null;

export interface MediaEngineFacadeMachineContext {
	activeConnection: MediaEngineFacadeConnectionTarget | null;
	pendingUserMove: MediaEngineFacadeConnectionTarget | null;
	pendingSessionRestore: MediaEngineFacadePendingSessionRestore | null;
	pendingScreenShareReconnect: unknown | null;
	pendingServerDisconnectConnectionId: string | null;
	localCleanupReason: MediaEngineFacadeLocalCleanupReason;
}

export type MediaEngineFacadeEvent =
	| {type: 'connection.connected'; guildId: string | null; channelId: string}
	| {type: 'userMove.requested'; guildId: string | null; channelId: string}
	| {type: 'channelMove.cleanupStarted'; reason: 'user' | 'server'}
	| {type: 'disconnect.cleanupStarted'; reason: 'user' | 'error' | 'server'}
	| {type: 'serverDisconnect.schedule'; connectionId: string}
	| {type: 'serverDisconnect.cancel'}
	| {type: 'serverDisconnect.timeoutElapsed'; connectionId: string}
	| {
			type: 'sessionRestore.prepare';
			guildId: string | null;
			channelId: string;
			restoreVideo: boolean;
			restoreStream: boolean;
	  }
	| {type: 'sessionRestore.clear'}
	| {type: 'sessionRestore.clearTarget'; guildId: string | null; channelId: string}
	| {type: 'sessionRestore.consume'}
	| {type: 'screenShareReconnect.prepare'; snapshot: unknown}
	| {type: 'screenShareReconnect.clear'}
	| {type: 'screenShareReconnect.consume'}
	| {type: 'unavailableTarget.clear'; channelId: string}
	| {type: 'cleanup.logoutStarted'}
	| {type: 'cleanup.cleanupStarted'}
	| {type: 'cleanup.reset'}
	| {type: 'cleanup.complete'};

export type MediaEngineFacadeStateValue = 'idle' | 'disconnecting' | 'channelMoving' | 'loggingOut' | 'cleaningUp';

export type MediaEngineFacadeConnectRequestDecision =
	| {type: 'noop'; reason: 'same-channel'}
	| {type: 'start'}
	| {type: 'move-user'; guildId: string | null; channelId: string};

export interface MediaEngineFacadeConnectPreflightInput {
	targetAvailable: boolean;
	isTimedOut: boolean;
	isUnclaimed: boolean;
	isGuildOwner: boolean;
	isDirectMessage: boolean;
	hasGatewaySocket: boolean;
	blockedByMatureGate: boolean;
	channelLimitAllowed: boolean;
}

export type MediaEngineFacadeConnectPreflightDecision =
	| {type: 'cleanup-unavailable-target'; showToast: true}
	| {type: 'toast'; reason: 'timed-out' | 'claim-account-guild' | 'claim-account-direct'}
	| {type: 'abort'; reason: 'missing-gateway-socket' | 'channel-limit'}
	| {type: 'navigate-channel-gate'}
	| {type: 'proceed'};

export interface MediaEngineFacadeGatewayErrorInput {
	code: GatewayErrorCode;
	connecting: boolean;
	connected: boolean;
	channelId: string | null;
}

export const VOICE_CAMERA_USER_LIMIT_ERROR_CODE = 'VOICE_CAMERA_USER_LIMIT';

export interface MediaEngineFacadeVoiceStateAckRejectionInput {
	status?: string;
	errorCode?: string;
}

export function shouldNotifyCameraUserLimitRejection(input: MediaEngineFacadeVoiceStateAckRejectionInput): boolean {
	if (input.status !== 'rejected') return false;
	return input.errorCode === VOICE_CAMERA_USER_LIMIT_ERROR_CODE;
}

export type MediaEngineFacadeGatewayErrorDecision =
	| {type: 'ignore'}
	| {
			type: 'handle';
			clearPendingSessionRestore: boolean;
			clearViewerStreamKeys: boolean;
			abortConnection: boolean;
			unavailableChannelId: string | null;
			showUnavailableToast: boolean;
			disconnectReason: 'server' | null;
			toast: 'timed-out' | 'connection-limit' | 'unclaimed-account' | null;
	  };

export interface MediaEngineFacadeDeferredDisconnectInput {
	connectionId: string;
	currentConnectionId: string | null;
	connected: boolean;
	currentVoiceStateChannelId: string | null;
}

export interface MediaEngineFacadeServerVoiceStateRemovalInput {
	voiceStateConnectionId: string | null | undefined;
	voiceStateChannelId: string | null | undefined;
	currentConnectionId: string | null;
	currentChannelId: string | null;
	connected: boolean;
	connecting: boolean;
}

export function createInitialMediaEngineFacadeContext(): MediaEngineFacadeMachineContext {
	return {
		activeConnection: null,
		pendingUserMove: null,
		pendingSessionRestore: null,
		pendingScreenShareReconnect: null,
		pendingServerDisconnectConnectionId: null,
		localCleanupReason: null,
	};
}

function clearSessionRestoreForTarget(
	context: MediaEngineFacadeMachineContext,
	guildId: string | null,
	channelId: string,
): MediaEngineFacadeMachineContext {
	if (context.pendingSessionRestore?.guildId !== guildId || context.pendingSessionRestore.channelId !== channelId) {
		return context;
	}
	return {
		...context,
		pendingSessionRestore: null,
	};
}

function clearUnavailableTarget(
	context: MediaEngineFacadeMachineContext,
	channelId: string,
): MediaEngineFacadeMachineContext {
	if (context.pendingSessionRestore?.channelId !== channelId) return context;
	return {
		...context,
		pendingSessionRestore: null,
	};
}

function resetForDisconnect(context: MediaEngineFacadeMachineContext): MediaEngineFacadeMachineContext {
	return {
		...context,
		activeConnection: null,
		pendingUserMove: null,
		pendingSessionRestore: null,
		pendingScreenShareReconnect: null,
		pendingServerDisconnectConnectionId: null,
		localCleanupReason: 'disconnect',
	};
}

function resetForChannelMove(
	context: MediaEngineFacadeMachineContext,
	reason: 'user' | 'server',
): MediaEngineFacadeMachineContext {
	return {
		...context,
		activeConnection: null,
		pendingUserMove: reason === 'server' ? null : context.pendingUserMove,
		pendingSessionRestore: null,
		pendingScreenShareReconnect: null,
		pendingServerDisconnectConnectionId: null,
		localCleanupReason: 'channelMove',
	};
}

function resetAll(localCleanupReason: MediaEngineFacadeLocalCleanupReason): MediaEngineFacadeMachineContext {
	return {
		...createInitialMediaEngineFacadeContext(),
		localCleanupReason,
	};
}

export const mediaEngineFacadeStateMachine = setup({
	types: {} as {
		context: MediaEngineFacadeMachineContext;
		events: MediaEngineFacadeEvent;
	},
	actions: {
		connected: assign(({context, event}) =>
			event.type === 'connection.connected'
				? {
						...context,
						activeConnection: {guildId: event.guildId, channelId: event.channelId},
						pendingUserMove: null,
					}
				: context,
		),
		requestUserMove: assign(({context, event}) =>
			event.type === 'userMove.requested'
				? {
						...context,
						pendingUserMove: {guildId: event.guildId, channelId: event.channelId},
					}
				: context,
		),
		startChannelMoveCleanup: assign(({context, event}) =>
			event.type === 'channelMove.cleanupStarted' ? resetForChannelMove(context, event.reason) : context,
		),
		startDisconnectCleanup: assign(({context}) => resetForDisconnect(context)),
		scheduleServerDisconnect: assign(({context, event}) =>
			event.type === 'serverDisconnect.schedule'
				? {
						...context,
						pendingServerDisconnectConnectionId: event.connectionId,
					}
				: context,
		),
		clearServerDisconnect: assign(({context}) => ({
			...context,
			pendingServerDisconnectConnectionId: null,
		})),
		clearMatchingServerDisconnect: assign(({context, event}) =>
			event.type === 'serverDisconnect.timeoutElapsed' &&
			context.pendingServerDisconnectConnectionId === event.connectionId
				? {
						...context,
						pendingServerDisconnectConnectionId: null,
					}
				: context,
		),
		prepareSessionRestore: assign(({context, event}) =>
			event.type === 'sessionRestore.prepare'
				? {
						...context,
						pendingSessionRestore: {
							guildId: event.guildId,
							channelId: event.channelId,
							restoreVideo: event.restoreVideo,
							restoreStream: event.restoreStream,
						},
					}
				: context,
		),
		clearSessionRestore: assign(({context}) => ({
			...context,
			pendingSessionRestore: null,
		})),
		clearTargetSessionRestore: assign(({context, event}) =>
			event.type === 'sessionRestore.clearTarget'
				? clearSessionRestoreForTarget(context, event.guildId, event.channelId)
				: context,
		),
		prepareScreenShareReconnect: assign(({context, event}) =>
			event.type === 'screenShareReconnect.prepare'
				? {
						...context,
						pendingScreenShareReconnect: event.snapshot,
					}
				: context,
		),
		clearScreenShareReconnect: assign(({context}) => ({
			...context,
			pendingScreenShareReconnect: null,
		})),
		clearUnavailableTarget: assign(({context, event}) =>
			event.type === 'unavailableTarget.clear' ? clearUnavailableTarget(context, event.channelId) : context,
		),
		startLogoutCleanup: assign(() => resetAll('logout')),
		startCleanup: assign(() => resetAll('cleanup')),
		reset: assign(() => resetAll('cleanup')),
		completeCleanup: assign(({context}) => ({
			...context,
			localCleanupReason: null,
		})),
	},
}).createMachine({
	id: 'mediaEngineFacade',
	context: () => createInitialMediaEngineFacadeContext(),
	initial: 'idle',
	states: {
		idle: {
			on: {
				'disconnect.cleanupStarted': {target: 'disconnecting', actions: 'startDisconnectCleanup'},
				'channelMove.cleanupStarted': {target: 'channelMoving', actions: 'startChannelMoveCleanup'},
				'cleanup.logoutStarted': {target: 'loggingOut', actions: 'startLogoutCleanup'},
				'cleanup.cleanupStarted': {target: 'cleaningUp', actions: 'startCleanup'},
				'cleanup.reset': {actions: 'reset'},
			},
		},
		disconnecting: {
			on: {
				'cleanup.complete': {target: 'idle', actions: 'completeCleanup'},
				'cleanup.reset': {target: 'idle', actions: 'reset'},
			},
		},
		channelMoving: {
			on: {
				'cleanup.complete': {target: 'idle', actions: 'completeCleanup'},
				'cleanup.reset': {target: 'idle', actions: 'reset'},
			},
		},
		loggingOut: {
			on: {
				'cleanup.complete': {target: 'idle', actions: 'completeCleanup'},
				'cleanup.reset': {target: 'idle', actions: 'reset'},
			},
		},
		cleaningUp: {
			on: {
				'cleanup.complete': {target: 'idle', actions: 'completeCleanup'},
				'cleanup.reset': {target: 'idle', actions: 'reset'},
			},
		},
	},
	on: {
		'disconnect.cleanupStarted': {target: '.disconnecting', actions: 'startDisconnectCleanup'},
		'channelMove.cleanupStarted': {target: '.channelMoving', actions: 'startChannelMoveCleanup'},
		'cleanup.logoutStarted': {target: '.loggingOut', actions: 'startLogoutCleanup'},
		'cleanup.cleanupStarted': {target: '.cleaningUp', actions: 'startCleanup'},
		'cleanup.reset': {target: '.idle', actions: 'reset'},
		'connection.connected': {actions: 'connected'},
		'userMove.requested': {actions: 'requestUserMove'},
		'serverDisconnect.schedule': {actions: 'scheduleServerDisconnect'},
		'serverDisconnect.cancel': {actions: 'clearServerDisconnect'},
		'serverDisconnect.timeoutElapsed': {actions: 'clearMatchingServerDisconnect'},
		'sessionRestore.prepare': {actions: 'prepareSessionRestore'},
		'sessionRestore.clear': {actions: 'clearSessionRestore'},
		'sessionRestore.clearTarget': {actions: 'clearTargetSessionRestore'},
		'sessionRestore.consume': {actions: 'clearSessionRestore'},
		'screenShareReconnect.prepare': {actions: 'prepareScreenShareReconnect'},
		'screenShareReconnect.clear': {actions: 'clearScreenShareReconnect'},
		'screenShareReconnect.consume': {actions: 'clearScreenShareReconnect'},
		'unavailableTarget.clear': {actions: 'clearUnavailableTarget'},
	},
});

export type MediaEngineFacadeSnapshot = SnapshotFrom<typeof mediaEngineFacadeStateMachine>;

export function createMediaEngineFacadeSnapshot(): MediaEngineFacadeSnapshot {
	return getInitialSnapshot(mediaEngineFacadeStateMachine);
}

export function transitionMediaEngineFacadeSnapshot(
	snapshot: MediaEngineFacadeSnapshot,
	event: MediaEngineFacadeEvent,
): MediaEngineFacadeSnapshot {
	return transition(mediaEngineFacadeStateMachine, snapshot, event)[0] as MediaEngineFacadeSnapshot;
}

export function getMediaEngineFacadeStateValue(snapshot: MediaEngineFacadeSnapshot): MediaEngineFacadeStateValue {
	return snapshot.value as MediaEngineFacadeStateValue;
}

export function selectMediaEngineConnectRequestDecision(
	snapshot: MediaEngineFacadeSnapshot,
	target: MediaEngineFacadeConnectionTarget & {
		connected: boolean;
		connecting: boolean;
		currentGuildId?: string | null;
		currentChannelId?: string | null;
	},
): MediaEngineFacadeConnectRequestDecision {
	if (target.connected || target.connecting) {
		const currentGuildId = target.currentGuildId ?? snapshot.context.activeConnection?.guildId ?? null;
		const currentChannelId = target.currentChannelId ?? snapshot.context.activeConnection?.channelId ?? null;
		if (currentGuildId === target.guildId && currentChannelId === target.channelId) {
			return {type: 'noop', reason: 'same-channel'};
		}
		return {type: 'move-user', guildId: target.guildId, channelId: target.channelId};
	}
	return {type: 'start'};
}

export function selectMediaEngineConnectPreflightDecision(
	input: MediaEngineFacadeConnectPreflightInput,
): MediaEngineFacadeConnectPreflightDecision {
	if (!input.targetAvailable) {
		return {type: 'cleanup-unavailable-target', showToast: true};
	}
	if (input.isTimedOut) {
		return {type: 'toast', reason: 'timed-out'};
	}
	if (input.isUnclaimed && input.isDirectMessage) {
		return {type: 'toast', reason: 'claim-account-direct'};
	}
	if (input.isUnclaimed && !input.isGuildOwner) {
		return {
			type: 'toast',
			reason: 'claim-account-guild',
		};
	}
	if (!input.hasGatewaySocket) {
		return {type: 'abort', reason: 'missing-gateway-socket'};
	}
	if (input.blockedByMatureGate) {
		return {type: 'navigate-channel-gate'};
	}
	if (!input.channelLimitAllowed) {
		return {type: 'abort', reason: 'channel-limit'};
	}
	return {type: 'proceed'};
}

const voiceGatewayErrorCodes = new Set<GatewayErrorCode>([
	GatewayErrorCodes.VOICE_CONNECTION_NOT_FOUND,
	GatewayErrorCodes.VOICE_CHANNEL_NOT_FOUND,
	GatewayErrorCodes.VOICE_INVALID_CHANNEL_TYPE,
	GatewayErrorCodes.VOICE_MEMBER_NOT_FOUND,
	GatewayErrorCodes.VOICE_MEMBER_TIMED_OUT,
	GatewayErrorCodes.VOICE_USER_NOT_IN_VOICE,
	GatewayErrorCodes.VOICE_GUILD_NOT_FOUND,
	GatewayErrorCodes.VOICE_PERMISSION_DENIED,
	GatewayErrorCodes.VOICE_CHANNEL_FULL,
	GatewayErrorCodes.VOICE_CONNECTION_LIMIT_REACHED,
	GatewayErrorCodes.VOICE_MISSING_CONNECTION_ID,
	GatewayErrorCodes.VOICE_TOKEN_FAILED,
	GatewayErrorCodes.VOICE_UNCLAIMED_ACCOUNT,
]);

const unavailableGatewayErrorCodes = new Set<GatewayErrorCode>([
	GatewayErrorCodes.VOICE_CHANNEL_NOT_FOUND,
	GatewayErrorCodes.VOICE_INVALID_CHANNEL_TYPE,
	GatewayErrorCodes.VOICE_GUILD_NOT_FOUND,
]);

const connectingAbortGatewayErrorCodes = new Set<GatewayErrorCode>([
	GatewayErrorCodes.VOICE_CONNECTION_NOT_FOUND,
	GatewayErrorCodes.VOICE_TOKEN_FAILED,
]);

const preConnectAbortGatewayErrorCodes = new Set<GatewayErrorCode>([
	GatewayErrorCodes.VOICE_PERMISSION_DENIED,
	GatewayErrorCodes.VOICE_CHANNEL_FULL,
	GatewayErrorCodes.VOICE_CONNECTION_LIMIT_REACHED,
	GatewayErrorCodes.VOICE_MEMBER_TIMED_OUT,
	GatewayErrorCodes.VOICE_UNCLAIMED_ACCOUNT,
]);

export function selectMediaEngineGatewayErrorDecision(
	snapshot: MediaEngineFacadeSnapshot,
	input: MediaEngineFacadeGatewayErrorInput,
): MediaEngineFacadeGatewayErrorDecision {
	if (!voiceGatewayErrorCodes.has(input.code)) return {type: 'ignore'};

	const abortConnection =
		input.connecting &&
		(connectingAbortGatewayErrorCodes.has(input.code) ||
			(!input.connected && preConnectAbortGatewayErrorCodes.has(input.code)));
	let unavailableChannelId: string | null = null;
	let disconnectReason: 'server' | null = null;
	if (unavailableGatewayErrorCodes.has(input.code)) {
		unavailableChannelId = input.channelId ?? snapshot.context.pendingSessionRestore?.channelId ?? null;
		if (input.connecting && !input.connected) {
			return {
				type: 'handle',
				clearPendingSessionRestore: false,
				clearViewerStreamKeys: true,
				abortConnection: true,
				unavailableChannelId,
				showUnavailableToast: unavailableChannelId !== null,
				disconnectReason: null,
				toast: null,
			};
		}
		if (unavailableChannelId && input.channelId === unavailableChannelId) {
			disconnectReason = 'server';
		}
	}

	let toast: 'timed-out' | 'connection-limit' | 'unclaimed-account' | null = null;
	if (input.code === GatewayErrorCodes.VOICE_MEMBER_TIMED_OUT) {
		toast = 'timed-out';
	} else if (input.code === GatewayErrorCodes.VOICE_CONNECTION_LIMIT_REACHED) {
		toast = 'connection-limit';
	} else if (input.code === GatewayErrorCodes.VOICE_UNCLAIMED_ACCOUNT) {
		toast = 'unclaimed-account';
	}

	return {
		type: 'handle',
		clearPendingSessionRestore: abortConnection,
		clearViewerStreamKeys: abortConnection,
		abortConnection,
		unavailableChannelId,
		showUnavailableToast: unavailableChannelId !== null,
		disconnectReason,
		toast,
	};
}

export function shouldRunMediaEngineDeferredDisconnect(
	snapshot: MediaEngineFacadeSnapshot,
	input: MediaEngineFacadeDeferredDisconnectInput,
): boolean {
	return (
		snapshot.context.pendingServerDisconnectConnectionId === input.connectionId &&
		input.currentConnectionId === input.connectionId &&
		input.connected &&
		input.currentVoiceStateChannelId === null
	);
}

function isCurrentServerVoiceStateRemoval(input: MediaEngineFacadeServerVoiceStateRemovalInput): boolean {
	return (
		input.voiceStateConnectionId != null &&
		input.voiceStateConnectionId === input.currentConnectionId &&
		input.voiceStateChannelId == null
	);
}

export function shouldImmediatelyDisconnectMediaEngineForServerVoiceStateRemoval(
	input: MediaEngineFacadeServerVoiceStateRemovalInput,
): boolean {
	if (!isCurrentServerVoiceStateRemoval(input)) return false;
	return input.connected || input.connecting || input.currentChannelId != null;
}

export function shouldCancelMediaEngineReconnectForServerVoiceStateRemoval(
	input: MediaEngineFacadeServerVoiceStateRemovalInput,
): boolean {
	if (!isCurrentServerVoiceStateRemoval(input)) return false;
	if (input.connected) return false;
	if (input.connecting) return false;
	return input.currentChannelId == null;
}
