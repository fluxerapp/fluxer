// SPDX-License-Identifier: AGPL-3.0-or-later

import {GatewayErrorCodes} from '@fluxer/constants/src/GatewayConstants';
import {describe, expect, it} from 'vitest';
import {
	createMediaEngineFacadeSnapshot,
	getMediaEngineFacadeStateValue,
	selectMediaEngineConnectPreflightDecision,
	selectMediaEngineConnectRequestDecision,
	selectMediaEngineGatewayErrorDecision,
	shouldCancelMediaEngineReconnectForServerVoiceStateRemoval,
	shouldImmediatelyDisconnectMediaEngineForServerVoiceStateRemoval,
	shouldNotifyCameraUserLimitRejection,
	shouldRunMediaEngineDeferredDisconnect,
	transitionMediaEngineFacadeSnapshot,
	VOICE_CAMERA_USER_LIMIT_ERROR_CODE,
} from './MediaEngineFacadeStateMachine';

function connected(guildId: string | null = 'guild-1', channelId = 'channel-1') {
	return transitionMediaEngineFacadeSnapshot(createMediaEngineFacadeSnapshot(), {
		type: 'connection.connected',
		guildId,
		channelId,
	});
}

describe('MediaEngineFacadeStateMachine', () => {
	it('noops connect requests for the current channel', () => {
		const snapshot = connected();
		const decision = selectMediaEngineConnectRequestDecision(snapshot, {
			guildId: 'guild-1',
			channelId: 'channel-1',
			connected: true,
			connecting: false,
		});
		expect(decision).toEqual({type: 'noop', reason: 'same-channel'});
	});

	it('tracks user channel moves through local cleanup and the next connection', () => {
		let snapshot = connected();
		const decision = selectMediaEngineConnectRequestDecision(snapshot, {
			guildId: 'guild-1',
			channelId: 'channel-2',
			connected: true,
			connecting: false,
		});
		expect(decision).toEqual({type: 'move-user', guildId: 'guild-1', channelId: 'channel-2'});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'userMove.requested',
			guildId: 'guild-1',
			channelId: 'channel-2',
		});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {type: 'channelMove.cleanupStarted', reason: 'user'});
		expect(getMediaEngineFacadeStateValue(snapshot)).toBe('channelMoving');
		expect(snapshot.context.pendingUserMove).toEqual({guildId: 'guild-1', channelId: 'channel-2'});
		expect(snapshot.context.activeConnection).toBeNull();
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {type: 'cleanup.complete'});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'connection.connected',
			guildId: 'guild-1',
			channelId: 'channel-2',
		});
		expect(snapshot.context.pendingUserMove).toBeNull();
		expect(snapshot.context.activeConnection).toEqual({guildId: 'guild-1', channelId: 'channel-2'});
	});

	it('clears pending user moves during server channel move cleanup', () => {
		let snapshot = connected();
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'userMove.requested',
			guildId: 'guild-1',
			channelId: 'channel-2',
		});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {type: 'channelMove.cleanupStarted', reason: 'server'});
		expect(snapshot.context.pendingUserMove).toBeNull();
		expect(snapshot.context.activeConnection).toBeNull();
		expect(snapshot.context.localCleanupReason).toBe('channelMove');
	});

	it('cancels and expires pending server disconnects by connection id', () => {
		let snapshot = createMediaEngineFacadeSnapshot();
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'serverDisconnect.schedule',
			connectionId: 'connection-1',
		});
		expect(
			shouldRunMediaEngineDeferredDisconnect(snapshot, {
				connectionId: 'connection-1',
				currentConnectionId: 'connection-1',
				connected: true,
				currentVoiceStateChannelId: null,
			}),
		).toBe(true);
		expect(
			shouldRunMediaEngineDeferredDisconnect(snapshot, {
				connectionId: 'connection-1',
				currentConnectionId: 'connection-1',
				connected: true,
				currentVoiceStateChannelId: 'channel-1',
			}),
		).toBe(false);
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {type: 'serverDisconnect.cancel'});
		expect(snapshot.context.pendingServerDisconnectConnectionId).toBeNull();
		expect(
			shouldRunMediaEngineDeferredDisconnect(snapshot, {
				connectionId: 'connection-1',
				currentConnectionId: 'connection-1',
				connected: true,
				currentVoiceStateChannelId: null,
			}),
		).toBe(false);

		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'serverDisconnect.schedule',
			connectionId: 'connection-2',
		});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'serverDisconnect.timeoutElapsed',
			connectionId: 'connection-2',
		});
		expect(snapshot.context.pendingServerDisconnectConnectionId).toBeNull();
	});

	it('disconnects immediately when the server removes the current local voice state', () => {
		expect(
			shouldImmediatelyDisconnectMediaEngineForServerVoiceStateRemoval({
				voiceStateConnectionId: 'connection-1',
				voiceStateChannelId: null,
				currentConnectionId: 'connection-1',
				currentChannelId: 'channel-1',
				connected: true,
				connecting: false,
			}),
		).toBe(true);

		expect(
			shouldImmediatelyDisconnectMediaEngineForServerVoiceStateRemoval({
				voiceStateConnectionId: 'connection-1',
				voiceStateChannelId: 'channel-1',
				currentConnectionId: 'connection-1',
				currentChannelId: 'channel-1',
				connected: true,
				connecting: false,
			}),
		).toBe(false);

		expect(
			shouldImmediatelyDisconnectMediaEngineForServerVoiceStateRemoval({
				voiceStateConnectionId: 'connection-2',
				voiceStateChannelId: null,
				currentConnectionId: 'connection-1',
				currentChannelId: 'channel-1',
				connected: true,
				connecting: false,
			}),
		).toBe(false);
	});

	it('cancels reconnect when local voice-state removal arrives after transport disconnect', () => {
		expect(
			shouldCancelMediaEngineReconnectForServerVoiceStateRemoval({
				voiceStateConnectionId: 'connection-1',
				voiceStateChannelId: null,
				currentConnectionId: 'connection-1',
				currentChannelId: null,
				connected: false,
				connecting: false,
			}),
		).toBe(true);
		expect(
			shouldCancelMediaEngineReconnectForServerVoiceStateRemoval({
				voiceStateConnectionId: 'connection-1',
				voiceStateChannelId: null,
				currentConnectionId: 'connection-1',
				currentChannelId: 'channel-1',
				connected: false,
				connecting: false,
			}),
		).toBe(false);
		expect(
			shouldCancelMediaEngineReconnectForServerVoiceStateRemoval({
				voiceStateConnectionId: 'connection-2',
				voiceStateChannelId: null,
				currentConnectionId: 'connection-1',
				currentChannelId: null,
				connected: false,
				connecting: false,
			}),
		).toBe(false);
	});

	it('selects gateway error abort decisions while connecting', () => {
		let snapshot = createMediaEngineFacadeSnapshot();
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'sessionRestore.prepare',
			guildId: 'guild-1',
			channelId: 'channel-1',
			restoreVideo: true,
			restoreStream: true,
		});
		const decision = selectMediaEngineGatewayErrorDecision(snapshot, {
			code: GatewayErrorCodes.VOICE_CONNECTION_NOT_FOUND,
			connecting: true,
			connected: false,
			channelId: 'channel-1',
		});
		expect(decision).toMatchObject({
			type: 'handle',
			clearPendingSessionRestore: true,
			clearViewerStreamKeys: true,
			abortConnection: true,
			disconnectReason: null,
		});
	});

	it('selects unavailable target cleanup and aborts unavailable connecting targets', () => {
		let snapshot = createMediaEngineFacadeSnapshot();
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'sessionRestore.prepare',
			guildId: 'guild-1',
			channelId: 'missing-channel',
			restoreVideo: false,
			restoreStream: false,
		});
		const decision = selectMediaEngineGatewayErrorDecision(snapshot, {
			code: GatewayErrorCodes.VOICE_CHANNEL_NOT_FOUND,
			connecting: true,
			connected: false,
			channelId: null,
		});
		expect(decision).toMatchObject({
			type: 'handle',
			unavailableChannelId: 'missing-channel',
			showUnavailableToast: true,
			clearViewerStreamKeys: true,
			abortConnection: true,
		});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'unavailableTarget.clear',
			channelId: 'missing-channel',
		});
		expect(snapshot.context.pendingSessionRestore).toBeNull();
	});

	it('preserves or clears pending session restore only for matching targets', () => {
		let snapshot = createMediaEngineFacadeSnapshot();
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'sessionRestore.prepare',
			guildId: null,
			channelId: 'dm-1',
			restoreVideo: true,
			restoreStream: false,
		});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'sessionRestore.clearTarget',
			guildId: null,
			channelId: 'dm-2',
		});
		expect(snapshot.context.pendingSessionRestore?.channelId).toBe('dm-1');
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'sessionRestore.clearTarget',
			guildId: null,
			channelId: 'dm-1',
		});
		expect(snapshot.context.pendingSessionRestore).toBeNull();
	});

	it('stores, clears, and consumes pending screen share reconnect snapshots', () => {
		const reconnectSnapshot = {sourceId: 'screen-1'};
		let snapshot = createMediaEngineFacadeSnapshot();
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'screenShareReconnect.prepare',
			snapshot: reconnectSnapshot,
		});
		expect(snapshot.context.pendingScreenShareReconnect).toBe(reconnectSnapshot);
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {type: 'screenShareReconnect.consume'});
		expect(snapshot.context.pendingScreenShareReconnect).toBeNull();
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'screenShareReconnect.prepare',
			snapshot: reconnectSnapshot,
		});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {type: 'screenShareReconnect.clear'});
		expect(snapshot.context.pendingScreenShareReconnect).toBeNull();
	});

	it('resets all orchestration state on logout, cleanup, and reset', () => {
		let snapshot = connected();
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'userMove.requested',
			guildId: 'guild-1',
			channelId: 'channel-2',
		});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'serverDisconnect.schedule',
			connectionId: 'connection-1',
		});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {type: 'cleanup.logoutStarted'});
		expect(getMediaEngineFacadeStateValue(snapshot)).toBe('loggingOut');
		expect(snapshot.context.activeConnection).toBeNull();
		expect(snapshot.context.pendingUserMove).toBeNull();
		expect(snapshot.context.pendingServerDisconnectConnectionId).toBeNull();
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {type: 'cleanup.complete'});
		expect(getMediaEngineFacadeStateValue(snapshot)).toBe('idle');

		snapshot = transitionMediaEngineFacadeSnapshot(connected(), {type: 'cleanup.cleanupStarted'});
		expect(getMediaEngineFacadeStateValue(snapshot)).toBe('cleaningUp');
		snapshot = transitionMediaEngineFacadeSnapshot(connected(), {type: 'cleanup.reset'});
		expect(snapshot.context).toMatchObject({
			activeConnection: null,
			pendingUserMove: null,
			pendingSessionRestore: null,
			pendingServerDisconnectConnectionId: null,
		});
	});

	it('handles repeated connect and disconnect cleanup sequences', () => {
		let snapshot = connected('guild-1', 'channel-1');
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {type: 'disconnect.cleanupStarted', reason: 'user'});
		expect(getMediaEngineFacadeStateValue(snapshot)).toBe('disconnecting');
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {type: 'cleanup.complete'});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {
			type: 'connection.connected',
			guildId: 'guild-1',
			channelId: 'channel-2',
		});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {type: 'disconnect.cleanupStarted', reason: 'server'});
		snapshot = transitionMediaEngineFacadeSnapshot(snapshot, {type: 'cleanup.complete'});
		expect(getMediaEngineFacadeStateValue(snapshot)).toBe('idle');
		expect(snapshot.context.activeConnection).toBeNull();
		expect(snapshot.context.pendingSessionRestore).toBeNull();
	});

	it('keeps connect preflight outcomes pure', () => {
		expect(
			selectMediaEngineConnectPreflightDecision({
				targetAvailable: false,
				isTimedOut: false,
				isUnclaimed: false,
				isGuildOwner: true,
				isDirectMessage: false,
				hasGatewaySocket: true,
				blockedByMatureGate: false,
				channelLimitAllowed: true,
			}),
		).toEqual({type: 'cleanup-unavailable-target', showToast: true});
		expect(
			selectMediaEngineConnectPreflightDecision({
				targetAvailable: true,
				isTimedOut: false,
				isUnclaimed: true,
				isGuildOwner: true,
				isDirectMessage: true,
				hasGatewaySocket: true,
				blockedByMatureGate: false,
				channelLimitAllowed: true,
			}),
		).toEqual({type: 'toast', reason: 'claim-account-direct'});
		expect(
			selectMediaEngineConnectPreflightDecision({
				targetAvailable: true,
				isTimedOut: false,
				isUnclaimed: false,
				isGuildOwner: true,
				isDirectMessage: false,
				hasGatewaySocket: true,
				blockedByMatureGate: true,
				channelLimitAllowed: true,
			}),
		).toEqual({type: 'navigate-channel-gate'});
	});

	it('notifies only for rejected voice state acks carrying the camera user limit error code', () => {
		expect(
			shouldNotifyCameraUserLimitRejection({
				status: 'rejected',
				errorCode: VOICE_CAMERA_USER_LIMIT_ERROR_CODE,
			}),
		).toBe(true);
		expect(
			shouldNotifyCameraUserLimitRejection({
				status: 'applied',
				errorCode: VOICE_CAMERA_USER_LIMIT_ERROR_CODE,
			}),
		).toBe(false);
		expect(
			shouldNotifyCameraUserLimitRejection({
				status: 'rejected',
				errorCode: 'VOICE_PERMISSION_DENIED',
			}),
		).toBe(false);
		expect(shouldNotifyCameraUserLimitRejection({})).toBe(false);
		expect(shouldNotifyCameraUserLimitRejection({status: 'rejected'})).toBe(false);
	});
});
