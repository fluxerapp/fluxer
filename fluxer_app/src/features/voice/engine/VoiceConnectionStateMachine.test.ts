// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	asVoiceEngineConnectionState,
	createVoiceConnectionSnapshot,
	getVoiceConnectionStateValue,
	isLatestVoiceConnectionAttempt,
	selectVoiceConnectionServerUpdateDecision,
	transitionVoiceConnectionSnapshot,
	type VoiceConnectionSnapshot,
	VoiceEngineConnectionState,
} from '@app/features/voice/engine/VoiceConnectionStateMachine';
import {describe, expect, it} from 'vitest';

const roomA = {id: 'room-a'};
const roomB = {id: 'room-b'};

function startAndAccept(): VoiceConnectionSnapshot {
	let snapshot = createVoiceConnectionSnapshot();
	snapshot = transitionVoiceConnectionSnapshot(snapshot, {
		type: 'connection.start',
		guildId: 'guild-1',
		channelId: 'channel-1',
	});
	return transitionVoiceConnectionSnapshot(snapshot, {
		type: 'voiceServer.accepted',
		guildId: 'guild-1',
		channelId: 'channel-1',
		endpoint: 'wss://voice-a.example',
		connectionId: 'connection-1',
		isChannelMove: false,
	});
}

describe('VoiceConnectionStateMachine', () => {
	it('normalizes external connection-state values into the Fluxer state contract', () => {
		expect(asVoiceEngineConnectionState('connecting')).toBe(VoiceEngineConnectionState.Connecting);
		expect(asVoiceEngineConnectionState('connected')).toBe(VoiceEngineConnectionState.Connected);
		expect(asVoiceEngineConnectionState('reconnecting')).toBe(VoiceEngineConnectionState.Reconnecting);
		expect(asVoiceEngineConnectionState('signalReconnecting')).toBe(VoiceEngineConnectionState.SignalReconnecting);
		expect(asVoiceEngineConnectionState('failed')).toBe(VoiceEngineConnectionState.Failed);
		expect(asVoiceEngineConnectionState('garbage')).toBe(VoiceEngineConnectionState.Disconnected);
		expect(asVoiceEngineConnectionState(undefined)).toBe(VoiceEngineConnectionState.Disconnected);
	});

	it('starts disconnected and tracks connection attempts', () => {
		let snapshot = createVoiceConnectionSnapshot();
		expect(getVoiceConnectionStateValue(snapshot)).toBe('disconnected');
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'connection.start',
			guildId: 'guild-1',
			channelId: 'channel-1',
		});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('connecting');
		expect(snapshot.context.connectionAttemptId).toBe(1);
		expect(snapshot.context.voiceServerTimeoutScheduled).toBe(true);
		expect(snapshot.context.connectionId).toBeNull();
	});

	it('marks recovered expectations without requiring an existing room', () => {
		let snapshot = createVoiceConnectionSnapshot();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'connection.recoverExpectation',
			guildId: null,
			channelId: 'dm-1',
		});
		expect(snapshot.context.expectedRecovery).toBe(true);
		expect(snapshot.context.guildId).toBeNull();
		expect(snapshot.context.channelId).toBe('dm-1');
		expect(snapshot.context.connecting).toBe(true);
	});

	it('accepts VOICE_SERVER_UPDATE for the latest attempt and ignores stale attempts', () => {
		let snapshot = createVoiceConnectionSnapshot();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'connection.start',
			guildId: 'guild-1',
			channelId: 'channel-1',
		});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'connection.start',
			guildId: 'guild-1',
			channelId: 'channel-2',
		});
		expect(isLatestVoiceConnectionAttempt(snapshot, 1)).toBe(false);
		const decision = selectVoiceConnectionServerUpdateDecision(
			snapshot,
			{
				guild_id: 'guild-1',
				channel_id: 'channel-2',
				connection_id: 'connection-1',
				endpoint: 'wss://voice.example',
				token: 'token',
			},
			1,
		);
		expect(decision).toMatchObject({type: 'ignore', reason: 'stale-attempt'});
	});

	it('ignores channel-move VOICE_SERVER_UPDATE while not connected', () => {
		let snapshot = createVoiceConnectionSnapshot();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'connection.start',
			guildId: 'guild-1',
			channelId: 'channel-1',
		});
		const decision = selectVoiceConnectionServerUpdateDecision(snapshot, {
			guild_id: 'guild-1',
			channel_id: 'channel-2',
			connection_id: 'connection-2',
			endpoint: 'wss://voice.example',
			token: 'token',
		});
		expect(decision).toMatchObject({type: 'ignore', reason: 'stale-channel-update'});
	});

	it('preserves connection id while accepting a connected server channel move', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.roomReady', room: roomA, attemptId: 1});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.connected'});
		const decision = selectVoiceConnectionServerUpdateDecision(snapshot, {
			guild_id: 'guild-1',
			channel_id: 'channel-2',
			connection_id: 'connection-2',
			endpoint: 'wss://voice-b.example',
			token: 'token-b',
		});
		expect(decision).toMatchObject({
			type: 'accept',
			isChannelMove: true,
			resolvedChannelId: 'channel-2',
		});
		if (decision.type !== 'accept') throw new Error('expected accepted decision');
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'voiceServer.accepted',
			guildId: decision.guildId,
			channelId: decision.resolvedChannelId,
			endpoint: decision.endpoint,
			connectionId: decision.connectionId,
			isChannelMove: decision.isChannelMove,
		});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('channelMove');
		expect(snapshot.context.connectionId).toBe('connection-2');
		expect(snapshot.context.channelId).toBe('channel-2');
	});

	it('detects same-channel endpoint changes as hot-swap candidates', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.roomReady', room: roomA, attemptId: 1});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.connected'});
		const decision = selectVoiceConnectionServerUpdateDecision(snapshot, {
			guild_id: 'guild-1',
			channel_id: 'channel-1',
			connection_id: 'connection-2',
			endpoint: 'wss://voice-b.example',
			token: 'token-b',
		});
		expect(decision).toMatchObject({type: 'accept', isRegionChange: true});
	});

	it('accepts roomReady after the connected event wins the LiveKit promise race', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.connected'});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('connected');
		expect(snapshot.context.room).toBeNull();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.roomReady', room: roomA, attemptId: 1});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('connected');
		expect(snapshot.context.room).toBe(roomA);
		expect(snapshot.context.connected).toBe(true);
		expect(snapshot.context.connecting).toBe(false);
	});

	it('applies only the matching voice server timeout and routes to failed', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'voiceServer.timeout',
			guildId: 'guild-1',
			channelId: 'other-channel',
		});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('connecting');
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'voiceServer.timeout',
			guildId: 'guild-1',
			channelId: 'channel-1',
		});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('failed');
		expect(snapshot.context.guildId).toBeNull();
		expect(snapshot.context.channelId).toBeNull();
		expect(snapshot.context.localDisconnectReason).toBe('error');
		expect(snapshot.context.failureReason).toBe('connectTimeout');
		expect(snapshot.context.failedGuildId).toBe('guild-1');
		expect(snapshot.context.failedChannelId).toBe('channel-1');
		expect(snapshot.context.expectedRecovery).toBe(false);
	});

	it('routes an error disconnect while connecting to failed with a transport reason', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.disconnected', reason: 'error'});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('failed');
		expect(snapshot.context.failureReason).toBe('transport');
		expect(snapshot.context.failedChannelId).toBe('channel-1');
		expect(snapshot.context.connectionId).toBe('connection-1');
	});

	it('routes a connection.failed event while connecting to failed with a transport reason', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.failed', reason: 'error'});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('failed');
		expect(snapshot.context.failureReason).toBe('transport');
	});

	it('stays failed when a follow-up error disconnect arrives but exits to disconnected on a user disconnect', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.failed', reason: 'error'});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('failed');
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.disconnected', reason: 'error'});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('failed');
		expect(snapshot.context.failureReason).toBe('transport');
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.disconnected', reason: 'user'});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('disconnected');
		expect(snapshot.context.failureReason).toBeNull();
	});

	it('retries from failed via connection.start and increments the attempt id', () => {
		let snapshot = startAndAccept();
		const attemptBeforeFailure = snapshot.context.connectionAttemptId;
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'voiceServer.timeout',
			guildId: 'guild-1',
			channelId: 'channel-1',
		});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('failed');
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'connection.start',
			guildId: 'guild-1',
			channelId: 'channel-1',
		});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('connecting');
		expect(snapshot.context.connectionAttemptId).toBe(attemptBeforeFailure + 1);
		expect(snapshot.context.failureReason).toBeNull();
		expect(snapshot.context.failedChannelId).toBeNull();
	});

	it('recovers from failed via connection.recover into connecting', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'voiceServer.timeout',
			guildId: 'guild-1',
			channelId: 'channel-1',
		});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('failed');
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'connection.recover',
			guildId: 'guild-1',
			channelId: 'channel-1',
		});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('connecting');
		expect(snapshot.context.guildId).toBe('guild-1');
		expect(snapshot.context.channelId).toBe('channel-1');
		expect(snapshot.context.failureReason).toBeNull();
	});

	it('dismisses a failed connection via a user disconnect into disconnected', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'voiceServer.timeout',
			guildId: 'guild-1',
			channelId: 'channel-1',
		});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('failed');
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.disconnected', reason: 'user'});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('disconnected');
		expect(snapshot.context.failureReason).toBeNull();
		expect(snapshot.context.failedChannelId).toBeNull();
	});

	it('resets a failed connection into disconnected', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'voiceServer.timeout',
			guildId: 'guild-1',
			channelId: 'channel-1',
		});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('failed');
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.reset'});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('disconnected');
		expect(snapshot.context.failureReason).toBeNull();
	});

	it('keeps server and error disconnect connection ids but clears user disconnect ids', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.disconnected', reason: 'server'});
		expect(snapshot.context.connectionId).toBe('connection-1');
		expect(snapshot.context.expectedRecovery).toBe(true);
		snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.disconnected', reason: 'error'});
		expect(snapshot.context.connectionId).toBe('connection-1');
		expect(snapshot.context.localDisconnectReason).toBe('error');
		snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.disconnected', reason: 'user'});
		expect(snapshot.context.connectionId).toBeNull();
		expect(snapshot.context.expectedRecovery).toBe(false);
	});

	it('tracks reconnecting and reconnected state', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.roomReady', room: roomA, attemptId: 1});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.connected'});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.reconnecting'});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('reconnecting');
		expect(snapshot.context.reconnecting).toBe(true);
		expect(snapshot.context.expectedRecovery).toBe(true);
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.reconnected'});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('connected');
		expect(snapshot.context.expectedRecovery).toBe(false);
	});

	it('preserves connection id when disconnecting for a user-initiated channel move', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.disconnectForChannelMove'});
		expect(getVoiceConnectionStateValue(snapshot)).toBe('channelMove');
		expect(snapshot.context.connectionId).toBe('connection-1');
		expect(snapshot.context.localDisconnectReason).toBe('channelMove');
	});

	it('tracks hot-swap queue, completion, abort, and reset bookkeeping', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.roomReady', room: roomA, attemptId: 1});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.connected'});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'hotSwap.start',
			pendingRoom: roomB,
			previousRoom: roomA,
		});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'hotSwap.queueOperation'});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'hotSwap.queueOperation'});
		expect(snapshot.context.hotSwap).toMatchObject({
			inProgress: true,
			pendingRoom: roomB,
			previousRoom: roomA,
			queuedOperationCount: 2,
		});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'hotSwap.complete',
			room: roomB,
			endpoint: 'wss://voice-b.example',
			connectionId: 'connection-2',
		});
		expect(snapshot.context.room).toBe(roomB);
		expect(snapshot.context.hotSwap.inProgress).toBe(false);
		expect(snapshot.context.hotSwap.queuedOperationCount).toBe(2);
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'hotSwap.drainQueue'});
		expect(snapshot.context.hotSwap.queuedOperationCount).toBe(0);
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'hotSwap.start',
			pendingRoom: roomA,
			previousRoom: roomB,
		});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'hotSwap.queueOperation'});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'hotSwap.abort'});
		expect(snapshot.context.hotSwap).toMatchObject({
			inProgress: false,
			abortRequested: true,
			queuedOperationCount: 0,
		});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'hotSwap.start',
			pendingRoom: roomA,
			previousRoom: roomB,
		});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'hotSwap.reset'});
		expect(snapshot.context.hotSwap.abortRequested).toBe(false);
	});

	it('restores the connected state when a hot-swap completes while the old room was reconnecting', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.roomReady', room: roomA, attemptId: 1});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.connected'});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'hotSwap.start',
			pendingRoom: roomB,
			previousRoom: roomA,
		});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.reconnecting'});
		expect(snapshot.context.reconnecting).toBe(true);
		expect(snapshot.context.connected).toBe(false);

		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'hotSwap.complete',
			room: roomB,
			endpoint: 'wss://voice-b.example',
			connectionId: 'connection-2',
		});

		expect(getVoiceConnectionStateValue(snapshot)).toBe('connected');
		expect(snapshot.context.connected).toBe(true);
		expect(snapshot.context.connecting).toBe(false);
		expect(snapshot.context.reconnecting).toBe(false);
		expect(snapshot.context.room).toBe(roomB);
		expect(snapshot.context.voiceServerEndpoint).toBe('wss://voice-b.example');
		expect(snapshot.context.hotSwap.inProgress).toBe(false);
	});

	it('ignores a hot-swap completion that arrives without an in-progress swap', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.roomReady', room: roomA, attemptId: 1});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.connected'});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.reconnecting'});

		const completed = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'hotSwap.complete',
			room: roomB,
			endpoint: 'wss://voice-b.example',
			connectionId: 'connection-2',
		});

		expect(getVoiceConnectionStateValue(completed)).toBe('reconnecting');
		expect(completed.context.connected).toBe(false);
		expect(completed.context.reconnecting).toBe(true);
		expect(completed.context.room).toBe(roomA);
	});

	it('marks a connected voice-server replacement as replaced instead of a user disconnect', () => {
		let snapshot = startAndAccept();
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.roomReady', room: roomA, attemptId: 1});
		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.connected'});
		expect(snapshot.context.localDisconnectReason).toBeNull();

		snapshot = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'voiceServer.accepted',
			guildId: 'guild-1',
			channelId: 'channel-1',
			endpoint: 'wss://voice-b.example',
			connectionId: 'connection-2',
			isChannelMove: false,
		});

		expect(snapshot.context.connected).toBe(false);
		expect(snapshot.context.localDisconnectReason).toBe('replaced');

		snapshot = transitionVoiceConnectionSnapshot(snapshot, {type: 'connection.connected'});
		expect(snapshot.context.localDisconnectReason).toBeNull();
	});

	it('keeps a null disconnect reason when a voice server is accepted while still connecting', () => {
		const snapshot = startAndAccept();
		expect(snapshot.context.connected).toBe(false);
		expect(snapshot.context.localDisconnectReason).toBeNull();

		const accepted = transitionVoiceConnectionSnapshot(snapshot, {
			type: 'voiceServer.accepted',
			guildId: 'guild-1',
			channelId: 'channel-1',
			endpoint: 'wss://voice-b.example',
			connectionId: 'connection-2',
			isChannelMove: false,
		});

		expect(accepted.context.localDisconnectReason).toBeNull();
	});
});
