// SPDX-License-Identifier: AGPL-3.0-or-later

import {transitionVoiceEngineV2} from '@fluxer/voice_engine_v2/src/core/reducer';
import {
	deriveVoiceEngineV2DesiredGatewayVoiceState,
	gatewayVoiceStateWriteMatchesReported,
	reconcileGatewayVoiceState,
	shouldApplyGatewayVoiceStateEcho,
} from '@fluxer/voice_engine_v2/src/core/reducers/gateway';
import type {VoiceEngineV2GatewayState, VoiceEngineV2Snapshot} from '@fluxer/voice_engine_v2/src/core/state';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {
	VoiceEngineV2GatewayDesiredVoiceState,
	VoiceEngineV2GatewayVoiceState,
} from '@fluxer/voice_engine_v2/src/protocol/types';
import {describe, expect, it} from 'vitest';

function reported(overrides: Partial<VoiceEngineV2GatewayVoiceState> = {}): VoiceEngineV2GatewayVoiceState {
	return {
		guildId: 'guild-1',
		channelId: 'channel-1',
		userId: 'user-1',
		sessionId: 'session-1',
		selfMute: true,
		selfDeaf: false,
		selfVideo: false,
		selfStream: false,
		suppress: false,
		requestToSpeakTimestamp: null,
		...overrides,
	};
}

function desired(
	overrides: Partial<VoiceEngineV2GatewayDesiredVoiceState> = {},
): VoiceEngineV2GatewayDesiredVoiceState {
	return {
		guildId: 'guild-1',
		channelId: 'channel-1',
		selfMute: false,
		selfDeaf: false,
		selfVideo: false,
		selfStream: false,
		...overrides,
	};
}

function connectedSnapshot(gatewayOverrides: Partial<VoiceEngineV2GatewayState> = {}): VoiceEngineV2Snapshot {
	const base = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
	return {
		...base,
		connection: {...base.connection, status: 'connected'},
		gateway: {...base.gateway, selfVoiceState: reported(), ...gatewayOverrides},
	};
}

describe('reconcileGatewayVoiceState', () => {
	it('emits a write when desired self-mute differs from reported (unmute drives convergence)', () => {
		const snapshot = connectedSnapshot({
			selfVoiceState: reported({selfMute: true}),
			desiredVoiceState: desired({selfMute: false}),
		});
		const result = reconcileGatewayVoiceState(snapshot);
		expect(result.commands).toHaveLength(1);
		expect(result.commands[0]).toMatchObject({type: 'gateway.voiceState.write', options: {selfMute: false}});
		expect(result.snapshot.gateway.desiredVoiceStateWrite).toMatchObject({selfMute: false});
		expect(result.snapshot.gateway.operationId).not.toBeNull();
	});

	it('emits no write when there is no desired intent yet', () => {
		const snapshot = connectedSnapshot({selfVoiceState: reported({selfMute: true}), desiredVoiceState: null});
		expect(reconcileGatewayVoiceState(snapshot).commands).toEqual([]);
	});

	it('emits no write when desired already equals reported (converged)', () => {
		const snapshot = connectedSnapshot({
			selfVoiceState: reported({selfMute: false}),
			desiredVoiceState: desired({selfMute: false}),
		});
		expect(reconcileGatewayVoiceState(snapshot).commands).toEqual([]);
	});

	it('emits no write while a write is already in flight (operationId set)', () => {
		const snapshot = connectedSnapshot({
			selfVoiceState: reported({selfMute: true}),
			desiredVoiceState: desired({selfMute: false}),
			operationId: 7,
		});
		expect(reconcileGatewayVoiceState(snapshot).commands).toEqual([]);
	});

	it('emits no write when the same value is already pending an echo (no optimistic re-send, S2)', () => {
		const snapshot = connectedSnapshot({
			selfVoiceState: reported({selfMute: true}),
			desiredVoiceState: desired({selfMute: false}),
			desiredVoiceStateWrite: {
				guildId: 'guild-1',
				channelId: 'channel-1',
				selfMute: false,
				selfDeaf: false,
				selfVideo: false,
				selfStream: false,
			},
			operationId: null,
		});
		expect(reconcileGatewayVoiceState(snapshot).commands).toEqual([]);
	});

	it('re-emits when desired changed while a different value was pending (no lost update)', () => {
		const snapshot = connectedSnapshot({
			selfVoiceState: reported({selfMute: false}),
			desiredVoiceState: desired({selfMute: true}),
			desiredVoiceStateWrite: {
				guildId: 'guild-1',
				channelId: 'channel-1',
				selfMute: false,
				selfDeaf: false,
				selfVideo: false,
				selfStream: false,
			},
			operationId: null,
		});
		const result = reconcileGatewayVoiceState(snapshot);
		expect(result.commands).toHaveLength(1);
		expect(result.commands[0]).toMatchObject({type: 'gateway.voiceState.write', options: {selfMute: true}});
	});

	it('emits no write when disconnected, but the recorded intent re-asserts on connect', () => {
		const base = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
		const disconnected: VoiceEngineV2Snapshot = {
			...base,
			connection: {...base.connection, status: 'idle'},
			gateway: {
				...base.gateway,
				selfVoiceState: reported({selfMute: true}),
				desiredVoiceState: desired({selfMute: false}),
			},
		};
		expect(reconcileGatewayVoiceState(disconnected).commands).toEqual([]);
	});

	it('emits a write to establish sync when the server state is not yet known', () => {
		const snapshot = connectedSnapshot({selfVoiceState: null, desiredVoiceState: desired({selfMute: false})});
		expect(deriveVoiceEngineV2DesiredGatewayVoiceState(snapshot)).toMatchObject({
			channelId: 'channel-1',
			selfMute: false,
		});
		const result = reconcileGatewayVoiceState(snapshot);
		expect(result.commands).toHaveLength(1);
		expect(result.commands[0]).toMatchObject({type: 'gateway.voiceState.write', options: {selfMute: false}});
	});

	it('emits no write when the desired channel is null (not in a channel)', () => {
		const snapshot = connectedSnapshot({selfVoiceState: null, desiredVoiceState: desired({channelId: null})});
		expect(deriveVoiceEngineV2DesiredGatewayVoiceState(snapshot)).toBeNull();
		expect(reconcileGatewayVoiceState(snapshot).commands).toEqual([]);
	});

	it('clears the pending write when the server echoes the desired value (convergence)', () => {
		const snapshot = connectedSnapshot({
			selfVoiceState: reported({selfMute: true}),
			desiredVoiceState: desired({selfMute: false}),
			desiredVoiceStateWrite: {
				guildId: 'guild-1',
				channelId: 'channel-1',
				selfMute: false,
				selfDeaf: false,
				selfVideo: false,
				selfStream: false,
			},
		});
		const echoed = transitionVoiceEngineV2(snapshot, {
			type: 'gateway.voiceStateUpdated',
			voiceState: reported({selfMute: false}),
		});
		expect(echoed.snapshot.gateway.selfVoiceState).toMatchObject({selfMute: false});
		expect(echoed.snapshot.gateway.desiredVoiceStateWrite).toBeNull();
	});

	it('keeps the pending write when the echo does not yet match the desired value', () => {
		const snapshot = connectedSnapshot({
			selfVoiceState: reported({selfMute: true}),
			desiredVoiceState: desired({selfMute: false}),
			desiredVoiceStateWrite: {
				guildId: 'guild-1',
				channelId: 'channel-1',
				selfMute: false,
				selfDeaf: false,
				selfVideo: false,
				selfStream: false,
			},
		});
		const echoed = transitionVoiceEngineV2(snapshot, {
			type: 'gateway.voiceStateUpdated',
			voiceState: reported({selfMute: true}),
		});
		expect(echoed.snapshot.gateway.desiredVoiceStateWrite).toMatchObject({selfMute: false});
	});

	it('converges across the full unmute lifecycle: desired change, write, success, echo (L1)', () => {
		let snapshot = connectedSnapshot({selfVoiceState: reported({selfMute: true})});

		const first = transitionVoiceEngineV2(snapshot, {
			type: 'gateway.desiredVoiceStateChanged',
			desired: desired({selfMute: false}),
		});
		expect(first.commands).toHaveLength(1);
		expect(first.commands[0]).toMatchObject({type: 'gateway.voiceState.write', options: {selfMute: false}});
		snapshot = first.snapshot;
		const operationId = snapshot.gateway.operationId;
		expect(operationId).not.toBeNull();

		snapshot = transitionVoiceEngineV2(snapshot, {type: 'gateway.voiceStateReconcileRequested'}).snapshot;
		expect(snapshot.gateway.operationId).toBe(operationId);

		snapshot = transitionVoiceEngineV2(snapshot, {
			type: 'gateway.voiceStateWriteSucceeded',
			operationId: operationId as number,
		}).snapshot;
		expect(snapshot.gateway.operationId).toBeNull();

		snapshot = transitionVoiceEngineV2(snapshot, {
			type: 'gateway.voiceStateUpdated',
			voiceState: reported({selfMute: false}),
		}).snapshot;

		expect(
			gatewayVoiceStateWriteMatchesReported(
				deriveVoiceEngineV2DesiredGatewayVoiceState(snapshot)!,
				snapshot.gateway.selfVoiceState!,
			),
		).toBe(true);
		expect(transitionVoiceEngineV2(snapshot, {type: 'gateway.voiceStateReconcileRequested'}).commands).toEqual([]);
	});

	it('retries after a failed write (clears pending so the next reconcile re-emits, L2)', () => {
		let snapshot = connectedSnapshot({
			selfVoiceState: reported({selfMute: true}),
			desiredVoiceState: desired({selfMute: false}),
		});
		const first = transitionVoiceEngineV2(snapshot, {type: 'gateway.voiceStateReconcileRequested'});
		snapshot = first.snapshot;
		const operationId = snapshot.gateway.operationId as number;
		snapshot = transitionVoiceEngineV2(snapshot, {
			type: 'gateway.voiceStateWriteFailed',
			operationId,
			error: {code: 'implementationError', message: 'boom'},
		}).snapshot;
		expect(snapshot.gateway.desiredVoiceStateWrite).toBeNull();
		expect(snapshot.gateway.operationId).toBeNull();
		const retry = transitionVoiceEngineV2(snapshot, {type: 'gateway.voiceStateReconcileRequested'});
		expect(retry.commands).toHaveLength(1);
		expect(retry.commands[0]).toMatchObject({type: 'gateway.voiceState.write', options: {selfMute: false}});
	});
});

describe('shouldApplyGatewayVoiceStateEcho', () => {
	it('applies the echo when there is no pending local write', () => {
		const snapshot = connectedSnapshot({desiredVoiceStateWrite: null});
		expect(shouldApplyGatewayVoiceStateEcho(snapshot, reported({selfMute: true}))).toBe(true);
	});

	it('applies the echo when it matches the pending local write', () => {
		const snapshot = connectedSnapshot({
			desiredVoiceStateWrite: {
				guildId: 'guild-1',
				channelId: 'channel-1',
				selfMute: false,
				selfDeaf: false,
				selfVideo: false,
				selfStream: false,
			},
		});
		expect(shouldApplyGatewayVoiceStateEcho(snapshot, reported({selfMute: false}))).toBe(true);
	});

	it('rejects a stale echo that differs from the pending local write', () => {
		const snapshot = connectedSnapshot({
			desiredVoiceStateWrite: {
				guildId: 'guild-1',
				channelId: 'channel-1',
				selfMute: false,
				selfDeaf: false,
				selfVideo: false,
				selfStream: false,
			},
		});
		expect(shouldApplyGatewayVoiceStateEcho(snapshot, reported({selfMute: true}))).toBe(false);
	});
});
