// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVoiceConnectionSnapshot,
	transitionVoiceConnectionSnapshot,
	type VoiceConnectionSnapshot,
} from '@app/features/voice/engine/VoiceConnectionStateMachine';
import {
	getRoomFromMediaEngine,
	getVoiceConnectionContextFromMediaEngine,
} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {afterEach, describe, expect, it} from 'vitest';

type MediaEngineWindow = typeof globalThis & {_mediaEngine?: unknown};

function connectedWithoutRoomSnapshot(): VoiceConnectionSnapshot {
	const started = transitionVoiceConnectionSnapshot(createVoiceConnectionSnapshot(), {
		type: 'connection.start',
		guildId: 'guild-1',
		channelId: 'channel-1',
	});
	return transitionVoiceConnectionSnapshot(started, {type: 'connection.connected'});
}

function seedMediaEngineFromSnapshot(snapshot: VoiceConnectionSnapshot, overrides: Record<string, unknown> = {}): void {
	const {context} = snapshot;
	(window as MediaEngineWindow)._mediaEngine = {
		guildId: context.guildId,
		channelId: context.channelId,
		connectionId: context.connectionId,
		connected: context.connected,
		connecting: context.connecting,
		reconnecting: context.reconnecting,
		room: context.room ?? undefined,
		...overrides,
	};
}

describe('voice connection projection while connected without a room', () => {
	afterEach(() => {
		delete (window as MediaEngineWindow)._mediaEngine;
	});

	it('models a connected connection whose LiveKit room is still null', () => {
		const {context} = connectedWithoutRoomSnapshot();

		expect(context.connected).toBe(true);
		expect(context.room).toBeNull();
	});

	it('projects the LiveKit room when the store reports connected without a room', () => {
		seedMediaEngineFromSnapshot(connectedWithoutRoomSnapshot());

		expect(getVoiceConnectionContextFromMediaEngine()?.connected).toBe(true);
		expect(getRoomFromMediaEngine()).toBeNull();
	});

	it('projects the LiveKit room when only the v2 model reports connected without a room', () => {
		seedMediaEngineFromSnapshot(connectedWithoutRoomSnapshot(), {
			connected: undefined,
			connecting: undefined,
			reconnecting: undefined,
			room: undefined,
			voiceEngineV2Model: {
				connection: {
					connected: true,
					connecting: false,
					reconnecting: false,
					status: 'connected',
					gateway: {selfVoiceState: {guildId: 'guild-1', channelId: 'channel-1'}},
				},
			},
		});

		expect(getVoiceConnectionContextFromMediaEngine()?.connected).toBe(true);
		expect(getRoomFromMediaEngine()).toBeNull();
	});
});
