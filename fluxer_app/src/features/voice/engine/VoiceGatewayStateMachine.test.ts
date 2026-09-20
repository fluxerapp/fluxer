// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import {
	createVoiceGatewayStateSnapshot,
	transitionVoiceGatewayStateSnapshot,
	type VoiceGatewayStateContext,
	type VoiceGatewayStateEvent,
	type VoiceGatewayStateSnapshot,
} from '@app/features/voice/engine/VoiceGatewayStateMachine';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/voice/state/ScreenShareDeliveryRollout', () => ({
	ScreenShareDeliveryRollout: {enabled: false},
	default: {enabled: false},
}));

function voiceState(overrides: Partial<VoiceState> = {}): VoiceState {
	return {
		guild_id: 'guild-1',
		channel_id: 'channel-1',
		user_id: 'user-1',
		connection_id: 'connection-1',
		mute: false,
		deaf: false,
		self_mute: false,
		self_deaf: false,
		self_video: false,
		self_stream: false,
		viewer_stream_keys: [],
		...overrides,
	};
}

function guild(id: string, voiceStates: ReadonlyArray<VoiceState> = []): GuildReadyData {
	return {
		id,
		properties: {} as GuildReadyData['properties'],
		channels: [],
		emojis: [],
		members: [],
		member_count: 0,
		voice_states: voiceStates,
		roles: [],
		joined_at: '2026-01-01T00:00:00.000Z',
	};
}

function transition(snapshot: VoiceGatewayStateSnapshot, event: VoiceGatewayStateEvent): VoiceGatewayStateSnapshot {
	return transitionVoiceGatewayStateSnapshot(snapshot, event);
}

function expectConnection(
	context: VoiceGatewayStateContext,
	connectionId: string,
	expected: Pick<VoiceState, 'guild_id' | 'channel_id' | 'user_id'>,
): void {
	const byConnection = context.connectionVoiceStates[connectionId];
	expect(byConnection).toMatchObject(expected);
	expect(context.voiceStates[expected.guild_id]?.[expected.channel_id ?? '']?.[connectionId]).toBe(byConnection);
	expect(context.userVoiceStates[expected.user_id]?.[expected.guild_id]?.[connectionId]).toBe(byConnection);
}

function expectNoGuild(context: VoiceGatewayStateContext, guildId: string): void {
	expect(context.voiceStates[guildId]).toBeUndefined();
	for (const userStates of Object.values(context.userVoiceStates)) {
		expect(userStates[guildId]).toBeUndefined();
	}
	for (const voiceStateValue of Object.values(context.connectionVoiceStates)) {
		expect(voiceStateValue.guild_id).not.toBe(guildId);
	}
}

function expectProjectionConsistent(context: VoiceGatewayStateContext): void {
	const seenConnectionIds = new Set<string>();
	for (const [guildId, guildStates] of Object.entries(context.voiceStates)) {
		for (const [channelId, channelStates] of Object.entries(guildStates)) {
			for (const [connectionId, state] of Object.entries(channelStates)) {
				expect(seenConnectionIds.has(connectionId)).toBe(false);
				seenConnectionIds.add(connectionId);
				expect(state.guild_id).toBe(guildId);
				expect(state.channel_id).toBe(channelId);
				expect(state.connection_id).toBe(connectionId);
				expect(context.connectionVoiceStates[connectionId]).toBe(state);
				expect(context.userVoiceStates[state.user_id]?.[guildId]?.[connectionId]).toBe(state);
			}
		}
	}

	for (const [connectionId, state] of Object.entries(context.connectionVoiceStates)) {
		expect(state.connection_id).toBe(connectionId);
		expect(state.channel_id).toBeTruthy();
		expect(context.voiceStates[state.guild_id]?.[state.channel_id ?? '']?.[connectionId]).toBe(state);
		expect(context.userVoiceStates[state.user_id]?.[state.guild_id]?.[connectionId]).toBe(state);
	}

	for (const [userId, userStates] of Object.entries(context.userVoiceStates)) {
		for (const [guildId, guildStates] of Object.entries(userStates)) {
			expect(Object.keys(guildStates).length).toBeGreaterThan(0);
			for (const [connectionId, state] of Object.entries(guildStates)) {
				expect(state.user_id).toBe(userId);
				expect(state.guild_id).toBe(guildId);
				expect(state.connection_id).toBe(connectionId);
				expect(context.connectionVoiceStates[connectionId]).toBe(state);
				expect(context.voiceStates[guildId]?.[state.channel_id ?? '']?.[connectionId]).toBe(state);
			}
		}
	}
}

function createRandom(seed: number): () => number {
	let state = seed;
	return () => {
		state = (state * 1664525 + 1013904223) >>> 0;
		return state / 0x100000000;
	};
}

describe('VoiceGatewayStateMachine', () => {
	it('moves a connection across channels and guilds without leaving stale indexes', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: voiceState({guild_id: 'ignored', channel_id: 'channel-1', connection_id: 'connection-1'}),
		});
		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: voiceState({channel_id: 'channel-2', connection_id: 'connection-1'}),
		});
		expect(snapshot.context.voiceStates['guild-1']?.['channel-1']).toBeUndefined();
		expectConnection(snapshot.context, 'connection-1', {
			guild_id: 'guild-1',
			channel_id: 'channel-2',
			user_id: 'user-1',
		});

		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-2',
			voiceState: voiceState({channel_id: 'channel-3', connection_id: 'connection-1'}),
		});
		expectNoGuild(snapshot.context, 'guild-1');
		expectConnection(snapshot.context, 'connection-1', {
			guild_id: 'guild-2',
			channel_id: 'channel-3',
			user_id: 'user-1',
		});
		expectProjectionConsistent(snapshot.context);
	});

	it('tracks multiple connections for one user and removes only the leaving connection', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: voiceState({connection_id: 'connection-1', channel_id: 'channel-1'}),
		});
		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: voiceState({connection_id: 'connection-2', channel_id: 'channel-2'}),
		});

		expect(Object.keys(snapshot.context.userVoiceStates['user-1']?.['guild-1'] ?? {})).toEqual([
			'connection-1',
			'connection-2',
		]);

		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: voiceState({connection_id: 'connection-1', channel_id: null}),
		});
		expect(snapshot.context.connectionVoiceStates['connection-1']).toBeUndefined();
		expectConnection(snapshot.context, 'connection-2', {
			guild_id: 'guild-1',
			channel_id: 'channel-2',
			user_id: 'user-1',
		});
		expectProjectionConsistent(snapshot.context);
	});

	it('replaces stale same-session connections for one user without removing other sessions', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: voiceState({connection_id: 'connection-old', session_id: 'session-a', channel_id: 'channel-1'}),
		});
		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: voiceState({connection_id: 'connection-other', session_id: 'session-b', channel_id: 'channel-1'}),
		});
		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: voiceState({connection_id: 'connection-new', session_id: 'session-a', channel_id: 'channel-1'}),
		});

		expect(snapshot.context.connectionVoiceStates['connection-old']).toBeUndefined();
		expectConnection(snapshot.context, 'connection-new', {
			guild_id: 'guild-1',
			channel_id: 'channel-1',
			user_id: 'user-1',
		});
		expectConnection(snapshot.context, 'connection-other', {
			guild_id: 'guild-1',
			channel_id: 'channel-1',
			user_id: 'user-1',
		});
		expectProjectionConsistent(snapshot.context);
	});

	it('deletes all user connections in a guild', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		snapshot = transition(snapshot, {
			type: 'connection.open',
			guilds: [
				guild('guild-1', [
					voiceState({connection_id: 'connection-1', user_id: 'user-1', channel_id: 'channel-1'}),
					voiceState({connection_id: 'connection-2', user_id: 'user-1', channel_id: 'channel-2'}),
					voiceState({connection_id: 'connection-3', user_id: 'user-2', channel_id: 'channel-2'}),
				]),
			],
		});
		snapshot = transition(snapshot, {type: 'voiceState.delete', guildId: 'guild-1', userId: 'user-1'});
		expect(snapshot.context.connectionVoiceStates['connection-1']).toBeUndefined();
		expect(snapshot.context.connectionVoiceStates['connection-2']).toBeUndefined();
		expectConnection(snapshot.context, 'connection-3', {
			guild_id: 'guild-1',
			channel_id: 'channel-2',
			user_id: 'user-2',
		});
		expectProjectionConsistent(snapshot.context);
	});

	it('ignores updates and snapshots with missing connection IDs', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: {...voiceState(), connection_id: undefined} as unknown as VoiceState,
		});
		snapshot = transition(snapshot, {
			type: 'connection.open',
			guilds: [guild('guild-1', [{...voiceState(), connection_id: ''}])],
		});
		expect(snapshot.context).toEqual({
			voiceStates: {},
			userVoiceStates: {},
			connectionVoiceStates: {},
		});
	});

	it('normalizes missing viewer stream keys on stored voice states', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: voiceState({
				connection_id: 'connection-1',
				viewer_stream_keys: undefined,
			}),
		});
		expect(snapshot.context.connectionVoiceStates['connection-1']?.viewer_stream_keys).toEqual([]);

		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: voiceState({
				connection_id: 'connection-1',
				viewer_stream_keys: ['stream-a', '', 'stream-a', 'stream-b'],
			}),
		});
		expect(snapshot.context.connectionVoiceStates['connection-1']?.viewer_stream_keys).toEqual([
			'stream-a',
			'stream-b',
		]);
		expect(snapshot.context.voiceStates['guild-1']?.['channel-1']?.['connection-1']?.viewer_stream_keys).toEqual([
			'stream-a',
			'stream-b',
		]);
		expectProjectionConsistent(snapshot.context);
	});

	it('replaces stale guild-create state and removes duplicate connections from old guilds', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		snapshot = transition(snapshot, {
			type: 'connection.open',
			guilds: [
				guild('guild-1', [voiceState({connection_id: 'old-connection', channel_id: 'old-channel'})]),
				guild('guild-2', [voiceState({connection_id: 'shared-connection', channel_id: 'other-channel'})]),
			],
		});

		snapshot = transition(snapshot, {
			type: 'guild.create',
			guild: guild('guild-1', [
				voiceState({connection_id: 'new-connection', channel_id: 'new-channel'}),
				voiceState({connection_id: 'shared-connection', channel_id: 'new-channel'}),
			]),
		});

		expect(snapshot.context.connectionVoiceStates['old-connection']).toBeUndefined();
		expect(snapshot.context.voiceStates['guild-1']?.['old-channel']).toBeUndefined();
		expect(snapshot.context.voiceStates['guild-2']?.['other-channel']?.['shared-connection']).toBeUndefined();
		expectConnection(snapshot.context, 'new-connection', {
			guild_id: 'guild-1',
			channel_id: 'new-channel',
			user_id: 'user-1',
		});
		expectConnection(snapshot.context, 'shared-connection', {
			guild_id: 'guild-1',
			channel_id: 'new-channel',
			user_id: 'user-1',
		});
		expectProjectionConsistent(snapshot.context);
	});

	it('purges all indexes for guild delete', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		snapshot = transition(snapshot, {
			type: 'connection.open',
			guilds: [
				guild('guild-1', [voiceState({connection_id: 'connection-1', channel_id: 'channel-1'})]),
				guild('guild-2', [
					voiceState({connection_id: 'connection-2', user_id: 'user-1', channel_id: 'channel-1'}),
					voiceState({connection_id: 'connection-3', user_id: 'user-2', channel_id: 'channel-2'}),
				]),
			],
		});
		snapshot = transition(snapshot, {type: 'guild.delete', guildId: 'guild-2'});
		expectNoGuild(snapshot.context, 'guild-2');
		expectConnection(snapshot.context, 'connection-1', {
			guild_id: 'guild-1',
			channel_id: 'channel-1',
			user_id: 'user-1',
		});
		expectProjectionConsistent(snapshot.context);
	});

	it('applies duplicate updates idempotently', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		const update: VoiceGatewayStateEvent = {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: voiceState({connection_id: 'connection-1', channel_id: 'channel-1'}),
		};
		snapshot = transition(snapshot, update);
		const once = snapshot.context;
		snapshot = transition(snapshot, update);
		expect(snapshot.context).toEqual(once);
		expectProjectionConsistent(snapshot.context);
	});

	it('removes one connection from all indexes without waiting for a full gateway refresh', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		snapshot = transition(snapshot, {
			type: 'connection.open',
			guilds: [
				guild('guild-1', [
					voiceState({connection_id: 'stale-connection', channel_id: 'channel-1'}),
					voiceState({connection_id: 'active-connection', channel_id: 'channel-1'}),
				]),
			],
		});
		snapshot = transition(snapshot, {type: 'voiceState.removeConnection', connectionId: 'stale-connection'});

		expect(snapshot.context.connectionVoiceStates['stale-connection']).toBeUndefined();
		expect(snapshot.context.voiceStates['guild-1']?.['channel-1']?.['stale-connection']).toBeUndefined();
		expect(snapshot.context.userVoiceStates['user-1']?.['guild-1']?.['stale-connection']).toBeUndefined();
		expectConnection(snapshot.context, 'active-connection', {
			guild_id: 'guild-1',
			channel_id: 'channel-1',
			user_id: 'user-1',
		});
		expectProjectionConsistent(snapshot.context);
	});

	it('connection-open snapshots replace all prior state', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		snapshot = transition(snapshot, {
			type: 'voiceState.update',
			guildId: 'guild-1',
			voiceState: voiceState({connection_id: 'stale-connection'}),
		});
		snapshot = transition(snapshot, {
			type: 'connection.open',
			guilds: [
				guild('guild-2', [
					voiceState({guild_id: 'ignored', user_id: 'user-2', connection_id: 'connection-2', channel_id: 'channel-2'}),
					voiceState({user_id: 'user-3', connection_id: 'connection-3', channel_id: null}),
				]),
			],
		});
		expect(snapshot.context.connectionVoiceStates['stale-connection']).toBeUndefined();
		expect(snapshot.context.connectionVoiceStates['connection-3']).toBeUndefined();
		expectConnection(snapshot.context, 'connection-2', {
			guild_id: 'guild-2',
			channel_id: 'channel-2',
			user_id: 'user-2',
		});
		expectProjectionConsistent(snapshot.context);
	});

	it('clears all projection indexes', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		snapshot = transition(snapshot, {
			type: 'connection.open',
			guilds: [guild('guild-1', [voiceState({connection_id: 'connection-1', channel_id: 'channel-1'})])],
		});
		snapshot = transition(snapshot, {type: 'clear.all'});
		expect(snapshot.context).toEqual({
			voiceStates: {},
			userVoiceStates: {},
			connectionVoiceStates: {},
		});
	});

	it('keeps projection indexes consistent after long randomized sequences', () => {
		let snapshot = createVoiceGatewayStateSnapshot();
		const random = createRandom(0x5eed);
		const guildIds = ['guild-1', 'guild-2', 'guild-3'];
		const channelIds = ['channel-1', 'channel-2', 'channel-3', 'channel-4'];
		const userIds = ['user-1', 'user-2', 'user-3', 'user-4'];
		const connectionIds = ['connection-1', 'connection-2', 'connection-3', 'connection-4', 'connection-5'];
		const pick = <T>(values: Array<T>): T => values[Math.floor(random() * values.length)];

		for (let i = 0; i < 500; i += 1) {
			const op = Math.floor(random() * 6);
			if (op === 0) {
				const guildId = pick(guildIds);
				snapshot = transition(snapshot, {
					type: 'voiceState.update',
					guildId,
					voiceState: voiceState({
						guild_id: 'payload-guild',
						user_id: pick(userIds),
						connection_id: pick(connectionIds),
						channel_id: pick(channelIds),
						self_mute: random() > 0.5,
						self_deaf: random() > 0.5,
					}),
				});
			} else if (op === 1) {
				snapshot = transition(snapshot, {
					type: 'voiceState.update',
					guildId: pick(guildIds),
					voiceState: voiceState({
						user_id: pick(userIds),
						connection_id: pick(connectionIds),
						channel_id: null,
					}),
				});
			} else if (op === 2) {
				snapshot = transition(snapshot, {
					type: 'voiceState.delete',
					guildId: pick(guildIds),
					userId: pick(userIds),
				});
			} else if (op === 3) {
				snapshot = transition(snapshot, {type: 'guild.delete', guildId: pick(guildIds)});
			} else if (op === 4) {
				const guildId = pick(guildIds);
				const count = Math.floor(random() * 4);
				const voiceStates = Array.from({length: count}, () =>
					voiceState({
						guild_id: 'payload-guild',
						user_id: pick(userIds),
						connection_id: pick(connectionIds),
						channel_id: random() > 0.2 ? pick(channelIds) : null,
					}),
				);
				snapshot = transition(snapshot, {type: 'guild.create', guild: guild(guildId, voiceStates)});
			} else {
				const readyGuilds = guildIds.map((guildId) => {
					const count = Math.floor(random() * 3);
					return guild(
						guildId,
						Array.from({length: count}, () =>
							voiceState({
								guild_id: 'payload-guild',
								user_id: pick(userIds),
								connection_id: pick(connectionIds),
								channel_id: random() > 0.2 ? pick(channelIds) : null,
							}),
						),
					);
				});
				snapshot = transition(snapshot, {type: 'connection.open', guilds: readyGuilds});
			}

			expectProjectionConsistent(snapshot.context);
		}
	});
});
