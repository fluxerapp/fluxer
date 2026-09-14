// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import {normalizeVoiceMediaGraphViewerStreamKeys} from '@app/features/voice/engine/VoiceMediaGraph';
import {ME} from '@fluxer/constants/src/AppConstants';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type NormalizedVoiceState = Omit<
	VoiceState,
	'guild_id' | 'channel_id' | 'connection_id' | 'viewer_stream_keys'
> & {
	guild_id: string;
	channel_id: string;
	connection_id: string;
	viewer_stream_keys: ReadonlyArray<string>;
};

export type VoiceGatewayVoiceStates = Record<string, Record<string, Record<string, NormalizedVoiceState>>>;
export type VoiceGatewayUserVoiceStates = Record<string, Record<string, Record<string, NormalizedVoiceState>>>;
export type VoiceGatewayConnectionVoiceStates = Record<string, NormalizedVoiceState>;

export interface VoiceGatewayStateContext {
	voiceStates: VoiceGatewayVoiceStates;
	userVoiceStates: VoiceGatewayUserVoiceStates;
	connectionVoiceStates: VoiceGatewayConnectionVoiceStates;
}

type VoiceStateWithIndexedConnection = VoiceState & {
	channel_id: string;
	connection_id: string;
};

export type VoiceGatewayStateEvent =
	| {type: 'voiceState.update'; guildId: string | null; voiceState: VoiceState}
	| {type: 'voiceState.delete'; guildId: string; userId: string}
	| {type: 'voiceState.removeConnection'; connectionId: string}
	| {type: 'connection.open'; guilds: ReadonlyArray<GuildReadyData>}
	| {type: 'guild.create'; guild: GuildReadyData}
	| {type: 'guild.delete'; guildId: string}
	| {type: 'clear.all'};

export function createEmptyVoiceGatewayStateContext(): VoiceGatewayStateContext {
	return {
		voiceStates: {},
		userVoiceStates: {},
		connectionVoiceStates: {},
	};
}

function resolveGuildId(guildId: string | null): string {
	return guildId ?? ME;
}

function hasIndexedVoiceConnection(voiceState: VoiceState): voiceState is VoiceStateWithIndexedConnection {
	return Boolean(voiceState.channel_id && voiceState.connection_id);
}

function normalizeVoiceStateForGatewayIndex(
	guildId: string,
	voiceState: VoiceStateWithIndexedConnection,
): NormalizedVoiceState {
	return {
		...voiceState,
		guild_id: guildId,
		viewer_stream_keys: normalizeVoiceMediaGraphViewerStreamKeys(voiceState.viewer_stream_keys),
	};
}

function removeConnectionFromVoiceStates(
	voiceStates: VoiceGatewayVoiceStates,
	guildId: string | null | undefined,
	channelId: string | null | undefined,
	connectionId: string,
): VoiceGatewayVoiceStates {
	if (!guildId || !channelId) return voiceStates;
	const guildStates = voiceStates[guildId];
	const channelStates = guildStates?.[channelId];
	if (!channelStates || !Object.hasOwn(channelStates, connectionId)) return voiceStates;

	const nextVoiceStates = {...voiceStates};
	const nextGuildStates = {...guildStates};
	const nextChannelStates = {...channelStates};
	delete nextChannelStates[connectionId];
	if (Object.keys(nextChannelStates).length === 0) {
		delete nextGuildStates[channelId];
	} else {
		nextGuildStates[channelId] = nextChannelStates;
	}
	if (Object.keys(nextGuildStates).length === 0) {
		delete nextVoiceStates[guildId];
	} else {
		nextVoiceStates[guildId] = nextGuildStates;
	}
	return nextVoiceStates;
}

function removeConnectionFromUserVoiceStates(
	userVoiceStates: VoiceGatewayUserVoiceStates,
	userId: string | null | undefined,
	guildId: string | null | undefined,
	connectionId: string,
): VoiceGatewayUserVoiceStates {
	if (!userId || !guildId) return userVoiceStates;
	const userStates = userVoiceStates[userId];
	const guildStates = userStates?.[guildId];
	if (!guildStates || !Object.hasOwn(guildStates, connectionId)) return userVoiceStates;

	const nextUserVoiceStates = {...userVoiceStates};
	const nextUserStates = {...userStates};
	const nextGuildStates = {...guildStates};
	delete nextGuildStates[connectionId];
	if (Object.keys(nextGuildStates).length === 0) {
		delete nextUserStates[guildId];
	} else {
		nextUserStates[guildId] = nextGuildStates;
	}
	if (Object.keys(nextUserStates).length === 0) {
		delete nextUserVoiceStates[userId];
	} else {
		nextUserVoiceStates[userId] = nextUserStates;
	}
	return nextUserVoiceStates;
}

function removeConnection(context: VoiceGatewayStateContext, connectionId: string): VoiceGatewayStateContext {
	const existing = context.connectionVoiceStates[connectionId];
	if (!existing) return context;

	const nextConnectionVoiceStates = {...context.connectionVoiceStates};
	delete nextConnectionVoiceStates[connectionId];
	return {
		voiceStates: removeConnectionFromVoiceStates(
			context.voiceStates,
			existing.guild_id,
			existing.channel_id,
			connectionId,
		),
		userVoiceStates: removeConnectionFromUserVoiceStates(
			context.userVoiceStates,
			existing.user_id,
			existing.guild_id,
			connectionId,
		),
		connectionVoiceStates: nextConnectionVoiceStates,
	};
}

function addConnection(
	context: VoiceGatewayStateContext,
	guildId: string,
	voiceState: VoiceState,
): VoiceGatewayStateContext {
	if (!hasIndexedVoiceConnection(voiceState)) return context;

	const channelId = voiceState.channel_id;
	const connectionId = voiceState.connection_id;
	const indexedVoiceState = normalizeVoiceStateForGatewayIndex(guildId, voiceState);
	const guildStates = context.voiceStates[guildId] ?? {};
	const channelStates = guildStates[channelId] ?? {};
	const userStates = context.userVoiceStates[indexedVoiceState.user_id] ?? {};
	const userGuildStates = userStates[guildId] ?? {};
	return {
		voiceStates: {
			...context.voiceStates,
			[guildId]: {
				...guildStates,
				[channelId]: {
					...channelStates,
					[connectionId]: indexedVoiceState,
				},
			},
		},
		userVoiceStates: {
			...context.userVoiceStates,
			[indexedVoiceState.user_id]: {
				...userStates,
				[guildId]: {
					...userGuildStates,
					[connectionId]: indexedVoiceState,
				},
			},
		},
		connectionVoiceStates: {
			...context.connectionVoiceStates,
			[connectionId]: indexedVoiceState,
		},
	};
}

function removeStaleSessionConnections(
	context: VoiceGatewayStateContext,
	guildId: string,
	voiceState: VoiceStateWithIndexedConnection,
): VoiceGatewayStateContext {
	if (!voiceState.session_id) return context;
	const indexedGuildStates = context.userVoiceStates[voiceState.user_id]?.[guildId];
	if (!indexedGuildStates) return context;
	let next = context;
	for (const [connectionId, existing] of Object.entries(indexedGuildStates)) {
		if (connectionId === voiceState.connection_id) continue;
		if (existing.session_id !== voiceState.session_id) continue;
		next = removeConnection(next, connectionId);
	}
	return next;
}

function upsertConnection(
	context: VoiceGatewayStateContext,
	guildId: string,
	voiceState: VoiceState,
): VoiceGatewayStateContext {
	const connectionId = voiceState.connection_id;
	if (!connectionId) return context;
	if (!voiceState.channel_id) return removeConnection(context, connectionId);
	if (!hasIndexedVoiceConnection(voiceState)) return context;
	const withoutCurrent = removeConnection(context, connectionId);
	const withoutStaleSession = removeStaleSessionConnections(withoutCurrent, guildId, voiceState);
	return addConnection(withoutStaleSession, guildId, voiceState);
}

function removeUserFromGuild(
	context: VoiceGatewayStateContext,
	guildId: string,
	userId: string,
): VoiceGatewayStateContext {
	const connectionIds = new Set<string>();
	const indexedGuildStates = context.userVoiceStates[userId]?.[guildId];
	if (indexedGuildStates) {
		for (const connectionId of Object.keys(indexedGuildStates)) {
			connectionIds.add(connectionId);
		}
	}
	for (const [connectionId, voiceState] of Object.entries(context.connectionVoiceStates)) {
		if (voiceState.guild_id === guildId && voiceState.user_id === userId) {
			connectionIds.add(connectionId);
		}
	}
	let next = context;
	for (const connectionId of connectionIds) {
		next = removeConnection(next, connectionId);
	}
	return next;
}

function removeGuildFromUserVoiceStates(
	userVoiceStates: VoiceGatewayUserVoiceStates,
	guildId: string,
): VoiceGatewayUserVoiceStates {
	let nextUserVoiceStates = userVoiceStates;
	for (const [userId, userStates] of Object.entries(userVoiceStates)) {
		if (!userStates[guildId]) continue;
		if (nextUserVoiceStates === userVoiceStates) {
			nextUserVoiceStates = {...userVoiceStates};
		}
		const nextUserStates = {...userStates};
		delete nextUserStates[guildId];
		if (Object.keys(nextUserStates).length === 0) {
			delete nextUserVoiceStates[userId];
		} else {
			nextUserVoiceStates[userId] = nextUserStates;
		}
	}
	return nextUserVoiceStates;
}

function removeGuild(context: VoiceGatewayStateContext, guildId: string): VoiceGatewayStateContext {
	let next = context;
	for (const [connectionId, voiceState] of Object.entries(context.connectionVoiceStates)) {
		if (voiceState.guild_id === guildId) {
			next = removeConnection(next, connectionId);
		}
	}

	const nextVoiceStates = Object.hasOwn(next.voiceStates, guildId) ? {...next.voiceStates} : next.voiceStates;
	if (nextVoiceStates !== next.voiceStates) {
		delete nextVoiceStates[guildId];
	}
	const nextUserVoiceStates = removeGuildFromUserVoiceStates(next.userVoiceStates, guildId);
	if (nextVoiceStates === next.voiceStates && nextUserVoiceStates === next.userVoiceStates) return next;
	return {
		...next,
		voiceStates: nextVoiceStates,
		userVoiceStates: nextUserVoiceStates,
	};
}

function replaceAllFromGuilds(guilds: ReadonlyArray<GuildReadyData>): VoiceGatewayStateContext {
	let next = createEmptyVoiceGatewayStateContext();
	for (const guild of guilds) {
		for (const voiceState of guild.voice_states ?? []) {
			next = upsertConnection(next, guild.id, voiceState);
		}
	}
	return next;
}

function replaceGuild(context: VoiceGatewayStateContext, guild: GuildReadyData): VoiceGatewayStateContext {
	let next = removeGuild(context, guild.id);
	for (const voiceState of guild.voice_states ?? []) {
		next = upsertConnection(next, guild.id, voiceState);
	}
	if (Object.hasOwn(next.voiceStates, guild.id)) return next;
	return {
		...next,
		voiceStates: {
			...next.voiceStates,
			[guild.id]: {},
		},
	};
}

export const voiceGatewayStateMachine = setup({
	types: {} as {
		context: VoiceGatewayStateContext;
		events: VoiceGatewayStateEvent;
	},
	actions: {
		updateVoiceState: assign(({context, event}) =>
			event.type === 'voiceState.update'
				? upsertConnection(context, resolveGuildId(event.guildId), event.voiceState)
				: context,
		),
		deleteVoiceState: assign(({context, event}) =>
			event.type === 'voiceState.delete' ? removeUserFromGuild(context, event.guildId, event.userId) : context,
		),
		removeVoiceStateConnection: assign(({context, event}) =>
			event.type === 'voiceState.removeConnection' ? removeConnection(context, event.connectionId) : context,
		),
		openConnection: assign(({event}) =>
			event.type === 'connection.open' ? replaceAllFromGuilds(event.guilds) : createEmptyVoiceGatewayStateContext(),
		),
		createGuild: assign(({context, event}) =>
			event.type === 'guild.create' ? replaceGuild(context, event.guild) : context,
		),
		deleteGuild: assign(({context, event}) =>
			event.type === 'guild.delete' ? removeGuild(context, event.guildId) : context,
		),
		clearAll: assign(() => createEmptyVoiceGatewayStateContext()),
	},
}).createMachine({
	id: 'voiceGatewayState',
	context: () => createEmptyVoiceGatewayStateContext(),
	initial: 'tracking',
	states: {
		tracking: {
			on: {
				'voiceState.update': {actions: 'updateVoiceState'},
				'voiceState.delete': {actions: 'deleteVoiceState'},
				'voiceState.removeConnection': {actions: 'removeVoiceStateConnection'},
				'connection.open': {actions: 'openConnection'},
				'guild.create': {actions: 'createGuild'},
				'guild.delete': {actions: 'deleteGuild'},
				'clear.all': {actions: 'clearAll'},
			},
		},
	},
});

export type VoiceGatewayStateSnapshot = SnapshotFrom<typeof voiceGatewayStateMachine>;

export function createVoiceGatewayStateSnapshot(): VoiceGatewayStateSnapshot {
	return initialTransition(voiceGatewayStateMachine)[0];
}

export function transitionVoiceGatewayStateSnapshot(
	snapshot: VoiceGatewayStateSnapshot,
	event: VoiceGatewayStateEvent,
): VoiceGatewayStateSnapshot {
	return transition(voiceGatewayStateMachine, snapshot, event)[0] as VoiceGatewayStateSnapshot;
}
