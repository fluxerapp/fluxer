// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Store, useStoreVersion} from '@app/features/voice/engine/Store';
import {
	createVoiceGatewayStateSnapshot,
	type NormalizedVoiceState,
	transitionVoiceGatewayStateSnapshot,
	type VoiceGatewayConnectionVoiceStates,
	type VoiceGatewayStateContext,
	type VoiceGatewayStateEvent,
	type VoiceGatewayStateSnapshot,
	type VoiceGatewayUserVoiceStates,
	type VoiceGatewayVoiceStates,
} from '@app/features/voice/engine/VoiceGatewayStateMachine';
import {ME} from '@fluxer/constants/src/AppConstants';
import {makeObservable, observable} from 'mobx';

const logger = new Logger('VoiceEngineV2AppVoiceStateAdapter');

function filterIgnoredVoiceStates(
	ignoredConnectionIds: ReadonlySet<string>,
	voiceStates: ReadonlyArray<VoiceState> | undefined,
): ReadonlyArray<VoiceState> | undefined {
	if (!voiceStates) return voiceStates;
	const filtered = voiceStates.filter(
		(voiceState) => voiceState.connection_id == null || !ignoredConnectionIds.has(voiceState.connection_id),
	);
	return filtered.length === voiceStates.length ? voiceStates : filtered;
}

function filterIgnoredGuildVoiceStates(
	ignoredConnectionIds: ReadonlySet<string>,
	guild: GuildReadyData,
): GuildReadyData {
	const filteredVoiceStates = filterIgnoredVoiceStates(ignoredConnectionIds, guild.voice_states);
	if (filteredVoiceStates === guild.voice_states) return guild;
	return {...guild, voice_states: filteredVoiceStates};
}

export class VoiceEngineV2AppVoiceStateAdapter extends Store {
	private snapshot: VoiceGatewayStateSnapshot = createVoiceGatewayStateSnapshot();
	private voiceStates: VoiceGatewayVoiceStates = {};
	private userVoiceStates: VoiceGatewayUserVoiceStates = {};
	private connectionVoiceStates: VoiceGatewayConnectionVoiceStates = {};
	private readonly guildVoiceStates = observable.map<string, VoiceGatewayVoiceStates[string]>(undefined, {
		deep: false,
	});
	private readonly ignoredConnectionIds = new Set<string>();

	constructor() {
		super();
		makeObservable<this, 'snapshot' | 'voiceStates' | 'userVoiceStates' | 'connectionVoiceStates'>(this, {
			snapshot: observable.ref,
			voiceStates: observable.ref,
			userVoiceStates: observable.ref,
			connectionVoiceStates: observable.ref,
		});
		this.applyVoiceGatewayStateContext(this.snapshot.context);
	}

	handleGatewayVoiceStateUpdate(guildId: string | null, voiceState: VoiceState): void {
		const connectionId = voiceState.connection_id;
		if (!connectionId) {
			logger.warn('Voice state missing connection_id:', voiceState);
			return;
		}
		if (this.isConnectionIgnored(connectionId)) {
			this.send({type: 'voiceState.removeConnection', connectionId});
			if (voiceState.channel_id) {
				logger.debug('Ignored voice state update for locally removed connection', {
					guildId,
					channelId: voiceState.channel_id,
					userId: voiceState.user_id,
					connectionId,
				});
			}
			return;
		}
		this.send({type: 'voiceState.update', guildId, voiceState});
	}

	handleGatewayVoiceStateDelete(guildId: string, userId: string): void {
		this.send({type: 'voiceState.delete', guildId, userId});
	}

	removeVoiceStateConnection(connectionId: string): void {
		this.ignoredConnectionIds.add(connectionId);
		this.send({type: 'voiceState.removeConnection', connectionId});
	}

	isConnectionIgnored(connectionId: string | null | undefined): boolean {
		return connectionId != null && this.ignoredConnectionIds.has(connectionId);
	}

	handleConnectionOpen(guilds: Array<GuildReadyData>): void {
		this.send({
			type: 'connection.open',
			guilds: guilds.map((guild) => filterIgnoredGuildVoiceStates(this.ignoredConnectionIds, guild)),
		});
	}

	handleGuildCreate(guild: GuildReadyData): void {
		this.send({type: 'guild.create', guild: filterIgnoredGuildVoiceStates(this.ignoredConnectionIds, guild)});
	}

	handleGuildDelete(guildId: string): void {
		this.send({type: 'guild.delete', guildId});
	}

	getCurrentUserVoiceState(
		guildId?: string | null,
		currentUserId?: string,
		connectionId?: string | null,
	): NormalizedVoiceState | null {
		const requestedGuildKey = guildId ?? ME;
		if (connectionId) {
			const byConnection = this.connectionVoiceStates[connectionId];
			if (byConnection && (!guildId || byConnection.guild_id === requestedGuildKey)) {
				return byConnection;
			}
		}
		if (!currentUserId) {
			logger.debug('Cannot get current user voice state: no user ID provided');
			return null;
		}
		const userStatesByGuild = this.userVoiceStates[currentUserId];
		if (!userStatesByGuild) return null;
		if (guildId) {
			const guildStates = userStatesByGuild[guildId];
			if (!guildStates) return null;
			const firstConnectionId = Object.keys(guildStates)[0];
			return firstConnectionId ? guildStates[firstConnectionId] : null;
		}
		const firstGuildId = Object.keys(userStatesByGuild)[0];
		const firstGuildStates = firstGuildId ? userStatesByGuild[firstGuildId] : null;
		if (!firstGuildStates) return null;
		const firstConnectionId = Object.keys(firstGuildStates)[0];
		return firstConnectionId ? firstGuildStates[firstConnectionId] : null;
	}

	getVoiceState(
		guildId: string | null,
		userId?: string,
		currentUserId?: string,
		connectionId?: string | null,
	): NormalizedVoiceState | null {
		const resolvedUserId = userId ?? currentUserId;
		if (!resolvedUserId) {
			logger.debug('Cannot get voice state: no user ID provided');
			return null;
		}
		const key = guildId ?? ME;
		if (connectionId) {
			const byConnection = this.connectionVoiceStates[connectionId];
			if (byConnection?.user_id === resolvedUserId && (!guildId || byConnection.guild_id === key)) {
				return byConnection;
			}
		}
		const guildStates = this.userVoiceStates[resolvedUserId]?.[key];
		if (!guildStates) return null;
		const firstConnectionId = Object.keys(guildStates)[0];
		return firstConnectionId ? guildStates[firstConnectionId] : null;
	}

	getVoiceStateByConnectionId(connectionId: string): NormalizedVoiceState | null {
		return this.connectionVoiceStates[connectionId] ?? null;
	}

	getConnectionVoiceStates(): Readonly<VoiceGatewayConnectionVoiceStates> {
		return this.connectionVoiceStates;
	}

	getAllVoiceStatesInChannel(guildId: string, channelId: string): Readonly<Record<string, NormalizedVoiceState>> {
		return this.guildVoiceStates.get(guildId)?.[channelId] ?? {};
	}

	getAllVoiceStatesInGuild(
		guildId: string,
	): Readonly<Record<string, Readonly<Record<string, NormalizedVoiceState>>>> | undefined {
		return this.guildVoiceStates.get(guildId);
	}

	getAllVoiceStates(): Readonly<
		Record<string, Readonly<Record<string, Readonly<Record<string, NormalizedVoiceState>>>>>
	> {
		return this.voiceStates;
	}

	clearAllVoiceStates(): void {
		this.send({type: 'clear.all'});
	}

	private send(event: VoiceGatewayStateEvent): void {
		this.update(() => {
			this.snapshot = transitionVoiceGatewayStateSnapshot(this.snapshot, event);
			this.applyVoiceGatewayStateContext(this.snapshot.context);
		});
	}

	private applyVoiceGatewayStateContext(context: VoiceGatewayStateContext): void {
		this.voiceStates = context.voiceStates;
		this.userVoiceStates = context.userVoiceStates;
		this.connectionVoiceStates = context.connectionVoiceStates;
		this.synchronizeGuildVoiceStates(context.voiceStates);
	}

	private synchronizeGuildVoiceStates(voiceStates: VoiceGatewayVoiceStates): void {
		for (const guildId of Array.from(this.guildVoiceStates.keys())) {
			if (!Object.hasOwn(voiceStates, guildId)) {
				this.guildVoiceStates.delete(guildId);
			}
		}
		for (const guildId in voiceStates) {
			const guildStates = voiceStates[guildId];
			if (this.guildVoiceStates.get(guildId) === guildStates) continue;
			this.guildVoiceStates.set(guildId, guildStates);
		}
	}
}

const voiceEngineV2AppVoiceStateAdapter = new VoiceEngineV2AppVoiceStateAdapter();

export function useVoiceGatewayStateVersion(): number {
	return useStoreVersion(voiceEngineV2AppVoiceStateAdapter);
}

export default voiceEngineV2AppVoiceStateAdapter;
