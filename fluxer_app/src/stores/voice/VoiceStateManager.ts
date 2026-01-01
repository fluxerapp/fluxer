/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable, observable, runInAction} from 'mobx';
import {ME} from '~/Constants';
import {Logger} from '~/lib/Logger';
import type {GuildMember} from '~/records/GuildMemberRecord';
import type {GuildReadyData} from '~/records/GuildRecord';
import {VoiceStateGatewayHandler} from './VoiceStateGatewayHandler';

const logger = new Logger('VoiceStateManager');

export interface VoiceState {
	guild_id: string;
	channel_id: string | null;
	user_id: string;
	connection_id: string;
	is_mobile?: boolean;
	mute: boolean;
	deaf: boolean;
	self_mute: boolean;
	self_deaf: boolean;
	self_video: boolean;
	self_stream: boolean;
	viewer_stream_key?: string | null;
	suppress: boolean;
	member?: GuildMember;
}

class VoiceStateManager {
	private voiceStates = observable.object<Record<string, Record<string, Record<string, VoiceState>>>>({});
	private userVoiceStates = observable.object<Record<string, Record<string, VoiceState>>>({});
	private connectionVoiceStates = observable.object<Record<string, VoiceState>>({});
	private gatewayHandler: VoiceStateGatewayHandler;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.gatewayHandler = new VoiceStateGatewayHandler(
			() => this.voiceStates,
			() => this.userVoiceStates,
			() => this.connectionVoiceStates,
			this.setVoiceStates.bind(this),
			this.setUserVoiceStates.bind(this),
			this.setConnectionVoiceStates.bind(this),
			this.removeVoiceState.bind(this),
		);
		logger.debug('[VoiceStateManager] Initialized');
	}

	private setVoiceStates(states: Record<string, Record<string, Record<string, VoiceState>>>) {
		this.voiceStates = states;
	}

	private setUserVoiceStates(states: Record<string, Record<string, VoiceState>>) {
		this.userVoiceStates = states;
	}

	private setConnectionVoiceStates(states: Record<string, VoiceState>) {
		this.connectionVoiceStates = states;
	}

	handleGatewayVoiceStateUpdate(guildId: string | null, voiceState: VoiceState): void {
		this.gatewayHandler.handleGatewayVoiceStateUpdate(guildId, voiceState);
	}

	handleGatewayVoiceStateDelete(guildId: string, userId: string): void {
		this.gatewayHandler.handleGatewayVoiceStateDelete(guildId, userId);
	}

	handleConnectionOpen(guilds: Array<GuildReadyData>): void {
		this.gatewayHandler.handleConnectionOpen(guilds);
	}

	handleGuildCreate(guild: GuildReadyData): void {
		this.gatewayHandler.handleGuildCreate(guild);
	}

	handleGuildDelete(deletedGuildId: string): void {
		this.gatewayHandler.handleGuildDelete(deletedGuildId);
	}

	getCurrentUserVoiceState(guildId?: string | null, currentUserId?: string): VoiceState | null {
		if (!currentUserId) {
			logger.debug('[VoiceStateManager] Cannot get current user voice state: no user ID provided');
			return null;
		}

		const userStates = this.userVoiceStates[currentUserId];
		if (!userStates) {
			return null;
		}

		if (guildId) {
			return userStates[guildId] ?? null;
		}

		const guildIds = Object.keys(userStates);
		return guildIds.length > 0 ? userStates[guildIds[0]] : null;
	}

	getVoiceState(guildId: string | null, userId?: string, currentUserId?: string): VoiceState | null {
		if (userId) {
			const userStates = this.userVoiceStates[userId];
			if (!userStates) {
				return null;
			}

			const key = guildId ?? ME;
			return userStates[key] ?? null;
		}

		if (!currentUserId) {
			logger.debug('[VoiceStateManager] Cannot get voice state: no user ID provided');
			return null;
		}

		const userStates = this.userVoiceStates[currentUserId];
		if (!userStates) {
			return null;
		}

		const key = guildId ?? ME;
		return userStates[key] ?? null;
	}

	getVoiceStateByConnectionId(connectionId: string): VoiceState | null {
		return this.connectionVoiceStates[connectionId] ?? null;
	}

	getAllVoiceStatesInChannel(guildId: string, channelId: string): Readonly<Record<string, VoiceState>> {
		return this.voiceStates[guildId]?.[channelId] ?? {};
	}

	getAllVoiceStates(): Readonly<Record<string, Readonly<Record<string, Readonly<Record<string, VoiceState>>>>>> {
		return this.voiceStates;
	}

	private removeVoiceState(connectionId: string, userId: string, guildId: string): void {
		const voiceState = this.connectionVoiceStates[connectionId];
		if (!voiceState) {
			logger.debug('[VoiceStateManager] No voice state to remove', {connectionId, userId, guildId});
			return;
		}

		runInAction(() => {
			const newVoiceStates = {...this.voiceStates};
			const newUserVoiceStates = {...this.userVoiceStates};
			const newConnectionVoiceStates = {...this.connectionVoiceStates};

			if (voiceState.channel_id && voiceState.guild_id) {
				const guildStates = {...(newVoiceStates[voiceState.guild_id] || {})};
				const channelStates = {...(guildStates[voiceState.channel_id] || {})};

				delete channelStates[connectionId];

				if (Object.keys(channelStates).length === 0) {
					delete guildStates[voiceState.channel_id];
				} else {
					guildStates[voiceState.channel_id] = channelStates;
				}

				if (Object.keys(guildStates).length === 0) {
					delete newVoiceStates[voiceState.guild_id];
				} else {
					newVoiceStates[voiceState.guild_id] = guildStates;
				}
			}

			const userStates = {...(newUserVoiceStates[userId] || {})};
			delete userStates[guildId];

			if (Object.keys(userStates).length === 0) {
				delete newUserVoiceStates[userId];
			} else {
				newUserVoiceStates[userId] = userStates;
			}

			delete newConnectionVoiceStates[connectionId];

			this.voiceStates = newVoiceStates;
			this.userVoiceStates = newUserVoiceStates;
			this.connectionVoiceStates = newConnectionVoiceStates;
		});

		logger.debug('[VoiceStateManager] Voice state removed', {connectionId, userId, guildId});
	}

	clearAllVoiceStates(): void {
		runInAction(() => {
			this.voiceStates = {};
			this.userVoiceStates = {};
			this.connectionVoiceStates = {};
		});

		logger.info('[VoiceStateManager] All voice states cleared');
	}
}

const instance = new VoiceStateManager();
(window as typeof window & {_voiceStateManager?: VoiceStateManager})._voiceStateManager = instance;
export default instance;
