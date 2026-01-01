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

import {runInAction} from 'mobx';
import {ME} from '~/Constants';
import {Logger} from '~/lib/Logger';
import type {GuildReadyData} from '~/records/GuildRecord';
import type {VoiceState} from './VoiceStateManager';

const logger = new Logger('VoiceStateGatewayHandler');

export class VoiceStateGatewayHandler {
	constructor(
		private getVoiceStates: () => Record<string, Record<string, Record<string, VoiceState>>>,
		private getUserVoiceStates: () => Record<string, Record<string, VoiceState>>,
		private getConnectionVoiceStates: () => Record<string, VoiceState>,
		private setVoiceStates: (states: Record<string, Record<string, Record<string, VoiceState>>>) => void,
		private setUserVoiceStates: (states: Record<string, Record<string, VoiceState>>) => void,
		private setConnectionVoiceStates: (states: Record<string, VoiceState>) => void,
		private removeVoiceState: (connectionId: string, userId: string, guildId: string) => void,
	) {}

	handleGatewayVoiceStateUpdate(guildId: string | null, voiceState: VoiceState): void {
		const key = guildId ?? ME;
		const channelId = voiceState.channel_id;
		const userId = voiceState.user_id;
		const connectionId = voiceState.connection_id;

		if (!connectionId) {
			logger.warn('[VoiceStateGatewayHandler] Voice state missing connection_id:', voiceState);
			return;
		}

		const vsWithGuild = {...voiceState, guild_id: key};

		if (!channelId) {
			this.removeVoiceState(connectionId, userId, key);
			logger.debug('[VoiceStateGatewayHandler] User left voice channel', {
				guildId: key,
				userId,
				connectionId,
			});
			return;
		}

		runInAction(() => {
			const newVoiceStates = {...this.getVoiceStates()};
			const newUserVoiceStates = {...this.getUserVoiceStates()};
			const newConnectionVoiceStates = {...this.getConnectionVoiceStates()};

			const oldVoiceState = this.getConnectionVoiceStates()[connectionId];

			if (oldVoiceState?.channel_id && oldVoiceState.guild_id) {
				const oldGuildId = oldVoiceState.guild_id;
				const oldGuildStates = {...(newVoiceStates[oldGuildId] || {})};
				const oldChannelStates = {...(oldGuildStates[oldVoiceState.channel_id] || {})};

				delete oldChannelStates[connectionId];

				if (Object.keys(oldChannelStates).length === 0) {
					delete oldGuildStates[oldVoiceState.channel_id];
				} else {
					oldGuildStates[oldVoiceState.channel_id] = oldChannelStates;
				}

				if (Object.keys(oldGuildStates).length === 0) {
					delete newVoiceStates[oldGuildId];
				} else {
					newVoiceStates[oldGuildId] = oldGuildStates;
				}

				const oldUserStates = {...(newUserVoiceStates[userId] || {})};
				delete oldUserStates[oldGuildId];

				if (Object.keys(oldUserStates).length === 0) {
					delete newUserVoiceStates[userId];
				} else {
					newUserVoiceStates[userId] = oldUserStates;
				}
			}

			const guildStates = {...(newVoiceStates[key] || {})};
			const channelStates = {...(guildStates[channelId] || {})};

			channelStates[connectionId] = vsWithGuild;
			guildStates[channelId] = channelStates;
			newVoiceStates[key] = guildStates;

			const userStates = {...(newUserVoiceStates[userId] || {})};
			userStates[key] = vsWithGuild;
			newUserVoiceStates[userId] = userStates;

			newConnectionVoiceStates[connectionId] = vsWithGuild;

			this.setVoiceStates(newVoiceStates);
			this.setUserVoiceStates(newUserVoiceStates);
			this.setConnectionVoiceStates(newConnectionVoiceStates);
		});

		logger.debug('[VoiceStateGatewayHandler] Voice state updated', {
			guildId: key,
			channelId,
			userId,
			connectionId,
			selfMute: voiceState.self_mute,
			selfDeaf: voiceState.self_deaf,
			selfVideo: voiceState.self_video,
			selfStream: voiceState.self_stream,
			viewerStreamKey: voiceState.viewer_stream_key,
		});
	}

	handleGatewayVoiceStateDelete(guildId: string, userId: string): void {
		const userStates = this.getUserVoiceStates()[userId];
		if (!userStates) {
			logger.debug('[VoiceStateGatewayHandler] No voice states found for user', {userId, guildId});
			return;
		}

		const voiceState = userStates[guildId];
		if (!voiceState) {
			logger.debug('[VoiceStateGatewayHandler] No voice state found for user in guild', {userId, guildId});
			return;
		}

		const connectionId = voiceState.connection_id;
		if (!connectionId) {
			logger.warn('[VoiceStateGatewayHandler] Voice state missing connection_id in delete:', voiceState);
			return;
		}

		this.removeVoiceState(connectionId, userId, guildId);
		logger.debug('[VoiceStateGatewayHandler] Voice state deleted', {guildId, userId, connectionId});
	}

	handleConnectionOpen(guilds: Array<GuildReadyData>): void {
		runInAction(() => {
			const newVoiceStates: Record<string, Record<string, Record<string, VoiceState>>> = {};
			const newUserVoiceStates: Record<string, Record<string, VoiceState>> = {};
			const newConnectionVoiceStates: Record<string, VoiceState> = {};

			for (const guild of guilds) {
				if (!guild.voice_states || guild.voice_states.length === 0) {
					continue;
				}

				for (const voiceState of guild.voice_states) {
					if (!voiceState.channel_id) {
						continue;
					}

					const connectionId = voiceState.connection_id;
					if (!connectionId) {
						logger.warn('[VoiceStateGatewayHandler] Skipping voice state without connection_id', {
							guildId: guild.id,
							userId: voiceState.user_id,
						});
						continue;
					}

					if (!newVoiceStates[guild.id]) {
						newVoiceStates[guild.id] = {};
					}
					if (!newVoiceStates[guild.id][voiceState.channel_id]) {
						newVoiceStates[guild.id][voiceState.channel_id] = {};
					}

					const vsWithGuild = {...voiceState, guild_id: guild.id};
					newVoiceStates[guild.id][voiceState.channel_id][connectionId] = vsWithGuild;

					if (!newUserVoiceStates[voiceState.user_id]) {
						newUserVoiceStates[voiceState.user_id] = {};
					}
					newUserVoiceStates[voiceState.user_id][guild.id] = vsWithGuild;

					newConnectionVoiceStates[connectionId] = vsWithGuild;
				}
			}

			this.setVoiceStates(newVoiceStates);
			this.setUserVoiceStates(newUserVoiceStates);
			this.setConnectionVoiceStates(newConnectionVoiceStates);
		});

		logger.info('[VoiceStateGatewayHandler] Initialized voice states from connection open', {
			guildCount: guilds.length,
			totalVoiceStates: Object.keys(this.getConnectionVoiceStates()).length,
		});
	}

	handleGuildCreate(guild: GuildReadyData): void {
		if (!guild.voice_states || guild.voice_states.length === 0) {
			logger.debug('[VoiceStateGatewayHandler] No voice states in guild create', {guildId: guild.id});
			return;
		}

		runInAction(() => {
			const guildVoiceStates: Record<string, Record<string, VoiceState>> = {};
			const newUserVoiceStates: Record<string, Record<string, VoiceState>> = {};
			const newConnectionVoiceStates: Record<string, VoiceState> = {};

			for (const voiceState of guild.voice_states!) {
				if (!voiceState.channel_id) {
					continue;
				}

				const connectionId = voiceState.connection_id;
				if (!connectionId) {
					logger.warn('[VoiceStateGatewayHandler] Skipping voice state without connection_id', {
						guildId: guild.id,
						userId: voiceState.user_id,
					});
					continue;
				}

				if (!guildVoiceStates[voiceState.channel_id]) {
					guildVoiceStates[voiceState.channel_id] = {};
				}

				const vsWithGuild = {...voiceState, guild_id: guild.id};
				guildVoiceStates[voiceState.channel_id][connectionId] = vsWithGuild;

				if (!newUserVoiceStates[voiceState.user_id]) {
					newUserVoiceStates[voiceState.user_id] = {};
				}
				newUserVoiceStates[voiceState.user_id][guild.id] = vsWithGuild;

				newConnectionVoiceStates[connectionId] = vsWithGuild;
			}

			this.setVoiceStates({...this.getVoiceStates(), [guild.id]: guildVoiceStates});
			this.setUserVoiceStates({...this.getUserVoiceStates(), ...newUserVoiceStates});
			this.setConnectionVoiceStates({...this.getConnectionVoiceStates(), ...newConnectionVoiceStates});
		});

		logger.debug('[VoiceStateGatewayHandler] Guild voice states loaded', {
			guildId: guild.id,
			voiceStateCount: guild.voice_states.length,
		});
	}

	handleGuildDelete(deletedGuildId: string): void {
		const has = Boolean(this.getVoiceStates()[deletedGuildId]);
		if (!has) {
			logger.debug('[VoiceStateGatewayHandler] No voice states to delete for guild', {guildId: deletedGuildId});
			return;
		}

		runInAction(() => {
			const newVoiceStates = {...this.getVoiceStates()};
			const newUserVoiceStates = {...this.getUserVoiceStates()};
			const newConnectionVoiceStates = {...this.getConnectionVoiceStates()};

			delete newVoiceStates[deletedGuildId];

			for (const [userId, userStates] of Object.entries(this.getUserVoiceStates())) {
				if (userStates[deletedGuildId]) {
					const updated = {...userStates};
					delete updated[deletedGuildId];

					if (Object.keys(updated).length === 0) {
						delete newUserVoiceStates[userId];
					} else {
						newUserVoiceStates[userId] = updated;
					}
				}
			}

			for (const [connectionId, voiceState] of Object.entries(this.getConnectionVoiceStates())) {
				if (voiceState.guild_id === deletedGuildId) {
					delete newConnectionVoiceStates[connectionId];
				}
			}

			this.setVoiceStates(newVoiceStates);
			this.setUserVoiceStates(newUserVoiceStates);
			this.setConnectionVoiceStates(newConnectionVoiceStates);
		});

		logger.info('[VoiceStateGatewayHandler] Removed all voice states for deleted guild', {guildId: deletedGuildId});
	}
}
