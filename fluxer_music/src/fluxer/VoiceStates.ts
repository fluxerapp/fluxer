// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceStateWire} from './Wire';

interface Entry {
	guildId: string;
	channelId: string;
	userId: string;
	connectionId: string;
}

/** Voice membership of guild channels, keyed by connection so one user can have several. */
export class VoiceStates {
	private readonly byConnection = new Map<string, Entry>();

	applyGuild(guildId: string, states: Array<VoiceStateWire>): void {
		for (const [key, entry] of this.byConnection) {
			if (entry.guildId === guildId) this.byConnection.delete(key);
		}
		for (const state of states) this.apply(state);
	}

	apply(state: VoiceStateWire): void {
		// Calls (no guild) and states without a connection are not tracked.
		if (state.guild_id === null || state.connection_id === null) return;
		if (state.channel_id === null) {
			this.byConnection.delete(state.connection_id);
			return;
		}
		this.byConnection.set(state.connection_id, {
			guildId: state.guild_id,
			channelId: state.channel_id,
			userId: state.user_id,
			connectionId: state.connection_id,
		});
	}

	usersIn(channelId: string): Set<string> {
		const users = new Set<string>();
		for (const entry of this.byConnection.values()) {
			if (entry.channelId === channelId) users.add(entry.userId);
		}
		return users;
	}

	isIn(userId: string, channelId: string): boolean {
		for (const entry of this.byConnection.values()) {
			if (entry.userId === userId && entry.channelId === channelId) return true;
		}
		return false;
	}

	connectionsOf(userId: string): Array<{guildId: string; channelId: string; connectionId: string}> {
		const result: Array<{guildId: string; channelId: string; connectionId: string}> = [];
		for (const entry of this.byConnection.values()) {
			if (entry.userId === userId) {
				result.push({guildId: entry.guildId, channelId: entry.channelId, connectionId: entry.connectionId});
			}
		}
		return result;
	}
}
