// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import {makeAutoObservable, observable} from 'mobx';

class GuildAvailability {
	unavailableGuilds = observable.set<string>();
	degradedGuilds = observable.set<string>();

	constructor() {
		makeAutoObservable(
			this,
			{
				unavailableGuilds: false,
				degradedGuilds: false,
			},
			{autoBind: true},
		);
	}

	setGuildAvailable(guildId: string): void {
		if (this.unavailableGuilds.has(guildId)) {
			this.unavailableGuilds.delete(guildId);
		}
	}

	setGuildUnavailable(guildId: string): void {
		this.degradedGuilds.delete(guildId);
		if (!this.unavailableGuilds.has(guildId)) {
			this.unavailableGuilds.add(guildId);
		}
	}

	setGuildDegraded(guildId: string, degraded: boolean): void {
		if (degraded) {
			this.degradedGuilds.add(guildId);
		} else {
			this.degradedGuilds.delete(guildId);
		}
	}

	handleGuildAvailability(guildId: string, unavailable = false, unavailableHidden = false): void {
		if (unavailable && !unavailableHidden) {
			this.setGuildUnavailable(guildId);
		} else {
			this.setGuildAvailable(guildId);
		}
	}

	loadUnavailableGuilds(guilds: ReadonlyArray<GuildReadyData>): void {
		this.unavailableGuilds.replace(
			guilds.filter((guild) => guild.unavailable && !guild.unavailable_hidden).map((guild) => guild.id),
		);
		this.degradedGuilds.replace(
			guilds.filter((guild) => guild.degraded && !guild.unavailable).map((guild) => guild.id),
		);
	}

	hydrateFromSnapshot(guilds: ReadonlyArray<{readonly id: string; readonly unavailable: boolean}>): void {
		this.unavailableGuilds.replace(guilds.filter((guild) => guild.unavailable).map((guild) => guild.id));
		this.degradedGuilds.clear();
	}

	get totalUnavailableGuilds(): number {
		return this.unavailableGuilds.size;
	}
}

export default new GuildAvailability();
