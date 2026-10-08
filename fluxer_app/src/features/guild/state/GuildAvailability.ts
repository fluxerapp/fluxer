// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import {makeAutoObservable, observable} from 'mobx';

class GuildAvailability {
	unavailableGuilds: Set<string> = observable.set();
	degradedGuilds: Set<string> = observable.set();

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
		this.unavailableGuilds.clear();
		this.degradedGuilds.clear();
		for (const guild of guilds) {
			if (guild.degraded && !guild.unavailable) {
				this.degradedGuilds.add(guild.id);
			}
			if (guild.unavailable && !guild.unavailable_hidden) {
				this.unavailableGuilds.add(guild.id);
			}
		}
	}

	hydrateFromSnapshot(guilds: ReadonlyArray<{readonly id: string; readonly unavailable: boolean}>): void {
		this.unavailableGuilds.clear();
		for (const guild of guilds) {
			if (guild.unavailable) {
				this.unavailableGuilds.add(guild.id);
			}
		}
	}

	get totalUnavailableGuilds(): number {
		return this.unavailableGuilds.size;
	}
}

export default new GuildAvailability();
