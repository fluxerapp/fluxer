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

import {makeAutoObservable, observable} from 'mobx';
import type {GuildReadyData} from '~/records/GuildRecord';

class GuildAvailabilityStore {
	unavailableGuilds: Set<string> = observable.set();

	constructor() {
		makeAutoObservable(
			this,
			{
				unavailableGuilds: false,
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
		if (!this.unavailableGuilds.has(guildId)) {
			this.unavailableGuilds.add(guildId);
		}
	}

	handleGuildAvailability(guildId: string, unavailable = false): void {
		if (unavailable) {
			this.setGuildUnavailable(guildId);
		} else {
			this.setGuildAvailable(guildId);
		}
	}

	loadUnavailableGuilds(guilds: ReadonlyArray<GuildReadyData>): void {
		const unavailableGuildIds = guilds.filter((guild) => guild.unavailable).map((guild) => guild.id);
		this.unavailableGuilds.clear();
		unavailableGuildIds.forEach((id) => this.unavailableGuilds.add(id));
	}

	get totalUnavailableGuilds(): number {
		return this.unavailableGuilds.size;
	}
}

export default new GuildAvailabilityStore();
