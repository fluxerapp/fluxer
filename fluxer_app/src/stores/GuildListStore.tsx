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

import {action, makeAutoObservable} from 'mobx';
import {type Guild, type GuildReadyData, GuildRecord} from '~/records/GuildRecord';
import GuildStore from '~/stores/GuildStore';
import UserSettingsStore from '~/stores/UserSettingsStore';

class GuildListStore {
	guilds: Array<GuildRecord> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	@action
	handleConnectionOpen(guilds: ReadonlyArray<GuildReadyData>): void {
		this.guilds = [];

		const availableGuilds = guilds
			.filter((guild) => !guild.unavailable)
			.map((guild) => GuildStore.getGuild(guild.id))
			.filter((guild): guild is GuildRecord => guild !== undefined);

		if (availableGuilds.length > 0) {
			this.guilds = [...this.sortGuildArray(availableGuilds)];
		}
	}

	@action
	handleGuild(guild: Guild | GuildReadyData): void {
		if (guild.unavailable) {
			return;
		}

		const guildRecord = GuildStore.getGuild(guild.id);
		if (!guildRecord) {
			return;
		}

		const index = this.guilds.findIndex((s) => s.id === guild.id);

		if (index === -1) {
			this.guilds = [...this.sortGuildArray([...this.guilds, guildRecord])];
		} else {
			this.guilds = [
				...this.sortGuildArray([...this.guilds.slice(0, index), guildRecord, ...this.guilds.slice(index + 1)]),
			];
		}
	}

	@action
	handleGuildDelete(guildId: string, unavailable?: boolean): void {
		const index = this.guilds.findIndex((s) => s.id === guildId);
		if (index === -1) {
			return;
		}

		if (unavailable) {
			const existingGuild = this.guilds[index];
			const updatedGuild = new GuildRecord({
				...existingGuild.toJSON(),
				unavailable: true,
			});

			this.guilds = [...this.guilds.slice(0, index), updatedGuild, ...this.guilds.slice(index + 1)];
		} else {
			this.guilds = [...this.guilds.slice(0, index), ...this.guilds.slice(index + 1)];
		}
	}

	@action
	sortGuilds(): void {
		this.guilds = [...this.sortGuildArray([...this.guilds])];
	}

	private sortGuildArray(guilds: ReadonlyArray<GuildRecord>): ReadonlyArray<GuildRecord> {
		const guildPositions = UserSettingsStore.guildPositions;

		return [...guilds].sort((a, b) => {
			const aIndex = guildPositions.indexOf(a.id);
			const bIndex = guildPositions.indexOf(b.id);

			if (aIndex === -1 && bIndex === -1) {
				return a.name.localeCompare(b.name);
			}

			if (aIndex === -1) return 1;
			if (bIndex === -1) return -1;

			return aIndex - bIndex;
		});
	}
}

export default new GuildListStore();
