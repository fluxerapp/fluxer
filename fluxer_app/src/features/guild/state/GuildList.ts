// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import UserSettings, {type GuildFolder} from '@app/features/user/state/UserSettings';
import {UNCATEGORIZED_FOLDER_ID} from '@fluxer/constants/src/UserConstants';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {action, makeAutoObservable} from 'mobx';

export type OrganizedItem =
	| {
			type: 'folder';
			folder: GuildFolder;
			guilds: Array<Guild>;
	  }
	| {
			type: 'guild';
			guild: Guild;
	  };

class GuildList {
	guilds: Array<Guild> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	@action
	handleGatewayReady(guilds: ReadonlyArray<GuildReadyData>): void {
		const availableGuilds: Array<Guild> = [];
		for (const guild of guilds) {
			if (guild.unavailable) continue;
			const record = Guilds.getGuild(guild.id);
			if (record) availableGuilds.push(record);
		}
		if (availableGuilds.length > 0) {
			this.sortGuildArrayInPlace(availableGuilds);
			this.guilds = availableGuilds;
		} else {
			this.guilds = [];
		}
	}

	@action
	handleGuild(guild: Guild | GuildReadyData | WireGuild): void {
		if (guild.unavailable) {
			return;
		}
		const guildRecord = Guilds.getGuild(guild.id);
		if (!guildRecord) {
			return;
		}
		const guilds = this.guilds;
		const index = guilds.findIndex((s) => s.id === guild.id);
		if (index >= 0 && guilds[index] === guildRecord) {
			return;
		}
		const next = guilds.slice();
		if (index === -1) {
			next.push(guildRecord);
		} else {
			next[index] = guildRecord;
		}
		this.sortGuildArrayInPlace(next);
		this.guilds = next;
	}

	@action
	handleGuildDelete(guildId: string, unavailable?: boolean): void {
		const index = this.guilds.findIndex((s) => s.id === guildId);
		if (index === -1) {
			return;
		}
		const next = this.guilds.slice();
		if (unavailable) {
			const existingGuild = next[index];
			next[index] = new Guild({
				...existingGuild.toJSON(),
				unavailable: true,
			});
		} else {
			next.splice(index, 1);
		}
		this.guilds = next;
	}

	@action
	sortGuilds(): void {
		const next = this.guilds.slice();
		this.sortGuildArrayInPlace(next);
		this.guilds = next;
	}

	get organizedGuildList(): Array<OrganizedItem> {
		return this.computeOrganizedGuildList();
	}

	getOrganizedGuildList(): Array<OrganizedItem> {
		return this.organizedGuildList;
	}

	private computeOrganizedGuildList(): Array<OrganizedItem> {
		const guildFolders = UserSettings.guildFolders;
		const guildMap = new Map(this.guilds.map((guild) => [guild.id, guild]));
		const result: Array<OrganizedItem> = [];
		const placedGuildIds = new Set<string>();
		const emittedFolderIds = new Set<number | null>();
		for (const folder of guildFolders) {
			if (folder.id !== UNCATEGORIZED_FOLDER_ID && emittedFolderIds.has(folder.id)) {
				continue;
			}
			const folderGuilds: Array<Guild> = [];
			for (const guildId of folder.guildIds) {
				if (placedGuildIds.has(guildId)) continue;
				const guild = guildMap.get(guildId);
				if (guild === undefined) continue;
				placedGuildIds.add(guildId);
				folderGuilds.push(guild);
			}
			if (folderGuilds.length === 0) {
				continue;
			}
			emittedFolderIds.add(folder.id);
			if (folder.id === UNCATEGORIZED_FOLDER_ID) {
				for (const guild of folderGuilds) {
					result.push({type: 'guild', guild});
				}
			} else {
				result.push({type: 'folder', folder, guilds: folderGuilds});
			}
		}
		const unplacedGuilds = this.guilds.filter((guild) => !placedGuildIds.has(guild.id) && !guild.unavailable);
		if (unplacedGuilds.length > 0) {
			const prefix: Array<OrganizedItem> = new Array(unplacedGuilds.length);
			for (let i = 0; i < unplacedGuilds.length; i++) {
				prefix[unplacedGuilds.length - 1 - i] = {type: 'guild', guild: unplacedGuilds[i]};
			}
			return prefix.concat(result);
		}
		return result;
	}

	private sortGuildArrayInPlace(guilds: Array<Guild>): void {
		const guildFolders = UserSettings.guildFolders;
		const positions = new Map<string, number>();
		let nextIndex = 0;
		for (const folder of guildFolders) {
			for (const id of folder.guildIds) {
				if (!positions.has(id)) positions.set(id, nextIndex++);
			}
		}
		guilds.sort((a, b) => {
			const aIndex = positions.get(a.id);
			const bIndex = positions.get(b.id);
			if (aIndex === undefined && bIndex === undefined) {
				return a.name.localeCompare(b.name);
			}
			if (aIndex === undefined) return -1;
			if (bIndex === undefined) return 1;
			return aIndex - bIndex;
		});
	}
}

export default new GuildList();
