// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID} from '@app/api/BrandedTypes';
import type {GuildFolder} from '@app/api/database/types/UserTypes';
import {DEFAULT_GUILD_FOLDER_ICON, type GuildFolderIcon} from '@fluxer/constants/src/UserConstants';

export class UserGuildFolder {
	readonly folderId: number;
	readonly name: string | null;
	readonly color: number | null;
	readonly flags: number;
	readonly icon: GuildFolderIcon;
	readonly guildIds: Array<GuildID>;

	constructor(folder: GuildFolder) {
		this.folderId = folder.folder_id;
		this.name = folder.name ?? null;
		this.color = folder.color ?? null;
		this.flags = folder.flags ?? 0;
		this.icon = folder.icon ?? DEFAULT_GUILD_FOLDER_ICON;
		this.guildIds = folder.guild_ids ?? [];
	}

	toGuildFolder(): GuildFolder {
		return {
			folder_id: this.folderId,
			name: this.name,
			color: this.color,
			flags: this.flags,
			icon: this.icon,
			guild_ids: this.guildIds.length > 0 ? this.guildIds : null,
		};
	}
}
