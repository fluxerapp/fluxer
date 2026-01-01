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

import type {GuildFolder} from '~/database/CassandraTypes';
import type {GuildID} from '../BrandedTypes';

export class UserGuildFolder {
	readonly folderId: number;
	readonly name: string | null;
	readonly color: number | null;
	readonly guildIds: Array<GuildID>;

	constructor(folder: GuildFolder) {
		this.folderId = folder.folder_id;
		this.name = folder.name ?? null;
		this.color = folder.color ?? null;
		this.guildIds = folder.guild_ids ?? [];
	}

	toGuildFolder(): GuildFolder {
		return {
			folder_id: this.folderId,
			name: this.name,
			color: this.color,
			guild_ids: this.guildIds.length > 0 ? this.guildIds : null,
		};
	}
}
