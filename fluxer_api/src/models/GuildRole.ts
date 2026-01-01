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

import type {GuildRoleRow} from '~/database/CassandraTypes';
import type {GuildID, RoleID} from '../BrandedTypes';

export class GuildRole {
	readonly guildId: GuildID;
	readonly id: RoleID;
	readonly name: string;
	readonly permissions: bigint;
	readonly position: number;
	readonly hoistPosition: number | null;
	readonly color: number;
	readonly iconHash: string | null;
	readonly unicodeEmoji: string | null;
	readonly isHoisted: boolean;
	readonly isMentionable: boolean;
	readonly version: number;

	constructor(row: GuildRoleRow) {
		this.guildId = row.guild_id;
		this.id = row.role_id;
		this.name = row.name;
		this.permissions = row.permissions;
		this.position = row.position;
		this.hoistPosition = row.hoist_position ?? null;
		this.color = row.color ?? 0;
		this.iconHash = row.icon_hash ?? null;
		this.unicodeEmoji = row.unicode_emoji ?? null;
		this.isHoisted = row.hoist ?? false;
		this.isMentionable = row.mentionable ?? false;
		this.version = row.version;
	}

	get effectiveHoistPosition(): number {
		return this.hoistPosition ?? this.position;
	}

	toRow(): GuildRoleRow {
		return {
			guild_id: this.guildId,
			role_id: this.id,
			name: this.name,
			permissions: this.permissions,
			position: this.position,
			hoist_position: this.hoistPosition,
			color: this.color,
			icon_hash: this.iconHash,
			unicode_emoji: this.unicodeEmoji,
			hoist: this.isHoisted,
			mentionable: this.isMentionable,
			version: this.version,
		};
	}
}
