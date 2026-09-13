// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, RoleID} from '@app/api/BrandedTypes';
import type {GuildRoleRow} from '@app/api/database/types/GuildTypes';

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
