// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildEventID, GuildID, UserID} from '@app/api/BrandedTypes';
import type {GuildEventRow} from '@app/api/database/types/GuildTypes';

export class GuildEvent {
	readonly guildId: GuildID;
	readonly id: GuildEventID;
	readonly creatorId: UserID;
	readonly name: string;
	readonly description: string | null;
	readonly location: string | null;
	readonly startsAt: Date;
	readonly endsAt: Date | null;
	readonly imageHash: string | null;
	readonly createdAt: Date;
	readonly version: number;

	constructor(row: GuildEventRow) {
		this.guildId = row.guild_id;
		this.id = row.event_id;
		this.creatorId = row.creator_id;
		this.name = row.name;
		this.description = row.description ?? null;
		this.location = row.location ?? null;
		this.startsAt = row.starts_at;
		this.endsAt = row.ends_at ?? null;
		this.imageHash = row.image_hash ?? null;
		this.createdAt = row.created_at;
		this.version = row.version;
	}

	toRow(): GuildEventRow {
		return {
			guild_id: this.guildId,
			event_id: this.id,
			creator_id: this.creatorId,
			name: this.name,
			description: this.description,
			location: this.location,
			starts_at: this.startsAt,
			ends_at: this.endsAt,
			image_hash: this.imageHash,
			created_at: this.createdAt,
			version: this.version,
		};
	}
}
