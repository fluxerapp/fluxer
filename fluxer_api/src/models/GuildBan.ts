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

import type {GuildBanRow} from '~/database/CassandraTypes';
import type {GuildID, UserID} from '../BrandedTypes';

export class GuildBan {
	readonly guildId: GuildID;
	readonly userId: UserID;
	readonly moderatorId: UserID;
	readonly bannedAt: Date;
	readonly expiresAt: Date | null;
	readonly reason: string | null;
	readonly ipAddress: string | null;

	constructor(row: GuildBanRow) {
		this.guildId = row.guild_id;
		this.userId = row.user_id;
		this.moderatorId = row.moderator_id;
		this.bannedAt = row.banned_at;
		this.expiresAt = row.expires_at ?? null;
		this.reason = row.reason ?? null;
		this.ipAddress = row.ip ?? null;
	}

	toRow(): GuildBanRow {
		return {
			guild_id: this.guildId,
			user_id: this.userId,
			moderator_id: this.moderatorId,
			banned_at: this.bannedAt,
			expires_at: this.expiresAt,
			reason: this.reason,
			ip: this.ipAddress,
		};
	}
}
