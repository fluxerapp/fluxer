// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {GuildBanRow} from '@app/api/database/types/GuildTypes';

export class GuildBan {
	readonly guildId: GuildID;
	readonly userId: UserID;
	readonly moderatorId: UserID;
	readonly bannedAt: Date;
	readonly expiresAt: Date | null;
	readonly reason: string | null;
	readonly ipAddress: string | null;
	readonly email: string | null;

	constructor(row: GuildBanRow) {
		this.guildId = row.guild_id;
		this.userId = row.user_id;
		this.moderatorId = row.moderator_id;
		this.bannedAt = row.banned_at;
		this.expiresAt = row.expires_at ?? null;
		this.reason = row.reason ?? null;
		this.ipAddress = row.ip ?? null;
		this.email = row.email ?? null;
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
			email: this.email,
		};
	}
}
