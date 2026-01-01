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

import type {InviteRow} from '~/database/CassandraTypes';
import type {ChannelID, GuildID, InviteCode, UserID} from '../BrandedTypes';

export class Invite {
	readonly code: InviteCode;
	readonly type: number;
	readonly guildId: GuildID | null;
	readonly channelId: ChannelID | null;
	readonly inviterId: UserID | null;
	readonly createdAt: Date;
	readonly uses: number;
	readonly maxUses: number;
	readonly maxAge: number;
	readonly temporary: boolean;
	readonly version: number;

	constructor(row: InviteRow) {
		this.code = row.code;
		this.type = row.type;
		this.guildId = row.guild_id ?? null;
		this.channelId = row.channel_id ?? null;
		this.inviterId = row.inviter_id ?? null;
		this.createdAt = row.created_at;
		this.uses = row.uses ?? 0;
		this.maxUses = row.max_uses ?? 0;
		this.maxAge = row.max_age ?? 0;
		this.temporary = row.temporary ?? false;
		this.version = row.version;
	}

	toRow(): InviteRow {
		return {
			code: this.code,
			type: this.type,
			guild_id: this.guildId,
			channel_id: this.channelId,
			inviter_id: this.inviterId,
			created_at: this.createdAt,
			uses: this.uses,
			max_uses: this.maxUses,
			max_age: this.maxAge,
			temporary: this.temporary,
			version: this.version,
		};
	}
}
