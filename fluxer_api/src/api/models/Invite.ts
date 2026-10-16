// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, InviteCode, UserID} from '@app/api/BrandedTypes';
import type {InviteRow} from '@app/api/database/types/ChannelTypes';

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
