// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID, WebhookID, WebhookToken} from '@app/api/BrandedTypes';
import type {WebhookRow} from '@app/api/database/types/ChannelTypes';

export class Webhook {
	readonly id: WebhookID;
	readonly token: WebhookToken;
	readonly type: number;
	readonly guildId: GuildID | null;
	readonly channelId: ChannelID | null;
	readonly creatorId: UserID | null;
	readonly name: string;
	readonly avatarHash: string | null;
	readonly version: number;

	constructor(row: WebhookRow) {
		this.id = row.webhook_id;
		this.token = row.webhook_token;
		this.type = row.type;
		this.guildId = row.guild_id ?? null;
		this.channelId = row.channel_id ?? null;
		this.creatorId = row.creator_id ?? null;
		this.name = row.name;
		this.avatarHash = row.avatar_hash ?? null;
		this.version = row.version;
	}

	toRow(): WebhookRow {
		return {
			webhook_id: this.id,
			webhook_token: this.token,
			type: this.type,
			guild_id: this.guildId,
			channel_id: this.channelId,
			creator_id: this.creatorId,
			name: this.name,
			avatar_hash: this.avatarHash,
			version: this.version,
		};
	}
}
