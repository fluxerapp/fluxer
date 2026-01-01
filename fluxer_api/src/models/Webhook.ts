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

import type {WebhookRow} from '~/database/CassandraTypes';
import type {ChannelID, GuildID, UserID, WebhookID, WebhookToken} from '../BrandedTypes';

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
