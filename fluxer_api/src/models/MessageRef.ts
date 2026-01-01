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

import type {MessageReference} from '~/database/CassandraTypes';
import type {ChannelID, GuildID, MessageID} from '../BrandedTypes';

export class MessageRef {
	readonly channelId: ChannelID;
	readonly messageId: MessageID;
	readonly guildId: GuildID | null;
	readonly type: number;

	constructor(ref: MessageReference) {
		this.channelId = ref.channel_id;
		this.messageId = ref.message_id;
		this.guildId = ref.guild_id ?? null;
		this.type = ref.type;
	}

	toMessageReference(): MessageReference {
		return {
			channel_id: this.channelId,
			message_id: this.messageId,
			guild_id: this.guildId,
			type: this.type,
		};
	}
}
