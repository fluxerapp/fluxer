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

import type {ChannelID, GuildID, MessageID} from '~/BrandedTypes';
import type {ChannelRow} from '~/database/CassandraTypes';
import type {Channel} from '~/Models';

export abstract class IChannelDataRepository {
	abstract findUnique(channelId: ChannelID): Promise<Channel | null>;
	abstract upsert(data: ChannelRow): Promise<Channel>;
	abstract updateLastMessageId(channelId: ChannelID, messageId: MessageID): Promise<void>;
	abstract delete(channelId: ChannelID, guildId?: GuildID): Promise<void>;
	abstract listGuildChannels(guildId: GuildID): Promise<Array<Channel>>;
	abstract countGuildChannels(guildId: GuildID): Promise<number>;
}
