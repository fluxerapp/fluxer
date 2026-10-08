// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID} from '@app/api/BrandedTypes';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import type {Channel} from '@app/api/models/Channel';

export type GuildChannelListMode = 'enrolled' | 'maintenance' | 'complete';

export abstract class IChannelDataRepository {
	abstract findUnique(channelId: ChannelID): Promise<Channel | null>;

	abstract upsert(data: ChannelRow, oldData?: ChannelRow | null): Promise<Channel>;

	abstract updateLastMessageId(channelId: ChannelID, messageId: MessageID, opts?: {isInsert?: boolean}): Promise<void>;

	abstract delete(channelId: ChannelID, guildId?: GuildID, type?: number): Promise<void>;

	abstract listGuildChannels(guildId: GuildID, mode: GuildChannelListMode): Promise<Array<Channel>>;

	abstract listChannels(channelIds: Array<ChannelID>): Promise<Array<Channel>>;

	abstract countGuildChannels(guildId: GuildID): Promise<number>;

	abstract patchIndexedAt(channelId: ChannelID, indexedAt: Date): Promise<void>;
}
