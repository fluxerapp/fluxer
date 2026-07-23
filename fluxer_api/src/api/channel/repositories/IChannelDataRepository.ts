// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '../../BrandedTypes';
import type {ChannelRow} from '../../database/types/ChannelTypes';
import type {Channel} from '../../models/Channel';

export abstract class IChannelDataRepository {
	abstract findUnique(channelId: ChannelID): Promise<Channel | null>;

	abstract upsert(data: ChannelRow): Promise<Channel>;

	abstract updateLastMessageId(channelId: ChannelID, messageId: MessageID): Promise<void>;

	abstract delete(channelId: ChannelID, guildId?: GuildID): Promise<void>;

	abstract listGuildChannels(guildId: GuildID): Promise<Array<Channel>>;

	abstract listChannels(channelIds: Array<ChannelID>): Promise<Array<Channel>>;

	abstract countGuildChannels(guildId: GuildID): Promise<number>;

	abstract upsertThreadByChannel(channelId: ChannelID, threadId: ChannelID): Promise<void>;

	abstract deleteThreadByChannel(channelId: ChannelID, threadId: ChannelID): Promise<void>;

	abstract listThreadsByChannel(channelId: ChannelID, limit?: number, before?: ChannelID): Promise<Array<ChannelID>>;

	abstract upsertThreadMember(params: {
		threadId: ChannelID;
		userId: UserID;
		joinedAt: Date;
		notificationOverride: number | null;
	}): Promise<void>;

	abstract removeThreadMember(threadId: ChannelID, userId: UserID): Promise<void>;

	abstract listJoinedThreadIds(userId: UserID): Promise<Array<ChannelID>>;

	abstract listThreadMembers(threadId: ChannelID): Promise<Array<{userId: UserID; joinedAt: Date}>>;

	abstract isThreadMember(threadId: ChannelID, userId: UserID): Promise<boolean>;

	abstract listExpiredOpenThreads(now: Date, limit: number): Promise<Array<ChannelID>>;

	abstract upsertOpenThread(threadId: ChannelID, expiresAt: Date | null): Promise<void>;

	abstract deleteOpenThread(threadId: ChannelID): Promise<void>;
}
