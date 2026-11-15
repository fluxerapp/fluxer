// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createChannelID, createGuildID, type GuildID, type MessageID} from '@app/api/BrandedTypes';
import type {ThreadState} from '@app/api/models/ThreadState';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export const THREAD_ARCHIVE_GUILDS_KEY = 'thread_archive_guilds';

export function threadArchiveQueueKey(guildId: GuildID): string {
	return `thread_archive:${guildId}`;
}

export function threadAutoArchiveDueAt(state: ThreadState, lastMessageId: MessageID | null): number {
	const lastActivity = lastMessageId ? snowflakeToDate(lastMessageId).getTime() : 0;
	const archivedAt = state.archiveTimestamp?.getTime() ?? state.createdAt.getTime();
	return Math.max(lastActivity, archivedAt) + state.autoArchiveDuration * 60_000;
}

export class KVThreadAutoArchiveQueueService {
	constructor(private readonly kvClient: IKVProvider) {}

	async schedule(state: ThreadState, lastMessageId: MessageID | null): Promise<void> {
		if (state.archived || state.isPinned) {
			await this.remove(state.guildId, state.threadId);
			return;
		}
		await this.kvClient.zadd(
			threadArchiveQueueKey(state.guildId),
			threadAutoArchiveDueAt(state, lastMessageId),
			state.threadId.toString(),
		);
		await this.kvClient.sadd(THREAD_ARCHIVE_GUILDS_KEY, state.guildId.toString());
	}

	async remove(guildId: GuildID, threadId: ChannelID): Promise<void> {
		const key = threadArchiveQueueKey(guildId);
		await this.kvClient.zrem(key, threadId.toString());
		if ((await this.kvClient.zcard(key)) !== 0) return;
		await this.kvClient.srem(THREAD_ARCHIVE_GUILDS_KEY, guildId.toString());
		if ((await this.kvClient.zcard(key)) !== 0) {
			await this.kvClient.sadd(THREAD_ARCHIVE_GUILDS_KEY, guildId.toString());
		}
	}

	async removeGuild(guildId: GuildID): Promise<void> {
		await this.kvClient.del(threadArchiveQueueKey(guildId));
		await this.kvClient.srem(THREAD_ARCHIVE_GUILDS_KEY, guildId.toString());
	}

	async getDue(guildId: GuildID, nowMs: number, limit: number): Promise<Array<ChannelID>> {
		const members = await this.kvClient.zrangebyscore(threadArchiveQueueKey(guildId), '-inf', nowMs, 'LIMIT', 0, limit);
		return members.flatMap((member) => (/^\d+$/.test(member) ? [createChannelID(BigInt(member))] : []));
	}

	async listGuilds(): Promise<Array<GuildID>> {
		const members = await this.kvClient.smembers(THREAD_ARCHIVE_GUILDS_KEY);
		return members.flatMap((member) => (/^\d+$/.test(member) ? [createGuildID(BigInt(member))] : []));
	}
}
