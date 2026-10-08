// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, channelIdToMessageId, type GuildID, type MessageID, type UserID} from '@app/api/BrandedTypes';
import {
	privateChannelFanOutTargets,
	privateChannelLastMessageIdPatch,
	privateChannelMetadataPatch,
} from '@app/api/channel/PrivateChannelSnapshot';
import {type GuildChannelListMode, IChannelDataRepository} from '@app/api/channel/repositories/IChannelDataRepository';
import {
	BatchBuilder,
	executeConditional,
	fetchMany,
	fetchManyInChunks,
	fetchOne,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import {buildPatchFromData, executeVersionedUpdate} from '@app/api/database/CassandraVersionedUpdate';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import {CHANNEL_COLUMNS} from '@app/api/database/types/ChannelTypes';
import type {ThreadStatsRow} from '@app/api/database/types/ThreadTypes';
import {guildActive, isTainted} from '@app/api/experiment/ChannelThreadsGate';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {Channel} from '@app/api/models/Channel';
import {Channels, ChannelsByGuild, PrivateChannels, ThreadOnlyChannelsByGuild, ThreadStats} from '@app/api/Tables';
import {THREAD_CHANNEL_TYPES, THREAD_ONLY_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';

const FETCH_CHANNEL_BY_ID = Channels.select({
	where: [Channels.where.eq('channel_id'), Channels.where.eq('soft_deleted')],
	limit: 1,
});
const FETCH_CHANNELS_BY_IDS = Channels.select({
	where: [Channels.where.in('channel_id', 'channel_ids'), Channels.where.eq('soft_deleted')],
});
const FETCH_GUILD_CHANNELS_BY_GUILD_ID = ChannelsByGuild.select({
	where: ChannelsByGuild.where.eq('guild_id'),
});
const FETCH_THREAD_ONLY_CHANNELS_BY_GUILD_ID = ThreadOnlyChannelsByGuild.select({
	where: ThreadOnlyChannelsByGuild.where.eq('guild_id'),
});
const THREAD_STATS_CAS_ATTEMPTS = 16;
const FETCH_THREAD_STATS = ThreadStats.select({where: ThreadStats.where.eq('thread_id'), limit: 1});
const FETCH_OPEN_PRIVATE_CHANNEL_TARGET = PrivateChannels.selectCql({
	columns: ['user_id'],
	where: [PrivateChannels.where.eq('user_id'), PrivateChannels.where.eq('channel_id')],
	limit: 1,
});

export class ChannelDataRepository extends IChannelDataRepository {
	constructor(private readonly requestCache?: RequestCache) {
		super();
	}

	async findUnique(channelId: ChannelID): Promise<Channel | null> {
		const prefetched = this.requestCache?.takeChannel(channelId);
		if (prefetched !== undefined) {
			return prefetched;
		}
		const channel = await fetchOne<ChannelRow>(
			FETCH_CHANNEL_BY_ID.bind({
				channel_id: channelId,
				soft_deleted: false,
			}),
		);
		return channel ? new Channel(channel) : null;
	}

	async upsert(data: ChannelRow, oldData?: ChannelRow | null): Promise<Channel> {
		const channelId = data.channel_id;
		this.requestCache?.channels.delete(channelId);
		const result = await executeVersionedUpdate<ChannelRow, 'channel_id' | 'soft_deleted'>(
			async () => fetchOne<ChannelRow>(FETCH_CHANNEL_BY_ID.bind({channel_id: channelId, soft_deleted: false})),
			(current) => ({
				pk: {channel_id: channelId, soft_deleted: false},
				patch: buildPatchFromData(data, current, CHANNEL_COLUMNS, ['channel_id', 'soft_deleted']),
			}),
			Channels,
			{initialData: oldData},
		);
		if (data.guild_id && !THREAD_CHANNEL_TYPES.has(data.type)) {
			await upsertOne(
				THREAD_ONLY_CHANNEL_TYPES.has(data.type)
					? ThreadOnlyChannelsByGuild.upsertAll({guild_id: data.guild_id, channel_id: channelId})
					: ChannelsByGuild.upsertAll({
							guild_id: data.guild_id,
							channel_id: channelId,
						}),
			);
		}
		const finalRow: ChannelRow = {...data, version: result.finalVersion ?? 0};
		await this.writeThroughPrivateChannelMetadata(finalRow);
		return new Channel(finalRow);
	}

	async updateLastMessageId(channelId: ChannelID, messageId: MessageID, opts?: {isInsert?: boolean}): Promise<void> {
		this.requestCache?.channels.delete(channelId);
		const existing = await fetchOne<ChannelRow>(
			FETCH_CHANNEL_BY_ID.bind({
				channel_id: channelId,
				soft_deleted: false,
			}),
		);
		if (!existing) return;
		if (opts?.isInsert && THREAD_CHANNEL_TYPES.has(existing.type) && messageId !== channelIdToMessageId(channelId)) {
			await this.adjustThreadStats(channelId, 1, 1);
		}
		const prev = existing.last_message_id ?? null;
		if (prev !== null && messageId <= prev) return;
		await upsertOne(
			Channels.patchByPk({channel_id: channelId, soft_deleted: false}, {last_message_id: Db.set(messageId)}),
		);
		void this.fanOutPrivateChannelLastMessageId(existing, messageId);
	}

	async adjustThreadStats(threadId: ChannelID, messageDelta: number, sentDelta: number): Promise<void> {
		for (let attempt = 0; attempt < THREAD_STATS_CAS_ATTEMPTS; attempt++) {
			const stats = await fetchOne<ThreadStatsRow>(FETCH_THREAD_STATS.bind({thread_id: threadId}));
			const messageCount = Math.max(0, (stats?.message_count ?? 0) + messageDelta);
			const totalMessageSent = Math.max(0, (stats?.total_message_sent ?? 0) + sentDelta);
			const applied = await executeConditional(
				stats
					? ThreadStats.conditionalPatchByPk(
							{thread_id: threadId},
							{message_count: Db.set(messageCount), total_message_sent: Db.set(totalMessageSent)},
							{message_count: stats.message_count ?? null, total_message_sent: stats.total_message_sent ?? null},
						)
					: ThreadStats.insertIfNotExists({
							thread_id: threadId,
							message_count: messageCount,
							total_message_sent: totalMessageSent,
						}),
			);
			if (applied) return;
		}
		Logger.warn({threadId: threadId.toString()}, 'Gave up adjusting thread stats under contention');
	}

	async patchIndexedAt(channelId: ChannelID, indexedAt: Date): Promise<void> {
		this.requestCache?.channels.delete(channelId);
		await upsertOne(Channels.patchByPk({channel_id: channelId, soft_deleted: false}, {indexed_at: Db.set(indexedAt)}));
	}

	private async writeThroughPrivateChannelMetadata(row: ChannelRow): Promise<void> {
		try {
			const targets = await this.listOpenPrivateChannelTargets(row);
			if (targets.length === 0) return;
			const patch = privateChannelMetadataPatch(row);
			const results = await Promise.allSettled(
				targets.map((userId) =>
					upsertOne(PrivateChannels.patchByPk({user_id: userId, channel_id: row.channel_id}, patch)),
				),
			);
			this.logFanOutFailures(results, row.channel_id, 'metadata');
		} catch (error) {
			this.logFanOutError(error, row.channel_id, 'metadata');
		}
	}

	private async fanOutPrivateChannelLastMessageId(existing: ChannelRow, messageId: MessageID): Promise<void> {
		try {
			const targets = await this.listOpenPrivateChannelTargets(existing);
			if (targets.length === 0) return;
			const patch = privateChannelLastMessageIdPatch(messageId);
			const results = await Promise.allSettled(
				targets.map((userId) =>
					upsertOne(PrivateChannels.patchByPk({user_id: userId, channel_id: existing.channel_id}, patch)),
				),
			);
			this.logFanOutFailures(results, existing.channel_id, 'last_message_id');
		} catch (error) {
			this.logFanOutError(error, existing.channel_id, 'last_message_id');
		}
	}

	private async listOpenPrivateChannelTargets(row: ChannelRow): Promise<Array<UserID>> {
		const targets = privateChannelFanOutTargets(row);
		if (targets.length === 0) return [];
		const openTargets = await Promise.all(
			targets.map(async (userId) => {
				const existing = await fetchOne<{user_id: UserID}>(FETCH_OPEN_PRIVATE_CHANNEL_TARGET, {
					user_id: userId,
					channel_id: row.channel_id,
				});
				return existing ? userId : null;
			}),
		);
		return openTargets.filter((userId): userId is UserID => userId != null);
	}

	private logFanOutFailures(results: Array<PromiseSettledResult<unknown>>, channelId: ChannelID, kind: string): void {
		const failures = results.filter((result) => result.status === 'rejected');
		if (failures.length === 0) return;
		Logger.warn(
			{
				channelId: channelId.toString(),
				kind,
				failureCount: failures.length,
				error:
					failures[0].status === 'rejected' && failures[0].reason instanceof Error
						? failures[0].reason.message
						: String(failures[0].status === 'rejected' ? failures[0].reason : ''),
			},
			'Failed to write through private channel snapshot fan-out',
		);
	}

	private logFanOutError(error: unknown, channelId: ChannelID, kind: string): void {
		Logger.warn(
			{
				channelId: channelId.toString(),
				kind,
				error: error instanceof Error ? error.message : String(error),
			},
			'Failed to write through private channel snapshot fan-out',
		);
	}

	async delete(channelId: ChannelID, guildId?: GuildID, type?: number): Promise<void> {
		this.requestCache?.channels.delete(channelId);
		const batch = new BatchBuilder();
		batch.addPrepared(
			Channels.deleteByPk({
				channel_id: channelId,
				soft_deleted: false,
			}),
		);
		if (guildId && (type === undefined || !THREAD_CHANNEL_TYPES.has(type))) {
			batch.addPrepared(
				type !== undefined && THREAD_ONLY_CHANNEL_TYPES.has(type)
					? ThreadOnlyChannelsByGuild.deleteByPk({guild_id: guildId, channel_id: channelId})
					: ChannelsByGuild.deleteByPk({
							guild_id: guildId,
							channel_id: channelId,
						}),
			);
		}
		await batch.execute();
	}

	async listGuildChannels(guildId: GuildID, mode: GuildChannelListMode): Promise<Array<Channel>> {
		const includeThreadOnly =
			mode === 'enrolled' ? guildActive(guildId) : await isTainted(guildId, {fresh: mode === 'complete'});
		const [guildChannels, threadOnlyChannels] = await Promise.all([
			fetchMany<{channel_id: bigint}>(FETCH_GUILD_CHANNELS_BY_GUILD_ID.bind({guild_id: guildId})),
			includeThreadOnly
				? fetchMany<{channel_id: bigint}>(FETCH_THREAD_ONLY_CHANNELS_BY_GUILD_ID.bind({guild_id: guildId}))
				: Promise.resolve([]),
		]);
		if (guildChannels.length === 0 && threadOnlyChannels.length === 0) return [];
		const channelIds = [...guildChannels, ...threadOnlyChannels].map((c) => c.channel_id);
		const channels = await fetchManyInChunks<ChannelRow>(FETCH_CHANNELS_BY_IDS, channelIds, (chunk) => ({
			channel_ids: chunk,
			soft_deleted: false,
		}));
		return channels.map((channel) => new Channel(channel));
	}

	async listChannels(channelIds: Array<ChannelID>): Promise<Array<Channel>> {
		if (channelIds.length === 0) return [];
		const channels = await fetchManyInChunks<ChannelRow>(FETCH_CHANNELS_BY_IDS, channelIds, (chunk) => ({
			channel_ids: chunk,
			soft_deleted: false,
		}));
		return channels.map((channel) => new Channel(channel));
	}

	async countGuildChannels(guildId: GuildID): Promise<number> {
		const [guildChannels, threadOnlyChannels] = await Promise.all([
			fetchMany<{channel_id: bigint}>(FETCH_GUILD_CHANNELS_BY_GUILD_ID.bind({guild_id: guildId})),
			guildActive(guildId)
				? fetchMany<{channel_id: bigint}>(FETCH_THREAD_ONLY_CHANNELS_BY_GUILD_ID.bind({guild_id: guildId}))
				: Promise.resolve([]),
		]);
		return guildChannels.length + threadOnlyChannels.length;
	}
}
