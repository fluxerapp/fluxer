// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '../../BrandedTypes';
import {BatchBuilder, fetchMany, fetchManyInChunks, fetchOne, upsertOne} from '../../database/CassandraQueryExecution';
import {Db} from '../../database/CassandraTypes';
import {buildPatchFromData, executeVersionedUpdate} from '../../database/CassandraVersionedUpdate';
import type {ChannelRow} from '../../database/types/ChannelTypes';
import {CHANNEL_COLUMNS} from '../../database/types/ChannelTypes';
import {Logger} from '../../Logger';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import {Channel} from '../../models/Channel';
import {Channels, ChannelsByGuild, PrivateChannels} from '../../Tables';
import {
	privateChannelFanOutTargets,
	privateChannelLastMessageIdPatch,
	privateChannelMetadataPatch,
} from '../PrivateChannelSnapshot';
import {IChannelDataRepository} from './IChannelDataRepository';

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
		if (data.guild_id) {
			await upsertOne(
				ChannelsByGuild.upsertAll({
					guild_id: data.guild_id,
					channel_id: channelId,
				}),
			);
		}
		const finalRow: ChannelRow = {...data, version: result.finalVersion ?? 0};
		await this.writeThroughPrivateChannelMetadata(finalRow);
		return new Channel(finalRow);
	}

	async updateLastMessageId(channelId: ChannelID, messageId: MessageID): Promise<void> {
		this.requestCache?.channels.delete(channelId);
		const existing = await fetchOne<ChannelRow>(
			FETCH_CHANNEL_BY_ID.bind({
				channel_id: channelId,
				soft_deleted: false,
			}),
		);
		if (!existing) return;
		const prev = existing.last_message_id ?? null;
		if (prev !== null && messageId <= prev) return;
		await upsertOne(
			Channels.patchByPk({channel_id: channelId, soft_deleted: false}, {last_message_id: Db.set(messageId)}),
		);
		void this.fanOutPrivateChannelLastMessageId(existing, messageId);
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

	async delete(channelId: ChannelID, guildId?: GuildID): Promise<void> {
		this.requestCache?.channels.delete(channelId);
		const batch = new BatchBuilder();
		batch.addPrepared(
			Channels.deleteByPk({
				channel_id: channelId,
				soft_deleted: false,
			}),
		);
		if (guildId) {
			batch.addPrepared(
				ChannelsByGuild.deleteByPk({
					guild_id: guildId,
					channel_id: channelId,
				}),
			);
		}
		await batch.execute();
	}

	async listGuildChannels(guildId: GuildID): Promise<Array<Channel>> {
		const guildChannels = await fetchMany<{
			channel_id: bigint;
		}>(FETCH_GUILD_CHANNELS_BY_GUILD_ID.bind({guild_id: guildId}));
		if (guildChannels.length === 0) return [];
		const channelIds = guildChannels.map((c) => c.channel_id);
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
		const guildChannels = await fetchMany<{
			channel_id: bigint;
		}>(FETCH_GUILD_CHANNELS_BY_GUILD_ID.bind({guild_id: guildId}));
		return guildChannels.length;
	}
}
