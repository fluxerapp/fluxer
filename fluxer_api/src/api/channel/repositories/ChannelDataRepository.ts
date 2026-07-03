// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '../../BrandedTypes';
import {BatchBuilder, fetchMany, fetchManyInChunks, fetchOne, upsertOne} from '../../database/CassandraQueryExecution';
import {Db} from '../../database/CassandraTypes';
import {buildPatchFromData, executeVersionedUpdate} from '../../database/CassandraVersionedUpdate';
import type {ChannelRow, ThreadMemberRow, ThreadMembersByUserRow, OpenThreadRow} from '../../database/types/ChannelTypes';
import {CHANNEL_COLUMNS} from '../../database/types/ChannelTypes';
import {Logger} from '../../Logger';
import {Channel} from '../../models/Channel';
import {Channels, ChannelsByGuild, OpenThreads, PrivateChannels, ThreadMembers, ThreadMembersByUser, ThreadsByChannel} from '../../Tables';
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

const FETCH_THREADS_BY_CHANNEL = ThreadsByChannel.select({
	where: [ThreadsByChannel.where.eq('channel_id')],
});

const FETCH_THREAD_MEMBER = ThreadMembers.select({
	where: [ThreadMembers.where.eq('thread_id'), ThreadMembers.where.eq('user_id')],
	limit: 1,
});

const FETCH_THREAD_MEMBERS = ThreadMembers.select({
	where: [ThreadMembers.where.eq('thread_id')],
});

const FETCH_JOINED_THREAD_IDS = ThreadMembersByUser.select({
	where: [ThreadMembersByUser.where.eq('user_id')],
});

const OPEN_THREADS_BUCKET = 'all';

const FETCH_OPEN_THREADS = OpenThreads.select({
	where: [OpenThreads.where.eq('bucket')],
});

export class ChannelDataRepository extends IChannelDataRepository {
	async findUnique(channelId: ChannelID): Promise<Channel | null> {
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
		const existing = await fetchOne<ChannelRow>(
			FETCH_CHANNEL_BY_ID.bind({
				channel_id: channelId,
				soft_deleted: false,
			}),
		);
		if (!existing) return;
		const prev = existing.last_message_id ?? null;
		if (prev !== null && messageId <= prev) return;
		const patch: Partial<Record<string, unknown>> = {last_message_id: Db.set(messageId)};
		if (existing.type === 11) {
			patch.thread_message_count = Db.set((existing.thread_message_count ?? 0) + 1);
			patch.thread_total_message_sent = Db.set((existing.thread_total_message_sent ?? 0) + 1);
		}
		await upsertOne(
			Channels.patchByPk({channel_id: channelId, soft_deleted: false}, patch as Parameters<typeof Channels.patchByPk>[1]),
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

	async upsertThreadByChannel(channelId: ChannelID, threadId: ChannelID): Promise<void> {
		await upsertOne(
			ThreadsByChannel.upsertAll({
				channel_id: channelId,
				thread_id: threadId,
			}),
		);
	}

	async deleteThreadByChannel(channelId: ChannelID, threadId: ChannelID): Promise<void> {
		await upsertOne(
			ThreadsByChannel.deleteByPk({
				channel_id: channelId,
				thread_id: threadId,
			}),
		);
	}

	async listThreadsByChannel(channelId: ChannelID, limit = 50, before?: ChannelID): Promise<Array<ChannelID>> {
		const rows = await fetchMany<{channel_id: ChannelID; thread_id: ChannelID}>(
			FETCH_THREADS_BY_CHANNEL.bind({channel_id: channelId}),
		);
		let result = rows.map((r) => r.thread_id);
		if (before !== undefined) {
			result = result.filter((id) => id < before);
		}
		return result.slice(0, limit);
	}

	async upsertThreadMember(params: {
		threadId: ChannelID;
		userId: UserID;
		joinedAt: Date;
		notificationOverride: number | null;
	}): Promise<void> {
		const existing = await fetchOne<{user_id: UserID}>(FETCH_THREAD_MEMBER.bind({thread_id: params.threadId, user_id: params.userId}));
		const isNew = existing === null;
		await Promise.all([
			upsertOne(
				ThreadMembers.upsertAll({
					thread_id: params.threadId,
					user_id: params.userId,
					joined_at: params.joinedAt,
					notification_override: params.notificationOverride,
				} as ThreadMemberRow),
			),
			upsertOne(
				ThreadMembersByUser.upsertAll({
					user_id: params.userId,
					thread_id: params.threadId,
				} as ThreadMembersByUserRow),
			),
		]);
		if (isNew) {
			const thread = await fetchOne<ChannelRow>(FETCH_CHANNEL_BY_ID.bind({channel_id: params.threadId, soft_deleted: false}));
			if (thread) {
				const current = thread.thread_member_count_actual ?? 0;
				if (current < 50) {
					await upsertOne(
						Channels.patchByPk(
							{channel_id: params.threadId, soft_deleted: false},
							{thread_member_count_actual: Db.set(current + 1)} as Parameters<typeof Channels.patchByPk>[1],
						),
					);
				}
			}
		}
	}

	async removeThreadMember(threadId: ChannelID, userId: UserID): Promise<void> {
		const existing = await fetchOne<{user_id: UserID}>(FETCH_THREAD_MEMBER.bind({thread_id: threadId, user_id: userId}));
		await Promise.all([
			upsertOne(ThreadMembers.deleteByPk({thread_id: threadId, user_id: userId})),
			upsertOne(ThreadMembersByUser.deleteByPk({user_id: userId, thread_id: threadId})),
		]);
		if (existing !== null) {
			const thread = await fetchOne<ChannelRow>(FETCH_CHANNEL_BY_ID.bind({channel_id: threadId, soft_deleted: false}));
			if (thread) {
				const current = thread.thread_member_count_actual ?? 0;
				if (current > 0) {
					await upsertOne(
						Channels.patchByPk(
							{channel_id: threadId, soft_deleted: false},
							{thread_member_count_actual: Db.set(current - 1)} as Parameters<typeof Channels.patchByPk>[1],
						),
					);
				}
			}
		}
	}

	async listJoinedThreadIds(userId: UserID): Promise<Array<ChannelID>> {
		const rows = await fetchMany<ThreadMembersByUserRow>(FETCH_JOINED_THREAD_IDS.bind({user_id: userId}));
		return rows.map((r) => r.thread_id);
	}

	async listThreadMembers(threadId: ChannelID): Promise<Array<{userId: UserID; joinedAt: Date}>> {
		const rows = await fetchMany<ThreadMemberRow>(FETCH_THREAD_MEMBERS.bind({thread_id: threadId}));
		return rows.map((r) => ({userId: r.user_id, joinedAt: r.joined_at}));
	}

	async isThreadMember(threadId: ChannelID, userId: UserID): Promise<boolean> {
		const row = await fetchOne<ThreadMemberRow>(FETCH_THREAD_MEMBER.bind({thread_id: threadId, user_id: userId}));
		return row !== null;
	}

	async listExpiredOpenThreads(now: Date, limit: number): Promise<Array<ChannelID>> {
		const rows = await fetchMany<OpenThreadRow>(FETCH_OPEN_THREADS.bind({bucket: OPEN_THREADS_BUCKET}));
		return rows
			.filter((row) => row.expires_at != null && row.expires_at <= now)
			.slice(0, limit)
			.map((row) => row.thread_id);
	}

	async upsertOpenThread(threadId: ChannelID, expiresAt: Date | null): Promise<void> {
		await upsertOne(
			OpenThreads.upsertAll({
				bucket: OPEN_THREADS_BUCKET,
				thread_id: threadId,
				expires_at: expiresAt,
			} as OpenThreadRow),
		);
	}

	async deleteOpenThread(threadId: ChannelID): Promise<void> {
		await upsertOne(OpenThreads.deleteByPk({bucket: OPEN_THREADS_BUCKET, thread_id: threadId}));
	}
}
