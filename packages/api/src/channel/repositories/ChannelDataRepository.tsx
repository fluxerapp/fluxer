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

import type {ChannelID, GuildID, MessageID} from '@fluxer/api/src/BrandedTypes';
import {IChannelDataRepository} from '@fluxer/api/src/channel/repositories/IChannelDataRepository';
import {
	BatchBuilder,
	buildPatchFromData,
	Db,
	executeConditional,
	executeVersionedUpdate,
	fetchMany,
	fetchManyInChunks,
	fetchOne,
} from '@fluxer/api/src/database/Cassandra';
import type {ChannelRow} from '@fluxer/api/src/database/types/ChannelTypes';
import {CHANNEL_COLUMNS} from '@fluxer/api/src/database/types/ChannelTypes';
import {Logger} from '@fluxer/api/src/Logger';
import {Channel} from '@fluxer/api/src/models/Channel';
import {Channels, ChannelsByGuild} from '@fluxer/api/src/Tables';

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

const DEFAULT_CAS_RETRIES = 8;

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
			await fetchOne(
				ChannelsByGuild.upsertAll({
					guild_id: data.guild_id,
					channel_id: channelId,
				}),
			);
		}

		return new Channel({...data, version: result.finalVersion ?? 0});
	}

	async updateLastMessageId(channelId: ChannelID, messageId: MessageID): Promise<void> {
		for (let i = 0; i < DEFAULT_CAS_RETRIES; i++) {
			const existing = await fetchOne<ChannelRow>(
				FETCH_CHANNEL_BY_ID.bind({
					channel_id: channelId,
					soft_deleted: false,
				}),
			);

			if (!existing) return;

			const prev = existing.last_message_id ?? null;
			if (prev !== null && messageId <= prev) return;

			const q = Channels.patchByPkIf(
				{channel_id: channelId, soft_deleted: false},
				{last_message_id: Db.set(messageId)},
				{col: 'last_message_id', expectedParam: 'prev_last_message_id', expectedValue: prev},
			);

			const res = await executeConditional(q);
			if (res.applied) return;
		}

		Logger.warn(
			{channelId: channelId.toString(), messageId: messageId.toString()},
			'Failed to advance Channels.last_message_id after retries',
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
		const guildChannels = await fetchMany<{channel_id: bigint}>(
			FETCH_GUILD_CHANNELS_BY_GUILD_ID.bind({guild_id: guildId}),
		);
		if (guildChannels.length === 0) return [];

		const channelIds = guildChannels.map((c) => c.channel_id);

		const channels = await fetchManyInChunks<ChannelRow>(FETCH_CHANNELS_BY_IDS, channelIds, (chunk) => ({
			channel_ids: chunk,
			soft_deleted: false,
		}));

		return channels.map((channel) => new Channel(channel));
	}

	async countGuildChannels(guildId: GuildID): Promise<number> {
		const guildChannels = await fetchMany<{channel_id: bigint}>(
			FETCH_GUILD_CHANNELS_BY_GUILD_ID.bind({guild_id: guildId}),
		);
		return guildChannels.length;
	}
}
