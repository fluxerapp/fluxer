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

import type {ChannelID, MessageID, UserID} from '~/BrandedTypes';
import {ChannelTypes} from '~/Constants';
import {BatchBuilder, deleteOneOrMany, fetchMany, fetchManyInChunks, fetchOne, upsertOne} from '~/database/Cassandra';
import type {ChannelRow, DmStateRow, PrivateChannelRow} from '~/database/CassandraTypes';
import {Channel} from '~/Models';
import {Channels, DmStates, PinnedDms, PrivateChannels, ReadStates} from '~/Tables';
import type {IUserChannelRepository, PrivateChannelSummary} from './IUserChannelRepository';

interface PinnedDmRow {
	user_id: UserID;
	channel_id: ChannelID;
	sort_order: number;
}

const CHECK_PRIVATE_CHANNEL_CQL = PrivateChannels.selectCql({
	columns: ['channel_id'],
	where: [PrivateChannels.where.eq('user_id'), PrivateChannels.where.eq('channel_id')],
});

const FETCH_CHANNEL_CQL = Channels.selectCql({
	columns: [
		'channel_id',
		'guild_id',
		'type',
		'name',
		'topic',
		'icon_hash',
		'url',
		'parent_id',
		'position',
		'owner_id',
		'recipient_ids',
		'nsfw',
		'rate_limit_per_user',
		'bitrate',
		'user_limit',
		'rtc_region',
		'last_message_id',
		'last_pin_timestamp',
		'permission_overwrites',
		'nicks',
		'soft_deleted',
	],
	where: [Channels.where.eq('channel_id'), {kind: 'eq', col: 'soft_deleted', param: 'soft_deleted'}],
	limit: 1,
});

const FETCH_DM_STATE_CQL = DmStates.selectCql({
	where: [DmStates.where.eq('hi_user_id'), DmStates.where.eq('lo_user_id')],
	limit: 1,
});

const FETCH_PINNED_DMS_CQL = PinnedDms.selectCql({
	where: PinnedDms.where.eq('user_id'),
});

const FETCH_PRIVATE_CHANNELS_CQL = PrivateChannels.selectCql({
	where: PrivateChannels.where.eq('user_id'),
});

const FETCH_CHANNEL_METADATA_CQL = Channels.selectCql({
	columns: ['channel_id', 'type', 'last_message_id', 'soft_deleted'],
	where: [Channels.where.in('channel_id', 'channel_ids'), {kind: 'eq', col: 'soft_deleted', param: 'soft_deleted'}],
});

const FETCH_CHANNELS_IN_CQL = Channels.selectCql({
	where: [Channels.where.in('channel_id', 'channel_ids'), {kind: 'eq', col: 'soft_deleted', param: 'soft_deleted'}],
});

const sortBySortOrder = (a: PinnedDmRow, b: PinnedDmRow): number => a.sort_order - b.sort_order;

async function fetchPinnedDms(userId: UserID): Promise<Array<PinnedDmRow>> {
	return fetchMany<PinnedDmRow>(FETCH_PINNED_DMS_CQL, {user_id: userId});
}

export class UserChannelRepository implements IUserChannelRepository {
	async addPinnedDm(userId: UserID, channelId: ChannelID): Promise<Array<ChannelID>> {
		const pinnedDms = await fetchPinnedDms(userId);

		const existingDm = pinnedDms.find((dm) => dm.channel_id === channelId);
		if (existingDm) {
			return pinnedDms.sort(sortBySortOrder).map((dm) => dm.channel_id);
		}

		const highestSortOrder = pinnedDms.length > 0 ? Math.max(...pinnedDms.map((dm) => dm.sort_order)) : -1;

		await upsertOne(
			PinnedDms.upsertAll({
				user_id: userId,
				channel_id: channelId,
				sort_order: highestSortOrder + 1,
			}),
		);

		const allPinnedDms: Array<PinnedDmRow> = [
			...pinnedDms,
			{
				user_id: userId,
				channel_id: channelId,
				sort_order: highestSortOrder + 1,
			},
		];

		return allPinnedDms.sort(sortBySortOrder).map((dm) => dm.channel_id);
	}

	async closeDmForUser(userId: UserID, channelId: ChannelID): Promise<void> {
		await deleteOneOrMany(
			PrivateChannels.deleteByPk({
				user_id: userId,
				channel_id: channelId,
			}),
		);
	}

	async createDmChannelAndState(user1Id: UserID, user2Id: UserID, channelId: ChannelID): Promise<Channel> {
		const hiUserId = user1Id > user2Id ? user1Id : user2Id;
		const loUserId = user1Id > user2Id ? user2Id : user1Id;

		const batch = new BatchBuilder();

		const channelRow: ChannelRow = {
			channel_id: channelId,
			guild_id: null,
			type: ChannelTypes.DM,
			name: null,
			topic: null,
			icon_hash: null,
			url: null,
			parent_id: null,
			position: null,
			owner_id: null,
			recipient_ids: new Set([user1Id, user2Id]),
			nsfw: null,
			rate_limit_per_user: null,
			bitrate: null,
			user_limit: null,
			rtc_region: null,
			last_message_id: null,
			last_pin_timestamp: null,
			permission_overwrites: null,
			nicks: null,
			soft_deleted: false,
			indexed_at: null,
			version: 1,
		};

		batch.addPrepared(Channels.upsertAll(channelRow));
		batch.addPrepared(
			DmStates.upsertAll({
				hi_user_id: hiUserId,
				lo_user_id: loUserId,
				channel_id: channelId,
			}),
		);
		batch.addPrepared(
			PrivateChannels.upsertAll({
				user_id: user1Id,
				channel_id: channelId,
				is_gdm: false,
			}),
		);

		await batch.execute();

		return new Channel(channelRow);
	}

	async deleteAllPrivateChannels(userId: UserID): Promise<void> {
		await deleteOneOrMany(
			PrivateChannels.deleteCql({
				where: PrivateChannels.where.eq('user_id'),
			}),
			{user_id: userId},
		);
	}

	async deleteAllReadStates(userId: UserID): Promise<void> {
		await deleteOneOrMany(
			ReadStates.deleteCql({
				where: ReadStates.where.eq('user_id'),
			}),
			{user_id: userId},
		);
	}

	async findExistingDmState(user1Id: UserID, user2Id: UserID): Promise<Channel | null> {
		const hiUserId = user1Id > user2Id ? user1Id : user2Id;
		const loUserId = user1Id > user2Id ? user2Id : user1Id;

		const dmState = await fetchOne<DmStateRow>(FETCH_DM_STATE_CQL, {
			hi_user_id: hiUserId,
			lo_user_id: loUserId,
		});

		if (!dmState) {
			return null;
		}

		const channel = await fetchOne<ChannelRow>(FETCH_CHANNEL_CQL, {
			channel_id: dmState.channel_id,
			soft_deleted: false,
		});

		return channel ? new Channel(channel) : null;
	}

	async getPinnedDms(userId: UserID): Promise<Array<ChannelID>> {
		const pinnedDms = await fetchPinnedDms(userId);
		return pinnedDms.sort(sortBySortOrder).map((dm) => dm.channel_id);
	}

	async getPinnedDmsWithDetails(userId: UserID): Promise<Array<{channel_id: ChannelID; sort_order: number}>> {
		const pinnedDms = await fetchPinnedDms(userId);
		return pinnedDms.sort(sortBySortOrder);
	}

	async isDmChannelOpen(userId: UserID, channelId: ChannelID): Promise<boolean> {
		const result = await fetchOne<{channel_id: bigint}>(CHECK_PRIVATE_CHANNEL_CQL, {
			user_id: userId,
			channel_id: channelId,
		});

		return result != null;
	}

	async listPrivateChannels(userId: UserID): Promise<Array<Channel>> {
		const rows = await fetchMany<PrivateChannelRow>(FETCH_PRIVATE_CHANNELS_CQL, {
			user_id: userId,
		});

		if (rows.length === 0) {
			return [];
		}

		const channelIds = rows.map((row) => row.channel_id);
		const channelRows = await fetchManyInChunks<ChannelRow>(FETCH_CHANNELS_IN_CQL, channelIds, (chunk) => ({
			channel_ids: chunk,
			soft_deleted: false,
		}));

		return channelRows.map((row) => new Channel(row));
	}

	async listPrivateChannelSummaries(userId: UserID): Promise<Array<PrivateChannelSummary>> {
		const rows = await fetchMany<PrivateChannelRow>(FETCH_PRIVATE_CHANNELS_CQL, {
			user_id: userId,
		});

		if (rows.length === 0) {
			return [];
		}

		const channelIds = rows.map((row) => row.channel_id);
		const fetchMetadataForSoftDeleted = async (
			ids: Array<ChannelID>,
			softDeleted: boolean,
		): Promise<
			Array<{
				channel_id: ChannelID;
				type: number;
				last_message_id: MessageID | null;
				soft_deleted: boolean;
			}>
		> => {
			return fetchManyInChunks(FETCH_CHANNEL_METADATA_CQL, ids, (chunk) => ({
				channel_ids: chunk,
				soft_deleted: softDeleted,
			}));
		};

		const channelMap = new Map<
			ChannelID,
			{
				channel_id: ChannelID;
				type: number;
				last_message_id: MessageID | null;
				soft_deleted: boolean;
			}
		>();

		const openChannelRows = await fetchMetadataForSoftDeleted(channelIds, false);
		for (const row of openChannelRows) {
			channelMap.set(row.channel_id, row);
		}

		const missingChannelIds = channelIds.filter((id) => !channelMap.has(id));
		if (missingChannelIds.length > 0) {
			const deletedChannelRows = await fetchMetadataForSoftDeleted(missingChannelIds, true);
			for (const row of deletedChannelRows) {
				if (!channelMap.has(row.channel_id)) {
					channelMap.set(row.channel_id, row);
				}
			}
		}

		return rows.map((row) => {
			const channelRow = channelMap.get(row.channel_id);
			return {
				channelId: row.channel_id,
				isGroupDm: row.is_gdm ?? false,
				channelType: channelRow ? channelRow.type : null,
				lastMessageId: channelRow ? channelRow.last_message_id : null,
				open: Boolean(channelRow && !channelRow.soft_deleted),
			};
		});
	}

	async openDmForUser(userId: UserID, channelId: ChannelID, isGroupDm?: boolean): Promise<void> {
		let resolvedIsGroupDm: boolean;
		if (isGroupDm !== undefined) {
			resolvedIsGroupDm = isGroupDm;
		} else {
			const channelRow = await fetchOne<ChannelRow>(FETCH_CHANNEL_CQL, {
				channel_id: channelId,
				soft_deleted: false,
			});
			resolvedIsGroupDm = channelRow?.type === ChannelTypes.GROUP_DM;
		}

		await upsertOne(
			PrivateChannels.upsertAll({
				user_id: userId,
				channel_id: channelId,
				is_gdm: resolvedIsGroupDm,
			}),
		);
	}

	async removePinnedDm(userId: UserID, channelId: ChannelID): Promise<Array<ChannelID>> {
		await deleteOneOrMany(
			PinnedDms.deleteByPk({
				user_id: userId,
				channel_id: channelId,
			}),
		);

		const pinnedDms = await fetchPinnedDms(userId);
		return pinnedDms.sort(sortBySortOrder).map((dm) => dm.channel_id);
	}

	async deletePinnedDmsByUserId(userId: UserID): Promise<void> {
		await deleteOneOrMany(
			PinnedDms.deleteCql({
				where: PinnedDms.where.eq('user_id'),
			}),
			{user_id: userId},
		);
	}
}
