// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelID, MessageID, UserID} from '../../BrandedTypes';
import {channelRowFromPrivateChannelSnapshot, privateChannelHydrationPatch} from '../../channel/PrivateChannelSnapshot';
import {
	BatchBuilder,
	deleteOneOrMany,
	fetchMany,
	fetchManyInChunks,
	fetchOne,
	upsertOne,
} from '../../database/CassandraQueryExecution';
import {Db} from '../../database/CassandraTypes';
import type {ChannelRow, DmStateRow, PrivateChannelRow} from '../../database/types/ChannelTypes';
import {Logger} from '../../Logger';
import {Channel} from '../../models/Channel';
import {Channels, DmStates, PinnedDms, PrivateChannels, ReadStates, UserDmHistory} from '../../Tables';
import type {
	HistoricalDmChannelSummary,
	IUserChannelRepository,
	ListHistoricalDmChannelOptions,
	PrivateChannelSummary,
} from './IUserChannelRepository';

interface PinnedDmRow {
	user_id: UserID;
	channel_id: ChannelID;
	sort_order: number;
}

interface ChannelDetailsRow {
	channel_id: ChannelID;
	type: number;
	recipient_ids: Set<UserID> | null;
	last_message_id: MessageID | null;
	soft_deleted: boolean;
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
const FETCH_OPEN_PRIVATE_CHANNELS_BY_IDS_CQL = PrivateChannels.selectCql({
	where: [PrivateChannels.where.eq('user_id'), PrivateChannels.where.in('channel_id', 'channel_ids')],
});
const HISTORICAL_DM_CHANNELS_CQL = UserDmHistory.selectCql({
	columns: ['channel_id'],
	where: UserDmHistory.where.eq('user_id'),
});
const FETCH_CHANNEL_DETAILS_CQL = Channels.selectCql({
	columns: ['channel_id', 'type', 'recipient_ids', 'last_message_id', 'soft_deleted'],
	where: [Channels.where.in('channel_id', 'channel_ids'), {kind: 'eq', col: 'soft_deleted', param: 'soft_deleted'}],
});
const FETCH_CHANNELS_IN_CQL = Channels.selectCql({
	where: [Channels.where.in('channel_id', 'channel_ids'), {kind: 'eq', col: 'soft_deleted', param: 'soft_deleted'}],
});

function sortBySortOrder(a: PinnedDmRow, b: PinnedDmRow): number {
	return a.sort_order - b.sort_order;
}

function isUserPrivateChannelMember(channel: Channel, userId: UserID): boolean {
	if (channel.type === ChannelTypes.DM_PERSONAL_NOTES) {
		return channel.ownerId === userId;
	}
	if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) {
		return true;
	}
	return channel.recipientIds.has(userId);
}

function isUserDmChannelMember(channel: ChannelDetailsRow, userId: UserID): boolean {
	const isDm = channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM;
	return isDm && Boolean(channel.recipient_ids?.has(userId));
}

function channelDetailsFromChannelRow(row: ChannelRow): ChannelDetailsRow {
	return {
		channel_id: row.channel_id,
		type: row.type,
		recipient_ids: row.recipient_ids ?? null,
		last_message_id: row.last_message_id ?? null,
		soft_deleted: row.soft_deleted,
	};
}

function buildDmChannelRow({
	channelId,
	user1Id,
	user2Id,
	ownerId,
}: {
	channelId: ChannelID;
	user1Id: UserID;
	user2Id: UserID;
	ownerId: UserID | null;
}): ChannelRow {
	return {
		channel_id: channelId,
		guild_id: null,
		type: ChannelTypes.DM,
		name: null,
		topic: null,
		icon_hash: null,
		url: null,
		parent_id: null,
		position: null,
		owner_id: ownerId,
		recipient_ids: new Set([user1Id, user2Id]),
		nsfw: null,
		content_warning_level: null,
		content_warning_text: null,
		rate_limit_per_user: null,
		bitrate: null,
		user_limit: null,
		voice_connection_limit: null,
		rtc_region: null,
		last_message_id: null,
		last_pin_timestamp: null,
		permission_overwrites: null,
		nicks: null,
		soft_deleted: false,
		indexed_at: null,
		version: 1,
		thread_parent_channel_id: null,
		thread_creator_username: null,
		thread_state: null,
		thread_expires_at: null,
		thread_source_message_id: null,
		thread_message_count: null,
		thread_total_message_sent: null,
		thread_member_count_actual: null,
		thread_archived: null,
		thread_locked: null,
		thread_auto_archive_duration: null,
		thread_archive_timestamp: null,
	};
}

async function fetchPinnedDms(userId: UserID): Promise<Array<PinnedDmRow>> {
	return fetchMany<PinnedDmRow>(FETCH_PINNED_DMS_CQL, {user_id: userId});
}

export class UserChannelRepository implements IUserChannelRepository {
	async addPinnedDm(userId: UserID, channelId: ChannelID): Promise<Array<ChannelID>> {
		const pinnedDms = [...(await fetchPinnedDms(userId))];
		const existingDm = pinnedDms.find((dm) => dm.channel_id === channelId);
		if (existingDm) {
			return pinnedDms.sort(sortBySortOrder).map((dm) => dm.channel_id);
		}
		let highestSortOrder = -1;
		for (const dm of pinnedDms) {
			if (dm.sort_order > highestSortOrder) {
				highestSortOrder = dm.sort_order;
			}
		}
		const newSortOrder = highestSortOrder + 1;
		await upsertOne(
			PinnedDms.upsertAll({
				user_id: userId,
				channel_id: channelId,
				sort_order: newSortOrder,
			}),
		);
		pinnedDms.push({
			user_id: userId,
			channel_id: channelId,
			sort_order: newSortOrder,
		});
		pinnedDms.sort(sortBySortOrder);
		return pinnedDms.map((dm) => dm.channel_id);
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
		const channelRow = buildDmChannelRow({channelId, user1Id, user2Id, ownerId: null});
		batch.addPrepared(Channels.upsertAll(channelRow));
		batch.addPrepared(
			DmStates.upsertAll({
				hi_user_id: hiUserId,
				lo_user_id: loUserId,
				channel_id: channelId,
			}),
		);
		batch.addPrepared(
			PrivateChannels.patchByPk(
				{user_id: user1Id, channel_id: channelId},
				privateChannelHydrationPatch(channelRow, new Date()),
			),
		);
		await batch.execute();
		return new Channel(channelRow);
	}

	async createLocalOnlyDmChannel(ownerId: UserID, recipientId: UserID, channelId: ChannelID): Promise<Channel> {
		const channelRow = buildDmChannelRow({channelId, user1Id: ownerId, user2Id: recipientId, ownerId});
		await upsertOne(Channels.upsertAll(channelRow));
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

	async getPinnedDmsWithDetails(userId: UserID): Promise<
		Array<{
			channel_id: ChannelID;
			sort_order: number;
		}>
	> {
		const pinnedDms = await fetchPinnedDms(userId);
		return pinnedDms.sort(sortBySortOrder);
	}

	async isDmChannelOpen(userId: UserID, channelId: ChannelID): Promise<boolean> {
		const result = await fetchOne<{
			channel_id: bigint;
		}>(CHECK_PRIVATE_CHANNEL_CQL, {
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
		const hydratedChannels: Array<Channel> = [];
		const coldChannelIds: Array<ChannelID> = [];
		for (const row of rows) {
			const snapshotRow = channelRowFromPrivateChannelSnapshot(row);
			if (snapshotRow) {
				hydratedChannels.push(new Channel(snapshotRow));
			} else {
				coldChannelIds.push(row.channel_id);
			}
		}
		let coldChannels: Array<Channel> = [];
		if (coldChannelIds.length > 0) {
			const channelRows = await fetchManyInChunks<ChannelRow>(FETCH_CHANNELS_IN_CQL, coldChannelIds, (chunk) => ({
				channel_ids: chunk,
				soft_deleted: false,
			}));
			coldChannels = channelRows.map((row) => new Channel(row));
		}
		const orphanChannelIds: Array<ChannelID> = [];
		const validChannels: Array<Channel> = [];
		const channelsToHydrate: Array<Channel> = [];
		for (const channel of hydratedChannels) {
			if (isUserPrivateChannelMember(channel, userId)) {
				validChannels.push(channel);
			} else {
				orphanChannelIds.push(channel.id);
			}
		}
		for (const channel of coldChannels) {
			if (isUserPrivateChannelMember(channel, userId)) {
				validChannels.push(channel);
				channelsToHydrate.push(channel);
			} else {
				orphanChannelIds.push(channel.id);
			}
		}
		if (orphanChannelIds.length > 0) {
			void this.cleanupOrphanedPrivateChannels(userId, orphanChannelIds);
		}
		if (channelsToHydrate.length > 0) {
			void this.hydratePrivateChannelSnapshots(userId, channelsToHydrate);
		}
		return validChannels;
	}

	private async hydratePrivateChannelSnapshots(userId: UserID, channels: Array<Channel>): Promise<void> {
		const hydratedAt = new Date();
		try {
			await Promise.allSettled(
				channels.map((channel) =>
					upsertOne(
						PrivateChannels.patchByPk(
							{user_id: userId, channel_id: channel.id},
							privateChannelHydrationPatch(channel.toRow(), hydratedAt),
						),
					),
				),
			);
		} catch (error) {
			Logger.warn(
				{userId: userId.toString(), error: error instanceof Error ? error.message : String(error)},
				'Failed to read-repair private channel snapshots',
			);
		}
	}

	private async cleanupOrphanedPrivateChannels(userId: UserID, channelIds: Array<ChannelID>): Promise<void> {
		try {
			for (const channelId of channelIds) {
				await deleteOneOrMany(PrivateChannels.deleteByPk({user_id: userId, channel_id: channelId}));
				await deleteOneOrMany(UserDmHistory.deleteByPk({user_id: userId, channel_id: channelId}));
			}
			Logger.warn(
				{userId: userId.toString(), channelIds: channelIds.map((id) => id.toString())},
				'Auto-repaired orphaned private channel entries (user no longer in recipient_ids)',
			);
		} catch (error) {
			Logger.warn(
				{userId: userId.toString(), error: error instanceof Error ? error.message : String(error)},
				'Failed to clean up orphaned private channel entries',
			);
		}
	}

	async listPrivateChannelSummaries(userId: UserID): Promise<Array<PrivateChannelSummary>> {
		const rows = await fetchMany<PrivateChannelRow>(FETCH_PRIVATE_CHANNELS_CQL, {
			user_id: userId,
		});
		if (rows.length === 0) {
			return [];
		}
		const channelMap = new Map<ChannelID, ChannelDetailsRow>();
		const coldChannelIds: Array<ChannelID> = [];
		for (const row of rows) {
			const snapshotRow = channelRowFromPrivateChannelSnapshot(row);
			if (snapshotRow) {
				channelMap.set(row.channel_id, channelDetailsFromChannelRow(snapshotRow));
			} else {
				coldChannelIds.push(row.channel_id);
			}
		}
		const fetchFullChannels = async (ids: Array<ChannelID>, softDeleted: boolean): Promise<Array<ChannelRow>> => {
			return fetchManyInChunks<ChannelRow>(FETCH_CHANNELS_IN_CQL, ids, (chunk) => ({
				channel_ids: chunk,
				soft_deleted: softDeleted,
			}));
		};
		const fetchMetadataForSoftDeleted = async (
			ids: Array<ChannelID>,
			softDeleted: boolean,
		): Promise<Array<ChannelDetailsRow>> => {
			return fetchManyInChunks(FETCH_CHANNEL_DETAILS_CQL, ids, (chunk) => ({
				channel_ids: chunk,
				soft_deleted: softDeleted,
			}));
		};
		const channelsToHydrate: Array<Channel> = [];
		if (coldChannelIds.length > 0) {
			const openChannelRows = await fetchFullChannels(coldChannelIds, false);
			for (const row of openChannelRows) {
				channelMap.set(row.channel_id, channelDetailsFromChannelRow(row));
				const channel = new Channel(row);
				if (isUserPrivateChannelMember(channel, userId)) {
					channelsToHydrate.push(channel);
				}
			}
			const missingChannelIds = coldChannelIds.filter((id) => !channelMap.has(id));
			if (missingChannelIds.length > 0) {
				const deletedChannelRows = await fetchMetadataForSoftDeleted(missingChannelIds, true);
				for (const row of deletedChannelRows) {
					if (!channelMap.has(row.channel_id)) {
						channelMap.set(row.channel_id, row);
					}
				}
			}
			if (channelsToHydrate.length > 0) {
				void this.hydratePrivateChannelSnapshots(userId, channelsToHydrate);
			}
		}
		return rows.flatMap((row) => {
			const channelRow = channelMap.get(row.channel_id);
			if (!channelRow) {
				return [];
			}
			if (
				(channelRow.type === ChannelTypes.DM || channelRow.type === ChannelTypes.GROUP_DM) &&
				!isUserDmChannelMember(channelRow, userId)
			) {
				return [];
			}
			return [
				{
					channelId: row.channel_id,
					isGroupDm: row.is_gdm ?? false,
					channelType: channelRow.type,
					lastMessageId: channelRow.last_message_id,
					open: !channelRow.soft_deleted,
				},
			];
		});
	}

	async listHistoricalDmChannelIds(userId: UserID): Promise<Array<ChannelID>> {
		const rows = await fetchMany<{
			channel_id: ChannelID;
		}>(HISTORICAL_DM_CHANNELS_CQL, {
			user_id: userId,
		});
		const channelIds = rows.map((row) => row.channel_id);
		if (channelIds.length === 0) {
			return [];
		}
		const openPrivateRows = await fetchManyInChunks<PrivateChannelRow>(
			FETCH_OPEN_PRIVATE_CHANNELS_BY_IDS_CQL,
			channelIds,
			(chunk) => ({
				user_id: userId,
				channel_ids: chunk,
			}),
		);
		const openChannelIds = new Set(openPrivateRows.map((row) => row.channel_id));
		const validChannelIds = new Set<ChannelID>();
		const unresolvedChannelIds = new Set(channelIds);
		for (const row of openPrivateRows) {
			const snapshotRow = channelRowFromPrivateChannelSnapshot(row);
			if (!snapshotRow) {
				continue;
			}
			const channelDetails = channelDetailsFromChannelRow(snapshotRow);
			if (isUserDmChannelMember(channelDetails, userId)) {
				validChannelIds.add(row.channel_id);
			}
			unresolvedChannelIds.delete(row.channel_id);
		}
		const channelsToHydrate: Array<Channel> = [];
		const channelRows = await fetchManyInChunks<ChannelRow>(
			FETCH_CHANNELS_IN_CQL,
			Array.from(unresolvedChannelIds),
			(chunk) => ({
				channel_ids: chunk,
				soft_deleted: false,
			}),
		);
		for (const row of channelRows) {
			const channelDetails = channelDetailsFromChannelRow(row);
			if (isUserDmChannelMember(channelDetails, userId)) {
				validChannelIds.add(row.channel_id);
				if (openChannelIds.has(row.channel_id)) {
					channelsToHydrate.push(new Channel(row));
				}
			}
		}
		if (channelsToHydrate.length > 0) {
			void this.hydratePrivateChannelSnapshots(userId, channelsToHydrate);
		}
		return channelIds.filter((channelId) => validChannelIds.has(channelId));
	}

	async listHistoricalDmChannelsPaginated(
		userId: UserID,
		options: ListHistoricalDmChannelOptions,
	): Promise<Array<HistoricalDmChannelSummary>> {
		if (options.beforeChannelId !== undefined && options.afterChannelId !== undefined) {
			throw new Error('Cannot paginate with both beforeChannelId and afterChannelId');
		}
		let rows: Array<{
			channel_id: ChannelID;
		}>;
		if (options.afterChannelId !== undefined) {
			const query = UserDmHistory.select({
				columns: ['channel_id'],
				where: [UserDmHistory.where.eq('user_id'), UserDmHistory.where.gt('channel_id', 'after_channel_id')],
				orderBy: {col: 'channel_id', direction: 'ASC'},
				limit: options.limit,
			});
			rows = await fetchMany<{
				channel_id: ChannelID;
			}>(
				query.bind({
					user_id: userId,
					after_channel_id: options.afterChannelId,
				}),
			);
			rows.reverse();
		} else if (options.beforeChannelId !== undefined) {
			const query = UserDmHistory.select({
				columns: ['channel_id'],
				where: [UserDmHistory.where.eq('user_id'), UserDmHistory.where.lt('channel_id', 'before_channel_id')],
				orderBy: {col: 'channel_id', direction: 'DESC'},
				limit: options.limit,
			});
			rows = await fetchMany<{
				channel_id: ChannelID;
			}>(
				query.bind({
					user_id: userId,
					before_channel_id: options.beforeChannelId,
				}),
			);
		} else {
			const query = UserDmHistory.select({
				columns: ['channel_id'],
				where: UserDmHistory.where.eq('user_id'),
				orderBy: {col: 'channel_id', direction: 'DESC'},
				limit: options.limit,
			});
			rows = await fetchMany<{
				channel_id: ChannelID;
			}>(
				query.bind({
					user_id: userId,
				}),
			);
		}
		const channelIds = rows.map((row) => row.channel_id);
		if (channelIds.length === 0) {
			return [];
		}
		const openPrivateRows = await fetchManyInChunks<PrivateChannelRow>(
			FETCH_OPEN_PRIVATE_CHANNELS_BY_IDS_CQL,
			channelIds,
			(chunk) => ({
				user_id: userId,
				channel_ids: chunk,
			}),
		);
		const openChannelIds = new Set(openPrivateRows.map((row) => row.channel_id));
		const unresolvedChannelIds = new Set(channelIds);
		const fetchChannelDetails = async (
			ids: Array<ChannelID>,
			softDeleted: boolean,
		): Promise<Array<ChannelDetailsRow>> => {
			return fetchManyInChunks<ChannelDetailsRow>(FETCH_CHANNEL_DETAILS_CQL, ids, (chunk) => ({
				channel_ids: chunk,
				soft_deleted: softDeleted,
			}));
		};
		const fetchFullChannels = async (ids: Array<ChannelID>, softDeleted: boolean): Promise<Array<ChannelRow>> => {
			return fetchManyInChunks<ChannelRow>(FETCH_CHANNELS_IN_CQL, ids, (chunk) => ({
				channel_ids: chunk,
				soft_deleted: softDeleted,
			}));
		};
		const channelMap = new Map<ChannelID, ChannelDetailsRow>();
		for (const row of openPrivateRows) {
			const snapshotRow = channelRowFromPrivateChannelSnapshot(row);
			if (!snapshotRow) {
				continue;
			}
			channelMap.set(row.channel_id, channelDetailsFromChannelRow(snapshotRow));
			unresolvedChannelIds.delete(row.channel_id);
		}
		const channelsToHydrate: Array<Channel> = [];
		const nonDeletedChannels = await fetchFullChannels(Array.from(unresolvedChannelIds), false);
		for (const channel of nonDeletedChannels) {
			channelMap.set(channel.channel_id, channelDetailsFromChannelRow(channel));
			unresolvedChannelIds.delete(channel.channel_id);
			if (openChannelIds.has(channel.channel_id)) {
				const channelModel = new Channel(channel);
				if (isUserPrivateChannelMember(channelModel, userId)) {
					channelsToHydrate.push(channelModel);
				}
			}
		}
		const missingChannelIds = Array.from(unresolvedChannelIds);
		if (missingChannelIds.length > 0) {
			const deletedChannels = await fetchChannelDetails(missingChannelIds, true);
			for (const channel of deletedChannels) {
				if (!channelMap.has(channel.channel_id)) {
					channelMap.set(channel.channel_id, channel);
				}
			}
		}
		if (channelsToHydrate.length > 0) {
			void this.hydratePrivateChannelSnapshots(userId, channelsToHydrate);
		}
		return channelIds.map((channelId) => {
			const channel = channelMap.get(channelId);
			return {
				channelId,
				channelType: channel?.type ?? null,
				recipientIds: channel?.recipient_ids ? Array.from(channel.recipient_ids) : [],
				lastMessageId: channel?.last_message_id ?? null,
				open: openChannelIds.has(channelId),
			};
		});
	}

	async openDmForUser(userId: UserID, channelId: ChannelID, isGroupDm?: boolean): Promise<void> {
		const channelRow = await fetchOne<ChannelRow>(FETCH_CHANNEL_CQL, {
			channel_id: channelId,
			soft_deleted: false,
		});
		if (channelRow) {
			await this.openPrivateChannelForUser(userId, new Channel(channelRow));
			return;
		}
		const resolvedIsGroupDm = isGroupDm ?? false;
		await this.recordHistoricalDmChannel(userId, channelId, resolvedIsGroupDm);
		await upsertOne(
			PrivateChannels.patchByPk({user_id: userId, channel_id: channelId}, {is_gdm: Db.set(resolvedIsGroupDm)}),
		);
	}

	async openPrivateChannelForUser(userId: UserID, channel: Channel): Promise<void> {
		const isGroupDm = channel.type === ChannelTypes.GROUP_DM;
		await this.recordHistoricalDmChannel(userId, channel.id, isGroupDm);
		await upsertOne(
			PrivateChannels.patchByPk(
				{user_id: userId, channel_id: channel.id},
				privateChannelHydrationPatch(channel.toRow(), new Date()),
			),
		);
	}

	async recordHistoricalDmChannel(userId: UserID, channelId: ChannelID, isGroupDm: boolean): Promise<void> {
		if (isGroupDm) {
			return;
		}
		await upsertOne(
			UserDmHistory.upsertAll({
				user_id: userId,
				channel_id: channelId,
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
