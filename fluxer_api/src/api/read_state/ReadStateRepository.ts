// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import {channelIdToMessageId, createMessageID} from '@app/api/BrandedTypes';
import {
	BatchBuilder,
	fetchMany,
	fetchManyInChunks,
	fetchOne,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db, type DbOp} from '@app/api/database/CassandraTypes';
import type {ReadStateRow} from '@app/api/database/types/ChannelTypes';
import {ReadState} from '@app/api/models/ReadState';
import {ReadStates} from '@app/api/Tables';
import {ReadStateFlags} from '@fluxer/constants/src/ThreadConstants';

export interface ReadStateUpsert {
	readState: ReadState;
	previous: ReadState | null;
}

export interface ReadStateMarker {
	flags: number;
	guildId: GuildID;
}

export interface ReadStateMentionUpdate {
	userId: UserID;
	channelId: ChannelID;
	messageId: MessageID;
	marker?: ReadStateMarker | null;
}

const FETCH_READ_STATES_CQL = ReadStates.selectCql({
	where: ReadStates.where.eq('user_id'),
});
const FETCH_READ_STATE_BY_USER_AND_CHANNEL_CQL = ReadStates.selectCql({
	where: [ReadStates.where.eq('user_id'), ReadStates.where.eq('channel_id')],
	limit: 1,
});
const FETCH_READ_STATES_BY_CHANNEL_IDS_CQL = ReadStates.selectCql({
	where: [ReadStates.where.eq('user_id'), ReadStates.where.in('channel_id', 'channel_ids')],
});
const BULK_READ_STATE_BATCH_QUERY_LIMIT = 50;

function baseReadStateRow(row: ReadStateRow): ReadStateRow {
	return {
		user_id: row.user_id,
		channel_id: row.channel_id,
		message_id: row.message_id,
		mention_count: row.mention_count,
		last_pin_timestamp: row.last_pin_timestamp,
	};
}

function markerNeeded(marker: ReadStateMarker | null | undefined, row: ReadStateRow | null): marker is ReadStateMarker {
	return marker != null && (row?.guild_id == null || row.flags !== marker.flags);
}

function markerColumns(marker: ReadStateMarker): Pick<ReadStateRow, 'flags' | 'guild_id'> {
	return {flags: marker.flags, guild_id: marker.guildId};
}

function markerPatch(marker: ReadStateMarker): Record<string, DbOp<unknown>> {
	return {flags: Db.set(marker.flags), guild_id: Db.set(marker.guildId)};
}

function markerOf(row: ReadStateRow | null): Pick<ReadStateRow, 'flags' | 'guild_id'> {
	return row?.guild_id != null ? {flags: row.flags ?? null, guild_id: row.guild_id} : {};
}

function mentionBaseline(channelId: ChannelID, marker: ReadStateMarker | null | undefined): MessageID {
	const baseline = channelIdToMessageId(channelId);
	if (marker != null && (marker.flags & ReadStateFlags.IS_THREAD) !== 0) {
		return createMessageID(baseline - 1n);
	}
	return baseline;
}

export class ReadStateRepository {
	async listReadStates(userId: UserID): Promise<Array<ReadState>> {
		const rows = await fetchMany<ReadStateRow>(FETCH_READ_STATES_CQL, {user_id: userId});
		return rows.map((row) => new ReadState(row));
	}

	async getReadState(userId: UserID, channelId: ChannelID): Promise<ReadState | null> {
		const row = await fetchOne<ReadStateRow>(FETCH_READ_STATE_BY_USER_AND_CHANNEL_CQL, {
			user_id: userId,
			channel_id: channelId,
		});
		return row ? new ReadState(row) : null;
	}

	async upsertReadState(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		mentionCount = 0,
		lastPinTimestamp?: Date,
		manual = false,
		marker?: ReadStateMarker | null,
	): Promise<ReadStateUpsert> {
		return this.upsertReadStateRow(userId, channelId, messageId, mentionCount, lastPinTimestamp, manual, marker);
	}

	private async upsertReadStateRow(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		mentionCount = 0,
		lastPinTimestamp?: Date,
		manual = false,
		marker?: ReadStateMarker | null,
	): Promise<ReadStateUpsert> {
		const currentReadState = await fetchOne<ReadStateRow>(FETCH_READ_STATE_BY_USER_AND_CHANNEL_CQL, {
			user_id: userId,
			channel_id: channelId,
		});
		const previous = currentReadState ? new ReadState(currentReadState) : null;
		const stamp = markerNeeded(marker, currentReadState);
		if (!manual && previous?.lastMessageId != null && previous.lastMessageId > messageId) {
			if (!stamp) return {readState: previous, previous};
			await upsertOne(ReadStates.patchByPk({user_id: userId, channel_id: channelId}, markerPatch(marker)));
			return {readState: new ReadState({...currentReadState!, ...markerColumns(marker)}), previous};
		}
		const patch: Record<string, DbOp<unknown>> = {
			message_id: Db.set(messageId),
			mention_count: Db.set(mentionCount),
		};
		if (lastPinTimestamp !== undefined) {
			patch['last_pin_timestamp'] = Db.set(lastPinTimestamp);
		}
		if (stamp) Object.assign(patch, markerPatch(marker));
		await upsertOne(ReadStates.patchByPk({user_id: userId, channel_id: channelId}, patch));
		const readState = new ReadState({
			user_id: userId,
			channel_id: channelId,
			message_id: messageId,
			mention_count: mentionCount,
			last_pin_timestamp: lastPinTimestamp ?? currentReadState?.last_pin_timestamp ?? null,
			...(stamp ? markerColumns(marker) : markerOf(currentReadState)),
		});
		return {readState, previous};
	}

	async incrementReadStateMentions(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		incrementBy = 1,
		marker?: ReadStateMarker | null,
	): Promise<ReadState | null> {
		return this.incrementReadStateMentionsRow(userId, channelId, messageId, incrementBy, marker);
	}

	private async incrementReadStateMentionsRow(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		incrementBy = 1,
		marker?: ReadStateMarker | null,
	): Promise<ReadState | null> {
		const currentReadState = await fetchOne<ReadStateRow>(FETCH_READ_STATE_BY_USER_AND_CHANNEL_CQL, {
			user_id: userId,
			channel_id: channelId,
		});
		if (!currentReadState) {
			const baselineMessageId = mentionBaseline(channelId, marker);
			if (baselineMessageId >= messageId) {
				return null;
			}
			return (
				await this.upsertReadStateRow(userId, channelId, baselineMessageId, incrementBy, undefined, false, marker)
			).readState;
		}
		if (currentReadState.message_id != null && currentReadState.message_id >= messageId) {
			return null;
		}
		const newMentionCount = (currentReadState.mention_count || 0) + incrementBy;
		const updatedReadState: ReadStateRow = {
			...baseReadStateRow(currentReadState),
			mention_count: newMentionCount,
			...(markerNeeded(marker, currentReadState) ? markerColumns(marker) : {}),
		};
		await upsertOne(ReadStates.upsertAll(updatedReadState));
		return new ReadState({...markerOf(currentReadState), ...updatedReadState});
	}

	async bulkIncrementMentionCounts(updates: Array<ReadStateMentionUpdate>): Promise<
		Array<{
			userId: UserID;
			channelId: ChannelID;
		}>
	> {
		if (updates.length === 0) {
			return [];
		}
		return this.bulkIncrementMentionCountsRows(updates);
	}

	private async bulkIncrementMentionCountsRows(updates: Array<ReadStateMentionUpdate>): Promise<
		Array<{
			userId: UserID;
			channelId: ChannelID;
		}>
	> {
		if (updates.length === 0) {
			return [];
		}
		const existingStates = await Promise.all(
			updates.map(({userId, channelId, messageId, marker}) =>
				fetchOne<ReadStateRow>(FETCH_READ_STATE_BY_USER_AND_CHANNEL_CQL, {
					user_id: userId,
					channel_id: channelId,
				}).then((state) => ({userId, channelId, messageId, marker, state})),
			),
		);
		const batch = new BatchBuilder();
		const appliedUpdates: Array<{
			userId: UserID;
			channelId: ChannelID;
		}> = [];
		for (const {userId, channelId, messageId, marker, state} of existingStates) {
			if (state) {
				if (state.message_id != null && state.message_id >= messageId) {
					continue;
				}
				batch.addPrepared(
					ReadStates.patchByPk(
						{user_id: userId, channel_id: channelId},
						{
							mention_count: Db.set((state.mention_count || 0) + 1),
							...(markerNeeded(marker, state) ? markerPatch(marker) : {}),
						},
					),
				);
				appliedUpdates.push({userId, channelId});
			} else {
				const baselineMessageId = mentionBaseline(channelId, marker);
				if (baselineMessageId >= messageId) {
					continue;
				}
				batch.addPrepared(
					ReadStates.upsertAll({
						user_id: userId,
						channel_id: channelId,
						message_id: baselineMessageId,
						mention_count: 1,
						last_pin_timestamp: null,
						...(marker != null ? markerColumns(marker) : {}),
					}),
				);
				appliedUpdates.push({userId, channelId});
			}
		}
		if (appliedUpdates.length > 0) {
			await batch.executeChunked(BULK_READ_STATE_BATCH_QUERY_LIMIT, false);
		}
		return appliedUpdates;
	}

	async bulkAckMessages(
		userId: UserID,
		readStates: Array<{
			channelId: ChannelID;
			messageId: MessageID;
			marker?: ReadStateMarker | null;
		}>,
	): Promise<Array<ReadState>> {
		return this.bulkAckMessageRows(userId, readStates);
	}

	private async bulkAckMessageRows(
		userId: UserID,
		readStates: Array<{
			channelId: ChannelID;
			messageId: MessageID;
			marker?: ReadStateMarker | null;
		}>,
	): Promise<Array<ReadState>> {
		const currentRows = await fetchManyInChunks<ReadStateRow, ChannelID>(
			FETCH_READ_STATES_BY_CHANNEL_IDS_CQL,
			readStates.map((readState) => readState.channelId),
			(chunk) => ({user_id: userId, channel_ids: chunk}),
		);
		const currentRowsByChannel = new Map(currentRows.map((row) => [row.channel_id, row]));
		const batch = new BatchBuilder();
		const results: Array<ReadState> = [];
		for (const readState of readStates) {
			const currentReadState = currentRowsByChannel.get(readState.channelId) ?? null;
			const {marker} = readState;
			const stamp = markerNeeded(marker, currentReadState);
			if (currentReadState?.message_id != null && currentReadState.message_id > readState.messageId) {
				if (stamp) {
					batch.addPrepared(
						ReadStates.patchByPk({user_id: userId, channel_id: readState.channelId}, markerPatch(marker)),
					);
				}
				results.push(new ReadState({...currentReadState, ...(stamp ? markerColumns(marker) : {})}));
				continue;
			}
			batch.addPrepared(
				ReadStates.patchByPk(
					{user_id: userId, channel_id: readState.channelId},
					{
						message_id: Db.set(readState.messageId),
						mention_count: Db.set(0),
						...(stamp ? markerPatch(marker) : {}),
					},
				),
			);
			results.push(
				new ReadState({
					user_id: userId,
					channel_id: readState.channelId,
					message_id: readState.messageId,
					mention_count: 0,
					last_pin_timestamp: currentReadState?.last_pin_timestamp ?? null,
					...(stamp ? markerColumns(marker) : markerOf(currentReadState)),
				}),
			);
		}
		await batch.executeChunked(BULK_READ_STATE_BATCH_QUERY_LIMIT, false);
		return results;
	}

	async upsertPinAck(
		userId: UserID,
		channelId: ChannelID,
		lastPinTimestamp: Date,
		marker?: ReadStateMarker | null,
	): Promise<void> {
		await upsertOne(
			ReadStates.patchByPk(
				{user_id: userId, channel_id: channelId},
				{last_pin_timestamp: Db.set(lastPinTimestamp), ...(marker != null ? markerPatch(marker) : {})},
			),
		);
	}
}
