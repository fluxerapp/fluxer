// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import {channelIdToMessageId, createMessageID} from '@app/api/BrandedTypes';
import {
	advanceRow,
	advanceRows,
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
		const values: Partial<ReadStateRow> = {message_id: messageId, mention_count: mentionCount};
		if (lastPinTimestamp !== undefined) values.last_pin_timestamp = lastPinTimestamp;
		if (stamp) Object.assign(values, markerColumns(marker));
		const write = async (): Promise<boolean> => {
			const patch: Record<string, DbOp<unknown>> = {};
			for (const [column, value] of Object.entries(values)) patch[column] = Db.set(value);
			await upsertOne(ReadStates.patchByPk({user_id: userId, channel_id: channelId}, patch));
			return true;
		};
		if (!manual) {
			const pk = {user_id: userId, channel_id: channelId};
			const advanced = await advanceRow(
				{
					table: ReadStates,
					pk,
					guard: {col: 'message_id', value: messageId, orEqual: true},
					set: values,
					insert: {...pk, ...values},
				},
				write,
			);
			if (!advanced) {
				return this.upsertReadStateRow(userId, channelId, messageId, mentionCount, lastPinTimestamp, manual, marker);
			}
		} else {
			await write();
		}
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
		const applied = await advanceRows(
			updates.map(({userId, channelId, messageId, marker}) => {
				const pk = {user_id: userId, channel_id: channelId};
				const baselineMessageId = mentionBaseline(channelId, marker);
				return {
					table: ReadStates,
					pk,
					guard: {col: 'message_id', value: messageId},
					set: marker != null ? markerColumns(marker) : {},
					increment: {mention_count: 1},
					insert:
						baselineMessageId < messageId
							? {
									...pk,
									message_id: baselineMessageId,
									mention_count: 1,
									last_pin_timestamp: null,
									...(marker != null ? markerColumns(marker) : {}),
								}
							: null,
				};
			}),
			() => this.incrementMentionCountsByRead(updates),
		);
		return updates.filter((_, index) => applied[index]).map(({userId, channelId}) => ({userId, channelId}));
	}

	private async incrementMentionCountsByRead(updates: Array<ReadStateMentionUpdate>): Promise<Array<boolean>> {
		const existingStates = await Promise.all(
			updates.map(({userId, channelId, messageId, marker}) =>
				fetchOne<ReadStateRow>(FETCH_READ_STATE_BY_USER_AND_CHANNEL_CQL, {
					user_id: userId,
					channel_id: channelId,
				}).then((state) => ({userId, channelId, messageId, marker, state})),
			),
		);
		const batch = new BatchBuilder();
		const applied: Array<boolean> = [];
		for (const {userId, channelId, messageId, marker, state} of existingStates) {
			if (state) {
				if (state.message_id != null && state.message_id >= messageId) {
					applied.push(false);
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
				applied.push(true);
			} else {
				const baselineMessageId = mentionBaseline(channelId, marker);
				if (baselineMessageId >= messageId) {
					applied.push(false);
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
				applied.push(true);
			}
		}
		if (applied.includes(true)) {
			await batch.executeChunked(BULK_READ_STATE_BATCH_QUERY_LIMIT, false);
		}
		return applied;
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
