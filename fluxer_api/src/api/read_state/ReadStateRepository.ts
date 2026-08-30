// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, UserID} from '../BrandedTypes';
import {channelIdToMessageId} from '../BrandedTypes';
import {
	BatchBuilder,
	deleteOneOrMany,
	fetchMany,
	fetchManyInChunks,
	fetchOne,
	upsertOne,
} from '../database/CassandraQueryExecution';
import {defineTable} from '../database/CassandraTableDsl';
import {Db, type DbOp} from '../database/CassandraTypes';
import type {ReadStateRow} from '../database/types/ChannelTypes';
import {READ_STATE_COLUMNS} from '../database/types/ChannelTypes';
import {ReadState} from '../models/ReadState';
import type {IReadStateRepository} from './IReadStateRepository';

const ReadStates = defineTable<ReadStateRow, 'user_id' | 'channel_id'>({
	name: 'read_states',
	columns: READ_STATE_COLUMNS,
	primaryKey: ['user_id', 'channel_id'],
});
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

export class ReadStateRepository implements IReadStateRepository {
	async listReadStates(userId: UserID): Promise<Array<ReadState>> {
		const rows = await fetchMany<ReadStateRow>(FETCH_READ_STATES_CQL, {user_id: userId});
		return rows.map((row) => new ReadState(row));
	}

	async upsertReadState(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		mentionCount = 0,
		lastPinTimestamp?: Date,
		manual = false,
	): Promise<ReadState> {
		return this.upsertReadStateRow(userId, channelId, messageId, mentionCount, lastPinTimestamp, manual);
	}

	private async upsertReadStateRow(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		mentionCount = 0,
		lastPinTimestamp?: Date,
		manual = false,
	): Promise<ReadState> {
		const currentReadState = await fetchOne<ReadStateRow>(FETCH_READ_STATE_BY_USER_AND_CHANNEL_CQL, {
			user_id: userId,
			channel_id: channelId,
		});
		if (!manual && currentReadState?.message_id != null && currentReadState.message_id > messageId) {
			return new ReadState(currentReadState);
		}
		const patch: Record<string, DbOp<unknown>> = {
			message_id: Db.set(messageId),
			mention_count: Db.set(mentionCount),
		};
		if (lastPinTimestamp !== undefined) {
			patch['last_pin_timestamp'] = Db.set(lastPinTimestamp);
		}
		await upsertOne(ReadStates.patchByPk({user_id: userId, channel_id: channelId}, patch));
		return new ReadState({
			user_id: userId,
			channel_id: channelId,
			message_id: messageId,
			mention_count: mentionCount,
			last_pin_timestamp: lastPinTimestamp ?? currentReadState?.last_pin_timestamp ?? null,
		});
	}

	async incrementReadStateMentions(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		incrementBy = 1,
	): Promise<ReadState | null> {
		return this.incrementReadStateMentionsRow(userId, channelId, messageId, incrementBy);
	}

	private async incrementReadStateMentionsRow(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		incrementBy = 1,
	): Promise<ReadState | null> {
		const currentReadState = await fetchOne<ReadStateRow>(FETCH_READ_STATE_BY_USER_AND_CHANNEL_CQL, {
			user_id: userId,
			channel_id: channelId,
		});
		if (!currentReadState) {
			const baselineMessageId = channelIdToMessageId(channelId);
			if (baselineMessageId >= messageId) {
				return null;
			}
			return this.upsertReadStateRow(userId, channelId, baselineMessageId, incrementBy);
		}
		if (currentReadState.message_id != null && currentReadState.message_id >= messageId) {
			return null;
		}
		const newMentionCount = (currentReadState.mention_count || 0) + incrementBy;
		const updatedReadState: ReadStateRow = {...currentReadState, mention_count: newMentionCount};
		await upsertOne(ReadStates.upsertAll(updatedReadState));
		return new ReadState(updatedReadState);
	}

	async bulkIncrementMentionCounts(
		updates: Array<{
			userId: UserID;
			channelId: ChannelID;
			messageId: MessageID;
		}>,
	): Promise<
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

	private async bulkIncrementMentionCountsRows(
		updates: Array<{
			userId: UserID;
			channelId: ChannelID;
			messageId: MessageID;
		}>,
	): Promise<
		Array<{
			userId: UserID;
			channelId: ChannelID;
		}>
	> {
		if (updates.length === 0) {
			return [];
		}
		const existingStates = await Promise.all(
			updates.map(({userId, channelId, messageId}) =>
				fetchOne<ReadStateRow>(FETCH_READ_STATE_BY_USER_AND_CHANNEL_CQL, {
					user_id: userId,
					channel_id: channelId,
				}).then((state) => ({userId, channelId, messageId, state})),
			),
		);
		const batch = new BatchBuilder();
		const appliedUpdates: Array<{
			userId: UserID;
			channelId: ChannelID;
		}> = [];
		for (const {userId, channelId, messageId, state} of existingStates) {
			if (state) {
				if (state.message_id != null && state.message_id >= messageId) {
					continue;
				}
				batch.addPrepared(
					ReadStates.patchByPk(
						{user_id: userId, channel_id: channelId},
						{mention_count: Db.set((state.mention_count || 0) + 1)},
					),
				);
				appliedUpdates.push({userId, channelId});
			} else {
				const baselineMessageId = channelIdToMessageId(channelId);
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

	async deleteReadState(userId: UserID, channelId: ChannelID): Promise<void> {
		await deleteOneOrMany(
			ReadStates.deleteByPk({
				user_id: userId,
				channel_id: channelId,
			}),
		);
	}

	async bulkAckMessages(
		userId: UserID,
		readStates: Array<{
			channelId: ChannelID;
			messageId: MessageID;
		}>,
	): Promise<Array<ReadState>> {
		return this.bulkAckMessageRows(userId, readStates);
	}

	private async bulkAckMessageRows(
		userId: UserID,
		readStates: Array<{
			channelId: ChannelID;
			messageId: MessageID;
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
			const currentReadState = currentRowsByChannel.get(readState.channelId);
			if (currentReadState?.message_id != null && currentReadState.message_id > readState.messageId) {
				results.push(new ReadState(currentReadState));
				continue;
			}
			batch.addPrepared(
				ReadStates.patchByPk(
					{user_id: userId, channel_id: readState.channelId},
					{
						message_id: Db.set(readState.messageId),
						mention_count: Db.set(0),
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
				}),
			);
		}
		await batch.executeChunked(BULK_READ_STATE_BATCH_QUERY_LIMIT, false);
		return results;
	}

	async upsertPinAck(userId: UserID, channelId: ChannelID, lastPinTimestamp: Date): Promise<void> {
		await upsertOne(
			ReadStates.patchByPk({user_id: userId, channel_id: channelId}, {last_pin_timestamp: Db.set(lastPinTimestamp)}),
		);
	}
}
