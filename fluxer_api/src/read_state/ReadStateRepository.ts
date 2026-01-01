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
import {channelIdToMessageId} from '~/BrandedTypes';
import {BatchBuilder, Db, defineTable, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '~/database/Cassandra';
import {READ_STATE_COLUMNS, type ReadStateRow} from '~/database/CassandraTypes';
import type {ReadState} from '~/Models';
import {ReadState as ReadStateModel} from '~/Models';
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

export class ReadStateRepository implements IReadStateRepository {
	async listReadStates(userId: UserID): Promise<Array<ReadState>> {
		const rows = await fetchMany<ReadStateRow>(FETCH_READ_STATES_CQL, {user_id: userId});
		return rows.map((row) => new ReadStateModel(row));
	}

	async upsertReadState(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		mentionCount = 0,
		lastPinTimestamp?: Date,
	): Promise<ReadState> {
		const patch: Record<string, ReturnType<typeof Db.set>> = {
			message_id: Db.set(messageId),
			mention_count: Db.set(mentionCount),
		};
		if (lastPinTimestamp !== undefined) {
			patch.last_pin_timestamp = Db.set(lastPinTimestamp);
		}
		await upsertOne(ReadStates.patchByPk({user_id: userId, channel_id: channelId}, patch));
		return new ReadStateModel({
			user_id: userId,
			channel_id: channelId,
			message_id: messageId,
			mention_count: mentionCount,
			last_pin_timestamp: lastPinTimestamp ?? null,
		});
	}

	async incrementReadStateMentions(userId: UserID, channelId: ChannelID, incrementBy = 1): Promise<ReadState | null> {
		const currentReadState = await fetchOne<ReadStateRow>(FETCH_READ_STATE_BY_USER_AND_CHANNEL_CQL, {
			user_id: userId,
			channel_id: channelId,
		});
		if (!currentReadState) {
			return this.upsertReadState(userId, channelId, channelIdToMessageId(channelId), incrementBy);
		}
		const newMentionCount = (currentReadState.mention_count || 0) + incrementBy;
		const updatedReadState: ReadStateRow = {...currentReadState, mention_count: newMentionCount};
		await upsertOne(ReadStates.upsertAll(updatedReadState));
		return new ReadStateModel(updatedReadState);
	}

	async bulkIncrementMentionCounts(updates: Array<{userId: UserID; channelId: ChannelID}>): Promise<void> {
		if (updates.length === 0) {
			return;
		}

		const existingStates = await Promise.all(
			updates.map(({userId, channelId}) =>
				fetchOne<ReadStateRow>(FETCH_READ_STATE_BY_USER_AND_CHANNEL_CQL, {
					user_id: userId,
					channel_id: channelId,
				}).then((state) => ({userId, channelId, state})),
			),
		);

		const batch = new BatchBuilder();
		for (const {userId, channelId, state} of existingStates) {
			if (state) {
				batch.addPrepared(
					ReadStates.patchByPk(
						{user_id: userId, channel_id: channelId},
						{mention_count: Db.set((state.mention_count || 0) + 1)},
					),
				);
			} else {
				batch.addPrepared(
					ReadStates.upsertAll({
						user_id: userId,
						channel_id: channelId,
						message_id: channelIdToMessageId(channelId),
						mention_count: 1,
						last_pin_timestamp: null,
					}),
				);
			}
		}
		await batch.execute();
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
		readStates: Array<{channelId: ChannelID; messageId: MessageID}>,
	): Promise<Array<ReadState>> {
		const batch = new BatchBuilder();
		const results: Array<ReadState> = [];
		for (const readState of readStates) {
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
				new ReadStateModel({
					user_id: userId,
					channel_id: readState.channelId,
					message_id: readState.messageId,
					mention_count: 0,
					last_pin_timestamp: null,
				}),
			);
		}
		await batch.execute();
		return results;
	}

	async upsertPinAck(userId: UserID, channelId: ChannelID, lastPinTimestamp: Date): Promise<void> {
		await upsertOne(
			ReadStates.patchByPk({user_id: userId, channel_id: channelId}, {last_pin_timestamp: Db.set(lastPinTimestamp)}),
		);
	}
}
