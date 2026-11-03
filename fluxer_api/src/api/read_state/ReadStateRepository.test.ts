// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createChannelID, createMessageID, createUserID, type UserID} from '@app/api/BrandedTypes';
import {fetchOne, setCassandraQueryExecutorForTesting, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {ReadStateRow} from '@app/api/database/types/ChannelTypes';
import {ReadStateRepository} from '@app/api/read_state/ReadStateRepository';
import {ReadStates} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const FETCH_READ_STATE = ReadStates.selectCql({
	where: [ReadStates.where.eq('user_id'), ReadStates.where.eq('channel_id')],
	limit: 1,
});

let executor: InMemoryCassandraQueryExecutor;

describe('ReadStateRepository row storage', () => {
	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});
	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});
	it('keeps normal acknowledgements monotonic', async () => {
		const userId = createUserID(1n);
		const channelId = createChannelID(10n);
		await seedReadState(userId, channelId, 100n, 3);
		const repository = new ReadStateRepository();
		const {readState} = await repository.upsertReadState(userId, channelId, createMessageID(90n), 0);
		expect(readState.lastMessageId).toBe(createMessageID(100n));
		expect(await loadReadState(userId, channelId)).toMatchObject({
			message_id: createMessageID(100n),
			mention_count: 3,
		});
	});
	it('keeps manual acknowledgements able to move backward', async () => {
		const userId = createUserID(1n);
		const channelId = createChannelID(10n);
		await seedReadState(userId, channelId, 100n, 0);
		const repository = new ReadStateRepository();
		const {readState} = await repository.upsertReadState(userId, channelId, createMessageID(90n), 2, undefined, true);
		expect(readState.lastMessageId).toBe(createMessageID(90n));
		expect(readState.mentionCount).toBe(2);
		expect(await loadReadState(userId, channelId)).toMatchObject({
			message_id: createMessageID(90n),
			mention_count: 2,
		});
	});
	it('increments mentions only when the message is newer than the read cursor', async () => {
		const userId = createUserID(1n);
		const channelId = createChannelID(10n);
		await seedReadState(userId, channelId, 100n, 1);
		const repository = new ReadStateRepository();
		const skipped = await repository.bulkIncrementMentionCounts([{userId, channelId, messageId: createMessageID(90n)}]);
		expect(skipped).toEqual([]);
		expect(await loadReadState(userId, channelId)).toMatchObject({message_id: createMessageID(100n), mention_count: 1});
		const updated = await repository.bulkIncrementMentionCounts([
			{userId, channelId, messageId: createMessageID(101n)},
		]);
		expect(updated).toEqual([{userId, channelId}]);
		expect(await loadReadState(userId, channelId)).toMatchObject({message_id: createMessageID(100n), mention_count: 2});
	});
	it('creates mention rows from the channel baseline when no read row exists', async () => {
		const userId = createUserID(1n);
		const channelId = createChannelID(10n);
		const repository = new ReadStateRepository();
		const applied = await repository.bulkIncrementMentionCounts([{userId, channelId, messageId: createMessageID(11n)}]);
		expect(applied).toEqual([{userId, channelId}]);
		expect(await loadReadState(userId, channelId)).toMatchObject({message_id: createMessageID(10n), mention_count: 1});
	});
	it('bulk acknowledges rows without moving newer cursors backward', async () => {
		const userId = createUserID(1n);
		const olderChannelId = createChannelID(10n);
		const newChannelId = createChannelID(20n);
		await seedReadState(userId, olderChannelId, 100n, 5);
		const repository = new ReadStateRepository();
		const readStates = await repository.bulkAckMessages(userId, [
			{channelId: olderChannelId, messageId: createMessageID(90n)},
			{channelId: newChannelId, messageId: createMessageID(200n)},
		]);
		expect(readStates.map((state) => state.lastMessageId)).toEqual([createMessageID(100n), createMessageID(200n)]);
		expect(await loadReadState(userId, olderChannelId)).toMatchObject({
			message_id: createMessageID(100n),
			mention_count: 5,
		});
		expect(await loadReadState(userId, newChannelId)).toMatchObject({
			message_id: createMessageID(200n),
			mention_count: 0,
		});
	});
	it('keeps the last pin timestamp of acked rows and leaves missing rows without one', async () => {
		const userId = createUserID(1n);
		const pinnedChannelId = createChannelID(10n);
		const missingChannelId = createChannelID(20n);
		const lastPinTimestamp = new Date('2026-01-01T00:00:00.000Z');
		await upsertOne(
			ReadStates.upsertAll({
				user_id: userId,
				channel_id: pinnedChannelId,
				message_id: createMessageID(100n),
				mention_count: 2,
				last_pin_timestamp: lastPinTimestamp,
			}),
		);
		const repository = new ReadStateRepository();
		const readStates = await repository.bulkAckMessages(userId, [
			{channelId: pinnedChannelId, messageId: createMessageID(200n)},
			{channelId: missingChannelId, messageId: createMessageID(200n)},
		]);
		expect(readStates[0]?.lastPinTimestamp).toEqual(lastPinTimestamp);
		expect(readStates[1]?.lastPinTimestamp).toBeNull();
	});
});

async function seedReadState(
	userId: UserID,
	channelId: ChannelID,
	messageId: bigint,
	mentionCount: number,
): Promise<void> {
	await upsertOne(
		ReadStates.upsertAll({
			user_id: userId,
			channel_id: channelId,
			message_id: createMessageID(messageId),
			mention_count: mentionCount,
			last_pin_timestamp: null,
		}),
	);
}

async function loadReadState(userId: UserID, channelId: ChannelID): Promise<ReadStateRow | null> {
	return fetchOne<ReadStateRow>(FETCH_READ_STATE, {user_id: userId, channel_id: channelId});
}
