// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {type ChannelID, createChannelID, createMessageID, createUserID, type UserID} from '../BrandedTypes';
import {getKvMeta} from '../database/CassandraMetaRegistry';
import {fetchOne, setCassandraQueryExecutorForTesting, upsertOne} from '../database/CassandraQueryExecution';
import {defineTable} from '../database/CassandraTableDsl';
import type {CassandraParams, KvQueryMeta, PreparedQuery} from '../database/CassandraTypes';
import type {ReadStateRow} from '../database/types/ChannelTypes';
import {READ_STATE_COLUMNS} from '../database/types/ChannelTypes';
import {InMemoryCassandraQueryExecutor} from '../test/InMemoryCassandraQueryExecutor';
import {ReadStateRepository} from './ReadStateRepository';

const ReadStates = defineTable<ReadStateRow, 'user_id' | 'channel_id'>({
	name: 'read_states',
	columns: READ_STATE_COLUMNS,
	primaryKey: ['user_id', 'channel_id'],
});
const FETCH_READ_STATE = ReadStates.selectCql({
	where: [ReadStates.where.eq('user_id'), ReadStates.where.eq('channel_id')],
	limit: 1,
});

class RecordingCassandraQueryExecutor {
	readonly statements: Array<string> = [];
	private readonly inner = new InMemoryCassandraQueryExecutor();

	async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		this.record(query.kvMeta ?? getKvMeta(query.cql));
		return this.inner.executeQuery<T>(query);
	}

	async executeBatch(queries: Array<{query: string; params: object; meta?: KvQueryMeta}>): Promise<void> {
		for (const entry of queries) {
			this.record(entry.meta);
		}
		await this.inner.executeBatch(queries);
	}

	countStatements(statement: string): number {
		return this.statements.filter((entry) => entry === statement).length;
	}

	private record(meta: KvQueryMeta | undefined): void {
		if (!meta) return;
		this.statements.push(`${meta.action}:${meta.table.name}`);
	}
}

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
		const readState = await repository.upsertReadState(userId, channelId, createMessageID(90n), 0);
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
		const readState = await repository.upsertReadState(userId, channelId, createMessageID(90n), 2, undefined, true);
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
		const skipped = await repository.incrementReadStateMentions(userId, channelId, createMessageID(90n));
		const updated = await repository.incrementReadStateMentions(userId, channelId, createMessageID(101n), 2);
		expect(skipped).toBeNull();
		expect(updated?.mentionCount).toBe(3);
		expect(await loadReadState(userId, channelId)).toMatchObject({
			message_id: createMessageID(100n),
			mention_count: 3,
		});
	});
	it('creates mention rows from the channel baseline when no read row exists', async () => {
		const userId = createUserID(1n);
		const channelId = createChannelID(10n);
		const repository = new ReadStateRepository();
		const readState = await repository.incrementReadStateMentions(userId, channelId, createMessageID(11n), 4);
		expect(readState?.lastMessageId).toBe(createMessageID(10n));
		expect(readState?.mentionCount).toBe(4);
		expect(await loadReadState(userId, channelId)).toMatchObject({
			message_id: createMessageID(10n),
			mention_count: 4,
		});
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
	it('reads every acked channel of a bulk acknowledgement in one query', async () => {
		const recording = new RecordingCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(recording);
		const userId = createUserID(1n);
		const seededChannelIds = [createChannelID(10n), createChannelID(20n), createChannelID(30n)];
		for (const channelId of seededChannelIds) {
			await seedReadState(userId, channelId, 100n, 5);
		}
		const missingChannelId = createChannelID(40n);
		recording.statements.length = 0;
		const repository = new ReadStateRepository();
		const readStates = await repository.bulkAckMessages(userId, [
			{channelId: seededChannelIds[0]!, messageId: createMessageID(90n)},
			{channelId: seededChannelIds[1]!, messageId: createMessageID(200n)},
			{channelId: seededChannelIds[2]!, messageId: createMessageID(300n)},
			{channelId: missingChannelId, messageId: createMessageID(400n)},
		]);
		expect(recording.countStatements('select:read_states')).toBe(1);
		expect(readStates.map((state) => state.channelId)).toEqual([...seededChannelIds, missingChannelId]);
		expect(readStates.map((state) => state.lastMessageId)).toEqual([
			createMessageID(100n),
			createMessageID(200n),
			createMessageID(300n),
			createMessageID(400n),
		]);
		expect(readStates.map((state) => state.mentionCount)).toEqual([5, 0, 0, 0]);
		expect(await loadReadState(userId, missingChannelId)).toMatchObject({
			message_id: createMessageID(400n),
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
