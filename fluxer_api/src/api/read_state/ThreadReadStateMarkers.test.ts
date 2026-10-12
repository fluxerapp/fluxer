// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {fetchOne, setCassandraQueryExecutorForTesting, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, PreparedQuery} from '@app/api/database/CassandraTypes';
import type {ReadStateRow} from '@app/api/database/types/ChannelTypes';
import {clearChannelThreadsTaintCacheForTesting} from '@app/api/experiment/ChannelThreadsGate';
import {ReadState} from '@app/api/models/ReadState';
import {
	clearReadStateChannelMetaCacheForTesting,
	resolveReadStateMarker,
} from '@app/api/read_state/ReadStateChannelMeta';
import {type ReadStateMarker, ReadStateRepository} from '@app/api/read_state/ReadStateRepository';
import {mapReadStateResponse} from '@app/api/read_state/ReadStateResponseMapper';
import {visibleReadStates} from '@app/api/read_state/ReadStateVisibility';
import {ReadStates} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ReadStateFlags} from '@fluxer/constants/src/ThreadConstants';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const USER = createUserID(1n);
const GUILD = createGuildID(500n);
const THREAD = createChannelID(1000n);
const TEXT = createChannelID(10n);
const THREAD_MARKER: ReadStateMarker = {
	flags: ReadStateFlags.IS_GUILD_CHANNEL | ReadStateFlags.IS_THREAD,
	guildId: GUILD,
};
const FETCH_READ_STATE = ReadStates.selectCql({
	where: [ReadStates.where.eq('user_id'), ReadStates.where.eq('channel_id')],
	limit: 1,
});
const LEGACY_UPSERT_CQL = `UPDATE read_states
SET message_id = :message_id, mention_count = :mention_count, last_pin_timestamp = :last_pin_timestamp
WHERE user_id = :user_id AND channel_id = :channel_id;
`;

class RecordingExecutor extends InMemoryCassandraQueryExecutor {
	readonly writes: Array<string> = [];

	override async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		if (!query.cql.trimStart().toUpperCase().startsWith('SELECT')) this.writes.push(query.cql);
		return super.executeQuery<T>(query);
	}
}

class CountingExecutor extends InMemoryCassandraQueryExecutor {
	reads = 0;

	override async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		if (query.cql.trimStart().toUpperCase().startsWith('SELECT')) this.reads++;
		return super.executeQuery<T>(query);
	}
}

async function loadRow(channelId = THREAD): Promise<ReadStateRow | null> {
	return fetchOne<ReadStateRow>(FETCH_READ_STATE, {user_id: USER, channel_id: channelId});
}

async function seed(channelId: typeof THREAD, messageId: bigint, mentionCount: number): Promise<void> {
	await upsertOne(
		ReadStates.upsertAll({
			user_id: USER,
			channel_id: channelId,
			message_id: createMessageID(messageId),
			mention_count: mentionCount,
			last_pin_timestamp: null,
		}),
	);
}

describe('thread read state markers', () => {
	let executor: RecordingExecutor;
	const repository = new ReadStateRepository();

	beforeEach(() => {
		executor = new RecordingExecutor();
		setCassandraQueryExecutorForTesting(executor);
		clearChannelThreadsTaintCacheForTesting();
		clearReadStateChannelMetaCacheForTesting();
	});

	afterEach(() => {
		setCassandraQueryExecutorForTesting(null);
	});

	describe('control rows', () => {
		it('creates bulk mention rows with the same full-row CQL as before', async () => {
			executor.writes.length = 0;
			await repository.bulkIncrementMentionCounts([{userId: USER, channelId: TEXT, messageId: createMessageID(20n)}]);
			expect(executor.writes).toEqual([LEGACY_UPSERT_CQL]);
		});

		it('never writes marker columns on acks, pins or mention increments', async () => {
			await repository.upsertReadState(USER, TEXT, createMessageID(50n), 0);
			await repository.bulkAckMessages(USER, [{channelId: TEXT, messageId: createMessageID(60n)}]);
			await repository.upsertPinAck(USER, TEXT, new Date('2026-09-01T00:00:00.000Z'));
			await repository.bulkIncrementMentionCounts([{userId: USER, channelId: TEXT, messageId: createMessageID(70n)}]);
			expect(executor.writes.some((cql) => cql.includes('flags') || cql.includes('guild_id'))).toBe(false);
			const readState = new ReadState((await loadRow(TEXT))!);
			expect(readState.isMarked).toBe(false);
			expect(mapReadStateResponse(readState)).not.toHaveProperty('flags');
		});

		it('keeps the channel id as the mention baseline for unmarked rows', async () => {
			await repository.bulkIncrementMentionCounts([
				{userId: USER, channelId: THREAD, messageId: createMessageID(1001n)},
			]);
			expect(await loadRow()).toMatchObject({message_id: createMessageID(1000n), mention_count: 1});
		});
	});

	describe('marked rows', () => {
		it('stamps an existing unmarked row on ack and keeps the stamp on later unmarked acks', async () => {
			await seed(THREAD, 1005n, 2);
			const {readState: acked} = await repository.upsertReadState(
				USER,
				THREAD,
				createMessageID(1010n),
				0,
				undefined,
				false,
				THREAD_MARKER,
			);
			expect(acked.isMarked).toBe(true);
			const {readState: later} = await repository.upsertReadState(USER, THREAD, createMessageID(1020n), 0);
			expect(later.flags).toBe(THREAD_MARKER.flags);
			expect(await loadRow()).toMatchObject({guild_id: GUILD, message_id: createMessageID(1020n)});
		});

		it('stamps rows through bulk ack, bulk mention increments and pin acks', async () => {
			const forum = createChannelID(2000n);
			const forumMarker: ReadStateMarker = {flags: ReadStateFlags.IS_GUILD_CHANNEL, guildId: GUILD};
			await repository.bulkAckMessages(USER, [
				{channelId: forum, messageId: createMessageID(2001n), marker: forumMarker},
			]);
			await repository.bulkIncrementMentionCounts([
				{userId: USER, channelId: THREAD, messageId: createMessageID(1001n), marker: THREAD_MARKER},
			]);
			const pinned = createChannelID(3000n);
			await repository.upsertPinAck(USER, pinned, new Date('2026-09-01T00:00:00.000Z'), THREAD_MARKER);
			expect(await loadRow(forum)).toMatchObject({flags: ReadStateFlags.IS_GUILD_CHANNEL, guild_id: GUILD});
			expect(await loadRow()).toMatchObject({
				flags: THREAD_MARKER.flags,
				guild_id: GUILD,
				message_id: createMessageID(999n),
				mention_count: 1,
			});
			expect(await loadRow(pinned)).toMatchObject({flags: THREAD_MARKER.flags, guild_id: GUILD});
		});

		it('carries flags on the wire only for marked rows', async () => {
			await repository.bulkIncrementMentionCounts([
				{userId: USER, channelId: THREAD, messageId: createMessageID(1001n), marker: THREAD_MARKER},
			]);
			const [readState] = await repository.listReadStates(USER);
			expect(mapReadStateResponse(readState!).flags).toBe(THREAD_MARKER.flags);
		});
	});

	describe('visibility', () => {
		const control = new ReadState({
			user_id: USER,
			channel_id: TEXT,
			message_id: createMessageID(5n),
			mention_count: 1,
			last_pin_timestamp: null,
		});
		const marked = new ReadState({
			user_id: USER,
			channel_id: THREAD,
			message_id: createMessageID(999n),
			mention_count: 3,
			last_pin_timestamp: null,
			flags: THREAD_MARKER.flags,
			guild_id: GUILD,
		});

		it('returns the same array when nothing is marked', () => {
			const rows = [control];
			expect(visibleReadStates(rows, {userId: USER, capable: false})).toBe(rows);
		});

		it('drops marked rows for incapable sessions and keeps them for viewers', () => {
			expect(visibleReadStates([control, marked], {userId: USER, capable: false})).toEqual([control]);
			expect(visibleReadStates([control, marked], {userId: USER, capable: true})).toEqual([control, marked]);
		});
	});

	describe('marker resolution', () => {
		it('does no IO for incapable sessions', async () => {
			const counting = new CountingExecutor();
			setCassandraQueryExecutorForTesting(counting);

			expect(await resolveReadStateMarker({userId: USER, channelId: THREAD, capable: false})).toBeNull();
			expect(counting.reads).toBe(0);
		});

		it('stamps threads and forums for viewers and nothing else', async () => {
			const resolve = (type: number, guildId: typeof GUILD | null = GUILD) =>
				resolveReadStateMarker({userId: USER, channelId: THREAD, capable: true, channel: {type, guildId}});
			expect(await resolve(ChannelTypes.PUBLIC_THREAD)).toEqual(THREAD_MARKER);
			expect(await resolve(ChannelTypes.PRIVATE_THREAD)).toEqual(THREAD_MARKER);
			expect(await resolve(ChannelTypes.GUILD_FORUM)).toEqual({flags: ReadStateFlags.IS_GUILD_CHANNEL, guildId: GUILD});
			expect(await resolve(ChannelTypes.GUILD_TEXT)).toBeNull();
			expect(await resolve(ChannelTypes.PUBLIC_THREAD, createGuildID(9n))).toEqual({
				...THREAD_MARKER,
				guildId: createGuildID(9n),
			});
		});
	});
});
