// SPDX-License-Identifier: AGPL-3.0-or-later

import {generateSnowflake} from '@fluxer/snowflake/src/Snowflake';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {type ChannelID, createChannelID, createMessageID, createUserID, type MessageID} from '../../../BrandedTypes';
import {
	deleteOneOrMany,
	fetchMany,
	fetchOne,
	setCassandraQueryExecutorForTesting,
} from '../../../database/CassandraQueryExecution';
import type {CassandraParams, KvQueryMeta, PreparedQuery} from '../../../database/CassandraTypes';
import type {
	ChannelEmptyBucketRow,
	ChannelMessageBucketRow,
	ChannelStateRow,
	MessageRow,
} from '../../../database/types/MessageTypes';
import {ChannelEmptyBuckets, ChannelMessageBuckets, ChannelState, Messages} from '../../../Tables';
import {InMemoryCassandraQueryExecutor} from '../../../test/InMemoryCassandraQueryExecutor';
import {MessageDataRepository} from './MessageDataRepository';

const FETCH_CHANNEL_STATE = ChannelState.select({where: ChannelState.where.eq('channel_id'), limit: 1});
const FETCH_MESSAGE_BUCKETS = ChannelMessageBuckets.select({
	columns: ['bucket'],
	where: ChannelMessageBuckets.where.eq('channel_id'),
	orderBy: {col: 'bucket', direction: 'DESC'},
});
const FETCH_EMPTY_BUCKETS = ChannelEmptyBuckets.select({
	columns: ['bucket'],
	where: ChannelEmptyBuckets.where.eq('channel_id'),
	orderBy: {col: 'bucket', direction: 'DESC'},
});

class RecordingCassandraQueryExecutor {
	readonly statements: Array<string> = [];
	readonly batchAtomicity: Array<boolean> = [];
	private readonly inner = new InMemoryCassandraQueryExecutor();

	async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		this.record(query.kvMeta);
		return this.inner.executeQuery<T>(query);
	}

	async executeBatch(
		queries: Array<{query: string; params: object; meta?: KvQueryMeta}>,
		atomic = true,
	): Promise<void> {
		this.batchAtomicity.push(atomic);
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

function makeMessageRow(channelId: ChannelID, messageId: MessageID): MessageRow {
	return {
		channel_id: channelId,
		bucket: BucketUtils.makeBucket(messageId),
		message_id: messageId,
		author_id: createUserID(7n),
		type: 0,
		webhook_id: null,
		webhook_name: null,
		webhook_avatar_hash: null,
		content: 'hello',
		edited_timestamp: null,
		pinned_timestamp: null,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: null,
		embeds: null,
		sticker_items: null,
		message_reference: null,
		message_snapshots: null,
		call: null,
		has_reaction: false,
		version: 1,
	};
}

let executor: RecordingCassandraQueryExecutor;

describe('MessageDataRepository.upsertMessage round trips', () => {
	beforeEach(() => {
		executor = new RecordingCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});
	afterEach(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});
	it('writes channel_state once and skips the read-before-write when the caller passes no previous row', async () => {
		const channelId = createChannelID(1710000000000000000n);
		const messageId = createMessageID(1720000000000000000n);
		const repository = new MessageDataRepository();

		await repository.upsertMessage(makeMessageRow(channelId, messageId), null);

		expect(executor.countStatements('select:messages')).toBe(0);
		expect(executor.countStatements('patch:channel_state')).toBe(1);
		expect(executor.batchAtomicity).toEqual([false]);
		expect(await loadChannelState(channelId)).toMatchObject({
			created_bucket: BucketUtils.makeBucket(channelId),
			has_messages: true,
			last_message_id: messageId,
			last_message_bucket: BucketUtils.makeBucket(messageId),
		});
	});
	it('keeps the newer last_message_id while still recording channel state for an older message', async () => {
		const channelId = createChannelID(1710000000000000000n);
		const newerMessageId = createMessageID(1730000000000000000n);
		const olderMessageId = createMessageID(1720000000000000000n);
		const repository = new MessageDataRepository();
		await repository.upsertMessage(makeMessageRow(channelId, newerMessageId), null);
		executor.statements.length = 0;
		executor.batchAtomicity.length = 0;

		await repository.upsertMessage(makeMessageRow(channelId, olderMessageId), null);

		expect(executor.countStatements('patch:channel_state')).toBe(1);
		expect(await loadChannelState(channelId)).toMatchObject({
			created_bucket: BucketUtils.makeBucket(channelId),
			has_messages: true,
			last_message_id: newerMessageId,
			last_message_bucket: BucketUtils.makeBucket(newerMessageId),
		});
	});
});

describe('MessageDataRepository read scans maintain the bucket index', () => {
	beforeEach(() => {
		executor = new RecordingCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});
	afterEach(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});
	it('re-indexes a bucket that still holds messages and keeps the index batch unlogged', async () => {
		const channelId = createChannelID(generateSnowflake());
		const messageId = createMessageID(generateSnowflake());
		const bucket = BucketUtils.makeBucket(messageId);
		const repository = new MessageDataRepository();
		await repository.upsertMessage(makeMessageRow(channelId, messageId), null);
		await deleteOneOrMany(ChannelMessageBuckets.deleteByPk({channel_id: channelId, bucket}));
		expect(await loadIndexedBuckets(channelId)).toEqual([]);
		executor.statements.length = 0;
		executor.batchAtomicity.length = 0;

		const messages = await repository.listMessages(channelId);

		expect(messages.map((message) => message.id)).toEqual([messageId]);
		expect(await loadIndexedBuckets(channelId)).toEqual([bucket]);
		expect(await loadEmptyBuckets(channelId)).toEqual([]);
		expect(executor.countStatements('upsert:channel_message_buckets')).toBe(1);
		expect(executor.batchAtomicity).toEqual([false]);
	});
	it('drops a drained bucket from the index and keeps the empty-bucket batch unlogged', async () => {
		const channelId = createChannelID(generateSnowflake());
		const messageId = createMessageID(generateSnowflake());
		const bucket = BucketUtils.makeBucket(messageId);
		const repository = new MessageDataRepository();
		await repository.upsertMessage(makeMessageRow(channelId, messageId), null);
		await deleteOneOrMany(Messages.deleteByPk({channel_id: channelId, bucket, message_id: messageId}));
		executor.statements.length = 0;
		executor.batchAtomicity.length = 0;

		const messages = await repository.listMessages(channelId);

		expect(messages).toEqual([]);
		expect(await loadIndexedBuckets(channelId)).toEqual([]);
		expect(await loadEmptyBuckets(channelId)).toEqual([bucket]);
		expect(executor.batchAtomicity).toEqual([false]);
	});
});

async function loadChannelState(channelId: ChannelID): Promise<ChannelStateRow | null> {
	return fetchOne<ChannelStateRow>(FETCH_CHANNEL_STATE.bind({channel_id: channelId}));
}

async function loadIndexedBuckets(channelId: ChannelID): Promise<Array<number>> {
	const rows = await fetchMany<Pick<ChannelMessageBucketRow, 'bucket'>>(
		FETCH_MESSAGE_BUCKETS.bind({channel_id: channelId}),
	);
	return rows.map((row) => row.bucket);
}

async function loadEmptyBuckets(channelId: ChannelID): Promise<Array<number>> {
	const rows = await fetchMany<Pick<ChannelEmptyBucketRow, 'bucket'>>(
		FETCH_EMPTY_BUCKETS.bind({channel_id: channelId}),
	);
	return rows.map((row) => row.bucket);
}
