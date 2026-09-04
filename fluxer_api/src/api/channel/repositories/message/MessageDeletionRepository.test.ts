// SPDX-License-Identifier: AGPL-3.0-or-later

import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {createChannelID, createMessageID, createUserID, type MessageID} from '../../../BrandedTypes';
import {setCassandraQueryExecutorForTesting, upsertOne} from '../../../database/CassandraQueryExecution';
import type {KvQueryMeta} from '../../../database/CassandraTypes';
import type {MessageRow} from '../../../database/types/MessageTypes';
import {Messages} from '../../../Tables';
import {InMemoryCassandraQueryExecutor} from '../../../test/InMemoryCassandraQueryExecutor';
import {MessageDataRepository} from './MessageDataRepository';
import {MessageDeletionRepository} from './MessageDeletionRepository';

class BatchRecordingCassandraQueryExecutor extends InMemoryCassandraQueryExecutor {
	readonly batchedQueries: Array<string> = [];

	override async executeBatch(queries: Array<{query: string; params: object; meta?: KvQueryMeta}>): Promise<void> {
		for (const {query} of queries) {
			this.batchedQueries.push(query);
		}
		await super.executeBatch(queries);
	}

	override reset(): void {
		super.reset();
		this.batchedQueries.length = 0;
	}

	countReactionDeletes(): number {
		return this.batchedQueries.filter((query) => /^\s*DELETE FROM message_reactions\b/.test(query)).length;
	}
}

const CHANNEL_ID = createChannelID(10n);
const AUTHOR_ID = createUserID(20n);

let executor: BatchRecordingCassandraQueryExecutor;

function buildMessageRow(messageId: MessageID, hasReaction: boolean | null): MessageRow {
	return {
		channel_id: CHANNEL_ID,
		bucket: BucketUtils.makeBucket(messageId),
		message_id: messageId,
		author_id: AUTHOR_ID,
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
		has_reaction: hasReaction,
		version: 1,
	};
}

async function seedMessage(messageId: MessageID, hasReaction: boolean | null): Promise<void> {
	await upsertOne(Messages.upsertAll(buildMessageRow(messageId, hasReaction)));
}

function createRepository(): MessageDeletionRepository {
	return new MessageDeletionRepository(new MessageDataRepository());
}

describe('MessageDeletionRepository reaction tombstones', () => {
	beforeEach(() => {
		executor = new BatchRecordingCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});
	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});
	it('skips the reaction delete when the message never had a reaction', async () => {
		const messageId = createMessageID(100n);
		await seedMessage(messageId, false);
		await createRepository().deleteMessage(CHANNEL_ID, messageId, AUTHOR_ID);
		expect(executor.countReactionDeletes()).toBe(0);
	});
	it('still deletes reactions when the message has one', async () => {
		const messageId = createMessageID(101n);
		await seedMessage(messageId, true);
		await createRepository().deleteMessage(CHANNEL_ID, messageId, AUTHOR_ID);
		expect(executor.countReactionDeletes()).toBe(1);
	});
	it('still deletes reactions when the message predates the has_reaction column', async () => {
		const messageId = createMessageID(102n);
		await seedMessage(messageId, null);
		await createRepository().deleteMessage(CHANNEL_ID, messageId, AUTHOR_ID);
		expect(executor.countReactionDeletes()).toBe(1);
	});
	it('still deletes reactions when the message row is gone', async () => {
		await createRepository().deleteMessage(CHANNEL_ID, createMessageID(103n), AUTHOR_ID);
		expect(executor.countReactionDeletes()).toBe(1);
	});
	it('only deletes reactions for the reacted messages in a bulk delete', async () => {
		const unreacted = [createMessageID(200n), createMessageID(201n), createMessageID(202n)];
		const reacted = createMessageID(203n);
		const legacy = createMessageID(204n);
		for (const messageId of unreacted) {
			await seedMessage(messageId, false);
		}
		await seedMessage(reacted, true);
		await seedMessage(legacy, null);
		await createRepository().bulkDeleteMessages(CHANNEL_ID, [...unreacted, reacted, legacy]);
		expect(executor.countReactionDeletes()).toBe(2);
	});
});
