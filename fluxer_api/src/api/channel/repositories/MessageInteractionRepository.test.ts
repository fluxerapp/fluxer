// SPDX-License-Identifier: AGPL-3.0-or-later

import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {type ChannelID, createChannelID, createMessageID, createUserID, type MessageID} from '../../BrandedTypes';
import {fetchOne, setCassandraQueryExecutorForTesting} from '../../database/CassandraQueryExecution';
import type {PreparedQuery} from '../../database/CassandraTypes';
import type {MessageRow} from '../../database/types/MessageTypes';
import {Messages} from '../../Tables';
import {InMemoryCassandraQueryExecutor} from '../../test/InMemoryCassandraQueryExecutor';
import {ChannelDataRepository} from './ChannelDataRepository';
import {MessageInteractionRepository} from './MessageInteractionRepository';
import {MessageRepository} from './MessageRepository';

const FETCH_MESSAGE_HAS_REACTION = Messages.selectCql({
	columns: ['has_reaction'],
	where: [Messages.where.eq('channel_id'), Messages.where.eq('bucket'), Messages.where.eq('message_id')],
	limit: 1,
});

class RecordingCassandraQueryExecutor extends InMemoryCassandraQueryExecutor {
	executed: Array<PreparedQuery> = [];

	override async executeQuery<T = Record<string, unknown>>(query: PreparedQuery): Promise<Array<T>> {
		this.executed.push(query);
		return super.executeQuery<T>(query);
	}

	override reset(): void {
		this.executed = [];
		super.reset();
	}

	countHasReactionWrites(): number {
		return this.executed.filter(
			(query) =>
				query.kvMeta?.action === 'patch' &&
				query.kvMeta.table.name === 'messages' &&
				(query.kvMeta.patchKeys ?? []).includes('has_reaction'),
		).length;
	}
}

let executor: RecordingCassandraQueryExecutor;

function createRepository(): MessageInteractionRepository {
	return new MessageInteractionRepository(new MessageRepository(new ChannelDataRepository()));
}

async function loadHasReaction(channelId: ChannelID, messageId: MessageID): Promise<boolean | null | undefined> {
	const row = await fetchOne<Pick<MessageRow, 'has_reaction'>>(FETCH_MESSAGE_HAS_REACTION, {
		channel_id: channelId,
		bucket: BucketUtils.makeBucket(messageId),
		message_id: messageId,
	});
	return row?.has_reaction;
}

describe('MessageInteractionRepository has_reaction writes', () => {
	beforeEach(() => {
		executor = new RecordingCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});
	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});
	it('flags the message when the first reaction is added', async () => {
		const channelId = createChannelID(10n);
		const messageId = createMessageID(100n);
		const repository = createRepository();
		await repository.addReaction(channelId, messageId, createUserID(1n), '🔥');
		expect(executor.countHasReactionWrites()).toBe(1);
		expect(await loadHasReaction(channelId, messageId)).toBe(true);
	});
	it('flags the message on every reaction add', async () => {
		const channelId = createChannelID(10n);
		const messageId = createMessageID(100n);
		const repository = createRepository();
		await repository.addReaction(channelId, messageId, createUserID(1n), '🔥');
		await repository.addReaction(channelId, messageId, createUserID(2n), '🔥');
		expect(executor.countHasReactionWrites()).toBe(2);
		expect(await repository.listMessageReactions(channelId, messageId)).toHaveLength(2);
		expect(await loadHasReaction(channelId, messageId)).toBe(true);
	});
	it('restores the flag when it was cleared after the message was read', async () => {
		const channelId = createChannelID(10n);
		const messageId = createMessageID(100n);
		const repository = createRepository();
		await repository.addReaction(channelId, messageId, createUserID(1n), '🔥');
		await repository.setHasReaction(channelId, messageId, false);
		expect(await loadHasReaction(channelId, messageId)).toBe(false);
		await repository.addReaction(channelId, messageId, createUserID(2n), '🔥');
		expect(await loadHasReaction(channelId, messageId)).toBe(true);
		expect(await repository.listMessageReactions(channelId, messageId)).toHaveLength(2);
	});
});
