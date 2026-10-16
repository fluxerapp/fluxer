// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createChannelID, createMessageID, createUserID, type MessageID} from '@app/api/BrandedTypes';
import {ChannelDataRepository} from '@app/api/channel/repositories/ChannelDataRepository';
import {MessageInteractionRepository} from '@app/api/channel/repositories/MessageInteractionRepository';
import {MessageRepository} from '@app/api/channel/repositories/MessageRepository';
import {fetchOne, setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import type {MessageRow} from '@app/api/database/types/MessageTypes';
import {Messages} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const FETCH_MESSAGE_HAS_REACTION = Messages.selectCql({
	columns: ['has_reaction'],
	where: [Messages.where.eq('channel_id'), Messages.where.eq('bucket'), Messages.where.eq('message_id')],
	limit: 1,
});

let executor: InMemoryCassandraQueryExecutor;

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
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});
	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});
	it('flags the message on every reaction add', async () => {
		const channelId = createChannelID(10n);
		const messageId = createMessageID(100n);
		const repository = createRepository();
		await repository.addReaction(channelId, messageId, createUserID(1n), '🔥');
		await repository.addReaction(channelId, messageId, createUserID(2n), '🔥');
		expect(
			executor.executed.filter(
				(meta) =>
					meta.action === 'patch' && meta.table.name === 'messages' && (meta.patchKeys ?? []).includes('has_reaction'),
			).length,
		).toBe(2);
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
