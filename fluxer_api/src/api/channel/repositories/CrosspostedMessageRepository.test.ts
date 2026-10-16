// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID, createMessageID, createWebhookID, type MessageID} from '@app/api/BrandedTypes';
import {CrosspostedMessageRepository} from '@app/api/channel/repositories/CrosspostedMessageRepository';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import type {CrosspostedMessageRow} from '@app/api/database/types/ChannelTypes';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const SOURCE_CHANNEL = createChannelID(10n);
const SOURCE_MESSAGE = createMessageID(100n);
const WEBHOOK = createWebhookID(500n);
const KEY = {sourceMessageId: SOURCE_MESSAGE, webhookId: WEBHOOK};

let executor: InMemoryCassandraQueryExecutor;
let repository: CrosspostedMessageRepository;

function pendingRow(overrides: Partial<CrosspostedMessageRow> = {}): CrosspostedMessageRow {
	return {
		source_message_id: SOURCE_MESSAGE,
		webhook_id: WEBHOOK,
		source_channel_id: SOURCE_CHANNEL,
		target_guild_id: createGuildID(20n),
		target_channel_id: createChannelID(30n),
		target_message_id: createMessageID(1000n),
		state: 'pending',
		reserved_at: new Date('2026-09-30T12:00:00.000Z'),
		source_fingerprint: null,
		created_at: new Date('2026-09-30T12:00:00.000Z'),
		...overrides,
	};
}

describe('CrosspostedMessageRepository', () => {
	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		repository = new CrosspostedMessageRepository();
	});
	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('inserts a pending row once and refuses a second reservation', async () => {
		expect(await repository.insertPending(pendingRow())).toBe(true);
		expect(await repository.insertPending(pendingRow({target_message_id: createMessageID(2000n)}))).toBe(false);
		const row = await repository.get(SOURCE_MESSAGE, WEBHOOK);
		expect(row?.state).toBe('pending');
		expect(row?.target_message_id).toBe(1000n);
		expect(row?.target_guild_id).toBe(20n);
		expect(row?.target_channel_id).toBe(30n);
		expect(row?.source_channel_id).toBe(10n);
	});

	it('always stores a reservation as pending', async () => {
		expect(await repository.insertPending(pendingRow({state: 'delivered'}))).toBe(true);
		expect((await repository.get(SOURCE_MESSAGE, WEBHOOK))?.state).toBe('pending');
	});

	it('reclaims a pending row only from the expected target id', async () => {
		await repository.insertPending(pendingRow());
		const reservedAt = new Date('2026-09-30T12:05:00.000Z');
		expect(
			await repository.reclaimPending(KEY, {
				fromTargetMessageId: createMessageID(999n),
				toTargetMessageId: createMessageID(2000n),
				reservedAt,
			}),
		).toBe(false);
		expect(
			await repository.reclaimPending(KEY, {
				fromTargetMessageId: createMessageID(1000n),
				toTargetMessageId: createMessageID(2000n),
				reservedAt,
			}),
		).toBe(true);
		const row = await repository.get(SOURCE_MESSAGE, WEBHOOK);
		expect(row?.target_message_id).toBe(2000n);
		expect(row?.reserved_at.getTime()).toBe(reservedAt.getTime());
		expect(row?.state).toBe('pending');
	});

	it('does not reclaim a delivered row', async () => {
		await repository.insertPending(pendingRow());
		await repository.markDelivered(KEY, {
			targetMessageId: createMessageID(1000n),
			sourceFingerprint: 'fp0',
		});
		expect(
			await repository.reclaimPending(KEY, {
				fromTargetMessageId: createMessageID(1000n),
				toTargetMessageId: createMessageID(2000n),
				reservedAt: new Date(),
			}),
		).toBe(false);
		expect((await repository.get(SOURCE_MESSAGE, WEBHOOK))?.target_message_id).toBe(1000n);
	});

	it('marks delivered only when the target id still matches', async () => {
		await repository.insertPending(pendingRow());
		await repository.reclaimPending(KEY, {
			fromTargetMessageId: createMessageID(1000n),
			toTargetMessageId: createMessageID(2000n),
			reservedAt: new Date(),
		});
		expect(
			await repository.markDelivered(KEY, {
				targetMessageId: createMessageID(1000n),
				sourceFingerprint: 'stale',
			}),
		).toBe(false);
		expect(
			await repository.markDelivered(KEY, {
				targetMessageId: createMessageID(2000n),
				sourceFingerprint: 'fp0',
			}),
		).toBe(true);
		const row = await repository.get(SOURCE_MESSAGE, WEBHOOK);
		expect(row?.state).toBe('delivered');
		expect(row?.source_fingerprint).toBe('fp0');
	});

	it('marks delivered with a null fingerprint for a recovered pending copy', async () => {
		await repository.insertPending(pendingRow({source_fingerprint: 'reserved'}));
		expect(
			await repository.markDelivered(KEY, {
				targetMessageId: createMessageID(1000n),
				sourceFingerprint: null,
			}),
		).toBe(true);
		const row = await repository.get(SOURCE_MESSAGE, WEBHOOK);
		expect(row?.state).toBe('delivered');
		expect(row?.source_fingerprint ?? null).toBeNull();
	});

	it('does not mark a missing row delivered', async () => {
		expect(
			await repository.markDelivered(KEY, {
				targetMessageId: createMessageID(1000n),
				sourceFingerprint: 'fp0',
			}),
		).toBe(false);
		expect(await repository.get(SOURCE_MESSAGE, WEBHOOK)).toBeNull();
	});

	it('updates sync state only on a delivered row with the same target id', async () => {
		await repository.insertPending(pendingRow());
		expect(
			await repository.updateSynced(KEY, {
				targetMessageId: createMessageID(1000n),
				sourceFingerprint: 'fp1',
			}),
		).toBe(false);
		await repository.markDelivered(KEY, {
			targetMessageId: createMessageID(1000n),
			sourceFingerprint: 'fp0',
		});
		expect(
			await repository.updateSynced(KEY, {
				targetMessageId: createMessageID(1001n),
				sourceFingerprint: 'fp1',
			}),
		).toBe(false);
		expect(
			await repository.updateSynced(KEY, {
				targetMessageId: createMessageID(1000n),
				sourceFingerprint: 'fp1',
			}),
		).toBe(true);
		const row = await repository.get(SOURCE_MESSAGE, WEBHOOK);
		expect(row?.source_fingerprint).toBe('fp1');
		expect(row?.state).toBe('delivered');
	});

	it('pages rows for a source message in webhook id order', async () => {
		const webhookIds = [505n, 501n, 503n, 502n, 504n];
		for (const id of webhookIds) {
			await repository.insertPending(pendingRow({webhook_id: createWebhookID(id)}));
		}
		await repository.insertPending(
			pendingRow({source_message_id: createMessageID(101n), webhook_id: createWebhookID(506n)}),
		);
		const first = await repository.listBySourceMessage(SOURCE_MESSAGE, {limit: 2});
		expect(first.map((row) => row.webhook_id)).toEqual([501n, 502n]);
		const second = await repository.listBySourceMessage(SOURCE_MESSAGE, {
			afterWebhookId: first[first.length - 1]!.webhook_id,
			limit: 2,
		});
		expect(second.map((row) => row.webhook_id)).toEqual([503n, 504n]);
		const third = await repository.listBySourceMessage(SOURCE_MESSAGE, {
			afterWebhookId: second[second.length - 1]!.webhook_id,
			limit: 2,
		});
		expect(third.map((row) => row.webhook_id)).toEqual([505n]);
	});

	it('deletes unconditionally without expected values', async () => {
		await repository.insertPending(pendingRow());
		expect(await repository.delete(KEY)).toBe(true);
		expect(await repository.get(SOURCE_MESSAGE, WEBHOOK)).toBeNull();
	});

	it('deletes conditionally only when the expected state and target match', async () => {
		await repository.insertPending(pendingRow());
		expect(await repository.delete(KEY, {state: 'delivered', target_message_id: createMessageID(1000n)})).toBe(false);
		expect(await repository.get(SOURCE_MESSAGE, WEBHOOK)).not.toBeNull();
		await repository.markDelivered(KEY, {
			targetMessageId: createMessageID(1000n),
			sourceFingerprint: 'fp0',
		});
		expect(await repository.delete(KEY, {state: 'delivered', target_message_id: createMessageID(999n)})).toBe(false);
		expect(await repository.delete(KEY, {state: 'delivered', target_message_id: createMessageID(1000n)})).toBe(true);
		expect(await repository.get(SOURCE_MESSAGE, WEBHOOK)).toBeNull();
	});

	it('pages published sources by channel and removes them', async () => {
		const messageIds: Array<MessageID> = [103n, 101n, 102n].map((id) => createMessageID(id));
		for (const sourceMessageId of messageIds) {
			await repository.addSource({sourceChannelId: SOURCE_CHANNEL, sourceMessageId});
		}
		await repository.addSource({sourceChannelId: SOURCE_CHANNEL, sourceMessageId: createMessageID(101n)});
		await repository.addSource({sourceChannelId: createChannelID(11n), sourceMessageId: createMessageID(104n)});
		expect(await repository.listSourcesByChannel(SOURCE_CHANNEL, {limit: 2})).toEqual([101n, 102n]);
		expect(
			await repository.listSourcesByChannel(SOURCE_CHANNEL, {afterMessageId: createMessageID(102n), limit: 2}),
		).toEqual([103n]);
		await repository.deleteSource({sourceChannelId: SOURCE_CHANNEL, sourceMessageId: createMessageID(102n)});
		expect(await repository.listSourcesByChannel(SOURCE_CHANNEL, {limit: 10})).toEqual([101n, 103n]);
		expect(await repository.listSourcesByChannel(createChannelID(11n), {limit: 10})).toEqual([104n]);
	});
});
