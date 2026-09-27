// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageID} from '../../BrandedTypes';
import {BatchBuilder, fetchMany, fetchManyInChunks, fetchOne} from '../../database/CassandraQueryExecution';
import {PollMessageById, PollMessageExpiry, type PollMessageExpiryRow} from '../../Tables';

export function getExpiryBucket(expiresAt: Date): number {
	return Number(
		`${expiresAt.getUTCFullYear()}${String(expiresAt.getUTCMonth() + 1).padStart(2, '0')}${String(expiresAt.getUTCDate()).padStart(2, '0')}`,
	);
}

const FETCH_BY_ID_CQL = PollMessageById.selectCql({
	where: PollMessageById.where.eq('message_id'),
	limit: 1,
});
const FETCH_BY_IDS_CQL = PollMessageById.selectCql({
	where: PollMessageById.where.in('message_id', 'message_ids'),
});
const createFetchExpiredByBucketQuery = (limit: number) =>
	PollMessageExpiry.select({
		where: [PollMessageExpiry.where.eq('expiry_bucket'), PollMessageExpiry.where.lte('expires_at', 'current_time')],
		limit,
	});

export class PollMessageExpiryRepository {
	async upsert(record: PollMessageExpiryRow): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(PollMessageById.upsertAll(record));
		batch.addPrepared(
			PollMessageExpiry.upsertAll({
				expiry_bucket: record.expiry_bucket,
				expires_at: record.expires_at,
				channel_id: record.channel_id,
				message_id: record.message_id,
			}),
		);
		await batch.execute();
	}

	async fetchById(messageId: MessageID): Promise<PollMessageExpiryRow | null> {
		const row = await fetchOne<PollMessageExpiryRow>(FETCH_BY_ID_CQL, {message_id: messageId});
		return row ?? null;
	}

	async fetchByIds(messageIds: Array<MessageID>): Promise<Map<MessageID, PollMessageExpiryRow>> {
		if (messageIds.length === 0) return new Map();
		const rows = await fetchManyInChunks<PollMessageExpiryRow, MessageID>(FETCH_BY_IDS_CQL, messageIds, (chunk) => ({
			message_ids: new Set(chunk),
		}));
		const map = new Map<MessageID, PollMessageExpiryRow>();
		for (const row of rows) {
			map.set(row.message_id, row);
		}
		return map;
	}

	async fetchExpiredByBucket(bucket: number, currentTime: Date, limit = 200): Promise<Array<PollMessageExpiryRow>> {
		const query = createFetchExpiredByBucketQuery(limit);
		return fetchMany(query.bind({expiry_bucket: bucket, current_time: currentTime}));
	}

	async deleteRecords(params: {expiry_bucket: number; expires_at: Date; message_id: MessageID}): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			PollMessageExpiry.deleteByPk({
				expiry_bucket: params.expiry_bucket,
				expires_at: params.expires_at,
				message_id: params.message_id,
			}),
		);
		batch.addPrepared(PollMessageById.deleteByPk({message_id: params.message_id}));
		await batch.execute();
	}

	async fetchAllByBucket(bucket: number, limit = 200): Promise<Array<PollMessageExpiryRow>> {
		const query = PollMessageExpiry.select({
			where: [PollMessageExpiry.where.eq('expiry_bucket')],
			limit,
		});
		return fetchMany<PollMessageExpiryRow>(query.bind({expiry_bucket: bucket}));
	}

	async deleteAllByBucket(bucket: number): Promise<number> {
		const records = await this.fetchAllByBucket(bucket);
		if (records.length === 0) return 0;
		const batch = new BatchBuilder();
		for (const record of records) {
			batch.addPrepared(
				PollMessageExpiry.deleteByPk({
					expiry_bucket: record.expiry_bucket,
					expires_at: record.expires_at,
					message_id: record.message_id,
				}),
			);
			batch.addPrepared(PollMessageById.deleteByPk({message_id: record.message_id}));
		}
		await batch.execute();
		return records.length;
	}

	async clearAll(days = 30): Promise<number> {
		let totalDeleted = 0;
		for (let i = 0; i < days; i++) {
			const date = new Date();
			date.setUTCDate(date.getUTCDate() - i);
			const bucket = getExpiryBucket(date);
			const deletedInBucket = await this.deleteAllByBucket(bucket);
			totalDeleted += deletedInBucket;
		}
		return totalDeleted;
	}
}
