// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID, MessageID} from '../BrandedTypes';
import {
	BatchBuilder,
	deleteOneOrMany,
	fetchMany,
	fetchManyInChunks,
	fetchOne,
} from '../database/CassandraQueryExecution';
import {AttachmentDecayByExpiry, AttachmentDecayById} from '../Tables';
import type {AttachmentDecayRow} from '../types/AttachmentDecayTypes';

interface AttachmentDecayExpiryRow {
	expiry_bucket: number;
	expires_at: Date;
	attachment_id: AttachmentID;
	channel_id: ChannelID;
	message_id: MessageID;
}

const FETCH_BY_ID_CQL = AttachmentDecayById.selectCql({
	where: AttachmentDecayById.where.eq('attachment_id'),
	limit: 1,
});
const FETCH_BY_IDS_CQL = AttachmentDecayById.selectCql({
	where: AttachmentDecayById.where.in('attachment_id', 'attachment_ids'),
});
const createFetchExpiredByBucketQuery = (limit: number) =>
	AttachmentDecayByExpiry.select({
		where: [
			AttachmentDecayByExpiry.where.eq('expiry_bucket'),
			AttachmentDecayByExpiry.where.lte('expires_at', 'current_time'),
		],
		limit,
	});

export class AttachmentDecayRepository {
	async upsert(
		record: AttachmentDecayRow & {
			expiry_bucket: number;
		},
	): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(AttachmentDecayById.upsertAll(record));
		batch.addPrepared(
			AttachmentDecayByExpiry.upsertAll({
				expiry_bucket: record.expiry_bucket,
				expires_at: record.expires_at,
				attachment_id: record.attachment_id,
				channel_id: record.channel_id,
				message_id: record.message_id,
			}),
		);
		await batch.execute();
	}

	async fetchById(attachmentId: AttachmentID): Promise<AttachmentDecayRow | null> {
		const row = await fetchOne<AttachmentDecayRow>(FETCH_BY_ID_CQL, {attachment_id: attachmentId});
		return row ?? null;
	}

	async fetchByIds(attachmentIds: Array<AttachmentID>): Promise<Map<AttachmentID, AttachmentDecayRow>> {
		if (attachmentIds.length === 0) return new Map();
		const rows = await fetchManyInChunks<AttachmentDecayRow, AttachmentID>(
			FETCH_BY_IDS_CQL,
			attachmentIds,
			(chunk) => ({attachment_ids: new Set(chunk)}),
		);
		const map = new Map<AttachmentID, AttachmentDecayRow>();
		for (const row of rows) {
			map.set(row.attachment_id, row);
		}
		return map;
	}

	async fetchExpiredByBucket(bucket: number, currentTime: Date, limit = 200): Promise<Array<AttachmentDecayExpiryRow>> {
		const query = createFetchExpiredByBucketQuery(limit);
		return fetchMany(query.bind({expiry_bucket: bucket, current_time: currentTime}));
	}

	async deleteExpiryRecord(params: {
		expiry_bucket: number;
		expires_at: Date;
		attachment_id: AttachmentID;
	}): Promise<void> {
		await deleteOneOrMany(
			AttachmentDecayByExpiry.deleteByPk({
				expiry_bucket: params.expiry_bucket,
				expires_at: params.expires_at,
				attachment_id: params.attachment_id,
			}),
		);
	}

	async deleteRecords(params: {expiry_bucket: number; expires_at: Date; attachment_id: AttachmentID}): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			AttachmentDecayByExpiry.deleteByPk({
				expiry_bucket: params.expiry_bucket,
				expires_at: params.expires_at,
				attachment_id: params.attachment_id,
			}),
		);
		batch.addPrepared(AttachmentDecayById.deleteByPk({attachment_id: params.attachment_id}));
		await batch.execute();
	}

	async fetchAllByBucket(bucket: number, limit = 200): Promise<Array<AttachmentDecayExpiryRow>> {
		const query = AttachmentDecayByExpiry.select({
			where: [AttachmentDecayByExpiry.where.eq('expiry_bucket')],
			limit,
		});
		return fetchMany<AttachmentDecayExpiryRow>(query.bind({expiry_bucket: bucket}));
	}

	async deleteAllByBucket(bucket: number): Promise<number> {
		const records = await this.fetchAllByBucket(bucket);
		if (records.length === 0) return 0;
		const batch = new BatchBuilder();
		for (const record of records) {
			batch.addPrepared(
				AttachmentDecayByExpiry.deleteByPk({
					expiry_bucket: record.expiry_bucket,
					expires_at: record.expires_at,
					attachment_id: record.attachment_id,
				}),
			);
			batch.addPrepared(AttachmentDecayById.deleteByPk({attachment_id: record.attachment_id}));
		}
		await batch.execute();
		return records.length;
	}

	async clearAll(days = 30): Promise<number> {
		let totalDeleted = 0;
		for (let i = 0; i < days; i++) {
			const date = new Date();
			date.setUTCDate(date.getUTCDate() - i);
			const bucket = parseInt(
				`${date.getUTCFullYear()}${String(date.getUTCMonth() + 1).padStart(2, '0')}${String(date.getUTCDate()).padStart(2, '0')}`,
				10,
			);
			const deletedInBucket = await this.deleteAllByBucket(bucket);
			totalDeleted += deletedInBucket;
		}
		return totalDeleted;
	}
}
