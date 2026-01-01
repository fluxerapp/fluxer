/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {AttachmentID, ChannelID, MessageID} from '~/BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne} from '~/database/Cassandra';
import {AttachmentDecayByExpiry, AttachmentDecayById} from '~/Tables';
import type {AttachmentDecayRow} from '~/types/AttachmentDecayTypes';

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

const createFetchExpiredByBucketQuery = (limit: number) =>
	AttachmentDecayByExpiry.selectCql({
		where: [
			AttachmentDecayByExpiry.where.eq('expiry_bucket'),
			AttachmentDecayByExpiry.where.lte('expires_at', 'current_time'),
		],
		limit,
	});

export class AttachmentDecayRepository {
	async upsert(record: AttachmentDecayRow & {expiry_bucket: number}): Promise<void> {
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

	async fetchExpiredByBucket(bucket: number, currentTime: Date, limit = 200): Promise<Array<AttachmentDecayExpiryRow>> {
		const query = createFetchExpiredByBucketQuery(limit);
		return fetchMany(query, {expiry_bucket: bucket, current_time: currentTime});
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
		const query = AttachmentDecayByExpiry.selectCql({
			where: [AttachmentDecayByExpiry.where.eq('expiry_bucket')],
			limit,
		});
		return fetchMany<AttachmentDecayExpiryRow>(query, {expiry_bucket: bucket});
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
				`${date.getUTCFullYear()}${String(date.getUTCMonth() + 1).padStart(2, '0')}${String(date.getUTCDate()).padStart(
					2,
					'0',
				)}`,
				10,
			);

			const deletedInBucket = await this.deleteAllByBucket(bucket);
			totalDeleted += deletedInBucket;
		}

		return totalDeleted;
	}
}
