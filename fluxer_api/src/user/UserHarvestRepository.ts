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

import type {UserID} from '~/BrandedTypes';
import {Db, fetchMany, fetchOne, upsertOne} from '~/database/Cassandra';
import type {UserHarvestRow} from '~/database/CassandraTypes';
import {Logger} from '~/Logger';
import {UserHarvests} from '~/Tables';
import {UserHarvest} from './UserHarvestModel';

const FIND_HARVEST_CQL = UserHarvests.selectCql({
	where: [UserHarvests.where.eq('user_id'), UserHarvests.where.eq('harvest_id')],
});

const createFindUserHarvestsQuery = (limit: number) =>
	UserHarvests.selectCql({
		where: UserHarvests.where.eq('user_id'),
		limit,
	});

const FIND_LATEST_HARVEST_CQL = UserHarvests.selectCql({
	where: UserHarvests.where.eq('user_id'),
	limit: 1,
});

export class UserHarvestRepository {
	async create(harvest: UserHarvest): Promise<void> {
		await upsertOne(UserHarvests.upsertAll(harvest.toRow()));
		Logger.debug({userId: harvest.userId, harvestId: harvest.harvestId}, 'Created harvest record');
	}

	async update(harvest: UserHarvest): Promise<void> {
		const row = harvest.toRow();
		await upsertOne(
			UserHarvests.patchByPk(
				{user_id: row.user_id, harvest_id: row.harvest_id},
				{
					started_at: Db.set(row.started_at),
					completed_at: Db.set(row.completed_at),
					failed_at: Db.set(row.failed_at),
					storage_key: Db.set(row.storage_key),
					file_size: Db.set(row.file_size),
					progress_percent: Db.set(row.progress_percent),
					progress_step: Db.set(row.progress_step),
					error_message: Db.set(row.error_message),
					download_url_expires_at: Db.set(row.download_url_expires_at),
				},
			),
		);
		Logger.debug({userId: harvest.userId, harvestId: harvest.harvestId}, 'Updated harvest record');
	}

	async findByUserAndHarvestId(userId: UserID, harvestId: bigint): Promise<UserHarvest | null> {
		const row = await fetchOne<UserHarvestRow>(FIND_HARVEST_CQL, {
			user_id: userId,
			harvest_id: harvestId,
		});
		return row ? new UserHarvest(row) : null;
	}

	async findByUserId(userId: UserID, limit: number = 10): Promise<Array<UserHarvest>> {
		const rows = await fetchMany<UserHarvestRow>(createFindUserHarvestsQuery(limit), {
			user_id: userId,
		});
		return rows.map((row) => new UserHarvest(row));
	}

	async findLatestByUserId(userId: UserID): Promise<UserHarvest | null> {
		const row = await fetchOne<UserHarvestRow>(FIND_LATEST_HARVEST_CQL, {
			user_id: userId,
		});
		return row ? new UserHarvest(row) : null;
	}

	async updateProgress(
		userId: UserID,
		harvestId: bigint,
		progressPercent: number,
		progressStep: string,
	): Promise<void> {
		await upsertOne(
			UserHarvests.patchByPk(
				{user_id: userId, harvest_id: harvestId},
				{
					progress_percent: Db.set(progressPercent),
					progress_step: Db.set(progressStep),
				},
			),
		);
		Logger.debug({userId, harvestId, progressPercent, progressStep}, 'Updated harvest progress');
	}

	async markAsStarted(userId: UserID, harvestId: bigint): Promise<void> {
		await upsertOne(
			UserHarvests.patchByPk(
				{user_id: userId, harvest_id: harvestId},
				{
					started_at: Db.set(new Date()),
					progress_percent: Db.set(0),
					progress_step: Db.set('Starting harvest'),
				},
			),
		);
		Logger.debug({userId, harvestId}, 'Marked harvest as started');
	}

	async markAsCompleted(
		userId: UserID,
		harvestId: bigint,
		storageKey: string,
		fileSize: bigint,
		downloadUrlExpiresAt: Date,
	): Promise<void> {
		await upsertOne(
			UserHarvests.patchByPk(
				{user_id: userId, harvest_id: harvestId},
				{
					completed_at: Db.set(new Date()),
					storage_key: Db.set(storageKey),
					file_size: Db.set(fileSize),
					download_url_expires_at: Db.set(downloadUrlExpiresAt),
					progress_percent: Db.set(100),
					progress_step: Db.set('Completed'),
				},
			),
		);
		Logger.debug({userId, harvestId, storageKey, fileSize}, 'Marked harvest as completed');
	}

	async markAsFailed(userId: UserID, harvestId: bigint, errorMessage: string): Promise<void> {
		await upsertOne(
			UserHarvests.patchByPk(
				{user_id: userId, harvest_id: harvestId},
				{
					failed_at: Db.set(new Date()),
					error_message: Db.set(errorMessage),
				},
			),
		);
		Logger.error({userId, harvestId, errorMessage}, 'Marked harvest as failed');
	}
}
