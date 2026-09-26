// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {randomUUID} from 'node:crypto';
import {ArchiveAttemptSupersededError} from '@app/api/archive/ArchiveAttemptSupersededError';
import type {UserID} from '@app/api/BrandedTypes';
import {executeConditional, fetchMany, fetchOne} from '@app/api/database/CassandraQueryExecution';
import {Db, type DbOp} from '@app/api/database/CassandraTypes';
import type {UserHarvestRow} from '@app/api/database/types/UserTypes';
import {Logger} from '@app/api/Logger';
import {UserHarvests} from '@app/api/Tables';
import {UserHarvest} from '@app/api/user/UserHarvestModel';
import {UnknownHarvestError} from '@fluxer/errors/src/domains/moderation/UnknownHarvestError';

const FIND_HARVEST_CQL = UserHarvests.selectCql({
	where: [UserHarvests.where.eq('user_id'), UserHarvests.where.eq('harvest_id')],
});
const createFindUserHarvestsQuery = (limit: number) =>
	UserHarvests.select({
		where: UserHarvests.where.eq('user_id'),
		limit,
	});
const FIND_LATEST_HARVEST_CQL = UserHarvests.selectCql({
	where: UserHarvests.where.eq('user_id'),
	limit: 1,
});

interface UserHarvestPatch {
	completed_at?: DbOp<Date>;
	failed_at?: DbOp<Date>;
	terminal_failed_at?: DbOp<Date>;
	storage_key?: DbOp<string>;
	file_size?: DbOp<bigint>;
	progress_percent?: DbOp<number>;
	progress_step?: DbOp<string>;
	error_message?: DbOp<string>;
	download_url_expires_at?: DbOp<Date>;
}

export class UserHarvestRepository {
	async create(harvest: UserHarvest): Promise<void> {
		const applied = await executeConditional(UserHarvests.insertIfNotExists(harvest.toRow()));
		if (!applied) throw new Error(`Harvest ${harvest.harvestId} for user ${harvest.userId} already exists`);
		Logger.debug({userId: harvest.userId, harvestId: harvest.harvestId}, 'Created harvest record');
	}

	async findByUserAndHarvestId(userId: UserID, harvestId: bigint): Promise<UserHarvest | null> {
		const row = await fetchOne<UserHarvestRow>(FIND_HARVEST_CQL, {
			user_id: userId,
			harvest_id: harvestId,
		});
		return row ? new UserHarvest(row) : null;
	}

	async findByUserId(userId: UserID, limit: number = 10): Promise<Array<UserHarvest>> {
		const rows = await fetchMany<UserHarvestRow>(
			createFindUserHarvestsQuery(limit).bind({
				user_id: userId,
			}),
		);
		return rows.map((row) => new UserHarvest(row));
	}

	async findLatestByUserId(userId: UserID): Promise<UserHarvest | null> {
		const row = await fetchOne<UserHarvestRow>(FIND_LATEST_HARVEST_CQL, {
			user_id: userId,
		});
		return row ? new UserHarvest(row) : null;
	}

	async markAsStarted(harvest: UserHarvest): Promise<UserHarvest> {
		const attemptId = randomUUID();
		const startedAt = new Date();
		const applied = await executeConditional(
			UserHarvests.conditionalPatchByPk(
				{user_id: harvest.userId, harvest_id: harvest.harvestId},
				{
					attempt_id: Db.set(attemptId),
					started_at: Db.set(startedAt),
					failed_at: Db.clear(),
					error_message: Db.clear(),
					progress_percent: Db.set(0),
					progress_step: Db.set('Starting harvest'),
				},
				{
					requested_at: harvest.requestedAt,
					attempt_id: harvest.attemptId ?? null,
					completed_at: null,
					terminal_failed_at: null,
				},
			),
		);
		if (!applied) throw new ArchiveAttemptSupersededError();
		Logger.debug({userId: harvest.userId, harvestId: harvest.harvestId, attemptId}, 'Claimed harvest attempt');
		return new UserHarvest({
			...harvest.toRow(),
			attempt_id: attemptId,
			started_at: startedAt,
			failed_at: null,
			error_message: null,
			progress_percent: 0,
			progress_step: 'Starting harvest',
		});
	}

	async updateProgress(harvest: UserHarvest, progressPercent: number, progressStep: string): Promise<void> {
		await this.patchOwned(harvest, {
			progress_percent: Db.set(progressPercent),
			progress_step: Db.set(progressStep),
		});
		Logger.debug(
			{userId: harvest.userId, harvestId: harvest.harvestId, progressPercent, progressStep},
			'Updated harvest progress',
		);
	}

	async markAsCompleted(
		harvest: UserHarvest,
		storageKey: string,
		fileSize: bigint,
		downloadUrlExpiresAt: Date,
	): Promise<void> {
		await this.patchOwned(harvest, {
			completed_at: Db.set(new Date()),
			failed_at: Db.clear(),
			error_message: Db.clear(),
			storage_key: Db.set(storageKey),
			file_size: Db.set(fileSize),
			download_url_expires_at: Db.set(downloadUrlExpiresAt),
			progress_percent: Db.set(100),
			progress_step: Db.set('Completed'),
		});
		Logger.debug(
			{userId: harvest.userId, harvestId: harvest.harvestId, storageKey, fileSize},
			'Marked harvest as completed',
		);
	}

	async markAsFailed(harvest: UserHarvest, errorMessage: string): Promise<void> {
		await this.patchOwned(harvest, {
			failed_at: Db.set(new Date()),
			error_message: Db.set(errorMessage),
		});
		Logger.error({userId: harvest.userId, harvestId: harvest.harvestId, errorMessage}, 'Marked harvest as failed');
	}

	async markAsTerminallyFailed(harvest: UserHarvest, errorMessage: string): Promise<void> {
		const failedAt = new Date();
		await this.patchOwned(harvest, {
			failed_at: Db.set(failedAt),
			terminal_failed_at: Db.set(failedAt),
			error_message: Db.set(errorMessage),
			progress_step: Db.set('Failed'),
		});
		Logger.error(
			{userId: harvest.userId, harvestId: harvest.harvestId, errorMessage},
			'Marked harvest as terminally failed',
		);
	}

	async setDownloadUrlExpiry(userId: UserID, harvestId: bigint, expiresAt: Date): Promise<void> {
		const harvest = await this.findByUserAndHarvestId(userId, harvestId);
		if (!harvest) throw new UnknownHarvestError();
		const applied = await executeConditional(
			UserHarvests.conditionalPatchByPk(
				{user_id: userId, harvest_id: harvestId},
				{download_url_expires_at: Db.set(expiresAt)},
				{requested_at: harvest.requestedAt},
			),
		);
		if (!applied) throw new UnknownHarvestError();
	}

	private async patchOwned(harvest: UserHarvest, patch: UserHarvestPatch): Promise<void> {
		assert(harvest.attemptId, 'A claimed harvest attempt is required');
		const applied = await executeConditional(
			UserHarvests.conditionalPatchByPk({user_id: harvest.userId, harvest_id: harvest.harvestId}, patch, {
				requested_at: harvest.requestedAt,
				attempt_id: harvest.attemptId,
				completed_at: null,
				failed_at: null,
				terminal_failed_at: null,
			}),
		);
		if (!applied) throw new ArchiveAttemptSupersededError();
	}
}
