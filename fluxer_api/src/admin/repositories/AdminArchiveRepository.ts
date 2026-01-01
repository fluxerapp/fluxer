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

import {BatchBuilder, Db, fetchMany, fetchOne} from '~/database/Cassandra';
import type {AdminArchiveRow} from '~/database/CassandraTypes';
import {Logger} from '~/Logger';
import {AdminArchivesByRequester, AdminArchivesBySubject, AdminArchivesByType} from '~/Tables';
import {AdminArchive, type ArchiveSubjectType} from '../models/AdminArchiveModel';

const RETENTION_DAYS = 365;
const SECONDS_PER_DAY = 24 * 60 * 60;
const RETENTION_SECONDS = RETENTION_DAYS * SECONDS_PER_DAY;
const DEFAULT_RETENTION_MS = RETENTION_SECONDS * 1000;

function computeTtlSeconds(expiresAt: Date): number {
	const diffSeconds = Math.floor((expiresAt.getTime() - Date.now()) / 1000);
	return Math.max(diffSeconds, 1);
}

function filterExpired(rows: Array<AdminArchiveRow>, includeExpired: boolean): Array<AdminArchiveRow> {
	if (includeExpired) return rows;
	const now = Date.now();
	return rows.filter((row) => !row.expires_at || row.expires_at.getTime() > now);
}

export class AdminArchiveRepository {
	private ensureExpiry(archive: AdminArchive): AdminArchive {
		if (!archive.expiresAt) {
			archive.expiresAt = new Date(Date.now() + DEFAULT_RETENTION_MS);
		}
		return archive;
	}

	async create(archive: AdminArchive): Promise<void> {
		const withExpiry = this.ensureExpiry(archive);
		const row = withExpiry.toRow();
		const ttlSeconds = computeTtlSeconds(withExpiry.expiresAt!);

		const batch = new BatchBuilder();
		batch.addPrepared(
			AdminArchivesBySubject.insertWithTtlParam({...row, ttl_seconds: ttlSeconds} as AdminArchiveRow, 'ttl_seconds'),
		);
		batch.addPrepared(
			AdminArchivesByRequester.insertWithTtlParam({...row, ttl_seconds: ttlSeconds} as AdminArchiveRow, 'ttl_seconds'),
		);
		batch.addPrepared(
			AdminArchivesByType.insertWithTtlParam({...row, ttl_seconds: ttlSeconds} as AdminArchiveRow, 'ttl_seconds'),
		);
		await batch.execute();

		Logger.debug(
			{subjectType: withExpiry.subjectType, subjectId: withExpiry.subjectId, archiveId: withExpiry.archiveId},
			'Created admin archive record',
		);
	}

	async update(archive: AdminArchive): Promise<void> {
		const withExpiry = this.ensureExpiry(archive);
		const row = withExpiry.toRow();
		const ttlSeconds = computeTtlSeconds(withExpiry.expiresAt!);

		const batch = new BatchBuilder();
		batch.addPrepared(
			AdminArchivesBySubject.insertWithTtlParam({...row, ttl_seconds: ttlSeconds} as AdminArchiveRow, 'ttl_seconds'),
		);
		batch.addPrepared(
			AdminArchivesByRequester.insertWithTtlParam({...row, ttl_seconds: ttlSeconds} as AdminArchiveRow, 'ttl_seconds'),
		);
		batch.addPrepared(
			AdminArchivesByType.insertWithTtlParam({...row, ttl_seconds: ttlSeconds} as AdminArchiveRow, 'ttl_seconds'),
		);
		await batch.execute();

		Logger.debug(
			{subjectType: withExpiry.subjectType, subjectId: withExpiry.subjectId, archiveId: withExpiry.archiveId},
			'Updated admin archive record',
		);
	}

	async markAsStarted(archive: AdminArchive, progressStep = 'Starting archive'): Promise<void> {
		const withExpiry = this.ensureExpiry(archive);
		const ttlSeconds = computeTtlSeconds(withExpiry.expiresAt!);

		const batch = new BatchBuilder();
		batch.addPrepared(
			AdminArchivesBySubject.patchByPkWithTtlParam(
				{
					subject_type: withExpiry.subjectType,
					subject_id: withExpiry.subjectId,
					archive_id: withExpiry.archiveId,
				},
				{
					started_at: Db.set(new Date()),
					progress_percent: Db.set(0),
					progress_step: Db.set(progressStep),
				},
				'ttl_seconds',
				ttlSeconds,
			),
		);
		batch.addPrepared(
			AdminArchivesByRequester.patchByPkWithTtlParam(
				{
					requested_by: withExpiry.requestedBy,
					archive_id: withExpiry.archiveId,
				},
				{
					started_at: Db.set(new Date()),
					progress_percent: Db.set(0),
					progress_step: Db.set(progressStep),
				},
				'ttl_seconds',
				ttlSeconds,
			),
		);
		batch.addPrepared(
			AdminArchivesByType.patchByPkWithTtlParam(
				{
					subject_type: withExpiry.subjectType,
					archive_id: withExpiry.archiveId,
				},
				{
					started_at: Db.set(new Date()),
					progress_percent: Db.set(0),
					progress_step: Db.set(progressStep),
				},
				'ttl_seconds',
				ttlSeconds,
			),
		);
		await batch.execute();
	}

	async updateProgress(archive: AdminArchive, progressPercent: number, progressStep: string): Promise<void> {
		const withExpiry = this.ensureExpiry(archive);
		const ttlSeconds = computeTtlSeconds(withExpiry.expiresAt!);

		const batch = new BatchBuilder();
		batch.addPrepared(
			AdminArchivesBySubject.patchByPkWithTtlParam(
				{
					subject_type: withExpiry.subjectType,
					subject_id: withExpiry.subjectId,
					archive_id: withExpiry.archiveId,
				},
				{
					progress_percent: Db.set(progressPercent),
					progress_step: Db.set(progressStep),
				},
				'ttl_seconds',
				ttlSeconds,
			),
		);
		batch.addPrepared(
			AdminArchivesByRequester.patchByPkWithTtlParam(
				{
					requested_by: withExpiry.requestedBy,
					archive_id: withExpiry.archiveId,
				},
				{
					progress_percent: Db.set(progressPercent),
					progress_step: Db.set(progressStep),
				},
				'ttl_seconds',
				ttlSeconds,
			),
		);
		batch.addPrepared(
			AdminArchivesByType.patchByPkWithTtlParam(
				{
					subject_type: withExpiry.subjectType,
					archive_id: withExpiry.archiveId,
				},
				{
					progress_percent: Db.set(progressPercent),
					progress_step: Db.set(progressStep),
				},
				'ttl_seconds',
				ttlSeconds,
			),
		);
		await batch.execute();

		Logger.debug({archiveId: withExpiry.archiveId, progressPercent, progressStep}, 'Updated admin archive progress');
	}

	async markAsCompleted(
		archive: AdminArchive,
		storageKey: string,
		fileSize: bigint,
		downloadUrlExpiresAt: Date,
	): Promise<void> {
		const withExpiry = this.ensureExpiry(archive);
		const ttlSeconds = computeTtlSeconds(withExpiry.expiresAt!);

		const batch = new BatchBuilder();
		batch.addPrepared(
			AdminArchivesBySubject.patchByPkWithTtlParam(
				{
					subject_type: withExpiry.subjectType,
					subject_id: withExpiry.subjectId,
					archive_id: withExpiry.archiveId,
				},
				{
					completed_at: Db.set(new Date()),
					storage_key: Db.set(storageKey),
					file_size: Db.set(fileSize),
					download_url_expires_at: Db.set(downloadUrlExpiresAt),
					progress_percent: Db.set(100),
					progress_step: Db.set('Completed'),
				},
				'ttl_seconds',
				ttlSeconds,
			),
		);
		batch.addPrepared(
			AdminArchivesByRequester.patchByPkWithTtlParam(
				{
					requested_by: withExpiry.requestedBy,
					archive_id: withExpiry.archiveId,
				},
				{
					completed_at: Db.set(new Date()),
					storage_key: Db.set(storageKey),
					file_size: Db.set(fileSize),
					download_url_expires_at: Db.set(downloadUrlExpiresAt),
					progress_percent: Db.set(100),
					progress_step: Db.set('Completed'),
				},
				'ttl_seconds',
				ttlSeconds,
			),
		);
		batch.addPrepared(
			AdminArchivesByType.patchByPkWithTtlParam(
				{
					subject_type: withExpiry.subjectType,
					archive_id: withExpiry.archiveId,
				},
				{
					completed_at: Db.set(new Date()),
					storage_key: Db.set(storageKey),
					file_size: Db.set(fileSize),
					download_url_expires_at: Db.set(downloadUrlExpiresAt),
					progress_percent: Db.set(100),
					progress_step: Db.set('Completed'),
				},
				'ttl_seconds',
				ttlSeconds,
			),
		);
		await batch.execute();
	}

	async markAsFailed(archive: AdminArchive, errorMessage: string): Promise<void> {
		const withExpiry = this.ensureExpiry(archive);
		const ttlSeconds = computeTtlSeconds(withExpiry.expiresAt!);

		const batch = new BatchBuilder();
		batch.addPrepared(
			AdminArchivesBySubject.patchByPkWithTtlParam(
				{
					subject_type: withExpiry.subjectType,
					subject_id: withExpiry.subjectId,
					archive_id: withExpiry.archiveId,
				},
				{
					failed_at: Db.set(new Date()),
					error_message: Db.set(errorMessage),
					progress_step: Db.set('Failed'),
				},
				'ttl_seconds',
				ttlSeconds,
			),
		);
		batch.addPrepared(
			AdminArchivesByRequester.patchByPkWithTtlParam(
				{
					requested_by: withExpiry.requestedBy,
					archive_id: withExpiry.archiveId,
				},
				{
					failed_at: Db.set(new Date()),
					error_message: Db.set(errorMessage),
					progress_step: Db.set('Failed'),
				},
				'ttl_seconds',
				ttlSeconds,
			),
		);
		batch.addPrepared(
			AdminArchivesByType.patchByPkWithTtlParam(
				{
					subject_type: withExpiry.subjectType,
					archive_id: withExpiry.archiveId,
				},
				{
					failed_at: Db.set(new Date()),
					error_message: Db.set(errorMessage),
					progress_step: Db.set('Failed'),
				},
				'ttl_seconds',
				ttlSeconds,
			),
		);
		await batch.execute();
	}

	async findBySubjectAndArchiveId(
		subjectType: ArchiveSubjectType,
		subjectId: bigint,
		archiveId: bigint,
	): Promise<AdminArchive | null> {
		const query = AdminArchivesBySubject.select({
			where: [
				AdminArchivesBySubject.where.eq('subject_type'),
				AdminArchivesBySubject.where.eq('subject_id'),
				AdminArchivesBySubject.where.eq('archive_id'),
			],
			limit: 1,
		});
		const row = await fetchOne<AdminArchiveRow>(
			query.bind({
				subject_type: subjectType,
				subject_id: subjectId,
				archive_id: archiveId,
			}),
		);
		return row ? new AdminArchive(row) : null;
	}

	async listBySubject(
		subjectType: ArchiveSubjectType,
		subjectId: bigint,
		limit = 20,
		includeExpired = false,
	): Promise<Array<AdminArchive>> {
		const query = AdminArchivesBySubject.select({
			where: [AdminArchivesBySubject.where.eq('subject_type'), AdminArchivesBySubject.where.eq('subject_id')],
			limit,
		});
		const rows = await fetchMany<AdminArchiveRow>(
			query.bind({
				subject_type: subjectType,
				subject_id: subjectId,
			}),
		);
		return filterExpired(rows, includeExpired).map((row) => new AdminArchive(row));
	}

	async listByType(subjectType: ArchiveSubjectType, limit = 50, includeExpired = false): Promise<Array<AdminArchive>> {
		const query = AdminArchivesByType.select({
			where: AdminArchivesByType.where.eq('subject_type'),
			limit,
		});
		const rows = await fetchMany<AdminArchiveRow>(
			query.bind({
				subject_type: subjectType,
			}),
		);
		return filterExpired(rows, includeExpired).map((row) => new AdminArchive(row));
	}

	async listByRequester(requestedBy: bigint, limit = 50, includeExpired = false): Promise<Array<AdminArchive>> {
		const query = AdminArchivesByRequester.select({
			where: AdminArchivesByRequester.where.eq('requested_by'),
			limit,
		});
		const rows = await fetchMany<AdminArchiveRow>(
			query.bind({
				requested_by: requestedBy,
			}),
		);
		return filterExpired(rows, includeExpired).map((row) => new AdminArchive(row));
	}
}
