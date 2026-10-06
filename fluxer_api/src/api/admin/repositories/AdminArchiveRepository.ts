// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {randomUUID} from 'node:crypto';
import {AdminArchive} from '@app/api/admin/models/AdminArchiveModel';
import {ArchiveAttemptSupersededError} from '@app/api/archive/ArchiveAttemptSupersededError';
import {BatchBuilder, executeConditional, fetchMany, fetchOne} from '@app/api/database/CassandraQueryExecution';
import {Db, type DbOp} from '@app/api/database/CassandraTypes';
import {
	ADMIN_ARCHIVE_INDEX_COLUMNS,
	type AdminArchiveIndexRow,
	type AdminArchiveRow,
} from '@app/api/database/types/AdminArchiveTypes';
import {Logger} from '@app/api/Logger';
import {AdminArchivesByRequester, AdminArchivesBySubject, AdminArchivesByType} from '@app/api/Tables';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import type {ArchiveSubjectType} from '@fluxer/schema/src/domains/admin/AdminArchiveSchemas';
import {ms} from 'itty-time';

const RETENTION_DAYS = 365;
const DEFAULT_RETENTION_MS = ms(`${RETENTION_DAYS} days`);
const ARCHIVE_READ_CONCURRENCY = 10;

interface AdminArchivePatch {
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

function computeTtlSeconds(expiresAt: Date | null): number {
	if (expiresAt === null || !Number.isFinite(expiresAt.getTime())) {
		throw new Error('Admin archive has no valid expiry');
	}
	const ttlSeconds = Math.floor((expiresAt.getTime() - Date.now()) / 1000);
	if (ttlSeconds < 1) throw new Error('Admin archive has expired');
	return ttlSeconds;
}

function filterExpired(rows: Array<AdminArchiveRow>, includeExpired: boolean): Array<AdminArchiveRow> {
	if (includeExpired) return rows;
	const now = Date.now();
	return rows.filter((row) => !row.expires_at || row.expires_at.getTime() > now);
}

export class AdminArchiveRepository {
	async create(archive: AdminArchive): Promise<void> {
		archive.expiresAt ??= new Date(archive.requestedAt.getTime() + DEFAULT_RETENTION_MS);
		const row = archive.toRow();
		const applied = await executeConditional(
			AdminArchivesBySubject.insertIfNotExistsWithTtl(row, computeTtlSeconds(row.expires_at)),
		);
		if (!applied) {
			throw new Error(`Admin archive ${row.archive_id} for ${row.subject_type} ${row.subject_id} already exists`);
		}
		const indexRow: AdminArchiveIndexRow = {
			subject_type: row.subject_type,
			subject_id: row.subject_id,
			archive_id: row.archive_id,
			requested_by: row.requested_by,
		};
		const ttlSeconds = computeTtlSeconds(row.expires_at);
		const batch = new BatchBuilder();
		batch.addPrepared(AdminArchivesByRequester.insertWithTtl(indexRow, ttlSeconds));
		batch.addPrepared(AdminArchivesByType.insertWithTtl(indexRow, ttlSeconds));
		await batch.execute();
		Logger.debug(
			{subjectType: archive.subjectType, subjectId: archive.subjectId, archiveId: archive.archiveId},
			'Created admin archive record',
		);
	}

	private async patchOwned(archive: AdminArchive, patch: AdminArchivePatch): Promise<void> {
		assert(archive.attemptId !== null, 'Admin archive mutation requires a claimed attempt');
		const applied = await executeConditional(
			AdminArchivesBySubject.conditionalPatchByPkWithTtl(
				{
					subject_type: archive.subjectType,
					subject_id: archive.subjectId,
					archive_id: archive.archiveId,
				},
				patch,
				{
					requested_at: archive.requestedAt,
					attempt_id: archive.attemptId,
					completed_at: null,
					failed_at: null,
					terminal_failed_at: null,
				},
				computeTtlSeconds(archive.expiresAt),
			),
		);
		if (!applied) throw new ArchiveAttemptSupersededError();
	}

	private async hydrateIndexRows(
		rows: ReadonlyArray<AdminArchiveIndexRow>,
		includeExpired: boolean,
	): Promise<Array<AdminArchive>> {
		const archives = await mapWithConcurrency(rows, ARCHIVE_READ_CONCURRENCY, (row) =>
			this.findBySubjectAndArchiveId(row.subject_type, row.subject_id, row.archive_id),
		);
		const now = Date.now();
		return archives.filter(
			(archive): archive is AdminArchive =>
				archive !== null && (includeExpired || archive.expiresAt === null || archive.expiresAt.getTime() > now),
		);
	}

	async markAsStarted(archive: AdminArchive, progressStep = 'Starting archive'): Promise<AdminArchive> {
		const attempt = new AdminArchive({
			...archive.toRow(),
			attempt_id: randomUUID(),
			started_at: new Date(),
			failed_at: null,
			error_message: null,
			progress_percent: 0,
			progress_step: progressStep,
		});
		const applied = await executeConditional(
			AdminArchivesBySubject.conditionalPatchByPkWithTtl(
				{
					subject_type: archive.subjectType,
					subject_id: archive.subjectId,
					archive_id: archive.archiveId,
				},
				{
					attempt_id: Db.set(attempt.attemptId),
					started_at: Db.set(attempt.startedAt),
					failed_at: Db.clear(),
					error_message: Db.clear(),
					progress_percent: Db.set(0),
					progress_step: Db.set(progressStep),
				},
				{
					requested_at: archive.requestedAt,
					attempt_id: archive.attemptId,
					completed_at: null,
					terminal_failed_at: null,
				},
				computeTtlSeconds(archive.expiresAt),
			),
		);
		if (!applied) throw new ArchiveAttemptSupersededError();
		return attempt;
	}

	async updateProgress(archive: AdminArchive, progressPercent: number, progressStep: string): Promise<void> {
		await this.patchOwned(archive, {
			progress_percent: Db.set(progressPercent),
			progress_step: Db.set(progressStep),
		});
		Logger.debug({archiveId: archive.archiveId, progressPercent, progressStep}, 'Updated admin archive progress');
	}

	async markAsCompleted(
		archive: AdminArchive,
		storageKey: string,
		fileSize: bigint,
		downloadUrlExpiresAt: Date,
	): Promise<void> {
		await this.patchOwned(archive, {
			completed_at: Db.set(new Date()),
			failed_at: Db.clear(),
			error_message: Db.clear(),
			storage_key: Db.set(storageKey),
			file_size: Db.set(fileSize),
			download_url_expires_at: Db.set(downloadUrlExpiresAt),
			progress_percent: Db.set(100),
			progress_step: Db.set('Completed'),
		});
	}

	async markAsFailed(archive: AdminArchive, errorMessage: string): Promise<void> {
		await this.patchOwned(archive, {
			failed_at: Db.set(new Date()),
			error_message: Db.set(errorMessage),
			progress_step: Db.set('Failed'),
		});
	}

	async markAsTerminallyFailed(archive: AdminArchive, errorMessage: string): Promise<void> {
		const failedAt = new Date();
		await this.patchOwned(archive, {
			failed_at: Db.set(failedAt),
			terminal_failed_at: Db.set(failedAt),
			error_message: Db.set(errorMessage),
			progress_step: Db.set('Failed'),
		});
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
			columns: ADMIN_ARCHIVE_INDEX_COLUMNS,
			where: AdminArchivesByType.where.eq('subject_type'),
			limit,
		});
		const rows = await fetchMany<AdminArchiveIndexRow>(
			query.bind({
				subject_type: subjectType,
			}),
		);
		return this.hydrateIndexRows(rows, includeExpired);
	}

	async listByRequester(requestedBy: bigint, limit = 50, includeExpired = false): Promise<Array<AdminArchive>> {
		const query = AdminArchivesByRequester.select({
			columns: ADMIN_ARCHIVE_INDEX_COLUMNS,
			where: AdminArchivesByRequester.where.eq('requested_by'),
			limit,
		});
		const rows = await fetchMany<AdminArchiveIndexRow>(
			query.bind({
				requested_by: requestedBy,
			}),
		);
		return this.hydrateIndexRows(rows, includeExpired);
	}
}
