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

import type {AdminArchiveRow} from '~/database/types/AdminArchiveTypes';

export type ArchiveSubjectType = 'user' | 'guild';

export class AdminArchive {
	subjectType: ArchiveSubjectType;
	subjectId: bigint;
	archiveId: bigint;
	requestedBy: bigint;
	requestedAt: Date;
	startedAt: Date | null;
	completedAt: Date | null;
	failedAt: Date | null;
	storageKey: string | null;
	fileSize: bigint | null;
	progressPercent: number;
	progressStep: string | null;
	errorMessage: string | null;
	downloadUrlExpiresAt: Date | null;
	expiresAt: Date | null;

	constructor(row: AdminArchiveRow) {
		this.subjectType = row.subject_type;
		this.subjectId = row.subject_id;
		this.archiveId = row.archive_id;
		this.requestedBy = row.requested_by;
		this.requestedAt = row.requested_at;
		this.startedAt = row.started_at ?? null;
		this.completedAt = row.completed_at ?? null;
		this.failedAt = row.failed_at ?? null;
		this.storageKey = row.storage_key ?? null;
		this.fileSize = row.file_size ?? null;
		this.progressPercent = row.progress_percent;
		this.progressStep = row.progress_step ?? null;
		this.errorMessage = row.error_message ?? null;
		this.downloadUrlExpiresAt = row.download_url_expires_at ?? null;
		this.expiresAt = row.expires_at ?? null;
	}

	toRow(): AdminArchiveRow {
		return {
			subject_type: this.subjectType,
			subject_id: this.subjectId,
			archive_id: this.archiveId,
			requested_by: this.requestedBy,
			requested_at: this.requestedAt,
			started_at: this.startedAt,
			completed_at: this.completedAt,
			failed_at: this.failedAt,
			storage_key: this.storageKey,
			file_size: this.fileSize,
			progress_percent: this.progressPercent,
			progress_step: this.progressStep,
			error_message: this.errorMessage,
			download_url_expires_at: this.downloadUrlExpiresAt,
			expires_at: this.expiresAt,
		};
	}

	toResponse(): {
		archive_id: string;
		subject_type: ArchiveSubjectType;
		subject_id: string;
		requested_by: string;
		requested_at: string;
		started_at: string | null;
		completed_at: string | null;
		failed_at: string | null;
		file_size: string | null;
		progress_percent: number;
		progress_step: string | null;
		error_message: string | null;
		download_url_expires_at: string | null;
		expires_at: string | null;
	} {
		return {
			archive_id: this.archiveId.toString(),
			subject_type: this.subjectType,
			subject_id: this.subjectId.toString(),
			requested_by: this.requestedBy.toString(),
			requested_at: this.requestedAt.toISOString(),
			started_at: this.startedAt?.toISOString() ?? null,
			completed_at: this.completedAt?.toISOString() ?? null,
			failed_at: this.failedAt?.toISOString() ?? null,
			file_size: this.fileSize?.toString() ?? null,
			progress_percent: this.progressPercent,
			progress_step: this.progressStep,
			error_message: this.errorMessage,
			download_url_expires_at: this.downloadUrlExpiresAt?.toISOString() ?? null,
			expires_at: this.expiresAt?.toISOString() ?? null,
		};
	}
}

export type AdminArchiveResponse = ReturnType<AdminArchive['toResponse']>;
