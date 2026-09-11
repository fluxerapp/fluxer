// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminArchiveResponse, ArchiveSubjectType} from '@fluxer/schema/src/domains/admin/AdminArchiveSchemas';
import type {AdminArchiveRow} from '../../database/types/AdminArchiveTypes';

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

	toResponse(): AdminArchiveResponse {
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
