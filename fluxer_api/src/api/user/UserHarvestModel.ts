// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HarvestStatus, HarvestStatusResponse} from '@fluxer/schema/src/domains/user/UserHarvestSchemas';
import type {UserID} from '../BrandedTypes';
import type {UserHarvestRow} from '../database/types/UserTypes';

export class UserHarvest {
	userId: UserID;
	harvestId: bigint;
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

	constructor(row: UserHarvestRow) {
		this.userId = row.user_id;
		this.harvestId = row.harvest_id;
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
	}

	toRow(): UserHarvestRow {
		return {
			user_id: this.userId,
			harvest_id: this.harvestId,
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
		};
	}

	toResponse(): HarvestStatusResponse {
		return {
			harvest_id: this.harvestId.toString(),
			status: this.getStatus(),
			created_at: this.requestedAt.toISOString(),
			started_at: this.startedAt?.toISOString() ?? null,
			completed_at: this.completedAt?.toISOString() ?? null,
			failed_at: this.failedAt?.toISOString() ?? null,
			file_size: this.fileSize?.toString() ?? null,
			progress_percent: this.progressPercent,
			progress_step: this.progressStep,
			error_message: this.errorMessage,
			download_url_expires_at: this.downloadUrlExpiresAt?.toISOString() ?? null,
			expires_at: this.downloadUrlExpiresAt?.toISOString() ?? null,
		};
	}

	getStatus(): HarvestStatus {
		if (this.failedAt) return 'failed';
		if (this.completedAt) return 'completed';
		if (this.startedAt) return 'processing';
		return 'pending';
	}
}
