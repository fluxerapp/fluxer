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

import type {GuildID, UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {
	HarvestExpiredError,
	HarvestFailedError,
	HarvestNotReadyError,
	UnknownGuildError,
	UnknownHarvestError,
	UnknownUserError,
} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {IStorageService} from '~/infrastructure/IStorageService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {IUserRepository} from '~/user/IUserRepository';
import type {WorkerService} from '~/worker/WorkerService';
import {AdminArchive, type AdminArchiveResponse, type ArchiveSubjectType} from '../models/AdminArchiveModel';
import type {AdminArchiveRepository} from '../repositories/AdminArchiveRepository';

const ARCHIVE_RETENTION_DAYS = 365;
const DOWNLOAD_LINK_DAYS = 7;
const DOWNLOAD_LINK_SECONDS = DOWNLOAD_LINK_DAYS * 24 * 60 * 60;

interface ListArchivesParams {
	subjectType?: ArchiveSubjectType | 'all';
	subjectId?: bigint;
	requestedBy?: bigint;
	limit?: number;
	includeExpired?: boolean;
}

export class AdminArchiveService {
	constructor(
		private readonly adminArchiveRepository: AdminArchiveRepository,
		private readonly userRepository: IUserRepository,
		private readonly guildRepository: IGuildRepository,
		private readonly storageService: IStorageService,
		private readonly snowflakeService: SnowflakeService,
		private readonly workerService: WorkerService,
	) {}

	private computeExpiry(): Date {
		return new Date(Date.now() + ARCHIVE_RETENTION_DAYS * 24 * 60 * 60 * 1000);
	}

	async triggerUserArchive(targetUserId: UserID, requestedBy: UserID): Promise<AdminArchiveResponse> {
		const user = await this.userRepository.findUnique(targetUserId);
		if (!user) {
			throw new UnknownUserError();
		}

		const archiveId = this.snowflakeService.generate();
		const archive = new AdminArchive({
			subject_type: 'user',
			subject_id: targetUserId,
			archive_id: archiveId,
			requested_by: requestedBy,
			requested_at: new Date(),
			started_at: null,
			completed_at: null,
			failed_at: null,
			storage_key: null,
			file_size: null,
			progress_percent: 0,
			progress_step: 'Queued',
			error_message: null,
			download_url_expires_at: null,
			expires_at: this.computeExpiry(),
		});

		await this.adminArchiveRepository.create(archive);

		await this.workerService.addJob('harvestUserData', {
			userId: targetUserId.toString(),
			harvestId: archive.archiveId.toString(),
			adminRequestedBy: requestedBy.toString(),
		});

		return archive.toResponse();
	}

	async triggerGuildArchive(targetGuildId: GuildID, requestedBy: UserID): Promise<AdminArchiveResponse> {
		const guild = await this.guildRepository.findUnique(targetGuildId);
		if (!guild) {
			throw new UnknownGuildError();
		}

		const archiveId = this.snowflakeService.generate();
		const archive = new AdminArchive({
			subject_type: 'guild',
			subject_id: targetGuildId,
			archive_id: archiveId,
			requested_by: requestedBy,
			requested_at: new Date(),
			started_at: null,
			completed_at: null,
			failed_at: null,
			storage_key: null,
			file_size: null,
			progress_percent: 0,
			progress_step: 'Queued',
			error_message: null,
			download_url_expires_at: null,
			expires_at: this.computeExpiry(),
		});

		await this.adminArchiveRepository.create(archive);

		await this.workerService.addJob('harvestGuildData', {
			guildId: targetGuildId.toString(),
			archiveId: archive.archiveId.toString(),
			requestedBy: requestedBy.toString(),
		});

		return archive.toResponse();
	}

	async getArchive(
		subjectType: ArchiveSubjectType,
		subjectId: bigint,
		archiveId: bigint,
	): Promise<AdminArchiveResponse | null> {
		const archive = await this.adminArchiveRepository.findBySubjectAndArchiveId(subjectType, subjectId, archiveId);
		return archive ? archive.toResponse() : null;
	}

	async listArchives(params: ListArchivesParams): Promise<Array<AdminArchiveResponse>> {
		const {subjectType = 'all', subjectId, requestedBy, limit = 50, includeExpired = false} = params;

		if (subjectId !== undefined && subjectType === 'all') {
			throw new Error('subject_type must be specified when subject_id is provided');
		}

		if (subjectId !== undefined) {
			const archives = await this.adminArchiveRepository.listBySubject(
				subjectType as ArchiveSubjectType,
				subjectId,
				limit,
				includeExpired,
			);
			return archives.map((a) => a.toResponse());
		}

		if (requestedBy !== undefined) {
			const archives = await this.adminArchiveRepository.listByRequester(requestedBy, limit, includeExpired);
			return archives.map((a) => a.toResponse());
		}

		if (subjectType === 'all') {
			const [users, guilds] = await Promise.all([
				this.adminArchiveRepository.listByType('user', limit, includeExpired),
				this.adminArchiveRepository.listByType('guild', limit, includeExpired),
			]);

			return [...users, ...guilds]
				.sort((a, b) => b.requestedAt.getTime() - a.requestedAt.getTime())
				.slice(0, limit)
				.map((a) => a.toResponse());
		}

		const archives = await this.adminArchiveRepository.listByType(
			subjectType as ArchiveSubjectType,
			limit,
			includeExpired,
		);
		return archives.map((a) => a.toResponse());
	}

	async getDownloadUrl(
		subjectType: ArchiveSubjectType,
		subjectId: bigint,
		archiveId: bigint,
	): Promise<{downloadUrl: string; expiresAt: string}> {
		const archive = await this.adminArchiveRepository.findBySubjectAndArchiveId(subjectType, subjectId, archiveId);
		if (!archive) {
			throw new UnknownHarvestError();
		}

		if (!archive.completedAt || !archive.storageKey) {
			throw new HarvestNotReadyError();
		}

		if (archive.failedAt) {
			throw new HarvestFailedError();
		}

		if (archive.expiresAt && archive.expiresAt < new Date()) {
			throw new HarvestExpiredError();
		}

		const downloadUrl = await this.storageService.getPresignedDownloadURL({
			bucket: Config.s3.buckets.harvests,
			key: archive.storageKey,
			expiresIn: DOWNLOAD_LINK_SECONDS,
		});

		const expiresAt = new Date(Date.now() + DOWNLOAD_LINK_SECONDS * 1000);
		return {downloadUrl, expiresAt: expiresAt.toISOString()};
	}
}
