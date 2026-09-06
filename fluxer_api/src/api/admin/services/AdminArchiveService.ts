// SPDX-License-Identifier: AGPL-3.0-or-later

import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {HarvestExpiredError} from '@fluxer/errors/src/domains/moderation/HarvestExpiredError';
import {HarvestFailedError} from '@fluxer/errors/src/domains/moderation/HarvestFailedError';
import {HarvestNotReadyError} from '@fluxer/errors/src/domains/moderation/HarvestNotReadyError';
import {UnknownHarvestError} from '@fluxer/errors/src/domains/moderation/UnknownHarvestError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {ms, seconds} from 'itty-time';
import type {GuildID, UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import type {IStorageService} from '../../infrastructure/IStorageService';
import type {IUserRepository} from '../../user/IUserRepository';
import type {WorkerTaskName} from '../../worker/WorkerLaneConfig';
import {AdminArchive, type AdminArchiveResponse, type ArchiveSubjectType} from '../models/AdminArchiveModel';
import type {AdminArchiveRepository} from '../repositories/AdminArchiveRepository';

const ARCHIVE_RETENTION_DAYS = 365;
const DOWNLOAD_LINK_DAYS = 7;
const DOWNLOAD_LINK_SECONDS = DOWNLOAD_LINK_DAYS * seconds('1 day');

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
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly storageService: IStorageService,
		private readonly snowflakeService: ISnowflakeService,
		private readonly workerService: IWorkerService<WorkerTaskName>,
	) {}

	private computeExpiry(): Date {
		return new Date(Date.now() + ARCHIVE_RETENTION_DAYS * ms('1 day'));
	}

	async triggerUserArchive(
		targetUserId: UserID,
		requestedBy: UserID,
		includeAttachments: boolean,
	): Promise<AdminArchiveResponse> {
		const user = await this.userRepository.findUnique(targetUserId);
		if (!user) {
			throw new UnknownUserError();
		}
		const archiveId = await this.snowflakeService.generate();
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
			includeAttachments,
		});
		return archive.toResponse();
	}

	async triggerGuildArchive(
		targetGuildId: GuildID,
		requestedBy: UserID,
		includeAttachments: boolean,
	): Promise<AdminArchiveResponse> {
		const guild = await this.guildRepository.findUnique(targetGuildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		const archiveId = await this.snowflakeService.generate();
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
			includeAttachments,
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
			throw InputValidationError.create(
				'subject_type',
				'subject_type must name user or guild when subject_id is supplied',
			);
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
	): Promise<{
		downloadUrl: string;
		expiresAt: string;
	}> {
		const archive = await this.adminArchiveRepository.findBySubjectAndArchiveId(subjectType, subjectId, archiveId);
		if (!archive) {
			throw new UnknownHarvestError();
		}
		if (archive.failedAt) {
			throw new HarvestFailedError();
		}
		if (!archive.completedAt || !archive.storageKey) {
			throw new HarvestNotReadyError();
		}
		if (archive.expiresAt && archive.expiresAt < new Date()) {
			throw new HarvestExpiredError();
		}
		const downloadUrl = await this.storageService.getPresignedDownloadURL({
			bucket: Config.s3.buckets.harvests,
			key: archive.storageKey,
			expiresIn: DOWNLOAD_LINK_SECONDS,
		});
		const expiresAt = new Date(Date.now() + ms('7 days'));
		return {downloadUrl, expiresAt: expiresAt.toISOString()};
	}
}
