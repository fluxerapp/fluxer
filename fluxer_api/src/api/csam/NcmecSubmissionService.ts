// SPDX-License-Identifier: AGPL-3.0-or-later

import {DeletionReasons} from '@fluxer/constants/src/Core';
import {CATEGORY_CHILD_SAFETY} from '@fluxer/constants/src/ReportCategories';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {NcmecAlreadySubmittedError} from '@fluxer/errors/src/domains/moderation/NcmecAlreadySubmittedError';
import {NcmecSubmissionFailedError} from '@fluxer/errors/src/domains/moderation/NcmecSubmissionFailedError';
import {UnknownReportError} from '@fluxer/errors/src/domains/moderation/UnknownReportError';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {ms} from 'itty-time';
import type {AdminArchiveService} from '../admin/services/AdminArchiveService';
import type {AdminAuditService} from '../admin/services/AdminAuditService';
import {AdminUserUpdatePropagator} from '../admin/services/AdminUserUpdatePropagator';
import {
	type AttachmentID,
	type ChannelID,
	createAttachmentID,
	createChannelID,
	createMessageID,
	createUserID,
	type MessageID,
	type ReportID,
	type UserID,
} from '../BrandedTypes';
import {Config} from '../Config';
import type {IChannelRepository} from '../channel/IChannelRepository';
import type {AttachmentUploadTraceRepository} from '../channel/repositories/message/AttachmentUploadTraceRepository';
import {
	collectMessageAttachments,
	makeAttachmentCdnKey,
	makeAttachmentCdnUrl,
	purgeMessageAttachments,
} from '../channel/services/message/MessageHelpers';
import type {AttachmentUploadTraceByAttachmentRow} from '../database/types/AttachmentUploadTypes';
import type {NcmecAttachmentSubmissionRow, NcmecUserWorkflowRow} from '../database/types/CsamTypes';
import type {IGuildRepositoryAggregate} from '../guild/repositories/IGuildRepositoryAggregate';
import type {IPurgeQueue} from '../infrastructure/BunnyPurgeQueue';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import type {IStorageService} from '../infrastructure/IStorageService';
import type {KVAccountDeletionQueueService} from '../infrastructure/KVAccountDeletionQueueService';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import {Logger} from '../Logger';
import type {Embed} from '../models/Embed';
import type {EmbedMedia} from '../models/EmbedMedia';
import type {Message} from '../models/Message';
import type {User} from '../models/User';
import type {IARMessageContext, IARSubmission} from '../report/IReportRepository';
import type {ReportRepository} from '../report/ReportRepository';
import {deleteMessageSearchDocuments} from '../search/MessageSearchIndexCleanup';
import type {IUserRepository} from '../user/IUserRepository';
import {reschedulePendingDeletion} from '../user/services/PendingDeletionCoordinator';
import type {WorkerTaskName} from '../worker/WorkerLaneConfig';
import type {NcmecApiClient} from './NcmecReporter';
import {buildNcmecFileDetailsXml, buildNcmecReportXml} from './NcmecReporter';
import type {NcmecRepository} from './NcmecRepository';

export type NcmecSubmissionStatus = 'not_submitted' | 'submitting' | 'submitted' | 'failed';

export interface NcmecAttachmentStatusResponse {
	status: NcmecSubmissionStatus;
	ncmec_report_id: string | null;
	submitted_at: string | null;
	submitted_by_admin_id: string | null;
	failure_reason: string | null;
}

interface NcmecAttachmentSubmitResult {
	success: true;
	ncmec_report_id: string;
	audit_log_reason: string;
}

interface NcmecUserUpdatePropagator {
	propagateUserUpdate(params: {userId: UserID; oldUser: User; updatedUser: User}): Promise<void>;
}

interface SubmitAttachmentToNcmecInput {
	channelId: ChannelID;
	messageId: MessageID;
	attachmentId: AttachmentID;
	filename: string;
	reporterFullName: string;
	adminUserId: UserID;
	sourceReportId?: ReportID | null;
}

interface ResolvedAttachment {
	channelId: ChannelID;
	messageId: MessageID;
	attachmentId: AttachmentID;
	filename: string;
	contentType: string | null;
	reportedAt: Date;
	userId: UserID | null;
	sourceReportId: ReportID | null;
	cdnBucket: string;
	storageKey: string;
	cdnUrl: string;
}

interface ReportedUserSnapshot {
	screenName: string | null;
	displayName: string | null;
	email: string | null;
	emailVerified: boolean;
	dateOfBirth: string | null;
	lastActiveIp: string | null;
}

interface NcmecSubmissionServiceDeps {
	reportRepository: ReportRepository;
	ncmecApi: NcmecApiClient;
	ncmecRepository: NcmecRepository;
	attachmentUploadTraceRepository: AttachmentUploadTraceRepository;
	storageService: IStorageService;
	channelRepository: IChannelRepository;
	userRepository: IUserRepository;
	guildRepository: IGuildRepositoryAggregate;
	gatewayService: IGatewayService;
	userCacheService: UserCacheService;
	adminArchiveService: AdminArchiveService;
	adminAuditService: AdminAuditService;
	purgeQueue: IPurgeQueue;
	workerService: IWorkerService<WorkerTaskName>;
	deletionQueue: KVAccountDeletionQueueService;
	updatePropagator?: NcmecUserUpdatePropagator;
}

const NCMEC_DELETION_GRACE_DAYS = 60;
const DEFAULT_FINALIZE_TASK_DELAY_MS = ms('15 seconds');
const DEFAULT_MAX_FINALIZE_REQUEUES = 240;

function getFinalizeDelayMs(): number {
	const raw = (
		Config.ncmec as {
			finalizeDelayMs?: number;
		}
	).finalizeDelayMs;
	return Number.isFinite(raw) && raw! > 0 ? raw! : DEFAULT_FINALIZE_TASK_DELAY_MS;
}

function getMaxFinalizeRequeues(): number {
	const raw = (
		Config.ncmec as {
			maxFinalizeRequeues?: number;
		}
	).maxFinalizeRequeues;
	return Number.isFinite(raw) && raw! > 0 ? raw! : DEFAULT_MAX_FINALIZE_REQUEUES;
}

export class NcmecSubmissionService {
	private readonly updatePropagator: NcmecUserUpdatePropagator;

	constructor(private readonly deps: NcmecSubmissionServiceDeps) {
		this.updatePropagator =
			deps.updatePropagator ??
			new AdminUserUpdatePropagator({
				userCacheService: deps.userCacheService,
				userRepository: deps.userRepository,
				guildRepository: deps.guildRepository,
				gatewayService: deps.gatewayService,
			});
	}

	async assertChildSafetyReport(reportId: ReportID): Promise<IARSubmission> {
		const report = await this.deps.reportRepository.getReport(reportId);
		if (!report || report.category !== CATEGORY_CHILD_SAFETY) {
			throw new UnknownReportError();
		}
		return report;
	}

	async getAttachmentStatus(attachmentId: AttachmentID): Promise<NcmecAttachmentStatusResponse> {
		const submission = await this.deps.ncmecRepository.getAttachmentSubmission(attachmentId);
		return toAttachmentStatusResponse(submission);
	}

	async getAttachmentStatuses(attachmentIds: Array<AttachmentID>): Promise<Map<string, NcmecAttachmentStatusResponse>> {
		const ids = [...new Set(attachmentIds.map((id) => id.toString()))].map((value) =>
			createAttachmentID(BigInt(value)),
		);
		const rows = await Promise.all(ids.map((id) => this.getAttachmentStatus(id)));
		const result = new Map<string, NcmecAttachmentStatusResponse>();
		for (let i = 0; i < ids.length; i++) {
			result.set(ids[i]!.toString(), rows[i]!);
		}
		return result;
	}

	async getUserPriorReportIds(userIds: Array<UserID>): Promise<Map<string, Array<string>>> {
		const unique = [...new Set(userIds.map((id) => id.toString()))];
		const out = new Map<string, Array<string>>();
		await Promise.all(
			unique.map(async (value) => {
				const workflow = await this.deps.ncmecRepository.getUserWorkflow(createUserID(BigInt(value)));
				if (workflow?.previous_report_ids && workflow.previous_report_ids.size > 0) {
					out.set(value, [...workflow.previous_report_ids].sort());
				}
			}),
		);
		return out;
	}

	async submitAttachmentToNcmec(input: SubmitAttachmentToNcmecInput): Promise<NcmecAttachmentSubmitResult> {
		const existing = await this.deps.ncmecRepository.getAttachmentSubmission(input.attachmentId);
		if (existing?.status === 'submitted' || existing?.status === 'submitting') {
			throw new NcmecAlreadySubmittedError();
		}
		const attachment = await this.resolveAttachment(input);
		const adminUser = await this.deps.userRepository.findUnique(input.adminUserId);
		const reporterEmail = requireReporterEmail((Config.ncmec.reporterEmail?.trim() || adminUser?.email) ?? null);
		const reportedUser = await this.snapshotReportedUser(attachment.userId);
		const attachmentUploadTrace = await this.deps.attachmentUploadTraceRepository.getByAttachmentId(
			attachment.attachmentId,
		);
		const existingWorkflow = attachment.userId
			? await this.deps.ncmecRepository.getUserWorkflow(attachment.userId)
			: null;
		const priorReportIds = [...(existingWorkflow?.previous_report_ids ?? new Set<string>())].sort();
		const contextLabel = `${attachment.channelId.toString()}/${attachment.attachmentId.toString()}`;
		const reportAdditionalInfo = buildAdditionalInfo({
			channelId: attachment.channelId,
			messageId: attachment.messageId,
			attachmentId: attachment.attachmentId,
			userId: attachment.userId,
			sourceReportId: attachment.sourceReportId,
			priorReportIds,
			reportedUser,
			attachmentUploadTrace,
		});
		const fileAdditionalInfo = reportAdditionalInfo;
		const reportXml = buildNcmecReportXml({
			attachmentUrl: attachment.cdnUrl,
			reportedAt: attachment.reportedAt,
			reporterFullName: input.reporterFullName,
			reporterEmail,
			reportedUser: {
				id: attachment.userId !== null ? BigInt(attachment.userId) : null,
				screenName: reportedUser.screenName,
				displayName: reportedUser.displayName,
				espService: 'Fluxer',
				permanentlyDisabledAt: existingWorkflow?.deleted_at ?? null,
				person: buildReportedUserPerson(reportedUser),
				ipCaptureEvents: buildReportedUserIpCaptureEvents(attachmentUploadTrace),
			},
			priorNcmecReportIds: priorReportIds,
			additionalInfo: reportAdditionalInfo,
		});
		await this.writeAttachmentSubmission({
			existing,
			attachment,
			status: 'submitting',
			ncmecReportId: null,
			ncmecFileId: null,
			reporterFullName: input.reporterFullName,
			adminUserId: input.adminUserId,
			failureReason: null,
		});
		let ncmecReportId: string | null = null;
		try {
			ncmecReportId = await this.deps.ncmecApi.submitReport(reportXml);
			const upload = await this.deps.ncmecApi.uploadEvidence(
				ncmecReportId,
				await this.readAttachmentBytes(attachment),
				attachment.filename,
			);
			await this.deps.ncmecApi.submitFileDetails(
				buildNcmecFileDetailsXml({
					reportId: ncmecReportId,
					fileId: upload.fileId,
					filename: attachment.filename,
					uploadedToEspTimestamp: getBestEffortUploadTimestamp(attachmentUploadTrace),
					fileViewedByEsp: true,
					ipCaptureEvent: buildBestEffortUploadIpCaptureEvent(attachmentUploadTrace),
					additionalInfo: fileAdditionalInfo,
				}),
			);
			await this.deps.ncmecApi.finish(ncmecReportId);
			await this.writeAttachmentSubmission({
				existing,
				attachment,
				status: 'submitted',
				ncmecReportId,
				ncmecFileId: upload.fileId,
				reporterFullName: input.reporterFullName,
				adminUserId: input.adminUserId,
				failureReason: null,
			});
			const auditLogReason = formatNcmecAuditLogReason(ncmecReportId, contextLabel);
			if (attachment.userId !== null) {
				await this.deps.adminAuditService.createAuditLog({
					adminUserId: input.adminUserId,
					targetType: 'user',
					targetId: BigInt(attachment.userId),
					action: 'NCMEC Report',
					auditLogReason,
					metadata: buildAuditMetadata({attachment, reportedUser, attachmentUploadTrace}),
				});
				const workflow = await this.advanceUserWorkflow({
					userId: attachment.userId,
					adminUserId: input.adminUserId,
					ncmecReportId,
					auditLogReason,
					contextLabel,
					existingWorkflow,
				});
				await this.deps.workerService.addJob(
					'finalizeNcmecAttachmentReport',
					{attachmentId: attachment.attachmentId.toString(), requeueCount: 0},
					{jobKey: finalizeJobKey(attachment.attachmentId), runAt: new Date(Date.now() + getFinalizeDelayMs())},
				);
				Logger.info(
					{
						attachmentId: attachment.attachmentId.toString(),
						userId: attachment.userId.toString(),
						ncmecReportId,
						archiveId: workflow.archive_id?.toString() ?? null,
					},
					'NCMEC attachment report submitted',
				);
			} else {
				await this.deps.adminAuditService.createAuditLog({
					adminUserId: input.adminUserId,
					targetType: 'report',
					targetId: BigInt(attachment.sourceReportId ?? input.attachmentId),
					action: 'NCMEC Report',
					auditLogReason,
					metadata: buildAuditMetadata({attachment, reportedUser, attachmentUploadTrace}),
				});
				Logger.info(
					{
						attachmentId: attachment.attachmentId.toString(),
						sourceReportId: attachment.sourceReportId?.toString() ?? null,
						ncmecReportId,
					},
					'NCMEC attachment report submitted (legacy report, unknown author — skipped ban/archive)',
				);
			}
			return {success: true, ncmec_report_id: ncmecReportId, audit_log_reason: auditLogReason};
		} catch (error) {
			if (ncmecReportId) {
				try {
					await this.deps.ncmecApi.retract(ncmecReportId);
				} catch (retractError) {
					Logger.warn(
						{error: retractError, attachmentId: input.attachmentId.toString(), ncmecReportId},
						'Failed to retract failed NCMEC attachment submission',
					);
				}
			}
			const rawMessage = error instanceof Error ? error.message : 'Unknown error';
			await this.writeAttachmentSubmission({
				existing,
				attachment,
				status: 'failed',
				ncmecReportId: null,
				ncmecFileId: null,
				reporterFullName: input.reporterFullName,
				adminUserId: input.adminUserId,
				failureReason: rawMessage,
			});
			Logger.error(
				{
					error,
					attachmentId: input.attachmentId.toString(),
					channelId: input.channelId.toString(),
					messageId: input.messageId.toString(),
				},
				'NCMEC attachment report submission failed',
			);
			throw new NcmecSubmissionFailedError(sanitizeFailureMessage(rawMessage));
		}
	}

	async finalizeAttachmentReport(attachmentId: AttachmentID, requeueCount = 0): Promise<void> {
		const submission = await this.deps.ncmecRepository.getAttachmentSubmission(attachmentId);
		if (!submission || submission.status !== 'submitted' || submission.content_deleted_at) {
			return;
		}
		if (submission.user_id === null) {
			Logger.warn({attachmentId: attachmentId.toString()}, 'Skipping finalize: submission has no user_id');
			return;
		}
		const submissionUserId = submission.user_id;
		const workflow = await this.deps.ncmecRepository.getUserWorkflow(createUserID(submissionUserId));
		if (!workflow) {
			Logger.warn({attachmentId: attachmentId.toString()}, 'Missing NCMEC user workflow during finalization');
			return;
		}
		const maxRequeues = getMaxFinalizeRequeues();
		if (requeueCount >= maxRequeues) {
			Logger.error(
				{
					attachmentId: attachmentId.toString(),
					requeueCount,
					maxRequeues,
					archiveId: workflow.archive_id?.toString() ?? null,
				},
				'NCMEC finalize gave up after max requeues; manual intervention required',
			);
			return;
		}
		const auditLogReason =
			workflow.archive_audit_log_reason ??
			(submission.ncmec_report_id
				? formatNcmecAuditLogReason(
						submission.ncmec_report_id,
						`${submission.channel_id.toString()}/${submission.attachment_id.toString()}`,
					)
				: null);
		const requesterAdminId =
			workflow.archive_requested_by_admin_id ??
			workflow.deleted_by_admin_id ??
			submission.submitted_by_admin_id ??
			null;
		if (requesterAdminId === null) {
			Logger.error(
				{attachmentId: attachmentId.toString(), userId: submissionUserId.toString()},
				'NCMEC finalize missing requester admin id; manual intervention required',
			);
			return;
		}
		const requester = createUserID(requesterAdminId);
		const refreshedWorkflow = await this.ensureArchiveAfterBan(
			workflow,
			createUserID(submissionUserId),
			requester,
			auditLogReason,
		);
		if (!refreshedWorkflow.archive_id) {
			await this.requeueFinalizer(attachmentId, requeueCount + 1);
			return;
		}
		const archive = await this.deps.adminArchiveService.getArchive(
			'user',
			submissionUserId,
			refreshedWorkflow.archive_id,
		);
		if (!archive || !archive.completed_at) {
			await this.requeueFinalizer(attachmentId, requeueCount + 1);
			return;
		}
		const archiveCompletedAt = new Date(archive.completed_at);
		if (!refreshedWorkflow.archive_completed_at) {
			await this.deps.ncmecRepository.upsertUserWorkflow({
				...refreshedWorkflow,
				archive_completed_at: archiveCompletedAt,
				updated_at: new Date(),
			});
		}
		await this.deleteMessageSilently(
			createChannelID(submission.channel_id),
			createMessageID(submission.message_id),
			createUserID(submissionUserId),
		);
		await this.deps.ncmecRepository.upsertAttachmentSubmission({
			...submission,
			archive_completed_at: archiveCompletedAt,
			content_deleted_at: new Date(),
			updated_at: new Date(),
		});
		if (!refreshedWorkflow.deletion_job_queued_at) {
			await this.deps.ncmecRepository.upsertUserWorkflow({
				...refreshedWorkflow,
				archive_completed_at: archiveCompletedAt,
				deletion_job_queued_at: new Date(),
				updated_at: new Date(),
			});
		}
	}

	private async resolveAttachment(input: SubmitAttachmentToNcmecInput): Promise<ResolvedAttachment> {
		const liveMessage = await this.deps.channelRepository.getMessage(input.channelId, input.messageId);
		if (liveMessage?.authorId) {
			const liveAttachment = collectMessageAttachments(liveMessage).find(
				(attachment) => attachment.id === input.attachmentId && attachment.filename === input.filename,
			);
			if (liveAttachment) {
				return this.buildAttachmentContext(input, {
					contentType: liveAttachment.contentType,
					reportedAt: snowflakeToDate(input.messageId),
					userId: liveMessage.authorId,
					sourceReportId: input.sourceReportId ?? null,
					bucket: Config.s3.buckets.cdn,
					storageKey: makeAttachmentCdnKey(input.channelId, input.attachmentId, input.filename),
				});
			}
			const embedMedia = findEmbedReferencedAttachmentMedia(
				liveMessage,
				makeAttachmentCdnUrl(input.channelId, input.attachmentId, input.filename),
			);
			if (embedMedia) {
				return this.buildAttachmentContext(input, {
					contentType: embedMedia.contentType,
					reportedAt: snowflakeToDate(input.messageId),
					userId: liveMessage.authorId,
					sourceReportId: input.sourceReportId ?? null,
					bucket: Config.s3.buckets.cdn,
					storageKey: makeAttachmentCdnKey(input.channelId, input.attachmentId, input.filename),
				});
			}
		}
		if (!input.sourceReportId) {
			throw new UnknownMessageError();
		}
		const report = await this.deps.reportRepository.getReport(input.sourceReportId);
		if (!report) {
			throw new UnknownReportError();
		}
		const context = findAttachmentInReport(
			report,
			input.channelId,
			input.messageId,
			input.attachmentId,
			input.filename,
		);
		if (context) {
			return this.buildAttachmentContext(input, {
				contentType: context.attachment.content_type,
				reportedAt: context.message.timestamp,
				userId: context.message.authorId,
				sourceReportId: input.sourceReportId,
				bucket: Config.s3.buckets.reports,
				storageKey: makeAttachmentCdnKey(input.channelId, input.attachmentId, input.filename),
			});
		}
		throw new UnknownMessageError();
	}

	private buildAttachmentContext(
		input: SubmitAttachmentToNcmecInput,
		context: {
			contentType: string | null;
			reportedAt: Date;
			userId: UserID | null;
			sourceReportId: ReportID | null;
			bucket: string;
			storageKey: string;
		},
	): ResolvedAttachment {
		validateImageAttachment(context.contentType);
		return {
			channelId: input.channelId,
			messageId: input.messageId,
			attachmentId: input.attachmentId,
			filename: input.filename,
			contentType: context.contentType,
			reportedAt: context.reportedAt,
			userId: context.userId,
			sourceReportId: context.sourceReportId,
			cdnBucket: context.bucket,
			storageKey: context.storageKey,
			cdnUrl: makeAttachmentCdnUrl(input.channelId, input.attachmentId, input.filename),
		};
	}

	private async readAttachmentBytes(attachment: ResolvedAttachment): Promise<Uint8Array> {
		const buffer = await this.deps.storageService.readObject(attachment.cdnBucket, attachment.storageKey);
		return new Uint8Array(buffer);
	}

	private async snapshotReportedUser(userId: UserID | null): Promise<ReportedUserSnapshot> {
		if (userId === null) {
			return {
				screenName: null,
				displayName: null,
				email: null,
				emailVerified: false,
				dateOfBirth: null,
				lastActiveIp: null,
			};
		}
		const user = await this.deps.userRepository.findUnique(userId);
		if (!user) {
			return {
				screenName: null,
				displayName: null,
				email: null,
				emailVerified: false,
				dateOfBirth: null,
				lastActiveIp: null,
			};
		}
		return {
			screenName: user.username ?? null,
			displayName: user.globalName ?? null,
			email: user.email ?? null,
			emailVerified: user.emailVerified ?? false,
			dateOfBirth: user.dateOfBirth ?? null,
			lastActiveIp: user.lastActiveIp ?? null,
		};
	}

	private async advanceUserWorkflow(args: {
		userId: UserID;
		adminUserId: UserID;
		ncmecReportId: string;
		auditLogReason: string;
		contextLabel: string;
		existingWorkflow: NcmecUserWorkflowRow | null;
	}): Promise<NcmecUserWorkflowRow> {
		const now = new Date();
		let workflow: NcmecUserWorkflowRow = args.existingWorkflow ?? {
			user_id: BigInt(args.userId),
			deleted_at: null,
			deleted_by_admin_id: null,
			deletion_private_reason: null,
			deletion_ncmec_report_id: null,
			archive_id: null,
			archive_requested_at: null,
			archive_requested_by_admin_id: null,
			archive_audit_log_reason: null,
			archive_completed_at: null,
			deletion_job_queued_at: null,
			previous_report_ids: null,
			created_at: now,
			updated_at: now,
		};
		workflow = await this.banUserIfNeeded(
			workflow,
			args.userId,
			args.adminUserId,
			args.ncmecReportId,
			args.contextLabel,
		);
		workflow = await this.ensureArchiveAfterBan(workflow, args.userId, args.adminUserId, args.auditLogReason);
		const priorReportIds = new Set(workflow.previous_report_ids ?? []);
		priorReportIds.add(args.ncmecReportId);
		const updated: NcmecUserWorkflowRow = {
			...workflow,
			archive_audit_log_reason: workflow.archive_audit_log_reason ?? args.auditLogReason,
			previous_report_ids: priorReportIds,
			updated_at: new Date(),
		};
		await this.deps.ncmecRepository.upsertUserWorkflow(updated);
		return updated;
	}

	private async banUserIfNeeded(
		workflow: NcmecUserWorkflowRow,
		userId: UserID,
		adminUserId: UserID,
		ncmecReportId: string,
		contextLabel: string,
	): Promise<NcmecUserWorkflowRow> {
		if (workflow.deleted_at) return workflow;
		const user = await this.deps.userRepository.findUnique(userId);
		if (!user) return workflow;
		const privateReason = `Confirmed CSAM - NCMEC Report ${ncmecReportId} - ${contextLabel}`;
		const pendingDeletionAt = new Date();
		pendingDeletionAt.setDate(pendingDeletionAt.getDate() + NCMEC_DELETION_GRACE_DAYS);
		const updatedUser = await this.deps.userRepository.patchUpsert(
			userId,
			{
				flags: user.flags | UserFlags.DELETED | UserFlags.DISABLED,
				temp_banned_until: null,
				pending_deletion_at: pendingDeletionAt,
				deletion_reason_code: DeletionReasons.CHILD_SEXUAL_CONTENT,
				deletion_public_reason: null,
				deletion_audit_log_reason: privateReason,
			},
			user.toRow(),
		);
		await reschedulePendingDeletion({
			userId,
			currentPendingDeletionAt: user.pendingDeletionAt,
			nextPendingDeletionAt: pendingDeletionAt,
			deletionReasonCode: DeletionReasons.CHILD_SEXUAL_CONTENT,
			userRepository: this.deps.userRepository,
			deletionQueue: this.deps.deletionQueue,
		});
		await this.deps.userRepository.deleteAllAuthSessions(userId);
		await this.updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser});
		return {
			...workflow,
			deleted_at: new Date(),
			deleted_by_admin_id: BigInt(adminUserId),
			deletion_private_reason: privateReason,
			deletion_ncmec_report_id: ncmecReportId,
			updated_at: new Date(),
		};
	}

	private async ensureArchiveAfterBan(
		workflow: NcmecUserWorkflowRow,
		userId: UserID,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<NcmecUserWorkflowRow> {
		if (BigInt(adminUserId) === 0n) return workflow;
		const banAt = workflow.deleted_at;
		const archivedAt = workflow.archive_requested_at;
		const hasPostBanArchive =
			workflow.archive_id !== null && archivedAt !== null && banAt !== null && archivedAt >= banAt;
		if (hasPostBanArchive) return workflow;
		if (workflow.archive_id !== null && banAt === null) return workflow;
		const archive = await this.deps.adminArchiveService.triggerUserArchive(userId, adminUserId, true);
		await this.deps.adminAuditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'trigger_user_archive',
			auditLogReason,
			metadata: new Map([['archive_id', archive.archive_id]]),
		});
		return {
			...workflow,
			archive_id: BigInt(archive.archive_id),
			archive_requested_at: new Date(),
			archive_requested_by_admin_id: BigInt(adminUserId),
			archive_audit_log_reason: workflow.archive_audit_log_reason ?? auditLogReason,
			archive_completed_at: null,
			deletion_job_queued_at: null,
			updated_at: new Date(),
		};
	}

	private async writeAttachmentSubmission(args: {
		existing: NcmecAttachmentSubmissionRow | null;
		attachment: ResolvedAttachment;
		status: 'submitting' | 'submitted' | 'failed';
		ncmecReportId: string | null;
		ncmecFileId: string | null;
		reporterFullName: string;
		adminUserId: UserID;
		failureReason: string | null;
	}): Promise<void> {
		const now = new Date();
		await this.deps.ncmecRepository.upsertAttachmentSubmission({
			attachment_id: BigInt(args.attachment.attachmentId),
			user_id: args.attachment.userId !== null ? BigInt(args.attachment.userId) : null,
			channel_id: BigInt(args.attachment.channelId),
			message_id: BigInt(args.attachment.messageId),
			filename: args.attachment.filename,
			source_report_id: args.attachment.sourceReportId ? BigInt(args.attachment.sourceReportId) : null,
			status: args.status,
			ncmec_report_id: args.ncmecReportId,
			ncmec_file_id: args.ncmecFileId,
			submitted_at: args.status === 'submitted' ? now : null,
			submitted_by_admin_id: BigInt(args.adminUserId),
			reporter_full_name: args.reporterFullName,
			failure_reason: args.failureReason,
			archive_completed_at: args.existing?.archive_completed_at ?? null,
			content_deleted_at: args.existing?.content_deleted_at ?? null,
			created_at: args.existing?.created_at ?? now,
			updated_at: now,
		});
	}

	private async deleteMessageSilently(
		channelId: ChannelID,
		messageId: MessageID,
		fallbackUserId: UserID,
	): Promise<void> {
		const channel = await this.deps.channelRepository.findUnique(channelId);
		const message = await this.deps.channelRepository.getMessage(channelId, messageId);
		if (!message) return;
		if (message.attachments.length > 0) {
			await purgeMessageAttachments(message, this.deps.storageService, this.deps.purgeQueue);
		}
		await this.deps.channelRepository.deleteMessage(
			channelId,
			messageId,
			message.authorId ?? fallbackUserId,
			message.pinnedTimestamp || undefined,
		);
		await deleteMessageSearchDocuments([messageId], {context: {source: 'ncmec_submission_delete'}});
		if (!channel) return;
		if (channel.guildId) {
			await this.deps.gatewayService.dispatchGuild({
				guildId: channel.guildId,
				event: 'MESSAGE_DELETE',
				data: {channel_id: channelId.toString(), id: messageId.toString()},
			});
			return;
		}
		for (const recipientId of channel.recipientIds) {
			await this.deps.gatewayService.dispatchPresence({
				userId: recipientId,
				event: 'MESSAGE_DELETE',
				data: {channel_id: channelId.toString(), id: messageId.toString()},
			});
		}
	}

	private async requeueFinalizer(attachmentId: AttachmentID, requeueCount: number): Promise<void> {
		await this.deps.workerService.addJob(
			'finalizeNcmecAttachmentReport',
			{attachmentId: attachmentId.toString(), requeueCount},
			{jobKey: finalizeJobKey(attachmentId), runAt: new Date(Date.now() + getFinalizeDelayMs())},
		);
	}
}

function toAttachmentStatusResponse(submission: NcmecAttachmentSubmissionRow | null): NcmecAttachmentStatusResponse {
	if (!submission) {
		return {
			status: 'not_submitted',
			ncmec_report_id: null,
			submitted_at: null,
			submitted_by_admin_id: null,
			failure_reason: null,
		};
	}
	return {
		status: submission.status as NcmecSubmissionStatus,
		ncmec_report_id: submission.ncmec_report_id,
		submitted_at: submission.submitted_at?.toISOString() ?? null,
		submitted_by_admin_id: submission.submitted_by_admin_id?.toString() ?? null,
		failure_reason: submission.failure_reason,
	};
}

function buildAdditionalInfo(args: {
	channelId: ChannelID;
	messageId: MessageID;
	attachmentId: AttachmentID;
	userId: UserID | null;
	sourceReportId: ReportID | null;
	priorReportIds: ReadonlyArray<string>;
	reportedUser: ReportedUserSnapshot;
	attachmentUploadTrace: AttachmentUploadTraceByAttachmentRow | null;
}): string {
	const lines = [
		'Fluxer admin attachment report.',
		`Channel ID: ${args.channelId.toString()}`,
		`Message ID: ${args.messageId.toString()}`,
		`Attachment ID: ${args.attachmentId.toString()}`,
		`Reported User ID: ${args.userId !== null ? args.userId.toString() : 'unknown'}`,
	];
	if (args.reportedUser.email) {
		lines.push(`Reported User Email: ${args.reportedUser.email}`);
		lines.push(`Reported User Email Verified: ${args.reportedUser.emailVerified ? 'true' : 'false'}`);
	}
	if (args.reportedUser.dateOfBirth) {
		lines.push(`Reported User Date of Birth: ${args.reportedUser.dateOfBirth}`);
	}
	if (args.attachmentUploadTrace) {
		lines.push(`Attachment Upload Mode: ${args.attachmentUploadTrace.upload_mode}`);
		lines.push(`Attachment Upload Request IP: ${args.attachmentUploadTrace.request_ip}`);
		lines.push(`Attachment Upload Request Time: ${args.attachmentUploadTrace.requested_at.toISOString()}`);
		lines.push(`Attachment Upload IP Source: ${describeBestEffortUploadIpSource(args.attachmentUploadTrace)}`);
		if (args.attachmentUploadTrace.completion_ip) {
			lines.push(`Attachment Upload Completion IP: ${args.attachmentUploadTrace.completion_ip}`);
		}
		if (args.attachmentUploadTrace.completed_at) {
			lines.push(`Attachment Upload Completion Time: ${args.attachmentUploadTrace.completed_at.toISOString()}`);
		}
	}
	if (!args.attachmentUploadTrace && args.reportedUser.lastActiveIp) {
		lines.push(`Attachment Upload IP Unavailable; Current Last Active IP: ${args.reportedUser.lastActiveIp}`);
	}
	if (args.sourceReportId) lines.push(`Source Report ID: ${args.sourceReportId.toString()}`);
	if (args.priorReportIds.length > 0) {
		lines.push(`Previous NCMEC Report IDs: ${args.priorReportIds.join(', ')}`);
	}
	return lines.join('\n');
}

function buildReportedUserPerson(reportedUser: ReportedUserSnapshot): {
	emails: Array<{
		address: string;
		verified: boolean;
		type: 'Home';
	}>;
	phones: Array<{
		number: string;
		type: 'Mobile';
	}>;
	dateOfBirth: string | null;
} | null {
	const emails = reportedUser.email
		? [{address: reportedUser.email, verified: reportedUser.emailVerified, type: 'Home' as const}]
		: [];
	const phones: Array<{number: string; type: 'Mobile'}> = [];
	if (emails.length === 0 && !reportedUser.dateOfBirth) {
		return null;
	}
	return {
		emails,
		phones,
		dateOfBirth: reportedUser.dateOfBirth,
	};
}

function buildReportedUserIpCaptureEvents(trace: AttachmentUploadTraceByAttachmentRow | null): Array<{
	ipAddress: string;
	eventName: 'Upload';
	dateTime: Date;
}> {
	if (!trace) {
		return [];
	}
	const events = [{ipAddress: trace.request_ip, eventName: 'Upload' as const, dateTime: trace.requested_at}];
	if (trace.completion_ip && trace.completed_at && trace.completion_ip !== trace.request_ip) {
		events.push({ipAddress: trace.completion_ip, eventName: 'Upload', dateTime: trace.completed_at});
	}
	return events;
}

function buildBestEffortUploadIpCaptureEvent(trace: AttachmentUploadTraceByAttachmentRow | null): {
	ipAddress: string;
	eventName: 'Upload';
	dateTime: Date;
} | null {
	if (!trace) {
		return null;
	}
	if (trace.completion_ip && trace.completed_at) {
		return {ipAddress: trace.completion_ip, eventName: 'Upload', dateTime: trace.completed_at};
	}
	return {ipAddress: trace.request_ip, eventName: 'Upload', dateTime: trace.requested_at};
}

function getBestEffortUploadTimestamp(trace: AttachmentUploadTraceByAttachmentRow | null): Date | null {
	return trace?.completed_at ?? trace?.requested_at ?? null;
}

function describeBestEffortUploadIpSource(trace: AttachmentUploadTraceByAttachmentRow): string {
	if (trace.completion_ip && trace.completed_at) {
		return 'multipart completion callback observed by Fluxer API';
	}
	if (trace.upload_mode === 'form_data') {
		return 'attachment upload request handled directly by Fluxer API';
	}
	return 'presigned upload authorization request observed by Fluxer API';
}

function buildAuditMetadata(args: {
	attachment: ResolvedAttachment;
	reportedUser: ReportedUserSnapshot;
	attachmentUploadTrace: AttachmentUploadTraceByAttachmentRow | null;
}): Map<string, string> {
	const metadata = new Map<string, string>([
		['channel_id', args.attachment.channelId.toString()],
		['message_id', args.attachment.messageId.toString()],
		['attachment_id', args.attachment.attachmentId.toString()],
	]);
	if (args.reportedUser.email) metadata.set('reported_user_email', args.reportedUser.email);
	metadata.set('reported_user_email_verified', args.reportedUser.emailVerified ? 'true' : 'false');
	if (args.reportedUser.dateOfBirth) metadata.set('reported_user_date_of_birth', args.reportedUser.dateOfBirth);
	if (args.reportedUser.lastActiveIp) metadata.set('reported_user_last_active_ip', args.reportedUser.lastActiveIp);
	if (args.attachmentUploadTrace) {
		metadata.set('attachment_upload_mode', args.attachmentUploadTrace.upload_mode);
		metadata.set('attachment_upload_request_ip', args.attachmentUploadTrace.request_ip);
		metadata.set('attachment_upload_requested_at', args.attachmentUploadTrace.requested_at.toISOString());
		metadata.set('attachment_upload_ip_source', describeBestEffortUploadIpSource(args.attachmentUploadTrace));
		if (args.attachmentUploadTrace.completion_ip) {
			metadata.set('attachment_upload_completion_ip', args.attachmentUploadTrace.completion_ip);
		}
		if (args.attachmentUploadTrace.completed_at) {
			metadata.set('attachment_upload_completed_at', args.attachmentUploadTrace.completed_at.toISOString());
		}
	}
	return metadata;
}

function findEmbedReferencedAttachmentMedia(message: Message, targetCdnUrl: string): EmbedMedia | null {
	const scan = (embeds: Array<Embed>): EmbedMedia | null => {
		for (const embed of embeds) {
			for (const media of [embed.image, embed.thumbnail, embed.video, embed.audio]) {
				if (media?.url === targetCdnUrl) {
					return media;
				}
			}
		}
		return null;
	};
	const direct = scan(message.embeds);
	if (direct) {
		return direct;
	}
	for (const snapshot of message.messageSnapshots) {
		const found = scan(snapshot.embeds);
		if (found) {
			return found;
		}
	}
	return null;
}

function findAttachmentInReport(
	report: IARSubmission,
	channelId: ChannelID,
	messageId: MessageID,
	attachmentId: AttachmentID,
	filename: string,
): {
	message: IARMessageContext;
	attachment: IARMessageContext['attachments'][number];
} | null {
	for (const message of report.messageContext ?? []) {
		if (message.messageId !== messageId) continue;
		if (message.channelId !== channelId) continue;
		const attachment = message.attachments.find(
			(item) => item.attachment_id === attachmentId && item.filename === filename,
		);
		if (attachment) return {message, attachment};
	}
	return null;
}

function formatNcmecAuditLogReason(reportId: string, contextLabel: string): string {
	return `NCMEC Report ${reportId} - ${contextLabel}`;
}

function finalizeJobKey(attachmentId: AttachmentID): string {
	return `ncmec-finalize:${attachmentId.toString()}`;
}

function sanitizeFailureMessage(rawMessage: string): string {
	const lowered = rawMessage.toLowerCase();
	if (lowered.includes('http 401') || lowered.includes('unauthorized')) {
		return 'NCMEC authentication failed. Check the service credentials.';
	}
	if (lowered.includes('responsecode')) {
		const match = rawMessage.match(/responseCode\s+(-?\d+)/i);
		return match ? `NCMEC rejected the submission (responseCode ${match[1]}).` : 'NCMEC rejected the submission.';
	}
	if (lowered.includes('http ')) {
		const match = rawMessage.match(/http\s+(\d{3})/i);
		return match ? `NCMEC request failed (HTTP ${match[1]}).` : 'NCMEC request failed.';
	}
	return 'NCMEC submission failed. See server logs for details.';
}

function validateImageAttachment(contentType: string | null | undefined): void {
	if (contentType?.startsWith('image/') || contentType?.startsWith('video/')) return;
	throw InputValidationError.fromCode('attachment_id', ValidationErrorCodes.NCMEC_ATTACHMENT_MUST_BE_IMAGE_OR_VIDEO);
}

function requireReporterEmail(email: string | null): string {
	const trimmed = email?.trim();
	if (trimmed) {
		return trimmed;
	}
	throw new Error('NCMEC reporter email is required for submission.');
}
