// SPDX-License-Identifier: AGPL-3.0-or-later

import {requireEmailVerified} from '@app/api/auth/EmailVerificationUtils';
import {createChannelID, createGuildID, createInviteCode, createMessageID, createUserID} from '@app/api/BrandedTypes';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {
	getReportFlowResponse,
	resolveReportFlowAnswers,
	resolveReportFlowLocale,
} from '@app/api/report/flows/ReportFlowRegistry';
import {type ReportStatus, reportStatusToString} from '@app/api/report/IReportRepository';
import type {ReportFlowRecord, ReportService} from '@app/api/report/ReportService';
import {UnclaimedAccountCannotSubmitReportsError} from '@fluxer/errors/src/domains/moderation/UnclaimedAccountCannotSubmitReportsError';
import type {
	ReportFlowMessageSubmissionRequest,
	ReportFlowResponse,
	ReportFlowStep,
	ReportFlowSurface,
	ReportFlowTargetType,
	ReportFlowUserSubmissionRequest,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import type {
	DsaReportEmailSendRequest,
	DsaReportEmailVerifyRequest,
	DsaReportRequest,
	ReportGuildRequest,
	ReportMessageRequest,
	ReportResponse,
	ReportUserRequest,
	TicketResponse,
} from '@fluxer/schema/src/domains/report/ReportSchemas';

interface ReportUserRequestContext<T> {
	user: User;
	data: T;
}

interface ReportDsaRequestContext<T> {
	data: T;
	locale?: string | null;
}

interface ReportFlowSubmissionContext<T> {
	user: User;
	data: T;
	locale: string | null;
}

interface ReportFlowAnswersInput {
	revision_hash: string;
	steps: Array<ReportFlowStep>;
	locale?: string | undefined;
}

interface ReportRecord {
	reportId: bigint;
	status: ReportStatus;
	reportedAt: Date;
}

export class ReportRequestService {
	constructor(private reportService: ReportService) {}

	async reportMessage({
		user,
		viewer,
		data,
	}: ReportUserRequestContext<ReportMessageRequest> & {viewer: ThreadViewer}): Promise<ReportResponse> {
		this.requireVerifiedAccount(user);
		const report = await this.reportService.reportMessage(
			this.createReporter(user),
			viewer,
			createChannelID(data.channel_id),
			createMessageID(data.message_id),
			data.category,
		);
		return this.toReportResponse(report);
	}

	async reportUser({user, data}: ReportUserRequestContext<ReportUserRequest>): Promise<ReportResponse> {
		this.requireVerifiedAccount(user);
		const report = await this.reportService.reportUser(
			this.createReporter(user),
			createUserID(data.user_id),
			data.category,
			data.guild_id ? createGuildID(data.guild_id) : undefined,
		);
		return this.toReportResponse(report);
	}

	async reportGuild({user, data}: ReportUserRequestContext<ReportGuildRequest>): Promise<ReportResponse> {
		this.requireVerifiedAccount(user);
		const report = await this.reportService.reportGuild(
			this.createReporter(user),
			createGuildID(data.guild_id),
			data.category,
			data.invite_code ? createInviteCode(data.invite_code) : undefined,
		);
		return this.toReportResponse(report);
	}

	getReportFlow(params: {
		target: ReportFlowTargetType;
		surface: ReportFlowSurface;
		locale: string | null;
	}): ReportFlowResponse {
		return getReportFlowResponse(params.target, params.surface, params.locale);
	}

	async submitMessageReportFlow({
		user,
		viewer,
		data,
		locale,
	}: ReportFlowSubmissionContext<ReportFlowMessageSubmissionRequest> & {
		viewer: ThreadViewer;
	}): Promise<ReportResponse> {
		this.requireVerifiedAccount(user);
		const flow = this.resolveInAppFlow('message', data, locale);
		const report = await this.reportService.reportMessage(
			this.createReporter(user),
			viewer,
			createChannelID(data.channel_id),
			createMessageID(data.message_id),
			flow.category,
			flow.record,
		);
		return this.toReportResponse(report);
	}

	async submitUserReportFlow({
		user,
		data,
		locale,
	}: ReportFlowSubmissionContext<ReportFlowUserSubmissionRequest>): Promise<ReportResponse> {
		this.requireVerifiedAccount(user);
		const flow = this.resolveInAppFlow('user', data, locale);
		const report = await this.reportService.reportUser(
			this.createReporter(user),
			createUserID(data.user_id),
			flow.category,
			data.guild_id ? createGuildID(data.guild_id) : undefined,
			flow.record,
		);
		return this.toReportResponse(report);
	}

	async sendDsaReportVerificationEmail({
		data,
		locale,
	}: ReportDsaRequestContext<DsaReportEmailSendRequest>): Promise<void> {
		await this.reportService.sendDsaReportVerificationCode(data.email, locale ?? null);
	}

	async verifyDsaReportEmail({data}: ReportDsaRequestContext<DsaReportEmailVerifyRequest>): Promise<TicketResponse> {
		const ticket = await this.reportService.verifyDsaReportEmail(data.email, data.code);
		return {ticket};
	}

	async createDsaReport({data, locale}: ReportDsaRequestContext<DsaReportRequest>): Promise<ReportResponse> {
		const report = await this.reportService.createDsaReport(data, locale ?? null);
		return this.toReportResponse(report);
	}

	private resolveInAppFlow(
		target: ReportFlowTargetType,
		data: ReportFlowAnswersInput,
		locale: string | null,
	): {category: string; record: ReportFlowRecord} {
		const answers = resolveReportFlowAnswers({
			target,
			surface: 'in_app',
			revisionHash: data.revision_hash,
			steps: data.steps,
		});
		if (!answers.isCurrentRevision) {
			Logger.warn(
				{clientRevision: data.revision_hash, currentRevision: answers.currentRevisionHash, target},
				'Accepted a report flow submission from an outdated revision',
			);
		}
		return {
			category: answers.legacyCategory,
			record: {
				reason: answers.reason,
				revisionHash: data.revision_hash,
				stepsJson: answers.stepsJson,
				locale: resolveReportFlowLocale(data.locale ?? locale),
				surface: 'in_app',
			},
		};
	}

	private requireVerifiedAccount(user: User): void {
		if (user.isUnclaimedAccount()) {
			throw new UnclaimedAccountCannotSubmitReportsError();
		}
		requireEmailVerified(user, 'report');
	}

	private createReporter(user: User) {
		return {
			id: user.id,
			email: user.email,
			fullLegalName: null,
			countryOfResidence: null,
		};
	}

	private toReportResponse(report: ReportRecord): ReportResponse {
		return {
			report_id: report.reportId.toString(),
			status: reportStatusToString(report.status),
			reported_at: report.reportedAt.toISOString(),
		};
	}
}
