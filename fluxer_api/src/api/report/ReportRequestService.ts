// SPDX-License-Identifier: AGPL-3.0-or-later

import {type BudgetEnv, budgetEnvFor} from '@app/api/auth/AccountBudgets';
import {createChannelID, createGuildID, createInviteCode, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {
	getReportFlowResponse,
	resolveReportFlowAnswers,
	resolveReportFlowLocale,
} from '@app/api/report/flows/ReportFlowRegistry';
import {admitLimitedReport, isLimitedReporter, type LimitedReportTarget} from '@app/api/report/LimitedReporterPolicy';
import {ReportStatus, reportStatusToString} from '@app/api/report/ReportModels';
import type {ReportFlowRecord, ReportService} from '@app/api/report/ReportService';
import {assertTierAllows, getAccountTier, NO_TIER_CONTEXT} from '@app/api/user/AccountTier';
import {TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
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
import {createSnowflakeFromTimestamp} from '@fluxer/snowflake/src/Snowflake';

interface ReportUserRequestContext<T> {
	user: User;
	data: T;
	request: Request;
}

interface ReportDsaRequestContext<T> {
	data: T;
	locale?: string | null;
}

interface ReportFlowSubmissionContext<T> {
	user: User;
	data: T;
	locale: string | null;
	request: Request;
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
	constructor(
		private reportService: ReportService,
		private readonly budgetServices: Pick<BudgetEnv, 'kv' | 'rateLimit'>,
	) {}

	async reportMessage({
		user,
		viewer,
		data,
		request,
	}: ReportUserRequestContext<ReportMessageRequest> & {viewer: ThreadViewer}): Promise<ReportResponse> {
		const admission = await this.admitReporter(user, request, {type: 'message', id: BigInt(data.message_id)});
		if (admission) return admission;
		const report = await this.reportService.reportMessage(
			this.createReporter(user),
			viewer,
			createChannelID(data.channel_id),
			createMessageID(data.message_id),
			data.category,
		);
		return this.toReportResponse(report);
	}

	async reportUser({user, data, request}: ReportUserRequestContext<ReportUserRequest>): Promise<ReportResponse> {
		const admission = await this.admitReporter(user, request, {type: 'user', id: BigInt(data.user_id)});
		if (admission) return admission;
		const report = await this.reportService.reportUser(
			this.createReporter(user),
			createUserID(data.user_id),
			data.category,
			data.guild_id ? createGuildID(data.guild_id) : undefined,
		);
		return this.toReportResponse(report);
	}

	async reportGuild({user, data, request}: ReportUserRequestContext<ReportGuildRequest>): Promise<ReportResponse> {
		const admission = await this.admitReporter(user, request, {type: 'guild', id: BigInt(data.guild_id)});
		if (admission) return admission;
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
		request,
	}: ReportFlowSubmissionContext<ReportFlowMessageSubmissionRequest> & {
		viewer: ThreadViewer;
	}): Promise<ReportResponse> {
		const admission = await this.admitReporter(user, request, {type: 'message', id: BigInt(data.message_id)});
		if (admission) return admission;
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
		request,
	}: ReportFlowSubmissionContext<ReportFlowUserSubmissionRequest>): Promise<ReportResponse> {
		const admission = await this.admitReporter(user, request, {type: 'user', id: BigInt(data.user_id)});
		if (admission) return admission;
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

	private async admitReporter(
		user: User,
		request: Request,
		target: LimitedReportTarget,
	): Promise<ReportResponse | null> {
		if (!isLimitedReporter(user)) {
			assertTierAllows(user, TierGatedActions.REPORT, NO_TIER_CONTEXT);
			return null;
		}
		const clientIp = requireClientIp(request, {
			trustClientIpHeader: Config.proxy.trust_client_ip_header,
			clientIpHeaderName: Config.proxy.client_ip_header,
		});
		const env = budgetEnvFor(this.budgetServices, request);
		if ((await admitLimitedReport(env, {user, clientIp, target})) === 'store') return null;
		const now = Date.now();
		return {
			report_id: createSnowflakeFromTimestamp(now).toString(),
			status: reportStatusToString(ReportStatus.PENDING),
			reported_at: new Date(now).toISOString(),
		};
	}

	private createReporter(user: User) {
		return {
			id: user.id,
			email: user.email,
			fullLegalName: null,
			countryOfResidence: null,
			tier: getAccountTier(user),
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
