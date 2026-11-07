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
import {isUrgentLegacyCategory} from '@app/api/report/flows/ReportReasonCatalog';
import {
	admitLimitedReport,
	isLimitedReporter,
	type LimitedReportTarget,
	releaseLimitedReport,
} from '@app/api/report/LimitedReporterPolicy';
import {ReportStatus, reportStatusToString} from '@app/api/report/ReportModels';
import type {ReportFlowRecord, ReportService} from '@app/api/report/ReportService';
import {assertTierAllows, getAccountTier, NO_TIER_CONTEXT} from '@app/api/user/AccountTier';
import {TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import type {
	ReportFlowGuildSubmissionRequest,
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
		const urgent = isUrgentLegacyCategory('message', data.category);
		return this.admitAndStore(user, request, {type: 'message', id: BigInt(data.message_id)}, urgent, () =>
			this.reportService.reportMessage(
				this.createReporter(user, urgent),
				viewer,
				createChannelID(data.channel_id),
				createMessageID(data.message_id),
				data.category,
			),
		);
	}

	async reportUser({user, data, request}: ReportUserRequestContext<ReportUserRequest>): Promise<ReportResponse> {
		const urgent = isUrgentLegacyCategory('user', data.category);
		return this.admitAndStore(user, request, {type: 'user', id: BigInt(data.user_id)}, urgent, () =>
			this.reportService.reportUser(
				this.createReporter(user, urgent),
				createUserID(data.user_id),
				data.category,
				data.guild_id ? createGuildID(data.guild_id) : undefined,
			),
		);
	}

	async reportGuild({user, data, request}: ReportUserRequestContext<ReportGuildRequest>): Promise<ReportResponse> {
		const urgent = isUrgentLegacyCategory('guild', data.category);
		return this.admitAndStore(user, request, {type: 'guild', id: BigInt(data.guild_id)}, urgent, () =>
			this.reportService.reportGuild(
				this.createReporter(user, urgent),
				createGuildID(data.guild_id),
				data.category,
				data.invite_code ? createInviteCode(data.invite_code) : undefined,
			),
		);
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
		const flow = this.resolveInAppFlow(user, 'message', data, locale);
		return this.admitAndStore(user, request, {type: 'message', id: BigInt(data.message_id)}, flow.urgent, () =>
			this.reportService.reportMessage(
				this.createReporter(user, flow.urgent),
				viewer,
				createChannelID(data.channel_id),
				createMessageID(data.message_id),
				flow.category,
				flow.record,
			),
		);
	}

	async submitUserReportFlow({
		user,
		data,
		locale,
		request,
	}: ReportFlowSubmissionContext<ReportFlowUserSubmissionRequest>): Promise<ReportResponse> {
		const flow = this.resolveInAppFlow(user, 'user', data, locale);
		return this.admitAndStore(user, request, {type: 'user', id: BigInt(data.user_id)}, flow.urgent, () =>
			this.reportService.reportUser(
				this.createReporter(user, flow.urgent),
				createUserID(data.user_id),
				flow.category,
				data.guild_id ? createGuildID(data.guild_id) : undefined,
				flow.record,
			),
		);
	}

	async submitGuildReportFlow({
		user,
		data,
		locale,
		request,
	}: ReportFlowSubmissionContext<ReportFlowGuildSubmissionRequest>): Promise<ReportResponse> {
		const flow = this.resolveInAppFlow(user, 'guild', data, locale);
		return this.admitAndStore(user, request, {type: 'guild', id: BigInt(data.guild_id)}, flow.urgent, () =>
			this.reportService.reportGuild(
				this.createReporter(user, flow.urgent),
				createGuildID(data.guild_id),
				flow.category,
				data.invite_code ? createInviteCode(data.invite_code) : undefined,
				flow.record,
			),
		);
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
		user: User,
		target: ReportFlowTargetType,
		data: ReportFlowAnswersInput,
		locale: string | null,
	): {category: string; record: ReportFlowRecord; urgent: boolean} {
		let answers: ReturnType<typeof resolveReportFlowAnswers>;
		try {
			answers = resolveReportFlowAnswers({
				target,
				surface: 'in_app',
				revisionHash: data.revision_hash,
				steps: data.steps,
			});
		} catch (error) {
			if (!isLimitedReporter(user)) assertTierAllows(user, TierGatedActions.REPORT, NO_TIER_CONTEXT);
			throw error;
		}
		if (!answers.isCurrentRevision) {
			Logger.warn(
				{clientRevision: data.revision_hash, currentRevision: answers.currentRevisionHash, target},
				'Accepted a report flow submission from an outdated revision',
			);
		}
		return {
			category: answers.legacyCategory,
			urgent: answers.urgent,
			record: {
				reason: answers.reason,
				revisionHash: data.revision_hash,
				stepsJson: answers.stepsJson,
				locale: resolveReportFlowLocale(data.locale ?? locale),
				surface: 'in_app',
			},
		};
	}

	private async admitAndStore(
		user: User,
		request: Request,
		target: LimitedReportTarget,
		urgent: boolean,
		store: () => Promise<ReportRecord>,
	): Promise<ReportResponse> {
		if (urgent) {
			return this.toReportResponse(await store());
		}
		if (!isLimitedReporter(user)) {
			assertTierAllows(user, TierGatedActions.REPORT, NO_TIER_CONTEXT);
			return this.toReportResponse(await store());
		}
		const clientIp = requireClientIp(request, {
			trustClientIpHeader: Config.proxy.trust_client_ip_header,
			clientIpHeaderName: Config.proxy.client_ip_header,
		});
		const env = budgetEnvFor(this.budgetServices, request);
		if ((await admitLimitedReport(env, {user, clientIp, target})) === 'store') {
			try {
				return this.toReportResponse(await store());
			} catch (error) {
				await releaseLimitedReport(env, clientIp, target);
				throw error;
			}
		}
		const now = Date.now();
		return {
			report_id: createSnowflakeFromTimestamp(now).toString(),
			status: reportStatusToString(ReportStatus.PENDING),
			reported_at: new Date(now).toISOString(),
		};
	}

	private createReporter(user: User, urgent = false) {
		return {
			id: user.id,
			email: user.email,
			fullLegalName: null,
			countryOfResidence: null,
			tier: getAccountTier(user),
			urgent,
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
