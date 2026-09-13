// SPDX-License-Identifier: AGPL-3.0-or-later

import {requireEmailVerified} from '@app/api/auth/EmailVerificationUtils';
import {createChannelID, createGuildID, createInviteCode, createMessageID, createUserID} from '@app/api/BrandedTypes';
import type {User} from '@app/api/models/User';
import {type ReportStatus, reportStatusToString} from '@app/api/report/IReportRepository';
import type {ReportService} from '@app/api/report/ReportService';
import {UnclaimedAccountCannotSubmitReportsError} from '@fluxer/errors/src/domains/moderation/UnclaimedAccountCannotSubmitReportsError';
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

interface ReportRecord {
	reportId: bigint;
	status: ReportStatus;
	reportedAt: Date;
}

export class ReportRequestService {
	constructor(private reportService: ReportService) {}

	async reportMessage({user, data}: ReportUserRequestContext<ReportMessageRequest>): Promise<ReportResponse> {
		this.requireVerifiedAccount(user);
		const report = await this.reportService.reportMessage(
			this.createReporter(user),
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

	async createDsaReport({data}: ReportDsaRequestContext<DsaReportRequest>): Promise<ReportResponse> {
		const report = await this.reportService.createDsaReport(data);
		return this.toReportResponse(report);
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
