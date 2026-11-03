// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {trySendAdminNotification} from '@app/api/admin/services/AdminNotification';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {listActiveFeatureLimits} from '@app/api/enforcement/FeatureLimitState';
import {resolveContactEmails} from '@app/api/instance/ContactEmails';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import {Logger} from '@app/api/Logger';
import {getEnforcementDecisionRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {type IARSubmission, ReportType} from '@app/api/report/ReportModels';
import {type SystemDmDeps, systemDmBody, trySendSystemDm} from '@app/api/user/services/SystemDmMessage';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {ReportItemOutcome} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {emailLinkUrls, renderEmailTemplateText, reportResolvedSlots} from '@pkgs/email/src/EmailService';
import type {ReportReference} from '@pkgs/email/src/email_i18n/EmailI18nTypes';

interface ReporterResolutionNotifierDeps {
	apiContext: ApiContext;
	systemDm: SystemDmDeps | null;
}

type ReporterNoticeSkipReason =
	| 'reporter_account_deleted'
	| 'reporter_report_banned'
	| 'reporter_lookup_failed'
	| 'no_reporter_email';

interface ReporterNoticeResult {
	dmSent: boolean;
	emailSent: boolean;
	dsaEmailSent: boolean;
	skipped: ReporterNoticeSkipReason | null;
}

type ReporterToldOutcome = Exclude<ReportItemOutcome, 'warned'>;

function skippedNotice(reason: ReporterNoticeSkipReason): ReporterNoticeResult {
	return {dmSent: false, emailSent: false, dsaEmailSent: false, skipped: reason};
}

export function reportReference(report: IARSubmission): ReportReference {
	const reference: ReportReference = {
		target: 'other',
		reportedAt: report.reportedAt,
		username: '',
		communityName: '',
		channelName: '',
	};
	switch (report.reportType) {
		case ReportType.MESSAGE:
			if (!report.reportedGuildId) {
				return {...reference, target: 'message_dm'};
			}
			if (report.reportedGuildName && report.reportedChannelName) {
				return {
					...reference,
					target: 'message_community',
					communityName: report.reportedGuildName,
					channelName: report.reportedChannelName,
				};
			}
			return {...reference, target: 'message'};
		case ReportType.USER: {
			const username = report.reportedProfileSnapshot?.user?.username;
			return username ? {...reference, target: 'user', username} : reference;
		}
		case ReportType.GUILD:
			return report.reportedGuildName
				? {...reference, target: 'community', communityName: report.reportedGuildName}
				: reference;
		default:
			return reference;
	}
}

export function wasReporterNotified(result: ReporterNoticeResult): boolean {
	return result.dmSent || result.emailSent || result.dsaEmailSent;
}

export function describeReporterNotice(result: ReporterNoticeResult): Array<[string, string]> {
	return [
		['reporter_dm_sent', result.dmSent ? 'true' : 'false'],
		['reporter_email_sent', result.emailSent ? 'true' : 'false'],
		['reporter_dsa_email_sent', result.dsaEmailSent ? 'true' : 'false'],
		...(result.skipped ? [['reporter_notice_skipped', result.skipped] as [string, string]] : []),
	];
}

export class ReporterResolutionNotifier {
	constructor(private readonly deps: ReporterResolutionNotifierDeps) {}

	async notifyReporterOfResolution(
		report: IARSubmission,
		publicComment: string | null,
		outcome: ReportItemOutcome,
	): Promise<ReporterNoticeResult> {
		const comment = publicComment ?? '';
		const told = outcome === 'warned' ? 'action_taken' : outcome;
		if (report.reporterId) {
			return this.notifyAccountReporter(report, report.reporterId, comment, told);
		}
		return this.notifyDsaReporter(report, comment, told);
	}

	private async notifyAccountReporter(
		report: IARSubmission,
		reporterId: UserID,
		comment: string,
		outcome: ReporterToldOutcome,
	): Promise<ReporterNoticeResult> {
		const {users: userRepository, email: emailService} = this.deps.apiContext.services;
		const reportId = report.reportId.toString();
		let reporter: User | null;
		try {
			reporter = await userRepository.findUnique(reporterId);
		} catch (error) {
			Logger.warn(
				{error, reportId, reporterId: reporterId.toString()},
				'Failed to load the reporter of a resolved report',
			);
			return skippedNotice('reporter_lookup_failed');
		}
		if (!reporter || (reporter.flags & UserFlags.DELETED) !== 0n) {
			return skippedNotice('reporter_account_deleted');
		}
		if (await this.isReportBannedWhenFiled(reporter, report)) {
			return skippedNotice('reporter_report_banned');
		}
		const dmSent = await this.sendSystemDm(reporter, report, comment, outcome);
		const email = reporter.email;
		const emailSent = email
			? await trySendAdminNotification(
					() =>
						emailService.send(
							'report_resolved',
							email,
							{reportId, publicComment: comment, outcome, reference: reportReference(report)},
							reporter.locale,
						),
					{action: 'resolve_report', targetId: reportId},
				)
			: false;
		return {dmSent, emailSent, dsaEmailSent: false, skipped: null};
	}

	private async isReportBannedWhenFiled(reporter: User, report: IARSubmission): Promise<boolean> {
		if ((reporter.flags & UserFlags.REPORT_BANNED) === 0n) {
			return false;
		}
		const limits = await listActiveFeatureLimits(getEnforcementDecisionRepository(), reporter.id, 'reports');
		if (limits.length === 0) {
			return true;
		}
		return limits.some((decision) => decision.created_at.getTime() < report.reportedAt.getTime());
	}

	private async notifyDsaReporter(
		report: IARSubmission,
		comment: string,
		outcome: ReporterToldOutcome,
	): Promise<ReporterNoticeResult> {
		const email = report.reporterEmail;
		if (!email) {
			return skippedNotice('no_reporter_email');
		}
		const {email: emailService} = this.deps.apiContext.services;
		const reportId = report.reportId.toString();
		const dsaEmailSent = await trySendAdminNotification(
			() =>
				emailService.send(
					'dsa_report_resolved',
					email,
					{reportId, publicComment: comment, outcome, reference: reportReference(report)},
					report.flowLocale,
				),
			{action: 'resolve_report', targetId: reportId},
		);
		return {dmSent: false, emailSent: false, dsaEmailSent, skipped: null};
	}

	private async sendSystemDm(
		reporter: User,
		report: IARSubmission,
		comment: string,
		outcome: ReporterToldOutcome,
	): Promise<boolean> {
		const systemDm = this.deps.systemDm;
		if (!systemDm) {
			return false;
		}
		const reportId = report.reportId.toString();
		const text = renderEmailTemplateText(
			'report_resolved',
			reporter.locale,
			{...reportReference(report), outcome, appeals_email: resolveContactEmails().appealsEmail},
			getInstanceProductName(),
			(template) => reportResolvedSlots(template, reportId, comment),
			{
				format: 'markdown',
				urls: emailLinkUrls({appBaseUrl: Config.endpoints.webApp, marketingBaseUrl: Config.endpoints.marketing}),
			},
		);
		if (text === null) {
			Logger.warn(
				{reportId, reporterId: reporter.id.toString(), locale: reporter.locale},
				'Skipping report review system DM because the email template could not be resolved',
			);
			return false;
		}
		return trySendSystemDm(systemDm, this.deps.apiContext.services.users, reporter.id, systemDmBody(text));
	}
}
