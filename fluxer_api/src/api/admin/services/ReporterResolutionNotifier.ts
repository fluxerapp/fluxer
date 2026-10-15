// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {trySendAdminNotification} from '@app/api/admin/services/AdminNotification';
import type {UserID} from '@app/api/BrandedTypes';
import {listActiveFeatureLimits} from '@app/api/enforcement/FeatureLimitState';
import {resolveContactEmails} from '@app/api/instance/ContactEmails';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import {Logger} from '@app/api/Logger';
import {getEnforcementDecisionRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import type {IARSubmission} from '@app/api/report/ReportModels';
import {type SystemDmDeps, systemDmBody, trySendSystemDm} from '@app/api/user/services/SystemDmMessage';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {ReportItemOutcome} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';

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

function skippedNotice(reason: ReporterNoticeSkipReason): ReporterNoticeResult {
	return {dmSent: false, emailSent: false, dsaEmailSent: false, skipped: reason};
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
		if (report.reporterId) {
			return this.notifyAccountReporter(report, report.reporterId, comment, outcome);
		}
		return this.notifyDsaReporter(report, comment, outcome);
	}

	private async notifyAccountReporter(
		report: IARSubmission,
		reporterId: UserID,
		comment: string,
		outcome: ReportItemOutcome,
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
		const dmSent = await this.sendSystemDm(reporter, reportId, comment, outcome);
		const email = reporter.email;
		const emailSent = email
			? await trySendAdminNotification(
					() =>
						emailService.sendReportResolvedEmail(email, reporter.username, reportId, comment, outcome, reporter.locale),
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
		outcome: ReportItemOutcome,
	): Promise<ReporterNoticeResult> {
		const email = report.reporterEmail;
		if (!email) {
			return skippedNotice('no_reporter_email');
		}
		const {email: emailService} = this.deps.apiContext.services;
		const reportId = report.reportId.toString();
		const dsaEmailSent = await trySendAdminNotification(
			() => emailService.sendDsaReportResolvedEmail(email, reportId, comment, outcome, report.flowLocale),
			{action: 'resolve_report', targetId: reportId},
		);
		return {dmSent: false, emailSent: false, dsaEmailSent, skipped: null};
	}

	private async sendSystemDm(
		reporter: User,
		reportId: string,
		comment: string,
		outcome: ReportItemOutcome,
	): Promise<boolean> {
		const systemDm = this.deps.systemDm;
		if (!systemDm) {
			return false;
		}
		const template = getEmailTemplate(
			'report_resolved',
			reporter.locale,
			{
				username: reporter.username,
				reportId,
				publicComment: comment,
				hasComment: comment ? 'yes' : 'no',
				outcome,
				appeals_email: resolveContactEmails().appealsEmail,
			},
			getInstanceProductName(),
		);
		if (!template.ok) {
			Logger.warn(
				{reportId, reporterId: reporter.id.toString(), locale: reporter.locale, error: template.error},
				'Skipping report review system DM because the email template could not be resolved',
			);
			return false;
		}
		return trySendSystemDm(
			systemDm,
			this.deps.apiContext.services.users,
			reporter.id,
			systemDmBody(template.value.body),
		);
	}
}
