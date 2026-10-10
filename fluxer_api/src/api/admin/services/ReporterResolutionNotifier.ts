// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {trySendAdminNotification} from '@app/api/admin/services/AdminNotification';
import type {UserID} from '@app/api/BrandedTypes';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import {SYSTEM_THREAD_VIEWER} from '@app/api/experiment/ChannelThreadsGate';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {resolveContactEmails} from '@app/api/instance/ContactEmails';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import {Logger} from '@app/api/Logger';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import type {IARSubmission} from '@app/api/report/ReportModels';
import type {UserChannelService} from '@app/api/user/services/UserChannelService';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';

interface ReporterSystemDmDeps {
	channelService: ChannelService;
	userChannelService: UserChannelService;
	userCacheService: UserCacheService;
}

interface ReporterResolutionNotifierDeps {
	apiContext: ApiContext;
	systemDm: ReporterSystemDmDeps | null;
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

	async notifyReporterOfResolution(report: IARSubmission, publicComment: string | null): Promise<ReporterNoticeResult> {
		const comment = publicComment ?? '';
		if (report.reporterId) {
			return this.notifyAccountReporter(report, report.reporterId, comment);
		}
		return this.notifyDsaReporter(report, comment);
	}

	private async notifyAccountReporter(
		report: IARSubmission,
		reporterId: UserID,
		comment: string,
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
		if ((reporter.flags & UserFlags.REPORT_BANNED) !== 0n) {
			return skippedNotice('reporter_report_banned');
		}
		const dmSent = await this.sendSystemDm(reporter, reportId, comment);
		const email = reporter.email;
		const emailSent = email
			? await trySendAdminNotification(
					() => emailService.sendReportResolvedEmail(email, reporter.username, reportId, comment, reporter.locale),
					{action: 'resolve_report', targetId: reportId},
				)
			: false;
		return {dmSent, emailSent, dsaEmailSent: false, skipped: null};
	}

	private async notifyDsaReporter(report: IARSubmission, comment: string): Promise<ReporterNoticeResult> {
		const email = report.reporterEmail;
		if (!email) {
			return skippedNotice('no_reporter_email');
		}
		const {email: emailService} = this.deps.apiContext.services;
		const reportId = report.reportId.toString();
		const dsaEmailSent = await trySendAdminNotification(
			() => emailService.sendDsaReportResolvedEmail(email, reportId, comment, report.flowLocale),
			{action: 'resolve_report', targetId: reportId},
		);
		return {dmSent: false, emailSent: false, dsaEmailSent, skipped: null};
	}

	private async sendSystemDm(reporter: User, reportId: string, comment: string): Promise<boolean> {
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
				safety_email: resolveContactEmails().safetyEmail,
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
		const {users: userRepository} = this.deps.apiContext.services;
		const requestCache = createRequestCache();
		try {
			const systemUser = await userRepository.findUniqueAssert(SYSTEM_USER_ID);
			const dmChannel = await systemDm.userChannelService.ensureDmOpenForBothUsers({
				userId: systemUser.id,
				recipientId: reporter.id,
				userCacheService: systemDm.userCacheService,
				requestCache,
			});
			await systemDm.channelService.messages.send.sendMessage({
				user: systemUser,
				viewer: SYSTEM_THREAD_VIEWER,
				channelId: dmChannel.id,
				data: {content: template.value.body},
				requestCache,
			});
			return true;
		} catch (error) {
			Logger.warn({reportId, reporterId: reporter.id.toString(), error}, 'Failed to send report review system DM');
			return false;
		} finally {
			requestCache.clear();
		}
	}
}
