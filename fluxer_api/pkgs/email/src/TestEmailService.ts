// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	EmailTemplateVariables,
	ReporterOutcome,
	ReportReceivedTargetKind,
} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
import type {IEmailService} from '@pkgs/email/src/IEmailService';

export interface SentEmailRecord {
	to: string;
	type: string;
	timestamp: Date;
	metadata: Record<string, string>;
}

function stringMetadata(variables: Record<string, string | null>): Record<string, string> {
	return Object.fromEntries(Object.entries(variables).map(([key, value]) => [key, value ?? '']));
}

export class TestEmailService implements IEmailService {
	private readonly sentEmails: Array<SentEmailRecord> = [];

	listSentEmails(): Array<SentEmailRecord> {
		return [...this.sentEmails];
	}

	clearSentEmails(): void {
		this.sentEmails.length = 0;
	}

	async sendPasswordResetEmail(
		email: string,
		_username: string,
		resetToken: string,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'password_reset', {token: resetToken});
	}

	async sendEmailVerification(
		email: string,
		_username: string,
		verificationToken: string,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'email_verification', {token: verificationToken});
	}

	async sendIpAuthorizationEmail(
		email: string,
		_username: string,
		authorizationToken: string,
		ipAddress: string,
		location: string,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'ip_authorization', {token: authorizationToken, ip: ipAddress, location});
	}

	async sendSelfDeletionScheduledEmail(
		email: string,
		_username: string,
		deletionDate: Date,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'self_deletion_scheduled', {deletion_date: deletionDate.toISOString()});
	}

	async sendAccountDeletionRequestedEmail(
		email: string,
		_username: string,
		deletionDate: string,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'account_deletion_scheduled_requested', {deletion_date: deletionDate});
	}

	async sendAccountDeletionInactivityEmail(
		email: string,
		_username: string,
		deletionDate: string,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'account_deletion_scheduled_inactivity', {deletion_date: deletionDate});
	}

	async sendAccountDeletionCancelledEmail(email: string, _username: string, _locale?: string | null): Promise<boolean> {
		return this.record(email, 'account_deletion_cancelled');
	}

	async sendEnforcementNoticeEmail(
		email: string,
		variables: EmailTemplateVariables['enforcement_notice'],
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'enforcement_notice', stringMetadata(variables));
	}

	async sendEnforcementEndedEmail(
		email: string,
		variables: EmailTemplateVariables['enforcement_ended'],
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'enforcement_ended', stringMetadata(variables));
	}

	async sendInactivityWarningEmail(
		email: string,
		_username: string,
		deletionDate: Date,
		lastActiveDate: Date,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'inactivity_warning', {
			deletion_date: deletionDate.toISOString(),
			last_active_date: lastActiveDate.toISOString(),
		});
	}

	async sendHarvestCompletedEmail(
		email: string,
		_username: string,
		downloadUrl: string,
		totalMessages: number,
		fileSize: number,
		expiresAt: Date,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'harvest_completed', {
			download_url: downloadUrl,
			total_messages: totalMessages.toString(10),
			file_size: fileSize.toString(10),
			expires_at: expiresAt.toISOString(),
		});
	}

	async sendGiftChargebackNotification(email: string, _username: string, _locale?: string | null): Promise<boolean> {
		return this.record(email, 'gift_chargeback_notification');
	}

	async sendReportResolvedEmail(
		email: string,
		_username: string,
		reportId: string,
		publicComment: string,
		outcome: ReporterOutcome,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'report_resolved', {report_id: reportId, public_comment: publicComment, outcome});
	}

	async sendDsaReportResolvedEmail(
		email: string,
		reportId: string,
		publicComment: string,
		outcome: ReporterOutcome,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'dsa_report_resolved', {report_id: reportId, public_comment: publicComment, outcome});
	}

	async sendReportReceivedEmail(
		email: string,
		reportId: string,
		targetKind: ReportReceivedTargetKind,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'report_received', {report_id: reportId, target_kind: targetKind});
	}

	async sendDsaReportVerificationCode(
		email: string,
		code: string,
		expiresAt: Date,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'dsa_report_verification', {code, expires_at: expiresAt.toISOString()});
	}

	async sendPasswordChangeVerification(
		email: string,
		_username: string,
		code: string,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'password_change_verification', {code});
	}

	async sendMfaBackupCodesVerification(
		email: string,
		_username: string,
		code: string,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'mfa_backup_codes_view', {code});
	}

	async sendEmailChangeOriginal(
		email: string,
		_username: string,
		code: string,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'email_change_original', {code});
	}

	async sendEmailChangeNew(email: string, _username: string, code: string, _locale?: string | null): Promise<boolean> {
		return this.record(email, 'email_change_new', {code});
	}

	async sendEmailChangeRevert(
		email: string,
		_username: string,
		newEmail: string,
		token: string,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'email_change_revert', {token, new_email: newEmail});
	}

	async sendDonationMagicLink(
		email: string,
		token: string,
		manageUrl: string,
		expiresAt: Date,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'donation_magic_link', {
			token,
			manage_url: manageUrl,
			expires_at: expiresAt.toISOString(),
		});
	}

	async sendDonationConfirmation(
		email: string,
		amountCents: number,
		currency: string,
		interval: string,
		manageUrl: string,
		_locale?: string | null,
	): Promise<boolean> {
		return this.record(email, 'donation_confirmation', {
			amount: (amountCents / 100).toString(),
			currency: currency.toUpperCase(),
			interval,
			manage_url: manageUrl,
		});
	}

	private record(to: string, type: string, metadata: Record<string, string> = {}): boolean {
		this.sentEmails.push({to, type, timestamp: new Date(), metadata});
		return true;
	}
}
