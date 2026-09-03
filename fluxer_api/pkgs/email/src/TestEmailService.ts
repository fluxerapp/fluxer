// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ITestEmailService, SentEmailRecord} from '@pkgs/email/src/ITestEmailService';

function maskToken(token: string): string {
	if (token.length <= 8) {
		return '*'.repeat(token.length);
	}
	return `${token.slice(0, 4)}...${token.slice(-4)}`;
}

const noopLogger = {info: (_message: string) => {}};

export class TestEmailService implements ITestEmailService {
	private readonly sentEmails: Array<SentEmailRecord> = [];
	private readonly logger: {
		info: (message: string) => void;
	};

	constructor(options?: {
		logger?: {
			info: (message: string) => void;
		};
	}) {
		this.logger = options?.logger ?? noopLogger;
	}

	listSentEmails(): Array<SentEmailRecord> {
		return [...this.sentEmails];
	}

	clearSentEmails(): void {
		this.sentEmails.length = 0;
	}

	async sendPasswordResetEmail(
		email: string,
		username: string,
		resetToken: string,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(`Password reset email sent to ${email} for user ${username}, token: ${maskToken(resetToken)}`);
		return this.record(email, 'password_reset', {token: resetToken});
	}

	async sendEmailVerification(
		email: string,
		username: string,
		verificationToken: string,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(
			`Email verification sent to ${email} for user ${username}, token: ${maskToken(verificationToken)}`,
		);
		return this.record(email, 'email_verification', {token: verificationToken});
	}

	async sendIpAuthorizationEmail(
		email: string,
		username: string,
		authorizationToken: string,
		ipAddress: string,
		location: string,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(
			`IP authorization email sent to ${email} for user ${username}, IP: ${ipAddress}, location: ${location}`,
		);
		return this.record(email, 'ip_authorization', {token: authorizationToken, ip: ipAddress, location});
	}

	async sendAccountDisabledForSuspiciousActivityEmail(
		email: string,
		username: string,
		reason: string | null,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(`Account disabled email sent to ${email} for user ${username}, reason: ${reason ?? 'none'}`);
		return this.record(email, 'account_disabled_suspicious', {reason: reason ?? ''});
	}

	async sendAccountTempBannedEmail(
		email: string,
		username: string,
		reason: string | null,
		durationHours: number,
		bannedUntil: Date,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(
			`Temp ban email sent to ${email} for user ${username}, ${durationHours}h until ${bannedUntil.toISOString()}`,
		);
		return this.record(email, 'account_temp_banned', {
			reason: reason ?? '',
			duration_hours: durationHours.toString(10),
			banned_until: bannedUntil.toISOString(),
		});
	}

	async sendAccountScheduledForDeletionEmail(
		email: string,
		username: string,
		reason: string | null,
		deletionDate: Date,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(
			`Scheduled deletion email sent to ${email} for user ${username}, date: ${deletionDate.toISOString()}`,
		);
		return this.record(email, 'account_scheduled_deletion', {
			reason: reason ?? '',
			deletion_date: deletionDate.toISOString(),
		});
	}

	async sendSelfDeletionScheduledEmail(
		email: string,
		username: string,
		deletionDate: Date,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(`Self deletion email sent to ${email} for user ${username}, date: ${deletionDate.toISOString()}`);
		return this.record(email, 'self_deletion_scheduled', {deletion_date: deletionDate.toISOString()});
	}

	async sendUnbanNotification(
		email: string,
		username: string,
		reason: string,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(`Unban notification sent to ${email} for user ${username}, reason: ${reason}`);
		return this.record(email, 'unban_notification', {reason});
	}

	async sendScheduledDeletionNotification(
		email: string,
		username: string,
		deletionDate: Date,
		reason: string,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(
			`Scheduled deletion notification sent to ${email} for user ${username}, date: ${deletionDate.toISOString()}`,
		);
		return this.record(email, 'scheduled_deletion_notification', {deletion_date: deletionDate.toISOString(), reason});
	}

	async sendInactivityWarningEmail(
		email: string,
		username: string,
		deletionDate: Date,
		lastActiveDate: Date,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(
			`Inactivity warning sent to ${email} for user ${username}, deletion: ${deletionDate.toISOString()}`,
		);
		return this.record(email, 'inactivity_warning', {
			deletion_date: deletionDate.toISOString(),
			last_active_date: lastActiveDate.toISOString(),
		});
	}

	async sendHarvestCompletedEmail(
		email: string,
		username: string,
		downloadUrl: string,
		totalMessages: number,
		fileSize: number,
		expiresAt: Date,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(
			`Harvest completed email sent to ${email} for user ${username}, ${totalMessages} messages, ${fileSize} bytes`,
		);
		return this.record(email, 'harvest_completed', {
			download_url: downloadUrl,
			total_messages: totalMessages.toString(10),
			file_size: fileSize.toString(10),
			expires_at: expiresAt.toISOString(),
		});
	}

	async sendGiftChargebackNotification(email: string, username: string, _locale?: string | null): Promise<boolean> {
		this.logger.info(`Gift chargeback notification sent to ${email} for user ${username}`);
		return this.record(email, 'gift_chargeback_notification');
	}

	async sendReportResolvedEmail(
		email: string,
		username: string,
		reportId: string,
		publicComment: string,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(`Report resolved email sent to ${email} for user ${username}, report: ${reportId}`);
		return this.record(email, 'report_resolved', {report_id: reportId, public_comment: publicComment});
	}

	async sendDsaReportVerificationCode(
		email: string,
		code: string,
		expiresAt: Date,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(`DSA report verification code sent to ${email}, code: ${maskToken(code)}`);
		return this.record(email, 'dsa_report_verification', {code, expires_at: expiresAt.toISOString()});
	}

	async sendRegistrationApprovedEmail(email: string, username: string, _locale?: string | null): Promise<boolean> {
		this.logger.info(`Registration approved email sent to ${email} for user ${username}`);
		return this.record(email, 'registration_approved');
	}

	async sendPasswordChangeVerification(
		email: string,
		username: string,
		code: string,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(`Password change verification sent to ${email} for user ${username}`);
		return this.record(email, 'password_change_verification', {code});
	}

	async sendMfaBackupCodesVerification(
		email: string,
		username: string,
		code: string,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(`MFA backup codes verification sent to ${email} for user ${username}`);
		return this.record(email, 'mfa_backup_codes_view', {code});
	}

	async sendEmailChangeOriginal(
		email: string,
		username: string,
		code: string,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(`Email change original verification sent to ${email} for user ${username}`);
		return this.record(email, 'email_change_original', {code});
	}

	async sendEmailChangeNew(email: string, username: string, code: string, _locale?: string | null): Promise<boolean> {
		this.logger.info(`Email change new verification sent to ${email} for user ${username}`);
		return this.record(email, 'email_change_new', {code});
	}

	async sendEmailChangeRevert(
		email: string,
		username: string,
		newEmail: string,
		token: string,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(`Email change revert notice sent to ${email} for user ${username}`);
		return this.record(email, 'email_change_revert', {token, new_email: newEmail});
	}

	async sendDonationMagicLink(
		email: string,
		token: string,
		manageUrl: string,
		expiresAt: Date,
		_locale?: string | null,
	): Promise<boolean> {
		this.logger.info(
			`Donation magic link sent to ${email}, token: ${maskToken(token)}, expires: ${expiresAt.toISOString()}`,
		);
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
		this.logger.info(
			`Donation confirmation sent to ${email}, amount: ${amountCents / 100} ${currency.toUpperCase()}/${interval}`,
		);
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
