// SPDX-License-Identifier: AGPL-3.0-or-later

import {createLogger} from '@fluxer/logger/src/Logger';
import type {IEmailI18nService} from '@pkgs/email/src/EmailI18nService';
import type {EmailConfig, IEmailProvider, UserBouncedEmailChecker} from '@pkgs/email/src/EmailProviderTypes';
import type {EmailTemplateVariables} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
import type {EmailTemplateKey} from '@pkgs/email/src/email_i18n/EmailI18nTypes.generated';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {ms} from 'itty-time';

const logger = createLogger('email-service');

export class EmailService implements IEmailService {
	private readonly config: EmailConfig;
	private readonly emailI18n: IEmailI18nService;
	private readonly provider: IEmailProvider | null;
	private readonly bouncedEmailChecker: UserBouncedEmailChecker | null;

	constructor(
		config: EmailConfig,
		emailI18n: IEmailI18nService,
		provider: IEmailProvider | null = null,
		bouncedEmailChecker: UserBouncedEmailChecker | null = null,
	) {
		this.config = config;
		this.emailI18n = emailI18n;
		this.provider = provider;
		this.bouncedEmailChecker = bouncedEmailChecker;
	}

	async sendPasswordResetEmail(
		email: string,
		username: string,
		resetToken: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'password_reset', locale, {
			username,
			resetUrl: `${this.config.appBaseUrl}/reset#token=${resetToken}`,
		});
	}

	async sendEmailVerification(
		email: string,
		username: string,
		verificationToken: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'email_verification', locale, {
			username,
			verifyUrl: `${this.config.appBaseUrl}/verify#token=${verificationToken}`,
		});
	}

	async sendIpAuthorizationEmail(
		email: string,
		username: string,
		authorizationToken: string,
		ipAddress: string,
		location: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'ip_authorization', locale, {
			username,
			authUrl: `${this.config.appBaseUrl}/authorize-ip#token=${authorizationToken}`,
			ipAddress,
			location,
		});
	}

	async sendAccountDisabledForSuspiciousActivityEmail(
		email: string,
		username: string,
		reason: string | null,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'account_disabled_suspicious', locale, {
			username,
			reason,
			forgotUrl: `${this.config.appBaseUrl}/forgot`,
		});
	}

	async sendAccountTempBannedEmail(
		email: string,
		username: string,
		reason: string | null,
		durationHours: number,
		bannedUntil: Date,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'account_temp_banned', locale, {
			username,
			reason,
			durationHours,
			bannedUntil,
			termsUrl: `${this.config.marketingBaseUrl}/terms`,
			guidelinesUrl: `${this.config.marketingBaseUrl}/guidelines`,
		});
	}

	async sendAccountScheduledForDeletionEmail(
		email: string,
		username: string,
		reason: string | null,
		deletionDate: Date,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'account_scheduled_deletion', locale, {
			username,
			reason,
			deletionDate,
			termsUrl: `${this.config.marketingBaseUrl}/terms`,
			guidelinesUrl: `${this.config.marketingBaseUrl}/guidelines`,
		});
	}

	async sendSelfDeletionScheduledEmail(
		email: string,
		username: string,
		deletionDate: Date,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'self_deletion_scheduled', locale, {username, deletionDate});
	}

	async sendUnbanNotification(
		email: string,
		username: string,
		reason: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'unban_notification', locale, {username, reason});
	}

	async sendScheduledDeletionNotification(
		email: string,
		username: string,
		deletionDate: Date,
		reason: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'scheduled_deletion_notification', locale, {username, deletionDate, reason});
	}

	async sendInactivityWarningEmail(
		email: string,
		username: string,
		deletionDate: Date,
		lastActiveDate: Date,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'inactivity_warning', locale, {
			username,
			deletionDate,
			lastActiveDate,
			loginUrl: `${this.config.appBaseUrl}/login`,
		});
	}

	async sendHarvestCompletedEmail(
		email: string,
		username: string,
		downloadUrl: string,
		totalMessages: number,
		fileSize: number,
		expiresAt: Date,
		locale: string | null = null,
	): Promise<boolean> {
		const fileSizeMB = Number.parseFloat((fileSize / 1024 / 1024).toFixed(2));
		return this.sendTemplatedEmail(email, 'harvest_completed', locale, {
			username,
			downloadUrl,
			totalMessages,
			fileSizeMB,
			expiresAt,
		});
	}

	async sendGiftChargebackNotification(
		email: string,
		username: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'gift_chargeback_notification', locale, {username});
	}

	async sendReportResolvedEmail(
		email: string,
		username: string,
		reportId: string,
		publicComment: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'report_resolved', locale, {
			username,
			reportId,
			publicComment,
			hasComment: publicComment ? 'yes' : 'no',
		});
	}

	async sendDsaReportVerificationCode(
		email: string,
		code: string,
		expiresAt: Date,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'dsa_report_verification', locale, {code, expiresAt});
	}

	async sendRegistrationApprovedEmail(email: string, username: string, locale: string | null = null): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'registration_approved', locale, {
			username,
			channelsUrl: `${this.config.appBaseUrl}/channels/@me`,
		});
	}

	async sendPasswordChangeVerification(
		email: string,
		username: string,
		code: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'password_change_verification', locale, {
			username,
			code,
			expiresAt: new Date(Date.now() + ms('10 minutes')),
		});
	}

	async sendMfaBackupCodesVerification(
		email: string,
		username: string,
		code: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'mfa_backup_codes_view', locale, {
			username,
			code,
			expiresAt: new Date(Date.now() + ms('10 minutes')),
		});
	}

	async sendEmailChangeOriginal(
		email: string,
		username: string,
		code: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'email_change_original', locale, {
			username,
			code,
			expiresAt: new Date(Date.now() + ms('10 minutes')),
		});
	}

	async sendEmailChangeNew(
		email: string,
		username: string,
		code: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'email_change_new', locale, {
			username,
			code,
			expiresAt: new Date(Date.now() + ms('10 minutes')),
		});
	}

	async sendEmailChangeRevert(
		email: string,
		username: string,
		newEmail: string,
		token: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'email_change_revert', locale, {
			username,
			revertUrl: `${this.config.appBaseUrl}/wasntme#token=${token}`,
			newEmail,
		});
	}

	async sendDonationMagicLink(
		email: string,
		_token: string,
		manageUrl: string,
		expiresAt: Date,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'donation_magic_link', locale, {manageUrl, expiresAt});
	}

	async sendDonationConfirmation(
		email: string,
		amountCents: number,
		currency: string,
		interval: string,
		manageUrl: string,
		locale: string | null = null,
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, 'donation_confirmation', locale, {
			amount: (amountCents / 100).toFixed(2),
			currency: currency.toUpperCase(),
			interval,
			manageUrl,
		});
	}

	private async sendTemplatedEmail<T extends EmailTemplateKey>(
		email: string,
		templateKey: T,
		locale: string | null,
		variables: EmailTemplateVariables[T],
	): Promise<boolean> {
		const result = this.emailI18n.getTemplate(templateKey, locale, variables);
		if (!result.ok) {
			logger.error({key: templateKey, locale: result.locale, error: result.error}, 'Failed to resolve email template');
			return false;
		}
		const {subject, body} = result.value;
		if (!this.config.enabled || !this.provider) {
			logger.info(
				{templateKey},
				`Email service disabled. Would have sent:\nTo: ${email}\nSubject: ${subject}\n\n${body}`,
			);
			return true;
		}
		if (this.bouncedEmailChecker) {
			const bounced = await this.bouncedEmailChecker.isEmailBounced(email);
			if (bounced) {
				logger.warn({email}, 'Refusing to send email to bounced address - email marked as hard bounced');
				return false;
			}
		}
		return this.provider.sendEmail({
			to: email,
			from: {email: this.config.fromEmail, name: this.config.fromName},
			subject,
			text: body,
		});
	}
}
