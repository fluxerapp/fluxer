/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import sgMail from '@sendgrid/mail';
import {Config} from '~/Config';
import type {IEmailService} from '~/infrastructure/IEmailService';
import {Logger} from '~/Logger';
import type {IUserRepository} from '~/user/IUserRepository';
import {EmailI18nService} from './EmailI18nService';

export class EmailService implements IEmailService {
	private readonly appBaseUrl: string;
	private readonly marketingBaseUrl: string;
	private readonly emailI18n: EmailI18nService;

	constructor(private readonly userRepository: IUserRepository) {
		this.appBaseUrl = Config.endpoints.webApp;
		this.marketingBaseUrl = Config.endpoints.marketing;
		this.emailI18n = new EmailI18nService();

		if (this.isEmailEnabled()) {
			sgMail.setApiKey(Config.email.apiKey!);
		}
	}

	private isEmailEnabled(): boolean {
		return Config.email.enabled && !!(Config.email.apiKey && Config.email.fromEmail);
	}

	private async sendEmailWithTemplate(
		email: string,
		subject: string,
		body: string,
		logContext: string,
	): Promise<boolean> {
		if (!this.isEmailEnabled()) {
			Logger.info(
				{logContext},
				`Email service disabled. Would have sent:\nTo: ${email}\nSubject: ${subject}\n\n${body}`,
			);
			return true;
		}
		return await this.sendEmail(email, subject, body);
	}

	async sendPasswordResetEmail(
		email: string,
		username: string,
		resetToken: string,
		locale: string | null = null,
	): Promise<boolean> {
		const resetUrl = `${this.appBaseUrl}/reset#token=${resetToken}`;
		const template = this.emailI18n.getTemplate('passwordReset', locale, {
			username,
			resetUrl,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'password reset');
	}

	async sendEmailVerification(
		email: string,
		username: string,
		verificationToken: string,
		locale: string | null = null,
	): Promise<boolean> {
		const verifyUrl = `${this.appBaseUrl}/verify#token=${verificationToken}`;
		const template = this.emailI18n.getTemplate('emailVerification', locale, {
			username,
			verifyUrl,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'verification');
	}

	async sendIpAuthorizationEmail(
		email: string,
		username: string,
		authorizationToken: string,
		ipAddress: string,
		location: string,
		locale: string | null = null,
	): Promise<boolean> {
		const authUrl = `${this.appBaseUrl}/authorize-ip#token=${authorizationToken}`;
		const template = this.emailI18n.getTemplate('ipAuthorization', locale, {
			username,
			authUrl,
			ipAddress,
			location,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'ip authorization');
	}

	async sendAccountDisabledForSuspiciousActivityEmail(
		email: string,
		username: string,
		reason: string | null,
		locale: string | null = null,
	): Promise<boolean> {
		const forgotUrl = `${this.appBaseUrl}/forgot`;
		const template = this.emailI18n.getTemplate('accountDisabledSuspicious', locale, {
			username,
			reason,
			forgotUrl,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'account disabled suspicious');
	}

	async sendAccountTempBannedEmail(
		email: string,
		username: string,
		reason: string | null,
		durationHours: number,
		bannedUntil: Date,
		locale: string | null = null,
	): Promise<boolean> {
		const termsUrl = `${this.marketingBaseUrl}/terms`;
		const guidelinesUrl = `${this.marketingBaseUrl}/guidelines`;
		const template = this.emailI18n.getTemplate('accountTempBanned', locale, {
			username,
			reason,
			durationHours,
			bannedUntil,
			termsUrl,
			guidelinesUrl,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'account temp banned');
	}

	async sendAccountScheduledForDeletionEmail(
		email: string,
		username: string,
		reason: string | null,
		deletionDate: Date,
		locale: string | null = null,
	): Promise<boolean> {
		const termsUrl = `${this.marketingBaseUrl}/terms`;
		const guidelinesUrl = `${this.marketingBaseUrl}/guidelines`;
		const template = this.emailI18n.getTemplate('accountScheduledDeletion', locale, {
			username,
			reason,
			deletionDate,
			termsUrl,
			guidelinesUrl,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'account scheduled deletion');
	}

	async sendSelfDeletionScheduledEmail(
		email: string,
		username: string,
		deletionDate: Date,
		locale: string | null = null,
	): Promise<boolean> {
		const template = this.emailI18n.getTemplate('selfDeletionScheduled', locale, {
			username,
			deletionDate,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'self deletion');
	}

	async sendUnbanNotification(
		email: string,
		username: string,
		reason: string,
		locale: string | null = null,
	): Promise<boolean> {
		const template = this.emailI18n.getTemplate('unbanNotification', locale, {
			username,
			reason,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'unban notification');
	}

	async sendScheduledDeletionNotification(
		email: string,
		username: string,
		deletionDate: Date,
		reason: string,
		locale: string | null = null,
	): Promise<boolean> {
		const template = this.emailI18n.getTemplate('scheduledDeletionNotification', locale, {
			username,
			deletionDate,
			reason,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'scheduled deletion notification');
	}

	async sendInactivityWarningEmail(
		email: string,
		username: string,
		deletionDate: Date,
		lastActiveDate: Date,
		locale: string | null = null,
	): Promise<boolean> {
		const loginUrl = `${this.appBaseUrl}/login`;
		const template = this.emailI18n.getTemplate('inactivityWarning', locale, {
			username,
			deletionDate,
			lastActiveDate,
			loginUrl,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'inactivity warning');
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
		const template = this.emailI18n.getTemplate('harvestCompleted', locale, {
			username,
			downloadUrl,
			totalMessages,
			fileSizeMB,
			expiresAt,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'harvest completed');
	}

	async sendGiftChargebackNotification(
		email: string,
		username: string,
		locale: string | null = null,
	): Promise<boolean> {
		const template = this.emailI18n.getTemplate('giftChargebackNotification', locale, {
			username,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'gift chargeback notification');
	}

	async sendReportResolvedEmail(
		email: string,
		username: string,
		reportId: string,
		publicComment: string,
		locale: string | null = null,
	): Promise<boolean> {
		const template = this.emailI18n.getTemplate('reportResolved', locale, {
			username,
			reportId,
			publicComment,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'report resolved');
	}

	async sendDsaReportVerificationCode(
		email: string,
		code: string,
		expiresAt: Date,
		locale: string | null = null,
	): Promise<boolean> {
		const template = this.emailI18n.getTemplate('dsaReportVerification', locale, {
			code,
			expiresAt,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'dsa report verification');
	}

	async sendRegistrationApprovedEmail(email: string, username: string, locale: string | null = null): Promise<boolean> {
		const channelsUrl = `${this.appBaseUrl}/channels/@me`;
		const template = this.emailI18n.getTemplate('registrationApproved', locale, {
			username,
			channelsUrl,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'registration approved');
	}

	async sendEmailChangeOriginal(
		email: string,
		username: string,
		code: string,
		locale: string | null = null,
	): Promise<boolean> {
		const expiresAt = new Date(Date.now() + 10 * 60 * 1000);
		const template = this.emailI18n.getTemplate('emailChangeOriginal', locale, {
			username,
			code,
			expiresAt,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'email change original');
	}

	async sendEmailChangeNew(
		email: string,
		username: string,
		code: string,
		locale: string | null = null,
	): Promise<boolean> {
		const expiresAt = new Date(Date.now() + 10 * 60 * 1000);
		const template = this.emailI18n.getTemplate('emailChangeNew', locale, {
			username,
			code,
			expiresAt,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'email change new');
	}

	async sendEmailChangeRevert(
		email: string,
		username: string,
		newEmail: string,
		token: string,
		locale: string | null = null,
	): Promise<boolean> {
		const revertUrl = `${this.appBaseUrl}/wasntme#token=${token}`;
		const template = this.emailI18n.getTemplate('emailChangeRevert', locale, {
			username,
			revertUrl,
			newEmail,
		});
		return this.sendEmailWithTemplate(email, template.subject, template.body, 'email change revert');
	}

	private async sendEmail(to: string, subject: string, textBody: string): Promise<boolean> {
		if (!this.isEmailEnabled()) return false;

		const user = await this.userRepository.findByEmail(to);
		if (user?.emailBounced) {
			Logger.warn(
				{email: to, userId: user.id},
				'Refusing to send email to bounced address - email marked as hard bounced',
			);
			return false;
		}

		try {
			const msg: sgMail.MailDataRequired = {
				to,
				from: {
					email: Config.email.fromEmail,
					name: Config.email.fromName,
				},
				subject,
				text: textBody,
				trackingSettings: {
					clickTracking: {
						enable: false,
						enableText: false,
					},
				},
			};
			await sgMail.send(msg);
			Logger.debug({to}, 'Email sent successfully via SendGrid');
			return true;
		} catch (error) {
			Logger.error({error}, 'Error sending email via SendGrid');
			return false;
		}
	}
}
