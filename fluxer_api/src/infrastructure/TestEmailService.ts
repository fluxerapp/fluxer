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

import type {ITestEmailService, SentEmailRecord} from '~/infrastructure/IEmailService';
import {Logger} from '~/Logger';

export class TestEmailService implements ITestEmailService {
	private sentEmails: Array<SentEmailRecord> = [];

	listSentEmails(): Array<SentEmailRecord> {
		return [...this.sentEmails];
	}

	clearSentEmails(): void {
		this.sentEmails = [];
	}

	private recordEmail(params: {to: string; subject: string; type: string; metadata?: Record<string, string>}): boolean {
		const {to, subject, type, metadata} = params;
		this.sentEmails.push({
			to,
			subject,
			type,
			timestamp: new Date(),
			metadata: metadata ?? {},
		});
		return true;
	}

	async sendPasswordResetEmail(
		email: string,
		username: string,
		resetToken: string,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] Password reset email sent to ${email} for user ${username}`);
		Logger.info(`[TestEmailService] Reset token: ${resetToken}`);
		return this.recordEmail({
			to: email,
			subject: 'Reset your Fluxer password',
			type: 'password_reset',
			metadata: {token: resetToken},
		});
	}

	async sendEmailVerification(
		email: string,
		username: string,
		verificationToken: string,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] Email verification sent to ${email} for user ${username}`);
		Logger.info(`[TestEmailService] Verification token: ${verificationToken}`);
		return this.recordEmail({
			to: email,
			subject: 'Verify your Fluxer email address',
			type: 'email_verification',
			metadata: {token: verificationToken},
		});
	}

	async sendIpAuthorizationEmail(
		email: string,
		username: string,
		authorizationToken: string,
		ipAddress: string,
		location: string,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] IP authorization email sent to ${email} for user ${username}`);
		Logger.info(`[TestEmailService] IP: ${ipAddress}, Location: ${location}`);
		Logger.info(`[TestEmailService] Authorization token: ${authorizationToken}`);
		return this.recordEmail({
			to: email,
			subject: 'Authorize login from new IP address',
			type: 'ip_authorization',
			metadata: {token: authorizationToken, ip: ipAddress, location},
		});
	}

	async sendAccountDisabledForSuspiciousActivityEmail(
		email: string,
		username: string,
		reason: string | null,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] Account disabled email sent to ${email} for user ${username}`);
		Logger.info(`[TestEmailService] Reason: ${reason ?? 'Not specified'}`);
		return this.recordEmail({
			to: email,
			subject: 'Your Fluxer account has been temporarily disabled',
			type: 'account_disabled_suspicious',
			metadata: {reason: reason ?? ''},
		});
	}

	async sendAccountTempBannedEmail(
		email: string,
		username: string,
		reason: string | null,
		durationHours: number,
		bannedUntil: Date,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] Temp ban email sent to ${email} for user ${username}`);
		Logger.info(`[TestEmailService] Duration: ${durationHours} hours, until ${bannedUntil.toISOString()}`);
		Logger.info(`[TestEmailService] Reason: ${reason ?? 'Not specified'}`);
		return this.recordEmail({
			to: email,
			subject: 'Your Fluxer account has been temporarily suspended',
			type: 'account_temp_banned',
			metadata: {
				reason: reason ?? '',
				duration_hours: durationHours.toString(10),
				banned_until: bannedUntil.toISOString(),
			},
		});
	}

	async sendAccountScheduledForDeletionEmail(
		email: string,
		username: string,
		reason: string | null,
		deletionDate: Date,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] Scheduled deletion email sent to ${email} for user ${username}`);
		Logger.info(`[TestEmailService] Deletion date: ${deletionDate.toISOString()}`);
		Logger.info(`[TestEmailService] Reason: ${reason ?? 'Not specified'}`);
		return this.recordEmail({
			to: email,
			subject: 'Your Fluxer account is scheduled for deletion',
			type: 'account_scheduled_deletion',
			metadata: {
				reason: reason ?? '',
				deletion_date: deletionDate.toISOString(),
			},
		});
	}

	async sendSelfDeletionScheduledEmail(
		email: string,
		username: string,
		deletionDate: Date,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] Self deletion email sent to ${email} for user ${username}`);
		Logger.info(`[TestEmailService] Deletion date: ${deletionDate.toISOString()}`);
		return this.recordEmail({
			to: email,
			subject: 'Your Fluxer account deletion has been scheduled',
			type: 'self_deletion_scheduled',
			metadata: {deletion_date: deletionDate.toISOString()},
		});
	}

	async sendUnbanNotification(
		email: string,
		username: string,
		reason: string,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] Unban notification sent to ${email} for user ${username}`);
		Logger.info(`[TestEmailService] Reason: ${reason}`);
		return this.recordEmail({
			to: email,
			subject: 'Your Fluxer account suspension has been lifted',
			type: 'unban_notification',
			metadata: {reason},
		});
	}

	async sendScheduledDeletionNotification(
		email: string,
		username: string,
		deletionDate: Date,
		reason: string,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] Scheduled deletion notification sent to ${email} for user ${username}`);
		Logger.info(`[TestEmailService] Deletion date: ${deletionDate.toISOString()}`);
		Logger.info(`[TestEmailService] Reason: ${reason}`);
		return this.recordEmail({
			to: email,
			subject: 'Your Fluxer account is scheduled for deletion',
			type: 'scheduled_deletion_notification',
			metadata: {deletion_date: deletionDate.toISOString(), reason},
		});
	}

	async sendInactivityWarningEmail(
		email: string,
		username: string,
		deletionDate: Date,
		lastActiveDate: Date,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] Inactivity warning email sent to ${email} for user ${username}`);
		Logger.info(`[TestEmailService] Deletion date: ${deletionDate.toISOString()}`);
		Logger.info(`[TestEmailService] Last active: ${lastActiveDate.toISOString()}`);
		return this.recordEmail({
			to: email,
			subject: 'Your Fluxer account will be deleted due to inactivity',
			type: 'inactivity_warning',
			metadata: {
				deletion_date: deletionDate.toISOString(),
				last_active_date: lastActiveDate.toISOString(),
			},
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
		Logger.info(`[TestEmailService] Harvest completed email sent to ${email} for user ${username}`);
		Logger.info(`[TestEmailService] Total messages: ${totalMessages}, File size: ${fileSize} bytes`);
		Logger.info(`[TestEmailService] Download URL: ${downloadUrl}`);
		Logger.info(`[TestEmailService] Expires at: ${expiresAt.toISOString()}`);
		return this.recordEmail({
			to: email,
			subject: 'Your Fluxer Data Export is Ready',
			type: 'harvest_completed',
			metadata: {
				download_url: downloadUrl,
				total_messages: totalMessages.toString(10),
				file_size: fileSize.toString(10),
				expires_at: expiresAt.toISOString(),
			},
		});
	}

	async sendGiftChargebackNotification(email: string, username: string, _locale?: string | null): Promise<boolean> {
		Logger.info(`[TestEmailService] Gift chargeback notification sent to ${email} for user ${username}`);
		return this.recordEmail({
			to: email,
			subject: 'Your Fluxer Premium gift has been revoked',
			type: 'gift_chargeback_notification',
		});
	}

	async sendReportResolvedEmail(
		email: string,
		username: string,
		reportId: string,
		publicComment: string,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] Report resolved email sent to ${email} for user ${username}`);
		Logger.info(`[TestEmailService] Report ID: ${reportId}`);
		Logger.info(`[TestEmailService] Comment: ${publicComment}`);
		return this.recordEmail({
			to: email,
			subject: 'Your Fluxer report has been reviewed',
			type: 'report_resolved',
			metadata: {report_id: reportId, public_comment: publicComment},
		});
	}

	async sendDsaReportVerificationCode(
		email: string,
		code: string,
		expiresAt: Date,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] DSA report verification code sent to ${email}`);
		Logger.info(`[TestEmailService] Verification code: ${code}`);
		return this.recordEmail({
			to: email,
			subject: 'Verify your DSA report email',
			type: 'dsa_report_verification',
			metadata: {
				code,
				expires_at: expiresAt.toISOString(),
			},
		});
	}

	async sendRegistrationApprovedEmail(email: string, username: string, _locale?: string | null): Promise<boolean> {
		Logger.info(`[TestEmailService] Registration approved email sent to ${email} for user ${username}`);
		return this.recordEmail({
			to: email,
			subject: 'Your Fluxer registration has been approved',
			type: 'registration_approved',
		});
	}

	async sendEmailChangeOriginal(
		email: string,
		username: string,
		code: string,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] Email change original verification sent to ${email} for user ${username}`);
		return this.recordEmail({
			to: email,
			subject: 'Confirm your Fluxer email change',
			type: 'email_change_original',
			metadata: {code},
		});
	}

	async sendEmailChangeNew(email: string, username: string, code: string, _locale?: string | null): Promise<boolean> {
		Logger.info(`[TestEmailService] Email change new verification sent to ${email} for user ${username}`);
		return this.recordEmail({
			to: email,
			subject: 'Verify your new Fluxer email',
			type: 'email_change_new',
			metadata: {code},
		});
	}

	async sendEmailChangeRevert(
		email: string,
		username: string,
		newEmail: string,
		token: string,
		_locale?: string | null,
	): Promise<boolean> {
		Logger.info(`[TestEmailService] Email change revert notice sent to ${email} for user ${username}`);
		return this.recordEmail({
			to: email,
			subject: 'Your Fluxer email was changed',
			type: 'email_change_revert',
			metadata: {token, new_email: newEmail},
		});
	}
}
