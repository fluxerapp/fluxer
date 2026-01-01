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

export interface SentEmailRecord {
	to: string;
	subject: string;
	type: string;
	timestamp: Date;
	metadata: Record<string, string>;
}

export interface IEmailService {
	sendPasswordResetEmail(email: string, username: string, resetToken: string, locale?: string | null): Promise<boolean>;

	sendEmailVerification(
		email: string,
		username: string,
		verificationToken: string,
		locale?: string | null,
	): Promise<boolean>;

	sendIpAuthorizationEmail(
		email: string,
		username: string,
		authorizationToken: string,
		ipAddress: string,
		location: string,
		locale?: string | null,
	): Promise<boolean>;

	sendAccountDisabledForSuspiciousActivityEmail(
		email: string,
		username: string,
		reason: string | null,
		locale?: string | null,
	): Promise<boolean>;

	sendAccountTempBannedEmail(
		email: string,
		username: string,
		reason: string | null,
		durationHours: number,
		bannedUntil: Date,
		locale?: string | null,
	): Promise<boolean>;

	sendAccountScheduledForDeletionEmail(
		email: string,
		username: string,
		reason: string | null,
		deletionDate: Date,
		locale?: string | null,
	): Promise<boolean>;

	sendSelfDeletionScheduledEmail(
		email: string,
		username: string,
		deletionDate: Date,
		locale?: string | null,
	): Promise<boolean>;

	sendUnbanNotification(email: string, username: string, reason: string, locale?: string | null): Promise<boolean>;

	sendScheduledDeletionNotification(
		email: string,
		username: string,
		deletionDate: Date,
		reason: string,
		locale?: string | null,
	): Promise<boolean>;

	sendInactivityWarningEmail(
		email: string,
		username: string,
		deletionDate: Date,
		lastActiveDate: Date,
		locale?: string | null,
	): Promise<boolean>;

	sendHarvestCompletedEmail(
		email: string,
		username: string,
		downloadUrl: string,
		totalMessages: number,
		fileSize: number,
		expiresAt: Date,
		locale?: string | null,
	): Promise<boolean>;

	sendGiftChargebackNotification(email: string, username: string, locale?: string | null): Promise<boolean>;

	sendReportResolvedEmail(
		email: string,
		username: string,
		reportId: string,
		publicComment: string,
		locale?: string | null,
	): Promise<boolean>;

	sendDsaReportVerificationCode(email: string, code: string, expiresAt: Date, locale?: string | null): Promise<boolean>;

	sendRegistrationApprovedEmail(email: string, username: string, locale?: string | null): Promise<boolean>;

	sendEmailChangeOriginal(email: string, username: string, code: string, locale?: string | null): Promise<boolean>;

	sendEmailChangeNew(email: string, username: string, code: string, locale?: string | null): Promise<boolean>;

	sendEmailChangeRevert(
		email: string,
		username: string,
		newEmail: string,
		token: string,
		locale?: string | null,
	): Promise<boolean>;
}

export interface ITestEmailService extends IEmailService {
	listSentEmails(): Array<SentEmailRecord>;
	clearSentEmails(): void;
}
