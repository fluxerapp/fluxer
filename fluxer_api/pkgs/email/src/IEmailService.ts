// SPDX-License-Identifier: AGPL-3.0-or-later

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
	sendPasswordChangeVerification(
		email: string,
		username: string,
		code: string,
		locale?: string | null,
	): Promise<boolean>;
	sendMfaBackupCodesVerification(
		email: string,
		username: string,
		code: string,
		locale?: string | null,
	): Promise<boolean>;
	sendDonationMagicLink(
		email: string,
		token: string,
		manageUrl: string,
		expiresAt: Date,
		locale?: string | null,
	): Promise<boolean>;
	sendDonationConfirmation(
		email: string,
		amountCents: number,
		currency: string,
		interval: string,
		manageUrl: string,
		locale?: string | null,
	): Promise<boolean>;
}
