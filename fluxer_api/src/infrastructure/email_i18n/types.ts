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

export type EmailTemplateKey =
	| 'passwordReset'
	| 'emailVerification'
	| 'emailChangeOriginal'
	| 'emailChangeNew'
	| 'emailChangeRevert'
	| 'ipAuthorization'
	| 'accountDisabledSuspicious'
	| 'accountTempBanned'
	| 'accountScheduledDeletion'
	| 'selfDeletionScheduled'
	| 'inactivityWarning'
	| 'harvestCompleted'
	| 'unbanNotification'
	| 'scheduledDeletionNotification'
	| 'giftChargebackNotification'
	| 'reportResolved'
	| 'dsaReportVerification'
	| 'registrationApproved';

export interface EmailTemplateVariables {
	passwordReset: {
		username: string;
		resetUrl: string;
	};
	emailVerification: {
		username: string;
		verifyUrl: string;
	};
	emailChangeOriginal: {
		username: string;
		code: string;
		expiresAt: Date;
	};
	emailChangeNew: {
		username: string;
		code: string;
		expiresAt: Date;
	};
	emailChangeRevert: {
		username: string;
		newEmail: string;
		revertUrl: string;
	};
	ipAuthorization: {
		username: string;
		authUrl: string;
		ipAddress: string;
		location: string;
	};
	accountDisabledSuspicious: {
		username: string;
		reason: string | null;
		forgotUrl: string;
	};
	accountTempBanned: {
		username: string;
		reason: string | null;
		durationHours: number;
		bannedUntil: Date;
		termsUrl: string;
		guidelinesUrl: string;
	};
	accountScheduledDeletion: {
		username: string;
		reason: string | null;
		deletionDate: Date;
		termsUrl: string;
		guidelinesUrl: string;
	};
	selfDeletionScheduled: {
		username: string;
		deletionDate: Date;
	};
	inactivityWarning: {
		username: string;
		deletionDate: Date;
		lastActiveDate: Date;
		loginUrl: string;
	};
	harvestCompleted: {
		username: string;
		downloadUrl: string;
		totalMessages: number;
		fileSizeMB: number;
		expiresAt: Date;
	};
	unbanNotification: {
		username: string;
		reason: string;
	};
	scheduledDeletionNotification: {
		username: string;
		deletionDate: Date;
		reason: string;
	};
	giftChargebackNotification: {
		username: string;
	};
	reportResolved: {
		username: string;
		reportId: string;
		publicComment: string;
	};
	dsaReportVerification: {
		code: string;
		expiresAt: Date;
	};
	registrationApproved: {
		username: string;
		channelsUrl: string;
	};
}

export interface EmailTemplate {
	subject: string;
	body: string;
}

export type EmailTranslations = Partial<Record<EmailTemplateKey, EmailTemplate>>;
