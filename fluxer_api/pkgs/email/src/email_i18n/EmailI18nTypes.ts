// SPDX-License-Identifier: AGPL-3.0-or-later

export type ReportReceivedTargetKind = 'message' | 'user' | 'guild';

export type EmailLegalLinks = 'both' | 'terms' | 'guidelines' | 'none';

export interface EmailTemplateVariables {
	account_deletion_cancelled: {
		username: string;
		safety_email: string | null;
	};
	account_deletion_scheduled_inactivity: {
		username: string;
		reason: string | null;
		deletionDate: Date;
		safety_email: string | null;
	};
	account_deletion_scheduled_requested: {
		username: string;
		reason: string | null;
		deletionDate: Date;
		safety_email: string | null;
	};
	account_scheduled_deletion: {
		username: string;
		reason: string | null;
		deletionDate: Date;
		termsUrl: string | null;
		guidelinesUrl: string | null;
		legalLinks: EmailLegalLinks;
		appeals_email: string | null;
	};
	account_temp_banned: {
		username: string;
		reason: string | null;
		durationHours: number;
		bannedUntil: Date;
		termsUrl: string | null;
		guidelinesUrl: string | null;
		legalLinks: EmailLegalLinks;
		appeals_email: string | null;
	};
	donation_confirmation: {
		amount: string;
		currency: string;
		interval: string;
		manageUrl: string;
	};
	donation_magic_link: {
		manageUrl: string;
		expiresAt: Date;
	};
	dsa_report_resolved: {
		reportId: string;
		publicComment: string;
		hasComment: 'yes' | 'no';
		appeals_email: string | null;
	};
	dsa_report_verification: {
		code: string;
		expiresAt: Date;
	};
	email_change_new: {
		username: string;
		code: string;
		expiresAt: Date;
	};
	email_change_original: {
		username: string;
		code: string;
		expiresAt: Date;
	};
	email_change_revert: {
		username: string;
		newEmail: string;
		revertUrl: string;
	};
	email_verification: {
		username: string;
		verifyUrl: string;
	};
	gift_chargeback_notification: {
		username: string;
		support_email: string | null;
	};
	harvest_completed: {
		username: string;
		downloadUrl: string;
		totalMessages: number;
		fileSizeMB: number;
		expiresAt: Date;
		support_email: string | null;
	};
	inactivity_warning: {
		username: string;
		deletionDate: Date;
		lastActiveDate: Date;
		loginUrl: string;
		support_email: string | null;
	};
	ip_authorization: {
		username: string;
		authUrl: string;
		ipAddress: string;
		location: string;
	};
	mfa_backup_codes_view: {
		username: string;
		code: string;
		expiresAt: Date;
	};
	password_change_verification: {
		username: string;
		code: string;
		expiresAt: Date;
	};
	password_reset: {
		username: string;
		resetUrl: string;
	};
	registration_approved: {
		username: string;
		channelsUrl: string;
	};
	report_received: {
		reportId: string;
		targetKind: ReportReceivedTargetKind;
	};
	report_resolved: {
		username: string;
		reportId: string;
		publicComment: string;
		hasComment: 'yes' | 'no';
		safety_email: string | null;
	};
	scheduled_deletion_notification: {
		username: string;
		deletionDate: Date;
		reason: string | null;
		appeals_email: string | null;
	};
	self_deletion_scheduled: {
		username: string;
		deletionDate: Date;
	};
	unban_notification: {
		username: string;
		reason: string | null;
	};
}
