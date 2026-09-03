// SPDX-License-Identifier: AGPL-3.0-or-later

export interface EmailTemplateVariables {
	account_disabled_suspicious: {
		username: string;
		reason: string | null;
		forgotUrl: string;
	};
	account_scheduled_deletion: {
		username: string;
		reason: string | null;
		deletionDate: Date;
		termsUrl: string;
		guidelinesUrl: string;
	};
	account_temp_banned: {
		username: string;
		reason: string | null;
		durationHours: number;
		bannedUntil: Date;
		termsUrl: string;
		guidelinesUrl: string;
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
	};
	harvest_completed: {
		username: string;
		downloadUrl: string;
		totalMessages: number;
		fileSizeMB: number;
		expiresAt: Date;
	};
	inactivity_warning: {
		username: string;
		deletionDate: Date;
		lastActiveDate: Date;
		loginUrl: string;
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
	report_resolved: {
		username: string;
		reportId: string;
		publicComment: string;
		hasComment: 'yes' | 'no';
	};
	scheduled_deletion_notification: {
		username: string;
		deletionDate: Date;
		reason: string;
	};
	self_deletion_scheduled: {
		username: string;
		deletionDate: Date;
	};
	unban_notification: {
		username: string;
		reason: string;
	};
}
