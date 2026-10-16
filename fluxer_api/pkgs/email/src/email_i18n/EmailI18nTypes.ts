// SPDX-License-Identifier: AGPL-3.0-or-later

export type ReportReceivedTargetKind = 'message' | 'user' | 'guild';

export type ReporterOutcome = 'removed' | 'restricted' | 'action_taken' | 'no_violation';

export type EnforcementNoticeKindLabel =
	| 'warning'
	| 'suspension'
	| 'ban'
	| 'closure'
	| 'feature_limit'
	| 'content_removal'
	| 'profile_reset'
	| 'community_removal'
	| 'bot';

export interface EmailTemplateVariables {
	account_deletion_cancelled: {
		username: string;
		safety_email: string | null;
	};
	account_deletion_scheduled_inactivity: {
		username: string;
		deletion_date: string;
		support_email: string | null;
	};
	account_deletion_scheduled_requested: {
		username: string;
		deletion_date: string;
		safety_email: string | null;
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
		outcome: ReporterOutcome;
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
	enforcement_ended: {
		username: string;
		outcome: 'lifted' | 'reversed';
		decision_id: string | null;
		restores_access: 'yes' | 'no';
		statement: string | null;
		content_removed: 'yes' | 'no';
	};
	enforcement_notice: {
		username: string;
		kind_label: EnforcementNoticeKindLabel;
		neutral: 'yes' | 'no';
		summary: string;
		effects: string;
		rule_title: string;
		rule_url: string | null;
		facts: string;
		statement: string | null;
		how_we_decided: string;
		appeals_email: string | null;
		appeal_deadline: string;
		redress: string | null;
		decision_id: string;
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
	report_received: {
		reportId: string;
		targetKind: ReportReceivedTargetKind;
	};
	report_resolved: {
		username: string;
		reportId: string;
		publicComment: string;
		hasComment: 'yes' | 'no';
		outcome: ReporterOutcome;
		appeals_email: string | null;
	};
	self_deletion_scheduled: {
		username: string;
		deletionDate: Date;
	};
}
