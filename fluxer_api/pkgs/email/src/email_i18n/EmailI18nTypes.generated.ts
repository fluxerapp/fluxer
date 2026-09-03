// SPDX-License-Identifier: AGPL-3.0-or-later

export type EmailTemplateKey =
	| 'account_disabled_suspicious'
	| 'account_scheduled_deletion'
	| 'account_temp_banned'
	| 'donation_confirmation'
	| 'donation_magic_link'
	| 'dsa_report_verification'
	| 'email_change_new'
	| 'email_change_original'
	| 'email_change_revert'
	| 'email_verification'
	| 'gift_chargeback_notification'
	| 'harvest_completed'
	| 'inactivity_warning'
	| 'ip_authorization'
	| 'mfa_backup_codes_view'
	| 'password_change_verification'
	| 'password_reset'
	| 'registration_approved'
	| 'report_resolved'
	| 'scheduled_deletion_notification'
	| 'self_deletion_scheduled'
	| 'unban_notification';

export interface EmailTemplate {
	subject: string;
	body: string;
}
