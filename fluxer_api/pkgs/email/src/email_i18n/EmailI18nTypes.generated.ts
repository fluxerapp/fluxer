// SPDX-License-Identifier: AGPL-3.0-or-later

export type EmailTemplateKey =
	| 'account_deletion_cancelled'
	| 'account_deletion_scheduled_inactivity'
	| 'account_deletion_scheduled_requested'
	| 'donation_confirmation'
	| 'donation_magic_link'
	| 'dsa_report_resolved'
	| 'dsa_report_verification'
	| 'email_change_new'
	| 'email_change_original'
	| 'email_change_revert'
	| 'email_verification'
	| 'enforcement_ended'
	| 'enforcement_notice'
	| 'gift_chargeback_notification'
	| 'harvest_completed'
	| 'inactivity_warning'
	| 'ip_authorization'
	| 'mfa_backup_codes_view'
	| 'password_change_verification'
	| 'password_reset'
	| 'report_received'
	| 'report_resolved'
	| 'self_deletion_scheduled';

export interface EmailTemplate {
	subject: string;
	body: string;
}
