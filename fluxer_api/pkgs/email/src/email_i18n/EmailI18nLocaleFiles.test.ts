import {extractMessageTemplatePlaceholders} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';
import {EMAIL_I18N_LOCALE_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nLocales';
import {EMAIL_I18N_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nMessages';
import type {EmailTemplateVariables} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
import type {EmailTemplateKey} from '@pkgs/email/src/email_i18n/EmailI18nTypes.generated';
import {describe, expect, it} from 'vitest';

const LOCALES = Object.keys(EMAIL_I18N_LOCALE_MESSAGES) as Array<keyof typeof EMAIL_I18N_LOCALE_MESSAGES>;
const TEMPLATE_KEYS = Object.keys(EMAIL_I18N_MESSAGES) as Array<EmailTemplateKey>;
const DATE = new Date('2026-10-01T23:30:00Z');

const FIXTURE: {[K in EmailTemplateKey]: EmailTemplateVariables[K]} = {
	account_deletion_cancelled: {username: 'testuser', safety_email: 'safety@fluxer.com'},
	account_deletion_scheduled_inactivity: {
		username: 'testuser',
		reason: 'Inactive',
		deletionDate: DATE,
		safety_email: 'safety@fluxer.com',
	},
	account_deletion_scheduled_requested: {
		username: 'testuser',
		reason: 'Requested',
		deletionDate: DATE,
		safety_email: 'safety@fluxer.com',
	},
	account_scheduled_deletion: {
		username: 'testuser',
		reason: 'Spam',
		deletionDate: DATE,
		termsUrl: 'https://example.com/terms',
		guidelinesUrl: 'https://example.com/guidelines',
		legalLinks: 'both',
		appeals_email: 'appeals@fluxer.com',
	},
	account_temp_banned: {
		username: 'testuser',
		reason: 'Spam',
		durationHours: 24,
		bannedUntil: DATE,
		termsUrl: 'https://example.com/terms',
		guidelinesUrl: 'https://example.com/guidelines',
		legalLinks: 'both',
		appeals_email: 'appeals@fluxer.com',
	},
	donation_confirmation: {amount: '$5.00', currency: 'USD', interval: 'month', manageUrl: 'https://example.com/m'},
	donation_magic_link: {manageUrl: 'https://example.com/m', expiresAt: DATE},
	dsa_report_resolved: {reportId: '1', publicComment: 'Thanks', hasComment: 'yes', appeals_email: 'appeals@fluxer.com'},
	dsa_report_verification: {code: '123456', expiresAt: DATE},
	email_change_new: {username: 'testuser', code: '123456', expiresAt: DATE},
	email_change_original: {username: 'testuser', code: '123456', expiresAt: DATE},
	email_change_revert: {username: 'testuser', newEmail: 'new@example.com', revertUrl: 'https://example.com/r'},
	email_verification: {username: 'testuser', verifyUrl: 'https://example.com/verify'},
	gift_chargeback_notification: {username: 'testuser', support_email: 'support@fluxer.com'},
	harvest_completed: {
		username: 'testuser',
		downloadUrl: 'https://example.com/d',
		totalMessages: 1200,
		fileSizeMB: 3.5,
		expiresAt: DATE,
		support_email: 'support@fluxer.com',
	},
	inactivity_warning: {
		username: 'testuser',
		deletionDate: DATE,
		lastActiveDate: DATE,
		loginUrl: 'https://example.com/login',
		support_email: 'support@fluxer.com',
	},
	ip_authorization: {
		username: 'testuser',
		authUrl: 'https://example.com/a',
		ipAddress: '192.0.2.1',
		location: 'Stockholm',
	},
	mfa_backup_codes_view: {username: 'testuser', code: '123456', expiresAt: DATE},
	password_change_verification: {username: 'testuser', code: '123456', expiresAt: DATE},
	password_reset: {username: 'testuser', resetUrl: 'https://example.com/reset'},
	report_received: {reportId: '1', targetKind: 'message'},
	report_resolved: {
		username: 'testuser',
		reportId: '1',
		publicComment: 'Thanks',
		hasComment: 'yes',
		safety_email: 'safety@fluxer.com',
	},
	scheduled_deletion_notification: {
		username: 'testuser',
		deletionDate: DATE,
		reason: 'Payment fraud',
		appeals_email: 'appeals@fluxer.com',
	},
	self_deletion_scheduled: {username: 'testuser', deletionDate: DATE},
	unban_notification: {username: 'testuser', reason: 'Appeal accepted'},
};

describe('EmailI18n locale files', () => {
	it.each(LOCALES)('%s loads without module errors', (locale) => {
		const template = getEmailTemplate(
			'email_verification',
			locale,
			{username: 'testuser', verifyUrl: 'https://example.com/verify'},
			'Fluxer',
		);
		expect(template.ok).toBe(true);
	});
	it.each(LOCALES)('%s has the same translation keys as the source catalog', (locale) => {
		const messagesKeys = Object.keys(EMAIL_I18N_MESSAGES).sort();
		const localeKeys = Object.keys(EMAIL_I18N_LOCALE_MESSAGES[locale]).sort();
		expect(localeKeys).toEqual(messagesKeys);
	});
	it.each(LOCALES)('%s keeps the source placeholders in every template', (locale) => {
		const messages: Partial<Record<EmailTemplateKey, {subject: string; body: string}>> =
			EMAIL_I18N_LOCALE_MESSAGES[locale];
		for (const key of TEMPLATE_KEYS) {
			const source = EMAIL_I18N_MESSAGES[key];
			const translated = messages[key];
			expect(translated, key).toBeDefined();
			if (!translated) continue;
			expect(extractMessageTemplatePlaceholders(translated.subject), `${key}.subject`).toEqual(
				extractMessageTemplatePlaceholders(source.subject),
			);
			expect(extractMessageTemplatePlaceholders(translated.body), `${key}.body`).toEqual(
				extractMessageTemplatePlaceholders(source.body),
			);
		}
	});
	it.each(['en-US', ...LOCALES])('%s renders every template with UTC times', (locale) => {
		for (const key of TEMPLATE_KEYS) {
			const result = getEmailTemplate(key, locale, FIXTURE[key], 'Fluxer');
			expect(result.ok, key).toBe(true);
			if (!result.ok) continue;
			expect(result.value.body, key).not.toContain('GMT');
			expect(result.value.body, key).not.toContain('Coordinated Universal Time');
		}
	});
});
