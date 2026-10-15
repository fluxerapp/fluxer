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
		deletion_date: 'Dec 1, 2026',
		support_email: 'support@fluxer.com',
	},
	account_deletion_scheduled_requested: {
		username: 'testuser',
		deletion_date: 'Dec 14, 2026',
		safety_email: 'safety@fluxer.com',
	},
	donation_confirmation: {amount: '$5.00', currency: 'USD', interval: 'month', manageUrl: 'https://example.com/m'},
	donation_magic_link: {manageUrl: 'https://example.com/m', expiresAt: DATE},
	dsa_report_resolved: {
		reportId: '1',
		publicComment: 'Thanks',
		hasComment: 'yes',
		outcome: 'removed',
		appeals_email: 'appeals@fluxer.com',
	},
	dsa_report_verification: {code: '123456', expiresAt: DATE},
	email_change_new: {username: 'testuser', code: '123456', expiresAt: DATE},
	email_change_original: {username: 'testuser', code: '123456', expiresAt: DATE},
	email_change_revert: {username: 'testuser', newEmail: 'new@example.com', revertUrl: 'https://example.com/r'},
	email_verification: {username: 'testuser', verifyUrl: 'https://example.com/verify'},
	gift_chargeback_notification: {username: 'testuser', support_email: 'support@fluxer.com'},
	enforcement_ended: {
		username: 'testuser',
		outcome: 'reversed',
		decision_id: '123',
		restores_access: 'yes',
		statement: 'We checked again',
		content_removed: 'no',
	},
	enforcement_notice: {
		username: 'testuser',
		kind_label: 'suspension',
		neutral: 'no',
		summary: 'We suspended your account.',
		effects: 'You cannot sign in.',
		rule_title: 'Spam',
		rule_url: 'https://example.com/guidelines',
		facts: 'We found spam.',
		statement: null,
		how_we_decided: 'A person decided.',
		appeals_email: 'appeals@fluxer.com',
		appeal_deadline: 'Dec 1, 2026',
		redress: null,
		decision_id: '123',
	},
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
		outcome: 'restricted',
		appeals_email: 'appeals@fluxer.com',
	},
	self_deletion_scheduled: {username: 'testuser', deletionDate: DATE},
};

describe('EmailI18n locale files', () => {
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
	it.each(['en-US', ...LOCALES])('%s gives every reporter outcome its own wording', (locale) => {
		for (const key of ['report_resolved', 'dsa_report_resolved'] as const) {
			const bodies = (['removed', 'restricted', 'action_taken', 'no_violation'] as const).map((outcome) => {
				const result = getEmailTemplate(key, locale, {...FIXTURE[key], outcome}, 'Fluxer');
				return result.ok ? result.value.body : null;
			});
			expect(bodies, key).not.toContain(null);
			expect(new Set(bodies).size, key).toBe(bodies.length);
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
