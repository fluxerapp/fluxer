// SPDX-License-Identifier: AGPL-3.0-or-later

import {extractMessageTemplatePlaceholders} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import {EmailI18nService} from '@pkgs/email/src/EmailI18nService';
import type {EmailMessage, IEmailProvider} from '@pkgs/email/src/EmailProviderTypes';
import {EmailService} from '@pkgs/email/src/EmailService';
import {getEmailTemplate, resetEmailI18n} from '@pkgs/email/src/email_i18n/EmailI18n';
import {EMAIL_I18N_LOCALE_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nLocales';
import {EMAIL_I18N_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nMessages';
import type {EmailTemplateVariables} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
import type {EmailTemplateKey} from '@pkgs/email/src/email_i18n/EmailI18nTypes.generated';
import {afterEach, describe, expect, it} from 'vitest';

const LOCALES = Object.keys(EMAIL_I18N_LOCALE_MESSAGES) as Array<keyof typeof EMAIL_I18N_LOCALE_MESSAGES>;
const TEMPLATE_KEYS = Object.keys(EMAIL_I18N_MESSAGES) as Array<EmailTemplateKey>;
const DATE = new Date('2026-10-01T23:30:00Z');

const FIXTURE: {[K in EmailTemplateKey]: EmailTemplateVariables[K]} = {
	account_deletion_cancelled: {username: 'testuser'},
	account_deletion_scheduled_inactivity: {username: 'testuser', reason: 'Inactive', deletionDate: DATE},
	account_deletion_scheduled_requested: {username: 'testuser', reason: 'Requested', deletionDate: DATE},
	account_disabled_suspicious: {username: 'testuser', reason: 'Spam', forgotUrl: 'https://example.com/forgot'},
	account_scheduled_deletion: {
		username: 'testuser',
		reason: 'Spam',
		deletionDate: DATE,
		termsUrl: 'https://example.com/terms',
		guidelinesUrl: 'https://example.com/guidelines',
	},
	account_temp_banned: {
		username: 'testuser',
		reason: 'Spam',
		durationHours: 24,
		bannedUntil: DATE,
		termsUrl: 'https://example.com/terms',
		guidelinesUrl: 'https://example.com/guidelines',
	},
	donation_confirmation: {amount: '$5.00', currency: 'USD', interval: 'month', manageUrl: 'https://example.com/m'},
	donation_magic_link: {manageUrl: 'https://example.com/m', expiresAt: DATE},
	dsa_report_verification: {code: '123456', expiresAt: DATE},
	email_change_new: {username: 'testuser', code: '123456', expiresAt: DATE},
	email_change_original: {username: 'testuser', code: '123456', expiresAt: DATE},
	email_change_revert: {username: 'testuser', newEmail: 'new@example.com', revertUrl: 'https://example.com/r'},
	email_verification: {username: 'testuser', verifyUrl: 'https://example.com/verify'},
	gift_chargeback_notification: {username: 'testuser'},
	harvest_completed: {
		username: 'testuser',
		downloadUrl: 'https://example.com/d',
		totalMessages: 1200,
		fileSizeMB: 3.5,
		expiresAt: DATE,
	},
	inactivity_warning: {
		username: 'testuser',
		deletionDate: DATE,
		lastActiveDate: DATE,
		loginUrl: 'https://example.com/login',
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
	registration_approved: {username: 'testuser', channelsUrl: 'https://example.com/channels'},
	report_resolved: {username: 'testuser', reportId: '1', publicComment: 'Thanks', hasComment: 'yes'},
	scheduled_deletion_notification: {username: 'testuser', deletionDate: DATE, reason: 'Payment fraud'},
	self_deletion_scheduled: {username: 'testuser', deletionDate: DATE},
	unban_notification: {username: 'testuser', reason: 'Appeal accepted'},
};

function renderBody<K extends EmailTemplateKey>(key: K, locale: string, variables: EmailTemplateVariables[K]): string {
	const result = getEmailTemplate(key, locale, variables);
	if (!result.ok) {
		throw new Error(result.error.message);
	}
	return result.value.body;
}

describe('EmailI18n locale files', () => {
	afterEach(() => {
		resetEmailI18n();
	});
	it.each(LOCALES)('%s loads without module errors', (locale) => {
		const template = getEmailTemplate('email_verification', locale, {
			username: 'testuser',
			verifyUrl: 'https://example.com/verify',
		});
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
			const result = getEmailTemplate(key, locale, FIXTURE[key]);
			expect(result.ok, key).toBe(true);
			if (!result.ok) continue;
			expect(result.value.body, key).not.toContain('GMT');
			expect(result.value.body, key).not.toContain('Coordinated Universal Time');
		}
	});
	it('renders dates and times in UTC with a zone label', () => {
		expect(renderBody('self_deletion_scheduled', 'en-US', {username: 'testuser', deletionDate: DATE})).toContain(
			'Thursday, October 1, 2026 at 11:30 PM UTC',
		);
	});
	it.each(['account_deletion_scheduled_requested', 'account_deletion_scheduled_inactivity'] as const)(
		'%s makes no enforcement claim',
		(key) => {
			const body = renderBody(key, 'en-US', {username: 'testuser', reason: 'Some reason', deletionDate: DATE});
			expect(body).not.toContain('Terms of Service');
			expect(body.toLowerCase()).not.toContain('appeal');
			expect(body).toContain('Reason: Some reason');
		},
	);
	it.each(['account_deletion_scheduled_requested', 'account_deletion_scheduled_inactivity'] as const)(
		'%s leaves no gap without a reason',
		(key) => {
			const body = renderBody(key, 'en-US', {username: 'testuser', reason: null, deletionDate: DATE});
			expect(body).not.toContain('Reason:');
			expect(body).not.toContain('\n\n\n');
		},
	);
	it('renders a blank reason the same as no reason', async () => {
		const sent: Array<EmailMessage> = [];
		const provider: IEmailProvider = {
			sendEmail: async (message) => {
				sent.push(message);
				return true;
			},
		};
		const service = new EmailService(
			{
				enabled: true,
				fromEmail: 'noreply@example.com',
				fromName: 'Fluxer',
				appBaseUrl: 'https://example.com',
				marketingBaseUrl: 'https://example.com',
			},
			new EmailI18nService(),
			provider,
		);
		await service.sendUnbanNotification('user@example.com', 'testuser', '  ', 'en-US');
		await service.sendUnbanNotification('user@example.com', 'testuser', null, 'en-US');
		await service.sendAccountDeletionRequestedEmail('user@example.com', 'testuser', '  ', DATE, 'en-US');
		await service.sendAccountDeletionRequestedEmail('user@example.com', 'testuser', null, DATE, 'en-US');
		expect(sent).toHaveLength(4);
		expect(sent[0].text).toBe(sent[1].text);
		expect(sent[0].text).not.toContain('Reason:');
		expect(sent[2].text).toBe(sent[3].text);
		expect(sent[2].text).not.toContain('Reason:');
	});
});
