// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {extractMessageTemplatePlaceholders} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import {EmailI18nService} from '@pkgs/email/src/EmailI18nService';
import type {EmailMessage, IEmailProvider} from '@pkgs/email/src/EmailProviderTypes';
import {EmailService} from '@pkgs/email/src/EmailService';
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';
import {EMAIL_I18N_LOCALE_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nLocales';
import {EMAIL_I18N_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nMessages';
import type {EmailLegalLinks, EmailTemplateVariables} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
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

const CONTACT_KEYS = [
	'account_deletion_cancelled',
	'account_deletion_scheduled_inactivity',
	'account_deletion_scheduled_requested',
	'account_scheduled_deletion',
	'account_temp_banned',
	'dsa_report_resolved',
	'report_resolved',
	'scheduled_deletion_notification',
] as const satisfies ReadonlyArray<EmailTemplateKey>;

const SUPPORT_KEYS = [
	'gift_chargeback_notification',
	'harvest_completed',
	'inactivity_warning',
] as const satisfies ReadonlyArray<EmailTemplateKey>;

function renderBody<K extends EmailTemplateKey>(key: K, locale: string, variables: EmailTemplateVariables[K]): string {
	const result = getEmailTemplate(key, locale, variables, 'Fluxer');
	if (!result.ok) {
		throw new Error(result.error.message);
	}
	return result.value.body;
}

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
	it('renders dates and times in UTC with a zone label', () => {
		expect(renderBody('self_deletion_scheduled', 'en-US', {username: 'testuser', deletionDate: DATE})).toContain(
			'Thursday, October 1, 2026 at 11:30 PM UTC',
		);
	});
	it.each(['account_deletion_scheduled_requested', 'account_deletion_scheduled_inactivity'] as const)(
		'%s makes no enforcement claim',
		(key) => {
			const body = renderBody(key, 'en-US', {...FIXTURE[key], reason: 'Some reason'});
			expect(body).not.toContain('Terms of Service');
			expect(body.toLowerCase()).not.toContain('appeal');
			expect(body).toContain('Reason: Some reason');
		},
	);
	it.each(['account_deletion_scheduled_requested', 'account_deletion_scheduled_inactivity'] as const)(
		'%s leaves no gap without a reason',
		(key) => {
			const body = renderBody(key, 'en-US', {...FIXTURE[key], reason: null});
			expect(body).not.toContain('Reason:');
			expect(body).not.toContain('\n\n\n');
		},
	);
	it.each(['en-US', ...LOCALES])('%s names each reported target kind in the receipt', (locale) => {
		const bodies = (['message', 'user', 'guild'] as const).map((targetKind) =>
			renderBody('report_received', locale, {reportId: '1234567890', targetKind}),
		);
		expect(new Set(bodies).size).toBe(3);
		for (const body of bodies) {
			expect(body).toContain('1234567890');
			expect(body).not.toContain('targetKind');
		}
	});
	it('renders the receipt for each target kind', () => {
		expect(renderBody('report_received', 'en-US', {reportId: '1', targetKind: 'message'})).toContain(
			'report about a message on Fluxer.',
		);
		expect(renderBody('report_received', 'en-US', {reportId: '1', targetKind: 'user'})).toContain(
			'report about an account on Fluxer.',
		);
		expect(renderBody('report_received', 'en-US', {reportId: '1', targetKind: 'guild'})).toContain(
			'report about a community on Fluxer.',
		);
	});
	it.each(['en-US', ...LOCALES])('%s tells a DSA reporter how to challenge the decision', (locale) => {
		const withComment = renderBody('dsa_report_resolved', locale, {
			reportId: '1234567890',
			publicComment: 'We removed the content.',
			hasComment: 'yes',
			appeals_email: 'appeals@fluxer.com',
		});
		const withoutComment = renderBody('dsa_report_resolved', locale, {
			reportId: '1234567890',
			publicComment: '',
			hasComment: 'no',
			appeals_email: 'appeals@fluxer.com',
		});
		for (const body of [withComment, withoutComment]) {
			expect(body).toContain('1234567890');
			expect(body).toContain('appeals@fluxer.com');
			expect(body).toContain('60');
			expect(body).not.toContain('\n\n\n');
		}
		expect(withComment).toContain('We removed the content.');
		expect(withComment.split('\n\n')).toHaveLength(withoutComment.split('\n\n').length + 1);
	});
	it.each(['en-US', ...LOCALES])('%s lists only the configured policy links in enforcement notices', (locale) => {
		const terms = 'https://example.com/terms';
		const guidelines = 'https://example.com/guidelines';
		for (const key of ['account_temp_banned', 'account_scheduled_deletion'] as const) {
			const render = (termsUrl: string | null, guidelinesUrl: string | null, legalLinks: EmailLegalLinks) =>
				renderBody(key, locale, {...FIXTURE[key], termsUrl, guidelinesUrl, legalLinks});
			const both = render(terms, guidelines, 'both');
			const termsOnly = render(terms, null, 'terms');
			const guidelinesOnly = render(null, guidelines, 'guidelines');
			const none = render(null, null, 'none');
			expect(both, key).toContain(terms);
			expect(both, key).toContain(guidelines);
			expect(termsOnly, key).toContain(terms);
			expect(termsOnly, key).not.toContain(guidelines);
			expect(guidelinesOnly, key).toContain(guidelines);
			expect(guidelinesOnly, key).not.toContain(terms);
			expect(none, key).not.toContain('https://');
			const bullets = (body: string) => body.split('\n').filter((line) => line.startsWith('- ')).length;
			expect(bullets(both) - bullets(none), key).toBe(2);
			expect(bullets(termsOnly) - bullets(none), key).toBe(1);
			expect(bullets(guidelinesOnly) - bullets(none), key).toBe(1);
			for (const body of [both, termsOnly, guidelinesOnly, none]) {
				expect(body, key).not.toContain('\n\n\n');
				expect(body, key).not.toContain('legalLinks');
				expect(body, key).toContain('appeals@fluxer.com');
			}
			expect(termsOnly.split('\n'), key).toHaveLength(both.split('\n').length - 1);
			expect(guidelinesOnly.split('\n'), key).toHaveLength(both.split('\n').length - 1);
			expect(none.split('\n\n'), key).toHaveLength(both.split('\n\n').length - 1);
		}
	});
	it.each(['en-US', ...LOCALES])('%s leaves no gap in enforcement notices without a reason', (locale) => {
		for (const key of ['account_temp_banned', 'account_scheduled_deletion'] as const) {
			for (const legalLinks of ['both', 'none'] as const) {
				const withReason = renderBody(key, locale, {...FIXTURE[key], legalLinks, reason: 'Repeated spam'});
				const withoutReason = renderBody(key, locale, {...FIXTURE[key], legalLinks, reason: null});
				expect(withReason, key).toContain('Repeated spam\n\n');
				expect(withoutReason, key).not.toContain('Repeated spam');
				for (const body of [withReason, withoutReason]) {
					expect(body, key).not.toContain('\n\n\n');
					expect(body, key).not.toContain('{');
				}
				expect(withoutReason.split('\n\n'), key).toHaveLength(withReason.split('\n\n').length - 1);
				expect(withoutReason.split('\n'), key).toHaveLength(withReason.split('\n').length - 2);
			}
		}
	});
	it.each(['en-US', ...LOCALES])('%s names no mailbox when the instance has none', (locale) => {
		const english = (key: (typeof CONTACT_KEYS)[number], variables: object) =>
			renderBody(key, 'en-US', {...FIXTURE[key], ...variables} as never);
		for (const key of CONTACT_KEYS) {
			const hosted = renderBody(key, locale, FIXTURE[key] as never);
			const neutral = renderBody(key, locale, {...FIXTURE[key], appeals_email: null, safety_email: null} as never);
			expect(hosted, key).toMatch(/(appeals|safety)@fluxer\.com/);
			expect(neutral, key).not.toContain('@');
			expect(neutral, key).not.toMatch(/\bnull\b/);
			expect(neutral, key).not.toContain('{');
			expect(neutral, key).not.toContain('\n\n\n');
			expect(neutral, key).not.toBe(hosted);
			expect(neutral.split('\n'), key).toHaveLength(hosted.split('\n').length);
			if (locale !== 'en-US' && locale !== 'en-GB') {
				const neutralLine = neutral.split('\n').find((line, index) => line !== hosted.split('\n')[index]);
				const englishNeutral = english(key, {appeals_email: null, safety_email: null}).split('\n');
				expect(neutralLine, key).toBeDefined();
				expect(englishNeutral, key).not.toContain(neutralLine);
				expect(neutralLine, key).not.toContain('the administrators of this instance');
			}
		}
	});
	it.each(['en-US', ...LOCALES])('%s points at the instance administrators instead of a support team', (locale) => {
		for (const key of SUPPORT_KEYS) {
			const hosted = renderBody(key, locale, FIXTURE[key] as never);
			const neutral = renderBody(key, locale, {...FIXTURE[key], support_email: null} as never);
			const hostedLines = hosted.split('\n');
			const neutralLines = neutral.split('\n');
			expect(neutralLines, key).toHaveLength(hostedLines.length);
			const changed = neutralLines.filter((line, index) => line !== hostedLines[index]);
			expect(changed, key).toHaveLength(1);
			expect(neutral, key).not.toMatch(/\bnull\b/);
			expect(neutral, key).not.toContain('{');
			expect(neutral, key).not.toContain('@');
			expect(changed[0], key).not.toMatch(/support/i);
			if (locale !== 'en-US' && locale !== 'en-GB') {
				const englishNeutral = renderBody(key, 'en-US', {...FIXTURE[key], support_email: null} as never).split('\n');
				expect(englishNeutral, key).not.toContain(changed[0]);
				expect(changed[0], key).not.toContain('the administrators of this instance');
			}
		}
	});

	it.each(['en-US', ...LOCALES])('%s leaves no gap in deletion and unban notices without a reason', (locale) => {
		for (const key of ['scheduled_deletion_notification', 'unban_notification'] as const) {
			const withReason = renderBody(key, locale, {...FIXTURE[key], reason: 'Repeated spam'});
			const withoutReason = renderBody(key, locale, {...FIXTURE[key], reason: null});
			expect(withReason, key).toContain('Repeated spam\n\n');
			expect(withoutReason, key).not.toContain('Repeated spam');
			for (const body of [withReason, withoutReason]) {
				expect(body, key).not.toContain('\n\n\n');
			}
			expect(withoutReason.split('\n\n'), key).toHaveLength(withReason.split('\n\n').length - 1);
		}
	});

	it('greets a DSA reporter without a username', () => {
		for (const key of ['report_received', 'dsa_report_resolved'] as const) {
			const result = getEmailTemplate(key, 'en-US', FIXTURE[key], 'Fluxer');
			expect(result.ok).toBe(true);
			if (!result.ok) continue;
			expect(result.value.body.startsWith('Hello,\n\n')).toBe(true);
			expect(result.value.body).not.toContain('{');
		}
	});
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
				termsUrl: 'https://example.com/terms',
				guidelinesUrl: 'https://example.com/guidelines',
				appealsEmail: 'appeals@fluxer.com',
				safetyEmail: 'safety@fluxer.com',
				supportEmail: 'support@fluxer.com',
				productName: 'Fluxer',
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

const HOSTED_RENDER_SHA256 = {
	email_verification: {
		'en-US': 'ea69bd530e1f9cf7cd04bc88d5c0a032069b2538bc5138eeee5bcea7dd21f539',
		de: 'beb941104326bf73a523de0ccd33b78d3b531f461eea6ce1fae37a88634e69fa',
		ja: 'e928c0d57baf6c1efd055d88661f6dc27f24119b0b972cafe614fed40663efb1',
	},
	password_reset: {
		'en-US': 'b37a3491ef9126f251fd01fe2aa642891eb50aa56c23c36d0d62ccd0a9a7785d',
		de: '148b9fbb52845befb1b9f24a063dafe2d5f000abc2104410d4e943ce56191917',
		ja: 'd4198d68c126ad796954010431fe966b4df2b2fbabb9b4ddd5da0fbfd3b645a5',
	},
	ip_authorization: {
		'en-US': '073ead7d3e270c886b44db16266f6cfb4a5b8b182d279a8bf17ddea49198b415',
		de: 'd1c38f6a791a87379ca5b67e270d66e3741db314933b0b68c10bb9aab1781de1',
		ja: 'adc16933f5efda8b176d7348942efaf6b2bff16bd09f14de9a48d727a76f011f',
	},
	gift_chargeback_notification: {
		'en-US': '4ec08598e9087578ba52aa2490a1198dbfe68dd3bd496f4175b3b8ce533a01ac',
		de: 'dd5a7f572a9063e26e5effa05193185286e28b5e6e256f3d0a98b97e7e31e952',
		ja: 'c8d3a0fce144d278303f3d36c919d17bd60ab0738578f7e0f3e15e79bdb2bf19',
	},
	unban_notification: {
		'en-US': '0cca736b44b8e1839bf3a1c9c8720c915ea80f40596801c57a9809c40c35c206',
		de: '926e3762019888815da2ce7c70e94661878b9fba417bf0ffb11836abb2b74904',
		ja: 'f5546b66f61d992aff2367c7b930d7683cd7a37891a3c2a069a8de2769113bd6',
	},
} as const satisfies Partial<Record<EmailTemplateKey, Record<'en-US' | 'de' | 'ja', string>>>;

function renderEmail(key: EmailTemplateKey, locale: string, productName: string): {subject: string; body: string} {
	const result = getEmailTemplate(key, locale, FIXTURE[key], productName);
	if (!result.ok) {
		throw new Error(result.error.message);
	}
	return result.value;
}

describe('EmailI18n product name', () => {
	it('renders the hosted name byte for byte as before the name became a parameter', () => {
		for (const [key, locales] of Object.entries(HOSTED_RENDER_SHA256)) {
			for (const [locale, expected] of Object.entries(locales)) {
				const {subject, body} = renderEmail(key as EmailTemplateKey, locale, 'Fluxer');
				const actual = createHash('sha256').update(`${subject}\n${body}`).digest('hex');
				expect(actual, `${key} ${locale}`).toBe(expected);
			}
		}
	});
	it.each(['en-US', ...LOCALES])('%s names the instance and never Fluxer in every template', (locale) => {
		for (const key of TEMPLATE_KEYS) {
			const {subject, body} = renderEmail(key, locale, 'Example Chat');
			const rendered = `${subject}\n${body}`;
			expect(rendered, key).toContain('Example Chat');
			expect(rendered, key).not.toContain('Fluxer');
			expect(rendered, key).not.toContain('{product_name}');
		}
	});
	it.each(['en-US', ...LOCALES])('%s differs from the hosted render only by the name', (locale) => {
		for (const key of TEMPLATE_KEYS) {
			const hosted = renderEmail(key, locale, 'Fluxer');
			const selfHosted = renderEmail(key, locale, 'Example Chat');
			expect(selfHosted.subject.replaceAll('Example Chat', 'Fluxer'), key).toBe(hosted.subject);
			expect(selfHosted.body.replaceAll('Example Chat', 'Fluxer'), key).toBe(hosted.body);
		}
	});
	it('prints a name with message syntax characters as written', () => {
		const {subject, body} = renderEmail('password_reset', 'en-US', "Bob's {Chat} #1");
		expect(`${subject}\n${body}`).toContain("Bob's {Chat} #1");
	});
});
