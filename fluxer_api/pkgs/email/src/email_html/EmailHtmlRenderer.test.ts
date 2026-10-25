// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmailConfig, EmailMessage, IEmailProvider} from '@pkgs/email/src/EmailProviderTypes';
import {EmailService} from '@pkgs/email/src/EmailService';
import {EMAIL_HTML_LAYOUTS} from '@pkgs/email/src/email_html/EmailHtmlLayouts.generated';
import {
	escapeHtml,
	getEmailTextDirection,
	renderEmailHtml,
	validateEmailUrl,
} from '@pkgs/email/src/email_html/EmailHtmlRenderer';
import {EMAIL_I18N_LOCALE_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nLocales';
import type {EmailTemplateVariables} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
import {describe, expect, it} from 'vitest';

const HTML_BUDGET_BYTES = 60_000;
const STYLE_BUDGET_BYTES = 16_000;
const LOCALES = ['en-US', ...Object.keys(EMAIL_I18N_LOCALE_MESSAGES)];

const CONFIG: EmailConfig = {
	enabled: true,
	fromEmail: 'noreply@example.com',
	fromName: 'Fluxer',
	appBaseUrl: 'https://web.example.com',
	apiBaseUrl: 'https://api.example.com',
	replyTo: 'support@fluxer.com',
	appealsEmail: 'appeals@fluxer.com',
	privacyEmail: 'privacy@fluxer.com',
	safetyEmail: 'safety@fluxer.com',
	supportEmail: 'support@fluxer.com',
	productName: 'Fluxer',
	marketingBaseUrl: 'https://example.com',
	staticCdnBaseUrl: 'https://static.example.com/',
	legalAddress: 'Fluxer Platform AB, c/o Embassy House, Östgötagatan 12, 116 25 Stockholm, Sweden',
	brandLogo: true,
};

const TOKEN = 'tok.abc';
const CODE = 'K7QD-M2XP';
const DATE = new Date('2026-11-01T12:00:00Z');
const USER = 'user@example.com';
const DOWNLOAD_URL = 'https://exports.example.com/harvest.zip?signature=a&expires=b';
const MANAGE_URL = 'https://example.com/donate/manage#token=abc';
const REPORT_ID = '1290874512349872128';
const REPORT_REFERENCE = {
	target: 'user',
	reportedAt: new Date('2026-10-24T18:12:00Z'),
	username: 'example',
	communityName: '',
	channelName: '',
} as const;
const DECISION_ID = '1290874512349872640';
const RULE_URL = 'https://example.com/guidelines#6-spam-and-service-abuse';
const ENFORCEMENT_NOTICE: EmailTemplateVariables['enforcement_notice'] = {
	kind_label: 'suspension',
	neutral: 'no',
	summary: 'We suspended your account until Nov 8, 2026, 12:00 PM UTC.',
	effects: "You can't sign in or use Fluxer anywhere until then.",
	rule_title: 'Spam',
	rule_url: RULE_URL,
	facts: 'We found that your account sent unwanted messages in bulk.\n2 messages in #general of Plant Club.',
	statement: 'You posted the same link in ten communities.',
	how_we_decided: 'We found this in our own review.\nA person on our team reviewed this and made the decision.',
	appeals_email: 'appeals@fluxer.com',
	appeal_deadline: 'Dec 31, 2026',
	redress: 'You can also complain to the Digital Services Coordinator in your EU country, or go to court.',
	decision_id: DECISION_ID,
};

type Send = (service: EmailService, locale: string) => Promise<boolean>;

const EMAILS: Array<[string, string | null, Send]> = [
	[
		'password_reset',
		`https://web.example.com/reset#token=${TOKEN}`,
		(service, locale) => service.send('password_reset', USER, {resetToken: TOKEN}, locale),
	],
	[
		'email_verification',
		`https://web.example.com/verify#token=${TOKEN}`,
		(service, locale) => service.send('email_verification', USER, {verificationToken: TOKEN}, locale),
	],
	[
		'ip_authorization',
		`https://web.example.com/authorize-ip#token=${TOKEN}`,
		(service, locale) =>
			service.send(
				'ip_authorization',
				USER,
				{authorizationToken: TOKEN, ipAddress: '203.0.113.42', location: 'Stockholm'},
				locale,
			),
	],
	[
		'email_change_revert',
		`https://web.example.com/wasntme#token=${TOKEN}`,
		(service, locale) => service.send('email_change_revert', USER, {newEmail: 'new@example.com', token: TOKEN}, locale),
	],
	[
		'email_change_original',
		null,
		(service, locale) => service.send('email_change_original', USER, {code: CODE}, locale),
	],
	['email_change_new', null, (service, locale) => service.send('email_change_new', USER, {code: CODE}, locale)],
	[
		'sign_in_code',
		null,
		(service, locale) =>
			service.send(
				'sign_in_code',
				USER,
				{code: CODE, expiresAt: DATE, requester: {location: 'Stockholm, Sweden', device: 'Firefox on Linux'}},
				locale,
			),
	],
	[
		'sign_up_code',
		null,
		(service, locale) =>
			service.send(
				'sign_up_code',
				USER,
				{code: CODE, expiresAt: DATE, requester: {location: 'Stockholm, Sweden', device: 'Firefox on Linux'}},
				locale,
			),
	],
	[
		'email_verification_code',
		null,
		(service, locale) => service.send('email_verification_code', USER, {code: CODE, expiresAt: DATE}, locale),
	],
	['reauth_code', null, (service, locale) => service.send('reauth_code', USER, {code: CODE, expiresAt: DATE}, locale)],
	[
		'password_change_verification',
		null,
		(service, locale) => service.send('password_change_verification', USER, {code: CODE}, locale),
	],
	[
		'mfa_backup_codes_view',
		null,
		(service, locale) => service.send('mfa_backup_codes_view', USER, {code: CODE}, locale),
	],
	[
		'dsa_report_verification',
		null,
		(service, locale) => service.send('dsa_report_verification', USER, {code: CODE, expiresAt: DATE}, locale),
	],
	[
		'donation_magic_link',
		MANAGE_URL,
		(service, locale) => service.send('donation_magic_link', USER, {manageUrl: MANAGE_URL, expiresAt: DATE}, locale),
	],
	[
		'registration_approved',
		'https://web.example.com/login',
		(service, locale) => service.send('registration_approved', USER, {}, locale),
	],
	[
		'registration_pending',
		'https://admin.example.com/pending-registrations',
		(service, locale) =>
			service.send(
				'registration_pending',
				USER,
				{reviewUrl: 'https://admin.example.com/pending-registrations'},
				locale,
			),
	],
	[
		'bulk_message_deletion_completed',
		null,
		(service, locale) =>
			service.send('bulk_message_deletion_completed', USER, {message_count: 1240, channel_count: 18}, locale),
	],
	[
		'donation_confirmation',
		MANAGE_URL,
		(service, locale) =>
			service.send(
				'donation_confirmation',
				USER,
				{amountCents: 1000, currency: 'eur', interval: 'month', manageUrl: MANAGE_URL},
				locale,
			),
	],
	[
		'harvest_completed',
		DOWNLOAD_URL,
		(service, locale) =>
			service.send(
				'harvest_completed',
				USER,
				{downloadUrl: DOWNLOAD_URL, totalMessages: 48213, fileSize: 734003200, expiresAt: DATE},
				locale,
			),
	],
	[
		'inactivity_warning',
		'https://web.example.com/login',
		(service, locale) => service.send('inactivity_warning', USER, {deletionDate: DATE, lastActiveDate: DATE}, locale),
	],
	[
		'self_deletion_scheduled',
		null,
		(service, locale) => service.send('self_deletion_scheduled', USER, {deletionDate: DATE}, locale),
	],
	[
		'account_deletion_cancelled',
		null,
		(service, locale) => service.send('account_deletion_cancelled', USER, {}, locale),
	],
	[
		'account_deletion_scheduled_requested',
		null,
		(service, locale) =>
			service.send('account_deletion_scheduled_requested', USER, {deletionDate: 'Nov 1, 2026'}, locale),
	],
	[
		'account_deletion_scheduled_inactivity',
		null,
		(service, locale) =>
			service.send('account_deletion_scheduled_inactivity', USER, {deletionDate: 'Nov 1, 2026'}, locale),
	],
	[
		'enforcement_notice',
		null,
		(service, locale) => service.send('enforcement_notice', USER, ENFORCEMENT_NOTICE, locale),
	],
	[
		'enforcement_ended',
		null,
		(service, locale) =>
			service.send(
				'enforcement_ended',
				USER,
				{
					outcome: 'reversed',
					decision_id: DECISION_ID,
					restores_access: 'yes',
					statement: 'The messages were quoted in a report.',
					content_removed: 'yes',
				},
				locale,
			),
	],
	[
		'report_resolved',
		null,
		(service, locale) =>
			service.send(
				'report_resolved',
				USER,
				{reportId: REPORT_ID, publicComment: 'Done', outcome: 'removed', reference: REPORT_REFERENCE},
				locale,
			),
	],
	[
		'report_received',
		null,
		(service, locale) => service.send('report_received', USER, {reportId: REPORT_ID, targetKind: 'message'}, locale),
	],
	[
		'dsa_report_resolved',
		null,
		(service, locale) =>
			service.send(
				'dsa_report_resolved',
				USER,
				{reportId: REPORT_ID, publicComment: 'Done', outcome: 'action_taken', reference: REPORT_REFERENCE},
				locale,
			),
	],
	[
		'gift_chargeback_notification',
		null,
		(service, locale) => service.send('gift_chargeback_notification', USER, {}, locale),
	],
	[
		'payment_dispute_warning',
		null,
		(service, locale) =>
			service.send(
				'payment_dispute_warning',
				USER,
				{
					product: 'plutonium',
					invoiceNumber: 'A1B2C3D4-0007',
					orderDate: DATE,
					amountMinor: 9900,
					currency: 'eur',
					warningDate: DATE,
					note: 'The card holder confirmed the purchase in chat.',
				},
				locale,
			),
	],
];

function captureService(): {service: EmailService; sent: Array<EmailMessage>} {
	const sent: Array<EmailMessage> = [];
	const provider: IEmailProvider = {
		sendEmail: async (message) => {
			sent.push(message);
			return true;
		},
	};
	return {service: new EmailService(CONFIG, provider), sent};
}

function emptySlots(): Record<string, string> {
	const slots: Record<string, string> = {};
	for (const slot of EMAIL_HTML_LAYOUTS.message.slots) {
		slots[slot] = slot.endsWith('_url') ? 'https://example.com/' : slot;
	}
	slots.lang = 'en-US';
	return slots;
}

describe('email HTML renderer', () => {
	it('escapes every HTML special character', () => {
		expect(escapeHtml(`<a href="x">'&'</a>`)).toBe('&lt;a href=&quot;x&quot;&gt;&#39;&amp;&#39;&lt;/a&gt;');
	});

	it.each(['https://example.com/a?b=1&c=2', 'http://localhost:8088/pass', 'http://127.0.0.1/x'])(
		'accepts %s',
		(url) => {
			expect(validateEmailUrl(url)).toBe(url);
		},
	);

	it.each([
		'javascript:alert(1)',
		'http://example.com/',
		'data:text/html,hi',
		'//example.com/x',
		'/relative',
		'https://user:pass@example.com/',
		'https://example.com/a b',
		'https://example.com/\n',
		'https://example.com/\u0000',
	])('rejects %s', (url) => {
		expect(() => validateEmailUrl(url)).toThrow();
	});

	it('accepts plain http only on the instance origins it is given', () => {
		const origins = new Set(['http://chat.example.org', 'http://static.example.org:8080']);
		expect(validateEmailUrl('http://chat.example.org/reset#token=a', origins)).toBe(
			'http://chat.example.org/reset#token=a',
		);
		expect(validateEmailUrl('http://static.example.org:8080/email/x.png', origins)).toBe(
			'http://static.example.org:8080/email/x.png',
		);
		expect(() => validateEmailUrl('http://chat.example.org:8081/reset', origins)).toThrow();
		expect(() => validateEmailUrl('http://other.example.org/reset', origins)).toThrow();
		expect(() => validateEmailUrl('http://chat.example.org/reset')).toThrow();
	});

	it('refuses a missing slot', () => {
		const slots = emptySlots();
		delete slots.intro;
		expect(() => renderEmailHtml('ltr', slots)).toThrow(/intro/);
	});

	it('escapes text slots and keeps the layout markup intact', () => {
		const html = renderEmailHtml('ltr', {...emptySlots(), intro: '<img src=x onerror=alert(1)>'});
		expect(html).toContain('&lt;img src=x onerror=alert(1)&gt;');
		expect(html).not.toContain('<img src=x');
		expect(html).not.toMatch(/%%|\[\[/);
	});

	it('keeps its <style> blocks inside the Gmail limit', () => {
		for (const direction of ['ltr', 'rtl'] as const) {
			for (const match of EMAIL_HTML_LAYOUTS.message[direction].matchAll(/<style[^>]*>([\s\S]*?)<\/style>/g)) {
				expect(Buffer.byteLength(match[1], 'utf8')).toBeLessThan(STYLE_BUDGET_BYTES);
				expect(match[1]).not.toContain('/*');
			}
		}
	});
});

function runningText(html: string): string {
	return html
		.replace(/<head>[\s\S]*?<\/head>/, '')
		.replace(/<td[^>]*x-fact[^>]*>[\s\S]*?<\/td>/g, '')
		.replace(/<a [^>]*>[\s\S]*?<\/a>/g, '')
		.replace(/<!--[\s\S]*?-->/g, '');
}

describe.each(EMAILS)('%s email', (_name, ctaUrl, send) => {
	it.each(LOCALES)('%s renders a complete HTML part within the size budget', async (locale) => {
		const {service, sent} = captureService();
		await expect(send(service, locale)).resolves.toBe(true);
		const message = sent[0];
		const html = message.html ?? '';
		const direction = getEmailTextDirection(locale);
		expect(Buffer.byteLength(html, 'utf8')).toBeLessThan(HTML_BUDGET_BYTES);
		expect(html.startsWith('<!doctype html>')).toBe(true);
		const running = runningText(html);
		expect(running, 'bare line break').not.toMatch(/<br\s*\/?>|\n(?![\s<])/);
		const words = running.replace(/<[^>]+>/g, ' ');
		expect(words, 'bare URL').not.toMatch(/https?:\/\/|www\./);
		expect(words, 'bare email address').not.toMatch(/[\w.+-]+@[\w-]+\.[\w.]+/);
		const factWords = (html.match(/<td[^>]*x-fact[^>]*>[\s\S]*?<\/td>/g) ?? [])
			.join(' ')
			.replace(/<a [^>]*>[\s\S]*?<\/a>/g, '')
			.replace(/<[^>]+>/g, ' ');
		expect(factWords, 'bare email address in a fact row').not.toMatch(/[\w.+-]+@[\w-]+\.[\w.]+/);
		expect(message.text, 'URL in parentheses').not.toMatch(/\(https?:\/\//);
		expect(message.text, 'raw markup').not.toMatch(/<\/?(mail|settings|dsa|arn)>|\]\(https?:/);
		expect(html).toContain(`<html lang="${locale}" dir="${direction}"`);
		expect(html).toContain(`role="article" lang="${locale}" dir="${direction}"`);
		expect(html).toContain(`<title>${escapeHtml(message.subject)}</title>`);
		expect(html).not.toMatch(/%%|\[\[/);
		expect(html).not.toMatch(/<h1[\s>]/);
		expect(message.text).not.toMatch(/%%|\{[A-Za-z_]+[,}]/);
		if (ctaUrl) {
			expect(html).toContain(`href="${escapeHtml(ctaUrl)}"`);
			expect(html).toContain('mso-padding-alt:12px 22px');
			expect(message.text).toContain(ctaUrl);
		} else {
			expect(html).not.toContain('mso-padding-alt');
		}
		for (const image of html.match(/<img [^>]*>/g) ?? []) {
			expect(image).toMatch(/ alt="[^"]+"/);
			expect(image).toMatch(/ width="\d+"/);
			expect(image).toMatch(/ height="\d+"/);
			expect(image).toMatch(/src="https:\/\/static\.example\.com\/email\/brand\/v1\/logo-(color|white)\.png"/);
		}
		if (direction === 'rtl') {
			expect(html).toContain('text-align:right;direction:rtl');
			expect(html).not.toMatch(/<td[^>]*text-align:left;direction:ltr/);
		}
	});
});
