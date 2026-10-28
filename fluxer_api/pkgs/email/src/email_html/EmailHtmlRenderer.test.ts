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
import type {EmailSendParams} from '@pkgs/email/src/IEmailService';
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
const INVOICE_URL = 'https://invoice.stripe.com/i/acct_1Example/live_abc';
const PAID_TOTALS = {
	totalMinor: 999,
	paidMinor: 499,
	currency: 'eur',
	tax: {amountMinor: 166, behavior: 'inclusive'},
	reverseCharge: false,
} as const;
const PREMIUM_URL = 'https://web.example.com/channels/@premium';
const CARD = {kind: 'card', brand: 'visa', last4: '4242'} as const;
const YEARLY_TOTALS = {...PAID_TOTALS, totalMinor: 4999, paidMinor: 4999, tax: null} as const;
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
	[
		'plutonium_payment_receipt',
		INVOICE_URL,
		(service, locale) =>
			service.send(
				'plutonium_payment_receipt',
				USER,
				{
					premiumName: 'Plutonium',
					plan: 'monthly',
					invoiceNumber: 'A1B2C3D4-0008',
					paidAt: DATE,
					period: {start: DATE, end: DATE},
					totals: PAID_TOTALS,
					invoice: {url: INVOICE_URL, linkUntil: DATE, pdf: null},
					purchase: {
						renewal: {interval: 'month', amountMinor: 999, tax: 'included'},
						consentAt: DATE,
						withdrawalEndsAt: DATE,
						seller: true,
					},
				},
				locale,
			),
	],
	[
		'plutonium_refund_receipt',
		INVOICE_URL,
		(service, locale) =>
			service.send(
				'plutonium_refund_receipt',
				USER,
				{
					premiumName: 'Plutonium',
					plan: 'gift_1_year',
					invoiceNumber: 'A1B2C3D4-0009',
					refundedAt: DATE,
					amountMinor: 500,
					paidMinor: 4999,
					paidAt: DATE,
					currency: 'usd',
					refundUrl: INVOICE_URL,
				},
				locale,
			),
	],
	[
		'plutonium_payment_failed',
		INVOICE_URL,
		(service, locale) =>
			service.send(
				'plutonium_payment_failed',
				USER,
				{
					premiumName: 'Plutonium',
					reason: 'action_required',
					invoiceNumber: 'A1B2C3D4-0010',
					attemptedAt: DATE,
					amountMinor: 499,
					currency: 'usd',
					invoiceUrl: INVOICE_URL,
					accessEndsAt: null,
				},
				locale,
			),
	],
	[
		'donation_receipt',
		INVOICE_URL,
		(service, locale) =>
			service.send(
				'donation_receipt',
				USER,
				{
					interval: 'year',
					invoiceNumber: 'A1B2C3D4-0011',
					paidAt: DATE,
					period: {start: DATE, end: DATE},
					totals: {...PAID_TOTALS, paidMinor: 999, tax: null},
					invoice: {url: INVOICE_URL, linkUntil: DATE, pdf: null},
					manageUrl: MANAGE_URL,
				},
				locale,
			),
	],
	[
		'donation_refund_receipt',
		null,
		(service, locale) =>
			service.send(
				'donation_refund_receipt',
				USER,
				{reference: '1234-5678', refundedAt: DATE, amountMinor: 1000, currency: 'eur', refundUrl: null},
				locale,
			),
	],
	[
		'donation_payment_failed',
		INVOICE_URL,
		(service, locale) =>
			service.send(
				'donation_payment_failed',
				USER,
				{
					reason: 'declined',
					invoiceNumber: 'A1B2C3D4-0012',
					attemptedAt: DATE,
					amountMinor: 1000,
					currency: 'eur',
					invoiceUrl: INVOICE_URL,
					manageUrl: MANAGE_URL,
				},
				locale,
			),
	],
	[
		'operator_pass_invoice',
		INVOICE_URL,
		(service, locale) =>
			service.send(
				'operator_pass_invoice',
				USER,
				{
					invoiceNumber: 'A1B2C3D4-0013',
					paidAt: DATE,
					totals: {...PAID_TOTALS, paidMinor: 999, tax: null, reverseCharge: true},
					invoice: {url: INVOICE_URL, linkUntil: DATE, pdf: null},
				},
				locale,
			),
	],
	[
		'operator_pass_refund_receipt',
		null,
		(service, locale) =>
			service.send(
				'operator_pass_refund_receipt',
				USER,
				{
					invoiceNumber: 'A1B2C3D4-0014',
					orderDate: DATE,
					refundedAt: DATE,
					amountMinor: 19900,
					currency: 'usd',
					full: false,
				},
				locale,
			),
	],
	[
		'plutonium_renewal_notice',
		PREMIUM_URL,
		(service, locale) =>
			service.send(
				'plutonium_renewal_notice',
				USER,
				{
					variant: 'gift',
					premiumName: 'Plutonium',
					plan: 'monthly',
					chargeAt: DATE,
					giftEndsAt: DATE,
					yearStartsAt: null,
					next: {...PAID_TOTALS, paidMinor: 999},
					regular: YEARLY_TOTALS,
					taxIncluded: true,
					paymentMethod: CARD,
					seller: true,
					timeZone: 'Europe/Stockholm',
				},
				locale,
			),
	],
	[
		'donation_renewal_notice',
		MANAGE_URL,
		(service, locale) =>
			service.send(
				'donation_renewal_notice',
				USER,
				{
					chargeAt: DATE,
					next: null,
					regular: YEARLY_TOTALS,
					taxIncluded: false,
					paymentMethod: {kind: 'named', name: 'SEPA Direct Debit'},
					seller: true,
					manageUrl: MANAGE_URL,
				},
				locale,
			),
	],
	[
		'plutonium_card_expiring',
		PREMIUM_URL,
		(service, locale) =>
			service.send(
				'plutonium_card_expiring',
				USER,
				{
					premiumName: 'Plutonium',
					brand: 'mastercard',
					last4: '4444',
					expMonth: 11,
					expYear: 2026,
					chargeAt: DATE,
					amount: YEARLY_TOTALS,
					taxIncluded: true,
					seller: true,
				},
				locale,
			),
	],
	[
		'donation_card_expiring',
		MANAGE_URL,
		(service, locale) =>
			service.send(
				'donation_card_expiring',
				USER,
				{
					brand: 'visa',
					last4: '4242',
					expMonth: 11,
					expYear: 2026,
					chargeAt: DATE,
					amount: YEARLY_TOTALS,
					taxIncluded: false,
					seller: false,
					manageUrl: MANAGE_URL,
				},
				locale,
			),
	],
	[
		'plutonium_cancellation_confirmation',
		PREMIUM_URL,
		(service, locale) =>
			service.send(
				'plutonium_cancellation_confirmation',
				USER,
				{
					premiumName: 'Plutonium',
					plan: 'yearly',
					receivedAt: DATE,
					endsAt: DATE,
					reference: 'sub_1Example',
					unpaid: YEARLY_TOTALS,
					seller: true,
					timeZone: 'America/New_York',
				},
				locale,
			),
	],
	[
		'donation_cancellation_confirmation',
		MANAGE_URL,
		(service, locale) =>
			service.send(
				'donation_cancellation_confirmation',
				USER,
				{
					receivedAt: DATE,
					endsAt: DATE,
					reference: 'sub_1Example',
					unpaid: null,
					seller: true,
					manageUrl: MANAGE_URL,
				},
				locale,
			),
	],
	[
		'plutonium_ended_unpaid',
		PREMIUM_URL,
		(service, locale) =>
			service.send(
				'plutonium_ended_unpaid',
				USER,
				{
					premiumName: 'Plutonium',
					invoiceNumber: 'A1B2C3D4-0015',
					invoiceStatus: 'open',
					amountMinor: 499,
					currency: 'usd',
					invoiceUrl: INVOICE_URL,
					seller: true,
				},
				locale,
			),
	],
	[
		'plutonium_gift_applied',
		PREMIUM_URL,
		(service, locale) =>
			service.send(
				'plutonium_gift_applied',
				USER,
				{
					premiumName: 'Plutonium',
					plan: 'monthly',
					trialEnd: DATE,
					amount: {...PAID_TOTALS, paidMinor: 999},
					taxIncluded: true,
					paymentMethod: {kind: 'none'},
					seller: true,
				},
				locale,
			),
	],
	[
		'billing_credit_note',
		null,
		(service, locale) =>
			service.send(
				'billing_credit_note',
				USER,
				{
					amountMinor: 250,
					currency: 'eur',
					invoiceNumber: 'A1B2C3D4-0016',
					pdfUrl: 'https://pay.stripe.com/credit_notes/acct_1Example/cn_1/pdf',
					upcomingInvoice: true,
					seller: true,
				},
				locale,
			),
	],
	[
		'plutonium_refund_receipt_subscription_ended',
		INVOICE_URL,
		(service, locale) =>
			service.send(
				'plutonium_refund_receipt',
				USER,
				{
					premiumName: 'Plutonium',
					plan: 'monthly',
					invoiceNumber: 'A1B2C3D4-0017',
					refundedAt: DATE,
					amountMinor: 499,
					paidMinor: 499,
					paidAt: DATE,
					currency: 'usd',
					refundUrl: INVOICE_URL,
					subscriptionEnded: {requestedAt: DATE, reference: 'sub_1Example'},
				},
				locale,
			),
	],
	[
		'plutonium_payment_failed_reminder',
		INVOICE_URL,
		(service, locale) =>
			service.send(
				'plutonium_payment_failed',
				USER,
				{
					premiumName: 'Plutonium',
					reason: 'action_required',
					invoiceNumber: 'A1B2C3D4-0018',
					attemptedAt: DATE,
					amountMinor: 499,
					currency: 'usd',
					invoiceUrl: INVOICE_URL,
					accessEndsAt: null,
					reminder: true,
				},
				locale,
			),
	],
	[
		'gift_chargeback_notification_billing_resumed',
		PREMIUM_URL,
		(service, locale) =>
			service.send(
				'gift_chargeback_notification',
				USER,
				{billingResumed: {premiumName: 'Plutonium', plan: 'yearly'}},
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

describe('billing email subjects', () => {
	it('uses the reminder subject for a reminder to confirm a payment', async () => {
		const {service, sent} = captureService();
		await EMAILS.find(([name]) => name === 'plutonium_payment_failed_reminder')![2](service, 'en-US');
		expect(sent[0]!.subject).toBe('Reminder to confirm your payment for Plutonium');
	});

	it('shows the time zone of a cancellation request', async () => {
		const {service, sent} = captureService();
		await EMAILS.find(([name]) => name === 'plutonium_cancellation_confirmation')![2](service, 'en-US');
		expect(sent[0]!.text).toContain('on November 1, 2026 at 7:00 AM EST.');
	});

	it('puts the cancellation deadline in the notice box', async () => {
		const {service, sent} = captureService();
		await EMAILS.find(([name]) => name === 'plutonium_renewal_notice')![2](service, 'en-US');
		expect(sent[0]!.html).toMatch(
			/x-box[^>]*>To stop the renewal, cancel before November 1, 2026 at 1:00 PM GMT\+1\.</,
		);
	});
});

async function sentText<K extends keyof EmailSendParams>(template: K, params: EmailSendParams[K]): Promise<string> {
	const {service, sent} = captureService();
	await service.send(template, USER, params, 'en-US');
	return sent[0]!.text ?? '';
}

function renewalNoticeText(patch: Partial<EmailSendParams['plutonium_renewal_notice']> = {}): Promise<string> {
	return sentText('plutonium_renewal_notice', {
		variant: 'renewal',
		premiumName: 'Plutonium',
		plan: 'monthly',
		chargeAt: DATE,
		giftEndsAt: null,
		yearStartsAt: null,
		next: PAID_TOTALS,
		regular: PAID_TOTALS,
		taxIncluded: true,
		paymentMethod: CARD,
		seller: true,
		...patch,
	});
}

function endedUnpaidText(invoiceStatus: EmailSendParams['plutonium_ended_unpaid']['invoiceStatus']): Promise<string> {
	return sentText('plutonium_ended_unpaid', {
		premiumName: 'Plutonium',
		invoiceNumber: 'A1B2C3D4-0015',
		invoiceStatus,
		amountMinor: 499,
		currency: 'usd',
		invoiceUrl: INVOICE_URL,
		seller: true,
	});
}

describe('billing email wording', () => {
	it('keeps access until the next monthly charge in the yearly reminder', async () => {
		const text = await renewalNoticeText({variant: 'annual', yearStartsAt: new Date('2026-12-01T12:00:00Z')});
		expect(text).toContain("If you cancel, you keep Plutonium until November 1, 2026 and we won't charge you again.");
	});

	it('says which charge starts the next year in the yearly reminder', async () => {
		const text = await renewalNoticeText({variant: 'annual', yearStartsAt: new Date('2026-12-01T12:00:00Z')});
		expect(text).toContain('The charge on December 1, 2026 starts your next year of Plutonium.');
	});

	it('says why the yearly reminder was sent', async () => {
		expect(await renewalNoticeText({variant: 'annual'})).toContain('This is your yearly reminder');
	});

	it('gives the renewal date once in a gift notice', async () => {
		const text = await renewalNoticeText({variant: 'gift', giftEndsAt: DATE});
		expect(text).not.toContain('renews automatically on');
	});

	it('names a card by its brand', async () => {
		expect(await renewalNoticeText()).toContain('to your Visa ending in 4242.');
	});

	it('names a card with an unknown brand as a card', async () => {
		const text = await renewalNoticeText({paymentMethod: {kind: 'card', brand: 'unknown', last4: '4242'}});
		expect(text).toContain('to your card ending in 4242.');
	});

	it('says an invoice marked uncollectible is no longer being collected', async () => {
		expect(await endedUnpaidText('uncollectible')).toContain("We've stopped trying to collect this invoice.");
	});

	it('says an open invoice is still unpaid', async () => {
		expect(await endedUnpaidText('open')).toContain('is still unpaid. You can pay it on the invoice page.');
	});

	it('offers a refund of credit when no invoice is coming up', async () => {
		const text = await sentText('billing_credit_note', {
			amountMinor: 250,
			currency: 'eur',
			invoiceNumber: null,
			pdfUrl: null,
			upcomingInvoice: false,
			seller: true,
		});
		expect(text).toContain("If you'd like it refunded, reply to this email.");
	});

	it('describes a plan change payment as a plan change', async () => {
		const text = await sentText('plutonium_payment_failed', {
			premiumName: 'Plutonium',
			reason: 'action_required',
			invoiceNumber: 'A1B2C3D4-0018',
			attemptedAt: DATE,
			amountMinor: 499,
			currency: 'usd',
			invoiceUrl: INVOICE_URL,
			accessEndsAt: null,
			planChange: true,
		});
		expect(text).toContain('confirm the payment for the change to your Plutonium subscription.');
	});
});

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
		expect(html.match(/<h1[\s>]/g)?.length ?? 0).toBeLessThanOrEqual(1);
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
