// SPDX-License-Identifier: AGPL-3.0-or-later

import * as fs from 'node:fs';
import * as path from 'node:path';
import {parseArgs} from 'node:util';
import {formatMinorUnitPrice} from '@fluxer/number_utils/src/PriceFormatting';
import type {EmailConfig, EmailMessage, IEmailProvider} from '@pkgs/email/src/EmailProviderTypes';
import {EmailService} from '@pkgs/email/src/EmailService';
import {EMAIL_I18N_LOCALE_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nLocales';
import type {EmailTemplateVariables} from '@pkgs/email/src/email_i18n/EmailI18nTypes';

const DEFAULT_LOCALES = ['en-US', 'sv-SE', 'de', 'ar', 'ja'];
const PRICES: Record<string, {currency: string; earlyBird: number}> = {
	'sv-SE': {currency: 'SEK', earlyBird: 225000},
	de: {currency: 'EUR', earlyBird: 19900},
	fr: {currency: 'EUR', earlyBird: 19900},
	'pt-BR': {currency: 'BRL', earlyBird: 102000},
	tr: {currency: 'TRY', earlyBird: 971000},
};
const USD = {currency: 'USD', earlyBird: 19900};

class CaptureEmailProvider implements IEmailProvider {
	last: EmailMessage | null = null;

	async sendEmail(message: EmailMessage): Promise<boolean> {
		this.last = message;
		return true;
	}
}

const {values} = parseArgs({
	options: {
		out: {type: 'string'},
		locale: {type: 'string', multiple: true},
		all: {type: 'boolean', default: false},
		only: {type: 'string', multiple: true},
		'static-base': {type: 'string', default: 'https://fluxerstatic.com'},
	},
});

if (!values.out) {
	throw new Error(
		'usage: RenderEmailPreviews.ts --out <dir> [--locale <tag>...] [--all] [--only <name>...] [--static-base <url>]',
	);
}

const outDir = path.resolve(values.out);
const locales = values.all ? ['en-US', ...Object.keys(EMAIL_I18N_LOCALE_MESSAGES)] : (values.locale ?? DEFAULT_LOCALES);
const config: EmailConfig = {
	enabled: true,
	fromEmail: 'noreply@m.fluxer.com',
	fromName: 'Fluxer',
	replyTo: 'support@fluxer.com',
	appBaseUrl: 'https://web.fluxer.app',
	apiBaseUrl: 'https://api.fluxer.app',
	appealsEmail: 'appeals@fluxer.com',
	privacyEmail: 'privacy@fluxer.com',
	safetyEmail: 'safety@fluxer.com',
	supportEmail: 'support@fluxer.com',
	productName: 'Fluxer',
	marketingBaseUrl: 'https://fluxer.app',
	staticCdnBaseUrl: values['static-base'],
	legalAddress: 'Fluxer Platform AB, c/o Embassy House, Östgötagatan 12, 116 25 Stockholm, Sweden',
	brandLogo: true,
};
const provider = new CaptureEmailProvider();
const service = new EmailService(config, provider);
const token = `${'0f'.repeat(16)}.${'a1'.repeat(32)}`;

type Preview = (locale: string) => Promise<boolean>;

const DATE = new Date('2026-10-09T14:30:00Z');
const USER = 'operator@example.com';
const REPORT_ID = '1290874512349872128';
const REPORT_REFERENCE = {
	target: 'message_community',
	reportedAt: new Date('2026-10-24T18:12:00Z'),
	username: '',
	communityName: 'Fluxer HQ',
	channelName: 'general',
} as const;
const REPORT_REFERENCE_ACCOUNT = {
	...REPORT_REFERENCE,
	target: 'user',
	username: 'example',
	communityName: '',
	channelName: '',
} as const;
const REPORT_REFERENCE_COMMUNITY = {...REPORT_REFERENCE, target: 'community', channelName: ''} as const;
const DECISION_ID = '1290874512349872640';
const SUSPENSION_NOTICE: EmailTemplateVariables['enforcement_notice'] = {
	kind_label: 'suspension',
	neutral: 'no',
	summary: 'We suspended your account until Oct 21, 2026, 10:00 AM UTC.',
	effects: "You can't sign in or use Fluxer anywhere until then. Your account, messages and communities are kept.",
	rule_title: 'Spam',
	rule_url: 'https://fluxer.app/guidelines#6-spam-and-service-abuse',
	facts:
		'We found that your account sent unwanted messages in bulk.\n2 messages in #general of Plant Club, between Oct 1, 2026, 9:00 AM UTC and Oct 2, 2026, 6:30 PM UTC.',
	statement: 'You posted the same link in ten communities.',
	how_we_decided: 'We found this in our own review, and a person on our team made the decision.',
	appeals_email: 'appeals@fluxer.com',
	appeal_deadline: 'Dec 13, 2026',
	redress:
		"If you're in the EU, you can also refer this decision to a certified out-of-court dispute settlement body. The European Commission keeps a [list of these bodies](https://digital-strategy.ec.europa.eu/en/policies/dsa-out-court-dispute-settlement). You can also complain to the Digital Services Coordinator in your EU country, or go to court.",
	decision_id: DECISION_ID,
};

function noticeDate(date: Date, locale: string): string {
	return new Intl.DateTimeFormat(locale, {dateStyle: 'medium', timeZone: 'UTC'}).format(date);
}

const previews: Record<string, Preview> = {
	waitlist_verification: (locale) =>
		service.send(
			'operator_pass_waitlist_verification',
			USER,
			{confirmUrl: `https://fluxer.app/pass/confirm?token=${token}`},
			locale,
		),
	sale_open: (locale) => {
		const price = PRICES[locale] ?? USD;
		return service.send(
			'operator_pass_sale_open',
			USER,
			{
				checkoutUrl: 'https://web.fluxer.app/channels/@operator-pass',
				priceFormatted: formatMinorUnitPrice(price.earlyBird, price.currency, locale),
				priceRisesAt: new Date('2027-01-02T12:00:00Z'),
				unsubscribeUrl: `https://api.fluxer.app/v1/operator-pass/waitlist/unsubscribe?token=${token}`,
				unsubscribePageUrl: `https://fluxer.app/pass/unsubscribe?token=${token}`,
			},
			locale,
		);
	},
	welcome: (locale) =>
		service.send(
			'operator_pass_welcome',
			USER,
			{
				labsInviteUrl: 'https://fluxer.gg/fluxer-labs',
				invoiceNumber: 'B1Q2W3E4-0001',
				deliveredAt: DATE,
				wallName: 'Maja Lind',
				passCount: 1,
			},
			locale,
		),
	welcome_multiple: (locale) =>
		service.send(
			'operator_pass_welcome',
			USER,
			{
				labsInviteUrl: 'https://fluxer.gg/fluxer-labs',
				invoiceNumber: 'E1Q2W3E4-0002',
				deliveredAt: DATE,
				wallName: null,
				passCount: 3,
			},
			locale,
		),
	password_reset: (locale) =>
		service.send(
			'password_reset',
			USER,
			{resetToken: token, ipAddress: '203.0.113.42', location: 'Stockholm, Sweden'},
			locale,
		),
	email_verification: (locale) => service.send('email_verification', USER, {verificationToken: token}, locale),
	ip_authorization: (locale) =>
		service.send(
			'ip_authorization',
			USER,
			{authorizationToken: token, ipAddress: '203.0.113.42', location: 'Stockholm, Sweden'},
			locale,
		),
	email_change_revert: (locale) =>
		service.send('email_change_revert', USER, {newEmail: 'new.address@example.net', token}, locale),
	email_change_original: (locale) => service.send('email_change_original', USER, {code: 'K7QD-M2XP'}, locale),
	email_change_new: (locale) => service.send('email_change_new', USER, {code: 'K7QD-M2XP'}, locale),
	password_change_verification: (locale) =>
		service.send('password_change_verification', USER, {code: 'K7QD-M2XP'}, locale),
	mfa_backup_codes_view: (locale) => service.send('mfa_backup_codes_view', USER, {code: 'K7QD-M2XP'}, locale),
	dsa_report_verification: (locale) =>
		service.send('dsa_report_verification', USER, {code: 'H4WN-9TRC', expiresAt: DATE}, locale),
	donation_magic_link: (locale) =>
		service.send(
			'donation_magic_link',
			USER,
			{manageUrl: `https://fluxer.app/donate/manage#token=${token}`, expiresAt: DATE},
			locale,
		),
	registration_approved: (locale) => service.send('registration_approved', USER, {}, locale),
	registration_pending: (locale) =>
		service.send('registration_pending', USER, {reviewUrl: 'https://admin.fluxer.app/pending-registrations'}, locale),
	bulk_message_deletion_completed: (locale) =>
		service.send('bulk_message_deletion_completed', USER, {message_count: 1240, channel_count: 18}, locale),
	donation_confirmation: (locale) =>
		service.send(
			'donation_confirmation',
			USER,
			{amountCents: 1000, currency: 'eur', interval: 'month', manageUrl: 'https://fluxer.app/donate/manage'},
			locale,
		),
	harvest_completed: (locale) =>
		service.send(
			'harvest_completed',
			USER,
			{
				downloadUrl: `https://exports.fluxer.app/harvest/1234567890.zip?signature=${token}`,
				totalMessages: 48213,
				fileSize: 734003200,
				expiresAt: DATE,
			},
			locale,
		),
	inactivity_warning: (locale) =>
		service.send(
			'inactivity_warning',
			USER,
			{deletionDate: DATE, lastActiveDate: new Date('2024-06-01T09:00:00Z')},
			locale,
		),
	self_deletion_scheduled: (locale) => service.send('self_deletion_scheduled', USER, {deletionDate: DATE}, locale),
	account_deletion_cancelled: (locale) => service.send('account_deletion_cancelled', USER, {}, locale),
	account_deletion_scheduled_requested: (locale) =>
		service.send('account_deletion_scheduled_requested', USER, {deletionDate: noticeDate(DATE, locale)}, locale),
	account_deletion_scheduled_inactivity: (locale) =>
		service.send('account_deletion_scheduled_inactivity', USER, {deletionDate: noticeDate(DATE, locale)}, locale),
	enforcement_notice_suspension: (locale) => service.send('enforcement_notice', USER, SUSPENSION_NOTICE, locale),
	enforcement_notice_neutral: (locale) =>
		service.send(
			'enforcement_notice',
			USER,
			{
				...SUSPENSION_NOTICE,
				kind_label: 'ban',
				neutral: 'yes',
				summary: 'We banned your account.',
				effects: "You can't sign in or use Fluxer anywhere. You may not create or use other accounts.",
				rule_title: 'Child safety',
				rule_url: 'https://fluxer.app/guidelines#4-sexual-content-and-protection-of-minors',
				facts: 'We found content or activity on your account that breaks our rules on protecting children.',
				statement: null,
			},
			locale,
		),
	enforcement_notice_warning_no_appeal: (locale) =>
		service.send(
			'enforcement_notice',
			USER,
			{
				...SUSPENSION_NOTICE,
				kind_label: 'warning',
				summary: "We're warning you about activity on your account.",
				effects:
					'Your account is not restricted. If this happens again, we may limit or suspend it. This decision applies everywhere Fluxer is available.',
				rule_title: 'Spam',
				rule_url: null,
				statement: null,
				appeals_email: null,
				appeal_deadline: '',
				redress: null,
			},
			locale,
		),
	enforcement_notice_community_removal: (locale) =>
		service.send(
			'enforcement_notice',
			USER,
			{
				...SUSPENSION_NOTICE,
				kind_label: 'community_removal',
				summary: 'We removed your community Plant Club.',
				effects:
					'The community and its channels and messages are no longer available to anyone. Your own account is not restricted. This decision applies everywhere Fluxer is available.',
				facts:
					'We found content in this community that breaks this rule.\n2 messages posted in #general, between Oct 1, 2026, 9:00 AM UTC and Oct 2, 2026, 6:30 PM UTC.',
				statement: null,
			},
			locale,
		),
	enforcement_ended_lifted: (locale) =>
		service.send(
			'enforcement_ended',
			USER,
			{
				outcome: 'lifted',
				decision_id: DECISION_ID,
				restores_access: 'yes',
				statement: null,
				content_removed: 'no',
			},
			locale,
		),
	enforcement_ended_reversed: (locale) =>
		service.send(
			'enforcement_ended',
			USER,
			{
				outcome: 'reversed',
				decision_id: DECISION_ID,
				restores_access: 'yes',
				statement: 'The messages were quoted in a report about someone else.',
				content_removed: 'yes',
			},
			locale,
		),
	report_resolved: (locale) =>
		service.send(
			'report_resolved',
			USER,
			{
				reportId: REPORT_ID,
				publicComment: 'We removed the reported messages and warned the account. Thanks for the detailed report.',
				outcome: 'removed',
				reference: REPORT_REFERENCE,
			},
			locale,
		),
	report_resolved_restricted: (locale) =>
		service.send(
			'report_resolved',
			USER,
			{reportId: REPORT_ID, publicComment: '', outcome: 'restricted', reference: REPORT_REFERENCE_ACCOUNT},
			locale,
		),
	report_resolved_no_violation: (locale) =>
		service.send(
			'report_resolved',
			USER,
			{
				reportId: REPORT_ID,
				publicComment: 'The messages were sarcastic but not aimed at anyone.',
				outcome: 'no_violation',
				reference: REPORT_REFERENCE_COMMUNITY,
			},
			locale,
		),
	report_received: (locale) =>
		service.send('report_received', USER, {reportId: REPORT_ID, targetKind: 'message'}, locale),
	dsa_report_resolved: (locale) =>
		service.send(
			'dsa_report_resolved',
			USER,
			{
				reportId: REPORT_ID,
				publicComment: 'We removed the reported message under our community guidelines.',
				outcome: 'action_taken',
				reference: REPORT_REFERENCE,
			},
			locale,
		),
	dsa_report_resolved_no_violation: (locale) =>
		service.send(
			'dsa_report_resolved',
			USER,
			{reportId: REPORT_ID, publicComment: '', outcome: 'no_violation', reference: REPORT_REFERENCE},
			locale,
		),
	gift_chargeback_notification: (locale) => service.send('gift_chargeback_notification', USER, {}, locale),
	purchase_confirmation: (locale) => {
		const price = PRICES[locale] ?? USD;
		return service.send(
			'operator_pass_purchase_confirmation',
			USER,
			{
				invoiceNumber: 'B1Q2W3E4-0001',
				orderDate: DATE,
				quantity: 1,
				amountMinor: price.earlyBird * 1,
				currency: price.currency,
				waiverConsentAt: DATE,
				countryConsent: false,
				withdrawalDays: null,
				withdrawalEndsOn: null,
				wallName: 'Maja Lind',
				unitAmountMinor: 19900,
				paymentMethod: 'Visa ···· 4242',
				consentText:
					"Deliver my pass now. I understand I lose my right to withdraw once it's delivered, and that the bonus isn't part of the price.",
			},
			locale,
		);
	},
	purchase_confirmation_multiple: (locale) => {
		const price = PRICES[locale] ?? USD;
		return service.send(
			'operator_pass_purchase_confirmation',
			USER,
			{
				invoiceNumber: 'E1Q2W3E4-0002',
				orderDate: DATE,
				quantity: 3,
				amountMinor: price.earlyBird * 3,
				currency: price.currency,
				waiverConsentAt: DATE,
				countryConsent: false,
				withdrawalDays: null,
				withdrawalEndsOn: null,
				wallName: 'Maja Lind',
				unitAmountMinor: 19900,
				paymentMethod: 'Visa ···· 4242',
				consentText:
					"Deliver all 3 passes now. I understand I lose my right to withdraw once they're delivered, and that the bonus isn't part of the price.",
			},
			locale,
		);
	},
	purchase_confirmation_country: (locale) => {
		const price = PRICES[locale] ?? USD;
		return service.send(
			'operator_pass_purchase_confirmation',
			USER,
			{
				invoiceNumber: 'C1Q2W3E4-0003',
				orderDate: DATE,
				quantity: 1,
				amountMinor: price.earlyBird * 1,
				currency: price.currency,
				waiverConsentAt: DATE,
				countryConsent: true,
				withdrawalDays: 7,
				withdrawalEndsOn: new Date(DATE.getTime() + 7 * 24 * 60 * 60 * 1000).toISOString().slice(0, 10),
				wallName: 'Maja Lind',
				unitAmountMinor: 19900,
				paymentMethod: 'Visa ···· 4242',
				consentText:
					"Deliver my pass now. I can still cancel within 7 days for a full refund. The bonus isn't part of the price.",
			},
			locale,
		);
	},
	purchase_confirmation_country_multiple: (locale) => {
		const price = PRICES[locale] ?? USD;
		return service.send(
			'operator_pass_purchase_confirmation',
			USER,
			{
				invoiceNumber: 'F1Q2W3E4-0004',
				orderDate: DATE,
				quantity: 2,
				amountMinor: price.earlyBird * 2,
				currency: price.currency,
				waiverConsentAt: DATE,
				countryConsent: true,
				withdrawalDays: 7,
				withdrawalEndsOn: new Date(DATE.getTime() + 7 * 24 * 60 * 60 * 1000).toISOString().slice(0, 10),
				wallName: null,
				unitAmountMinor: 19900,
				paymentMethod: 'Visa ···· 4242',
				consentText:
					"Deliver all 2 passes now. I can still cancel each one within 7 days for a full refund. The bonus isn't part of the price.",
			},
			locale,
		);
	},
	withdrawal_receipt: (locale) => {
		const price = PRICES[locale] ?? USD;
		return service.send(
			'operator_pass_withdrawal_receipt',
			USER,
			{
				cancellation: false,
				name: 'Alex Lindqvist',
				receivedAt: DATE,
				invoiceNumber: 'C1Q2W3E4-0003',
				orderDate: DATE,
				amountMinor: price.earlyBird,
				currency: price.currency,
				lastPass: true,
				hasDonation: true,
				refundViaDispute: false,
			},
			locale,
		);
	},
	withdrawal_receipt_cancellation: (locale) => {
		const price = PRICES[locale] ?? USD;
		return service.send(
			'operator_pass_withdrawal_receipt',
			USER,
			{
				cancellation: true,
				name: 'Alex Lindqvist',
				receivedAt: DATE,
				invoiceNumber: 'C1Q2W3E4-0003',
				orderDate: DATE,
				amountMinor: price.earlyBird,
				currency: price.currency,
				lastPass: true,
				hasDonation: false,
				refundViaDispute: false,
			},
			locale,
		);
	},
	offer_received: (locale) =>
		service.send(
			'operator_pass_offer_received',
			USER,
			{senderName: 'Maja Lind', arrivesAt: new Date(DATE.getTime() + 72 * 60 * 60 * 1000)},
			locale,
		),
	offer_accepted: (locale) =>
		service.send('operator_pass_offer_accepted', USER, {recipientName: 'Tomas Varga', remainingCount: 2}, locale),
	offer_declined: (locale) =>
		service.send('operator_pass_offer_ended', USER, {otherName: 'Tomas Varga', ended: 'declined'}, locale),
	offer_taken_back: (locale) =>
		service.send('operator_pass_offer_ended', USER, {otherName: 'Maja Lind', ended: 'withdrawn'}, locale),
	offer_canceled: (locale) =>
		service.send('operator_pass_offer_ended', USER, {otherName: 'Maja Lind', ended: 'cancelled'}, locale),
	revoked: (locale) => service.send('operator_pass_revoked', USER, {cause: 'refund', lastPass: false}, locale),
	removal_notice: (locale) =>
		service.send(
			'operator_pass_removal_notice',
			USER,
			{
				rule: 'Community guidelines section 2, threats',
				facts: 'Sent threats to another operator in #operators on two days.',
				replyBy: DATE,
			},
			locale,
		),
	wall_hidden: (locale) =>
		service.send(
			'operator_pass_wall_hidden',
			USER,
			{reason: 'impersonation', note: 'The listing poses as a member of the Fluxer team.'},
			locale,
		),
	wall_name_replaced: (locale) =>
		service.send(
			'operator_pass_wall_name_replaced',
			USER,
			{
				target: 'name',
				shown: 'username',
				reason: 'impersonation',
				note: 'The name copies the name of another open source project.',
			},
			locale,
		),
	perks_removed: (locale) =>
		service.send(
			'operator_pass_perks_removed',
			USER,
			{
				until: DATE,
				rule: 'Community guidelines section 4, advertising',
				facts: 'Posted ads for a hosting company in #operators three times.',
				automated: false,
			},
			locale,
		),
	payment_dispute_warning: (locale) => {
		const price = PRICES[locale] ?? USD;
		return service.send(
			'payment_dispute_warning',
			USER,
			{
				product: 'plutonium',
				invoiceNumber: 'B1Q2W3E4-0001',
				orderDate: DATE,
				amountMinor: price.earlyBird,
				currency: price.currency,
				warningDate: new Date('2026-10-27T12:00:00Z'),
				note: null,
			},
			locale,
		);
	},
};

fs.mkdirSync(outDir, {recursive: true});
const index: Array<{name: string; locale: string; subject: string; htmlBytes: number; textBytes: number}> = [];
const selected = Object.entries(previews).filter(([name]) => !values.only || values.only.includes(name));
for (const [name, send] of selected) {
	for (const locale of locales) {
		provider.last = null;
		if (!(await send(locale)) || !provider.last) {
			throw new Error(`${name} (${locale}) was not sent`);
		}
		const message: EmailMessage = provider.last;
		if (!message.html) {
			throw new Error(`${name} (${locale}) has no HTML part`);
		}
		const base = path.join(outDir, `${name}.${locale}`);
		fs.writeFileSync(`${base}.html`, message.html, 'utf8');
		fs.writeFileSync(`${base}.txt`, `Subject: ${message.subject}\n\n${message.text}\n`, 'utf8');
		index.push({
			name,
			locale,
			subject: message.subject,
			htmlBytes: Buffer.byteLength(message.html, 'utf8'),
			textBytes: Buffer.byteLength(message.text, 'utf8'),
		});
	}
}
fs.writeFileSync(path.join(outDir, 'index.json'), `${JSON.stringify(index, null, '\t')}\n`, 'utf8');
console.log(`rendered ${index.length} previews -> ${outDir}`);
