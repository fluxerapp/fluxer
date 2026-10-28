// SPDX-License-Identifier: AGPL-3.0-or-later

import {getOperatorPassEarlyBirdEndsOn} from '@fluxer/constants/src/OperatorPassEarlyBird';
import {createLogger} from '@fluxer/logger/src/Logger';
import {formatMinorUnitPrice, formatMinorUnitPriceWithFraction} from '@fluxer/number_utils/src/PriceFormatting';
import type {
	EmailAttachment,
	EmailConfig,
	EmailListUnsubscribe,
	IEmailProvider,
	UserBouncedEmailChecker,
} from '@pkgs/email/src/EmailProviderTypes';
import {
	type EmailTextLinks,
	emailLinkHosts,
	getEmailTextDirection,
	renderEmailHtml,
	renderEmailText,
	restoreEmailLiterals,
	stripInlineMarkup,
} from '@pkgs/email/src/email_html/EmailHtmlRenderer';
import {getEmailTemplate, resolveEmailTimeZone} from '@pkgs/email/src/email_i18n/EmailI18n';
import type {EmailTemplateVariables} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
import type {EmailTemplate, EmailTemplateKey} from '@pkgs/email/src/email_i18n/EmailI18nTypes.generated';
import type {
	BillingEndedInvoiceStatus,
	BillingInvoiceDocument,
	BillingPaymentMethod,
	BillingTotals,
	EmailSendParams,
	IBulkEmailSession,
	IEmailService,
	PaymentDisputeWarningEmail,
} from '@pkgs/email/src/IEmailService';
import {getOperatorPassUnlockDay} from '@pkgs/email/src/OperatorPassUnlockDay';
import {
	getOperatorPassTermsUrl,
	OPERATOR_PASS_TERMS_VERSION,
	OPERATOR_PASS_TRADER_EMAIL,
	resolveOperatorPassTermsLanguage,
} from '@pkgs/email/src/operator_pass_terms/OperatorPassTerms';
import {ms} from 'itty-time';

const OPERATOR_PASS_WELCOME_FOUNDER_NAME = 'Hampus';
const PAYMENT_DISPUTE_REVIEW_LOCALE = 'en-US';

const logger = createLogger('email-service');
const EMAIL_ASSETS_PATH = '/email';

type EmailSlots = Record<string, string | null | undefined>;

interface TemplatedEmailOptions {
	slots?: (template: EmailTemplate) => EmailSlots;
	listUnsubscribe?: EmailListUnsubscribe;
	attachments?: ReadonlyArray<EmailAttachment>;
	timeZone?: string | null;
}

interface PaymentDisputeEmailLinks {
	marketingBaseUrl: string;
	appealsEmail: string | null;
	privacyEmail: string | null;
}

type FactRow = readonly [string | undefined, string | null | undefined, string?];

function facts(...rows: Array<FactRow>): EmailSlots {
	const slots: EmailSlots = {};
	let count = 0;
	for (const [label, value, url] of rows) {
		if (!label || !value) {
			continue;
		}
		count += 1;
		slots[`fact_${count}_label`] = label;
		if (url) {
			slots[`fact_${count}_link`] = value;
			slots[`fact_${count}_link_url`] = url;
		} else {
			slots[`fact_${count}_value`] = value;
		}
	}
	return slots;
}

type SummaryRow = readonly [string | undefined, string | null | undefined];

const SUMMARY_ROWS_BEFORE_TOTAL = 5;
const SUMMARY_ROWS_AFTER_TOTAL = 2;
const POP_DIRECTIONAL_ISOLATE = '\u2069';
const SELLER_ADDRESS = 'c/o Embassy House, Östgötagatan 12, 116 25 Stockholm, Sweden';

function summary(
	rows: Array<SummaryRow>,
	total: readonly [string | undefined, string],
	tax: string | null = null,
	after: Array<SummaryRow> = [],
): EmailSlots {
	const slots: EmailSlots = {sum_total_label: total[0], sum_total_value: total[1], sum_tax: tax};
	const fill = (list: Array<SummaryRow>, first: number, max: number) => {
		let n = first;
		for (const [label, value] of list) {
			if (!label || !value || n >= first + max) {
				continue;
			}
			slots[`sum_${n}_label`] = label;
			slots[`sum_${n}_value`] = value;
			n += 1;
		}
	};
	fill(rows, 1, SUMMARY_ROWS_BEFORE_TOTAL);
	fill(after, SUMMARY_ROWS_BEFORE_TOTAL + 1, SUMMARY_ROWS_AFTER_TOTAL);
	return slots;
}

const SHARED_CURRENCY_SYMBOLS = new Set(['$', '¥', '￥']);

function withCurrencyCode(formatted: string, currency: string): string {
	const code = currency.toUpperCase();
	const symbol = formatted.replace(/[\p{Nd}\p{Zs}\p{Cf}.,'\u066b\u066c+\-\u2212]/gu, '');
	if (formatted.includes(code) || !SHARED_CURRENCY_SYMBOLS.has(symbol)) {
		return formatted;
	}
	return formatted.endsWith(POP_DIRECTIONAL_ISOLATE)
		? `${formatted.slice(0, -1)}\u00a0${code}${POP_DIRECTIONAL_ISOLATE}`
		: `${formatted}\u00a0${code}`;
}

function amountWithCode(amountMinor: number, currency: string, locale: string | null): string {
	return withCurrencyCode(formatMinorUnitPrice(amountMinor, currency, locale), currency);
}

interface FormattedTotals {
	total: string;
	totalWithCode: string;
	tax: string | null;
	credit: string | null;
	paid: string | null;
}

function formatTotals(totals: BillingTotals, locale: string | null): FormattedTotals {
	const format = totals.tax
		? (amount: number) => formatMinorUnitPriceWithFraction(amount, totals.currency, locale)
		: (amount: number) => formatMinorUnitPrice(amount, totals.currency, locale);
	const total = format(totals.totalMinor);
	const credit = totals.totalMinor - totals.paidMinor;
	return {
		total,
		totalWithCode: withCurrencyCode(total, totals.currency),
		tax: totals.tax ? format(totals.tax.amountMinor) : null,
		credit: credit > 0 ? `-${format(credit)}` : null,
		paid: credit > 0 ? withCurrencyCode(format(totals.paidMinor), totals.currency) : null,
	};
}

function taxLine(template: EmailTemplate, totals: BillingTotals): string | null {
	if (totals.reverseCharge) {
		return template.reverse_charge;
	}
	if (!totals.tax) {
		return null;
	}
	return totals.tax.behavior === 'inclusive' ? template.tax_included : template.tax_added;
}

function invoiceSlots(template: EmailTemplate, invoice: BillingInvoiceDocument | null): EmailSlots {
	return invoice ? {cta_url: invoice.url, expiry: template.link_expiry} : {lead: null, cta: null};
}

function yesNo(value: boolean): 'yes' | 'no' {
	return value ? 'yes' : 'no';
}

const ENDED_INVOICE_OWED: Record<BillingEndedInvoiceStatus, 'none' | 'stopped' | 'open' | 'unknown'> = {
	void: 'none',
	uncollectible: 'stopped',
	open: 'open',
	paid: 'unknown',
	other: 'unknown',
};

const CARD_BRAND_NAMES: Record<string, string> = {
	amex: 'American Express',
	cartes_bancaires: 'Cartes Bancaires',
	diners: 'Diners Club',
	discover: 'Discover',
	eftpos_au: 'eftpos',
	jcb: 'JCB',
	mastercard: 'Mastercard',
	unionpay: 'UnionPay',
	visa: 'Visa',
};

function cardBrandName(brand: string): string | null {
	if (!brand || brand === 'unknown') {
		return null;
	}
	return CARD_BRAND_NAMES[brand] ?? `${brand.charAt(0).toUpperCase()}${brand.slice(1)}`;
}

function paymentMethodVariables(method: BillingPaymentMethod) {
	const brand = method.kind === 'card' ? cardBrandName(method.brand) : null;
	return {
		method: method.kind === 'card' && !brand ? ('plain_card' as const) : method.kind,
		brand: brand ?? '',
		last4: method.kind === 'card' ? method.last4 : '',
		methodName: method.kind === 'named' ? method.name : '',
	};
}

function sellerVariables() {
	return {sellerAddress: SELLER_ADDRESS, sellerEmail: OPERATOR_PASS_TRADER_EMAIL};
}

function formatWithLocale(date: Date, locale: string | null, options: Intl.DateTimeFormatOptions): string {
	try {
		return new Intl.DateTimeFormat(locale ?? 'en-US', options).format(date);
	} catch {
		return new Intl.DateTimeFormat('en-US', options).format(date);
	}
}

function formatMoment(date: Date, locale: string | null, timeZone: string | null | undefined): string {
	return formatWithLocale(date, locale, {
		year: 'numeric',
		month: 'long',
		day: 'numeric',
		hour: 'numeric',
		minute: '2-digit',
		timeZone: resolveEmailTimeZone(timeZone) ?? 'UTC',
		timeZoneName: 'short',
	});
}

function formatCardExpiry(month: number, year: number, locale: string | null): string {
	return formatWithLocale(new Date(Date.UTC(year, month - 1, 1)), locale, {
		month: 'long',
		year: 'numeric',
		timeZone: 'UTC',
	});
}

function chargeFormatted(totals: BillingTotals, locale: string | null): string {
	return formatTotals(totals, locale).totalWithCode;
}

function paragraphsOf(...lines: Array<string | null | undefined>): string {
	return lines.filter((line): line is string => Boolean(line)).join('\n');
}

function appealSection(template: EmailTemplate): EmailSlots {
	return {
		section_1_title: template.appeal_title,
		section_1_body: template.appeal_body,
		section_1_note: template.redress,
	};
}

type ReplyInbox = 'appeals' | 'privacy' | 'safety' | 'support';

const EMAIL_REPLY_INBOX: Record<EmailTemplateKey, ReplyInbox> = {
	account_deletion_cancelled: 'safety',
	account_deletion_scheduled_inactivity: 'support',
	account_deletion_scheduled_requested: 'safety',
	billing_credit_note: 'support',
	bulk_message_deletion_completed: 'privacy',
	donation_cancellation_confirmation: 'support',
	donation_card_expiring: 'support',
	donation_confirmation: 'support',
	donation_magic_link: 'support',
	donation_payment_failed: 'support',
	donation_receipt: 'support',
	donation_refund_receipt: 'support',
	donation_renewal_notice: 'support',
	dsa_report_resolved: 'appeals',
	dsa_report_verification: 'safety',
	email_change_new: 'support',
	email_change_original: 'support',
	email_change_revert: 'safety',
	email_verification: 'support',
	email_verification_code: 'support',
	enforcement_ended: 'appeals',
	enforcement_notice: 'appeals',
	gift_chargeback_notification: 'support',
	harvest_completed: 'privacy',
	inactivity_warning: 'support',
	ip_authorization: 'support',
	mfa_backup_codes_view: 'support',
	operator_pass_invoice: 'support',
	operator_pass_offer_accepted: 'support',
	operator_pass_offer_ended: 'support',
	operator_pass_offer_received: 'support',
	operator_pass_perks_removed: 'appeals',
	operator_pass_purchase_confirmation: 'support',
	operator_pass_refund_receipt: 'support',
	operator_pass_removal_notice: 'appeals',
	operator_pass_revoked: 'support',
	operator_pass_sale_open: 'support',
	operator_pass_waitlist_verification: 'support',
	operator_pass_wall_hidden: 'appeals',
	operator_pass_wall_name_replaced: 'appeals',
	operator_pass_welcome: 'support',
	operator_pass_withdrawal_receipt: 'support',
	password_change_verification: 'support',
	password_reset: 'support',
	payment_dispute_warning: 'appeals',
	plutonium_cancellation_confirmation: 'support',
	plutonium_card_expiring: 'support',
	plutonium_ended_unpaid: 'support',
	plutonium_gift_applied: 'support',
	plutonium_payment_failed: 'support',
	plutonium_payment_receipt: 'support',
	plutonium_refund_receipt: 'support',
	plutonium_renewal_notice: 'support',
	reauth_code: 'support',
	registration_approved: 'support',
	registration_pending: 'support',
	report_received: 'safety',
	report_resolved: 'appeals',
	self_deletion_scheduled: 'support',
	sign_in_code: 'support',
	sign_up_code: 'support',
};

function resolveEmailReplyTo(config: EmailConfig, templateKey: EmailTemplateKey): string | null {
	const support = config.replyTo ?? config.supportEmail;
	switch (EMAIL_REPLY_INBOX[templateKey]) {
		case 'appeals':
			return config.appealsEmail ?? support;
		case 'privacy':
			return config.privacyEmail ?? support;
		case 'safety':
			return config.safetyEmail ?? support;
		case 'support':
			return support;
	}
}

export function emailLinkUrls(config: Pick<EmailConfig, 'appBaseUrl' | 'marketingBaseUrl'>): EmailSlots {
	return {
		settings_url: `${config.appBaseUrl}/channels/@operator-pass?view=passes`,
		dsa_url: `${config.marketingBaseUrl}/help/dsa-dispute-resolution`,
		arn_url: 'https://www.arn.se/',
	};
}

function minutesUntil(expiresAt: Date): number {
	return Math.max(1, Math.round((expiresAt.getTime() - Date.now()) / ms('1 minute')));
}

function daysUntil(date: Date): number {
	return Math.max(1, Math.round((date.getTime() - Date.now()) / ms('1 day')));
}

const CODE_MINUTES = 10;

function plutoniumUrl(appBaseUrl: string): string {
	return `${appBaseUrl}/channels/@premium`;
}

function isGiftPlan(plan: string): boolean {
	return plan === 'gift_1_month' || plan === 'gift_1_year';
}

function getPaymentDisputeTermsUrl(product: 'operator_pass' | 'plutonium', marketingBaseUrl: string): string {
	return product === 'plutonium' ? `${marketingBaseUrl}/terms` : getOperatorPassTermsUrl(marketingBaseUrl);
}

export function getPaymentDisputeWarningVariables(
	params: PaymentDisputeWarningEmail,
	links: PaymentDisputeEmailLinks,
): EmailTemplateVariables['payment_dispute_warning'] {
	return {
		appeals_email: links.appealsEmail,
		privacy_email: links.privacyEmail,
		product: params.product,
		orderDate: params.orderDate,
		warningDate: params.warningDate,
	};
}

export function paymentDisputeWarningSlots(
	template: EmailTemplate,
	params: PaymentDisputeWarningEmail,
	locale: string | null,
	marketingBaseUrl: string,
): EmailSlots {
	return {
		...facts(
			[template.order_number_label, params.invoiceNumber],
			[template.order_date_label, template.order_date_value],
			[template.amount_label, formatMinorUnitPrice(params.amountMinor, params.currency, locale)],
			[template.currency_label, params.currency.toUpperCase()],
			[template.ground_label, template.ground, getPaymentDisputeTermsUrl(params.product, marketingBaseUrl)],
			[template.deadline_label, template.deadline_value],
		),
		lead: template.deadline_body,
		section_1_title: template.why_title,
		section_1_body: template.why_body,
		section_1_note: params.note?.trim() || null,
		section_2_title: template.next_title,
		section_2_body: template.next_body,
		section_3_title: template.appeal_title,
		section_3_body: template.appeal_body,
	};
}

export function enforcementNoticeSlots(
	template: EmailTemplate,
	variables: EmailTemplateVariables['enforcement_notice'],
): EmailSlots {
	return {
		...facts(
			[template.rule_label, variables.rule_title, variables.rule_url ?? undefined],
			[template.decision_label, variables.decision_id],
		),
		intro: `${template.intro}\n\n${template.effects}`,
		section_1_title: template.why_title,
		section_1_body: template.why_body,
		section_1_note: template.statement,
		section_2_title: template.how_title,
		section_2_body: template.how_body,
		...(template.appeal_body
			? {section_3_title: template.appeal_title, section_3_body: template.appeal_body, section_3_note: template.redress}
			: {}),
	};
}

export function enforcementEndedSlots(template: EmailTemplate): EmailSlots {
	return {text_1: template.statement, text_2: template.content_removed};
}

export function reportResolvedSlots(template: EmailTemplate, reportId: string, publicComment: string): EmailSlots {
	return {
		intro: `${template.about}\n${template.intro}`,
		...facts([template.report_label, reportId], [template.comment_label, publicComment]),
		text_1: publicComment ? undefined : template.thanks,
	};
}

export function renderEmailTemplateText<T extends EmailTemplateKey>(
	templateKey: T,
	locale: string | null,
	variables: EmailTemplateVariables[T],
	productName: string,
	slots: (template: EmailTemplate) => EmailSlots = () => ({}),
	links: {format: EmailTextLinks; urls?: EmailSlots; reply?: 'yes' | 'no'} = {format: 'plain'},
): string | null {
	const result = getEmailTemplate(templateKey, locale, {...variables, reply: links.reply ?? 'no'}, productName);
	return result.ok ? renderEmailText({...links.urls, ...result.value, ...slots(result.value)}, links.format) : null;
}

function getOwnHttpOrigins(config: EmailConfig): ReadonlySet<string> {
	const origins = new Set<string>();
	for (const base of [config.appBaseUrl, config.apiBaseUrl, config.marketingBaseUrl, config.staticCdnBaseUrl]) {
		if (!URL.canParse(base)) {
			continue;
		}
		const url = new URL(base);
		if (url.protocol === 'http:') {
			origins.add(url.origin);
		}
	}
	return origins;
}

export class EmailService implements IEmailService {
	private readonly config: EmailConfig;
	private readonly provider: IEmailProvider | null;
	private readonly bouncedEmailChecker: UserBouncedEmailChecker | null;
	private readonly ownHttpOrigins: ReadonlySet<string>;
	private readonly linkHosts: ReadonlySet<string>;

	constructor(
		config: EmailConfig,
		provider: IEmailProvider | null = null,
		bouncedEmailChecker: UserBouncedEmailChecker | null = null,
	) {
		this.config = config;
		this.provider = provider;
		this.bouncedEmailChecker = bouncedEmailChecker;
		this.ownHttpOrigins = getOwnHttpOrigins(config);
		this.linkHosts = emailLinkHosts([
			config.appBaseUrl,
			config.apiBaseUrl,
			config.marketingBaseUrl,
			config.staticCdnBaseUrl,
		]);
	}

	send<K extends keyof EmailSendParams>(
		templateKey: K,
		email: string,
		params: EmailSendParams[K],
		locale: string | null = null,
	): Promise<boolean> {
		return this.handlers[templateKey](email, params, locale);
	}

	private readonly handlers: {
		[K in keyof EmailSendParams]: (
			email: string,
			params: EmailSendParams[K],
			locale: string | null,
		) => Promise<boolean>;
	} = {
		password_change_verification: async (email, {code}, locale) => {
			return this.sendCode(email, 'password_change_verification', locale, code, {minutes: CODE_MINUTES});
		},
		mfa_backup_codes_view: async (email, {code}, locale) => {
			return this.sendCode(email, 'mfa_backup_codes_view', locale, code, {minutes: CODE_MINUTES});
		},
		sign_in_code: async (email, {code, expiresAt, requester}, locale) => {
			return this.sendCode(email, 'sign_in_code', locale, code, {minutes: minutesUntil(expiresAt)}, (template) =>
				facts([template.location_label, requester.location], [template.device_label, requester.device]),
			);
		},
		sign_up_code: async (email, {code, expiresAt, requester}, locale) => {
			return this.sendCode(email, 'sign_up_code', locale, code, {minutes: minutesUntil(expiresAt)}, (template) =>
				facts([template.location_label, requester.location], [template.device_label, requester.device]),
			);
		},
		email_verification_code: async (email, {code, expiresAt}, locale) => {
			return this.sendCode(email, 'email_verification_code', locale, code, {minutes: minutesUntil(expiresAt)});
		},
		reauth_code: async (email, {code, expiresAt}, locale) => {
			return this.sendCode(email, 'reauth_code', locale, code, {minutes: minutesUntil(expiresAt)});
		},
		email_change_original: async (email, {code}, locale) => {
			return this.sendCode(email, 'email_change_original', locale, code, {minutes: CODE_MINUTES});
		},
		email_change_new: async (email, {code}, locale) => {
			return this.sendCode(email, 'email_change_new', locale, code, {minutes: CODE_MINUTES});
		},
		password_reset: async (email, {resetToken, ipAddress, location}, locale) => {
			const resetUrl = `${this.config.appBaseUrl}/reset#token=${resetToken}`;
			return this.sendTemplatedEmail(
				email,
				'password_reset',
				locale,
				{},
				{
					slots: (template) => ({
						cta_url: resetUrl,
						...facts([template.ip_label, ipAddress], [template.location_label, location]),
					}),
				},
			);
		},
		email_verification: async (email, {verificationToken}, locale) => {
			const verifyUrl = `${this.config.appBaseUrl}/verify#token=${verificationToken}`;
			return this.sendTemplatedEmail(email, 'email_verification', locale, {}, {slots: () => ({cta_url: verifyUrl})});
		},
		ip_authorization: async (email, {authorizationToken, ipAddress, location}, locale) => {
			const authUrl = `${this.config.appBaseUrl}/authorize-ip#token=${authorizationToken}`;
			return this.sendTemplatedEmail(
				email,
				'ip_authorization',
				locale,
				{},
				{
					slots: (template) => ({
						cta_url: authUrl,
						...facts([template.ip_label, ipAddress], [template.location_label, location]),
					}),
				},
			);
		},
		email_change_revert: async (email, {newEmail, token}, locale) => {
			const revertUrl = `${this.config.appBaseUrl}/wasntme#token=${token}`;
			return this.sendTemplatedEmail(
				email,
				'email_change_revert',
				locale,
				{},
				{slots: (template) => ({cta_url: revertUrl, ...facts([template.new_email_label, `<mail>${newEmail}</mail>`])})},
			);
		},
		registration_approved: async (email, _params, locale) => {
			const loginUrl = `${this.config.appBaseUrl}/login`;
			return this.sendTemplatedEmail(email, 'registration_approved', locale, {}, {slots: () => ({cta_url: loginUrl})});
		},
		registration_pending: async (email, {reviewUrl}, locale) => {
			return this.sendTemplatedEmail(email, 'registration_pending', locale, {}, {slots: () => ({cta_url: reviewUrl})});
		},
		self_deletion_scheduled: async (email, {deletionDate}, locale) => {
			return this.sendTemplatedEmail(email, 'self_deletion_scheduled', locale, {deletionDate});
		},
		account_deletion_scheduled_requested: async (email, {deletionDate}, locale) => {
			return this.sendTemplatedEmail(email, 'account_deletion_scheduled_requested', locale, {
				deletion_date: deletionDate,
			});
		},
		account_deletion_scheduled_inactivity: async (email, {deletionDate}, locale) => {
			return this.sendTemplatedEmail(email, 'account_deletion_scheduled_inactivity', locale, {
				deletion_date: deletionDate,
			});
		},
		account_deletion_cancelled: async (email, _params, locale) => {
			return this.sendTemplatedEmail(email, 'account_deletion_cancelled', locale, {});
		},
		inactivity_warning: async (email, {deletionDate, lastActiveDate}, locale) => {
			const loginUrl = `${this.config.appBaseUrl}/login`;
			return this.sendTemplatedEmail(
				email,
				'inactivity_warning',
				locale,
				{deletionDate, lastActiveDate},
				{slots: () => ({cta_url: loginUrl})},
			);
		},
		enforcement_notice: async (email, variables, locale) => {
			return this.sendTemplatedEmail(email, 'enforcement_notice', locale, variables, {
				slots: (template) => enforcementNoticeSlots(template, variables),
			});
		},
		enforcement_ended: async (email, variables, locale) => {
			return this.sendTemplatedEmail(email, 'enforcement_ended', locale, variables, {slots: enforcementEndedSlots});
		},
		report_resolved: async (email, {reportId, publicComment, outcome, reference}, locale) => {
			return this.sendTemplatedEmail(
				email,
				'report_resolved',
				locale,
				{...reference, outcome, appeals_email: this.config.appealsEmail},
				{slots: (template) => reportResolvedSlots(template, reportId, publicComment)},
			);
		},
		dsa_report_resolved: async (email, {reportId, publicComment, outcome, reference}, locale) => {
			return this.sendTemplatedEmail(
				email,
				'dsa_report_resolved',
				locale,
				{...reference, outcome, appeals_email: this.config.appealsEmail},
				{slots: (template) => reportResolvedSlots(template, reportId, publicComment)},
			);
		},
		report_received: async (email, {reportId, targetKind}, locale) => {
			return this.sendTemplatedEmail(
				email,
				'report_received',
				locale,
				{targetKind},
				{slots: (template) => ({...facts([template.report_label, reportId]), text_1: template.next})},
			);
		},
		dsa_report_verification: async (email, {code, expiresAt}, locale) => {
			return this.sendCode(email, 'dsa_report_verification', locale, code, {minutes: minutesUntil(expiresAt)});
		},
		donation_magic_link: async (email, {manageUrl, expiresAt}, locale) => {
			return this.sendTemplatedEmail(
				email,
				'donation_magic_link',
				locale,
				{minutes: minutesUntil(expiresAt)},
				{slots: () => ({cta_url: manageUrl})},
			);
		},
		donation_confirmation: async (email, params, locale) => {
			const once = params.interval === 'once';
			return this.sendTemplatedEmail(
				email,
				'donation_confirmation',
				locale,
				{
					amount: formatMinorUnitPrice(params.amountCents, params.currency, locale),
					interval: params.interval,
					paidAt: params.paidAt ?? new Date(),
				},
				{
					slots: (template) => ({
						cta_url: params.manageUrl,
						...facts(
							[template.amount_label, template.amount_value],
							[template.currency_label, params.currency.toUpperCase()],
							[template.date_label, once && params.paidAt ? template.date_value : null],
							[template.reference_label, once ? params.reference : null],
						),
						...(once && params.receiptUrl ? {end_link: template.receipt_link, end_link_url: params.receiptUrl} : {}),
					}),
				},
			);
		},
		donation_receipt: async (email, params, locale) => {
			const totals = formatTotals(params.totals, locale);
			const invoice = params.invoice;
			return this.sendTemplatedEmail(
				email,
				'donation_receipt',
				locale,
				{
					interval: params.interval,
					paidAt: params.paidAt,
					periodStart: params.period?.start ?? params.paidAt,
					periodEnd: params.period?.end ?? params.paidAt,
					amountFormatted: totals.total,
					pdf: yesNo(invoice?.pdf != null),
					invoiceLinkUntil: invoice?.linkUntil ?? params.paidAt,
				},
				{
					slots: (template) => ({
						...summary(
							[
								[template.invoice_label, params.invoiceNumber],
								[template.date_label, template.date_value],
								[template.period_label, params.period ? template.period_value : null],
							],
							[template.total_label, totals.totalWithCode],
						),
						...invoiceSlots(template, invoice),
						end_link: template.manage_link,
						end_link_url: params.manageUrl,
					}),
					attachments: invoice?.pdf ? [invoice.pdf] : undefined,
				},
			);
		},
		donation_refund_receipt: async (email, params, locale) => {
			const amountFormatted = formatMinorUnitPrice(params.amountMinor, params.currency, locale);
			return this.sendTemplatedEmail(
				email,
				'donation_refund_receipt',
				locale,
				{refundedAt: params.refundedAt, amountFormatted},
				{
					slots: (template) => ({
						...summary(
							[
								[template.reference_label, params.reference],
								[template.date_label, template.date_value],
							],
							[template.refund_label, amountWithCode(params.amountMinor, params.currency, locale)],
						),
						...(params.refundUrl ? {cta_url: params.refundUrl} : {cta: null}),
					}),
				},
			);
		},
		donation_payment_failed: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'donation_payment_failed',
				locale,
				{
					reason: params.reason,
					attemptedAt: params.attemptedAt,
					amountFormatted: formatMinorUnitPrice(params.amountMinor, params.currency, locale),
				},
				{
					slots: (template) => ({
						...summary(
							[
								[template.invoice_label, params.invoiceNumber],
								[template.date_label, template.date_value],
							],
							[template.amount_label, amountWithCode(params.amountMinor, params.currency, locale)],
						),
						...(params.invoiceUrl ? {cta_url: params.invoiceUrl} : {cta: null}),
						end_link: template.manage_link,
						end_link_url: params.manageUrl,
						...(params.reminder ? {subject: template.reminder_subject} : {}),
					}),
				},
			);
		},
		harvest_completed: async (email, {downloadUrl, totalMessages, fileSize, expiresAt}, locale) => {
			const fileSizeMB = Number.parseFloat((fileSize / 1024 / 1024).toFixed(2));
			return this.sendTemplatedEmail(
				email,
				'harvest_completed',
				locale,
				{totalMessages, fileSizeMB, expiresAt},
				{
					slots: (template) => ({
						cta_url: downloadUrl,
						...facts([template.messages_label, template.messages], [template.size_label, template.size]),
					}),
				},
			);
		},
		bulk_message_deletion_completed: async (email, totals, locale) => {
			return this.sendTemplatedEmail(email, 'bulk_message_deletion_completed', locale, totals);
		},
		gift_chargeback_notification: async (email, params, locale) => {
			const resumed = params.billingResumed ?? null;
			return this.sendTemplatedEmail(
				email,
				'gift_chargeback_notification',
				locale,
				{premiumName: resumed?.premiumName ?? '', plan: resumed?.plan ?? 'monthly'},
				{
					slots: (template) =>
						resumed
							? {
									lead: template.billing_resumed,
									cta: template.manage_cta,
									cta_url: plutoniumUrl(this.config.appBaseUrl),
								}
							: {},
				},
			);
		},
		operator_pass_welcome: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'operator_pass_welcome',
				locale,
				{
					founderName: OPERATOR_PASS_WELCOME_FOUNDER_NAME,
					deliveredAt: params.deliveredAt,
					wallName: params.wallName ?? '',
					wallOptOut: params.wallName ? 'no' : 'yes',
				},
				{
					slots: (template) => ({
						...facts(
							[template.order_number_label, params.invoiceNumber],
							[template.delivered_label, template.delivered_value],
							[template.pass_count_label, params.passCount > 1 ? String(params.passCount) : null],
						),
						points_list: `${template.wall_line}\n${template.badge_line}`,
						lead: template.labs_line,
						cta_url: params.labsInviteUrl,
						end_link: template.terms_link,
						end_link_url: getOperatorPassTermsUrl(this.config.marketingBaseUrl),
					}),
					timeZone: params.timeZone,
				},
			);
		},
		operator_pass_purchase_confirmation: async (email, params, locale) => {
			const attachment = await this.renderTermsAttachment(locale);
			if (!attachment) {
				return false;
			}
			const countryRule = params.withdrawalDays !== null && params.withdrawalEndsOn !== null;
			return this.sendTemplatedEmail(
				email,
				'operator_pass_purchase_confirmation',
				locale,
				{
					termsVersion: OPERATOR_PASS_TERMS_VERSION,
					orderDate: params.orderDate,
					quantity: params.quantity,
					priceFormatted: formatMinorUnitPrice(params.amountMinor, params.currency, locale),
					withdrawalEndsOn: params.withdrawalEndsOn ?? params.orderDate.toISOString().slice(0, 10),
					waiverConsentAt: params.waiverConsentAt ?? params.orderDate,
					consentText: params.consentText ?? '',
					wallName: params.wallName ?? '',
					wallOptOut: params.wallName ? 'no' : 'yes',
					sellerEmail: OPERATOR_PASS_TRADER_EMAIL,
				},
				{
					slots: (template) => {
						let withdrawalBodyKey = 'withdrawal_waived';
						if (countryRule) {
							withdrawalBodyKey = params.countryConsent ? 'withdrawal_cancel_until' : 'withdrawal_rule_cancel_until';
						}
						return {
							...facts(
								[template.order_number_label, params.invoiceNumber],
								[template.order_date_label, template.order_date_value],
								[template.product_label, template.product_value],
								[
									template.unit_price_label,
									params.quantity > 1 ? formatMinorUnitPrice(params.unitAmountMinor, params.currency, locale) : null,
								],
								[template.price_label, template.price_value],
								[template.currency_label, params.currency.toUpperCase()],
								[template.payment_label, params.paymentMethod],
							),
							section_1_title: template.included_heading,
							section_1_list: [template.included_perks, template.included_pass_on].join('\n'),
							section_1_note: template.wall_status,
							section_2_title: template.bonus_heading,
							section_2_body: template.bonus_line,
							...(params.consentText
								? {section_3_title: template.consent_heading, section_3_body: template.consent_line}
								: {}),
							section_4_title: template.withdrawal_heading,
							section_4_body: template[withdrawalBodyKey],
							note: template.seller,
							end_link: template.terms_link,
							end_link_url: getOperatorPassTermsUrl(this.config.marketingBaseUrl),
						};
					},
					attachments: [attachment],
					timeZone: params.timeZone,
				},
			);
		},
		operator_pass_withdrawal_receipt: async (email, params, locale) => {
			const amountFormatted = formatMinorUnitPrice(params.amountMinor, params.currency, locale);
			return this.sendTemplatedEmail(
				email,
				'operator_pass_withdrawal_receipt',
				locale,
				{
					cancellation: params.cancellation ? 'yes' : 'no',
					receivedAt: params.receivedAt,
					orderDate: params.orderDate,
					amountFormatted,
					lastPass: params.lastPass ? 'yes' : 'no',
					refundViaDispute: params.refundViaDispute ? 'yes' : 'no',
				},
				{
					slots: (template) => ({
						...facts(
							[template.name_label, params.name],
							[template.order_number_label, params.invoiceNumber],
							[template.order_date_label, template.order_date_value],
							[template.refund_label, amountFormatted],
							[template.currency_label, params.currency.toUpperCase()],
						),
						lead: params.hasDonation ? `${template.refund}\n\n${template.donation}` : template.refund,
						...(params.hasDonation ? {cta_url: `${this.config.marketingBaseUrl}/donate/manage`} : {cta: null}),
					}),
					timeZone: params.timeZone,
				},
			);
		},
		operator_pass_offer_received: async (email, params, locale) => {
			const appUrl = `${this.config.appBaseUrl}/channels/@operator-pass`;
			return this.sendTemplatedEmail(
				email,
				'operator_pass_offer_received',
				locale,
				{
					senderName: params.senderName,
					arrivesAt: getOperatorPassUnlockDay(params.arrivesAt ?? new Date(), new Date(), params.timeZone),
				},
				{slots: () => ({cta_url: appUrl})},
			);
		},
		operator_pass_offer_accepted: async (email, params, locale) => {
			return this.sendTemplatedEmail(email, 'operator_pass_offer_accepted', locale, {
				recipientName: params.recipientName,
				remainingCount: params.remainingCount,
			});
		},
		operator_pass_offer_ended: async (email, params, locale) => {
			return this.sendTemplatedEmail(email, 'operator_pass_offer_ended', locale, {
				otherName: params.otherName,
				ended: params.ended,
			});
		},
		operator_pass_revoked: async (email, params, locale) => {
			return this.sendTemplatedEmail(email, 'operator_pass_revoked', locale, {
				cause: params.cause,
				lastPass: params.lastPass ? 'yes' : 'no',
			});
		},
		operator_pass_removal_notice: async (email, params, locale) => {
			const termsUrl = getOperatorPassTermsUrl(this.config.marketingBaseUrl);
			return this.sendTemplatedEmail(
				email,
				'operator_pass_removal_notice',
				locale,
				{replyDays: daysUntil(params.replyBy)},
				{
					slots: (template) => ({
						...facts(
							[template.rule_label, params.rule],
							[template.facts_label, params.facts],
							[template.ground_label, template.ground, termsUrl],
						),
						text_1: template.reply,
						text_2: template.next,
					}),
				},
			);
		},
		operator_pass_wall_hidden: async (email, params, locale) => {
			const termsUrl = getOperatorPassTermsUrl(this.config.marketingBaseUrl);
			return this.sendTemplatedEmail(
				email,
				'operator_pass_wall_hidden',
				locale,
				{reason: params.reason},
				{
					slots: (template) => ({
						...facts(
							[template.reason_label, template.reason_value],
							[template.facts_label, params.note],
							[template.ground_label, template.ground, termsUrl],
						),
						text_1: template.automated_text,
						text_2: template.next,
						...appealSection(template),
					}),
				},
			);
		},
		operator_pass_wall_name_replaced: async (email, params, locale) => {
			const termsUrl = getOperatorPassTermsUrl(this.config.marketingBaseUrl);
			return this.sendTemplatedEmail(
				email,
				'operator_pass_wall_name_replaced',
				locale,
				{target: params.target, shown: params.shown, reason: params.reason},
				{
					slots: (template) => ({
						...facts(
							[template.reason_label, template.reason_value],
							[template.facts_label, params.note],
							[template.ground_label, template.ground, termsUrl],
						),
						text_1: template.automated_text,
						text_2: template.next,
						...appealSection(template),
					}),
				},
			);
		},
		operator_pass_perks_removed: async (email, params, locale) => {
			const termsUrl = getOperatorPassTermsUrl(this.config.marketingBaseUrl);
			return this.sendTemplatedEmail(
				email,
				'operator_pass_perks_removed',
				locale,
				{
					scope: params.until ? 'temporary' : 'permanent',
					until: params.until ?? new Date(0),
					automated: params.automated ? 'yes' : 'no',
				},
				{
					slots: (template) => ({
						...facts(
							[template.rule_label, params.rule],
							[template.facts_label, params.facts],
							[template.ground_label, template.ground, termsUrl],
						),
						text_1: template.automated_text,
						text_2: template.refund,
						...appealSection(template),
					}),
				},
			);
		},
		payment_dispute_warning: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'payment_dispute_warning',
				locale,
				getPaymentDisputeWarningVariables(params, this.config),
				{slots: (template) => paymentDisputeWarningSlots(template, params, locale, this.config.marketingBaseUrl)},
			);
		},
		payment_dispute_review_alert: async (email, params) => {
			const product = params.product === 'operator_pass' ? 'Operator Pass' : 'Plutonium';
			const lines = [
				`A ${product} payment dispute opened and needs a review.`,
				'',
				`Dispute: ${params.disputeId}`,
				...(params.invoiceNumber ? [`Invoice: ${params.invoiceNumber}`] : []),
				`Account: ${params.purchaserId}`,
				`Amount: ${formatMinorUnitPrice(params.amountMinor, params.currency, PAYMENT_DISPUTE_REVIEW_LOCALE)}`,
				`Reason: ${params.reason}`,
				`Evidence due: ${params.evidenceDueBy?.toISOString() ?? 'not given'}`,
				params.blockers.length > 0
					? `The terms rule out finding it unjustified: ${params.blockers.join(', ')}.`
					: 'Nothing in the terms rules out finding it unjustified.',
				'',
				`Review it: ${params.reviewUrl}`,
			];
			return this.sendPlainEmail(email, `${product} payment dispute ${params.disputeId}`, lines.join('\n'));
		},
		plutonium_payment_receipt: async (email, params, locale) => {
			const gift = isGiftPlan(params.plan);
			const totals = formatTotals(params.totals, locale);
			const invoice = params.invoice;
			const purchase = params.purchase;
			const renewal = purchase?.renewal ?? null;
			const withdrawal = purchase?.seller && purchase.withdrawalEndsAt ? purchase.withdrawalEndsAt : null;
			return this.sendTemplatedEmail(
				email,
				'plutonium_payment_receipt',
				locale,
				{
					premiumName: params.premiumName,
					plan: params.plan,
					gift: yesNo(gift),
					paidAt: params.paidAt,
					periodStart: params.period?.start ?? params.paidAt,
					periodEnd: params.period?.end ?? params.paidAt,
					amountFormatted: totals.total,
					taxFormatted: totals.tax ?? '',
					pdf: yesNo(invoice?.pdf != null),
					invoiceLinkUntil: invoice?.linkUntil ?? params.paidAt,
					renewal: renewal?.interval ?? 'none',
					renewalFormatted: renewal ? formatMinorUnitPrice(renewal.amountMinor, params.totals.currency, locale) : '',
					renewalTax: renewal?.tax ?? 'none',
					consentAt: purchase?.consentAt ?? params.paidAt,
					withdrawalEndsAt: withdrawal ?? params.paidAt,
					invoiceNumber: params.invoiceNumber ?? '',
					sellerAddress: SELLER_ADDRESS,
					sellerEmail: OPERATOR_PASS_TRADER_EMAIL,
				},
				{
					slots: (template) => ({
						...summary(
							[
								[template.invoice_label, params.invoiceNumber],
								[template.date_label, template.date_value],
								[template.product_label, template.product_value],
								[template.period_label, params.period && !gift ? template.period_value : null],
							],
							[template.total_label, totals.totalWithCode],
							taxLine(template, params.totals),
							[
								[template.credit_label, totals.credit],
								[template.paid_label, totals.paid],
							],
						),
						...invoiceSlots(template, invoice),
						lead:
							[gift ? template.gift_note : null, invoice ? template.lead : null].filter(Boolean).join('\n\n') || null,
						...(purchase
							? {
									section_1_title: template.plan_title,
									section_1_body: template.plan_body,
									...(withdrawal && purchase.consentAt
										? {section_2_title: template.consent_title, section_2_body: template.consent_body}
										: {}),
									...(withdrawal
										? {
												section_3_title: template.withdrawal_title,
												section_3_body: template.withdrawal_body,
												section_3_note: template.withdrawal_note,
												section_4_title: template.form_title,
												section_4_body: template.form_body,
											}
										: {}),
									note: purchase.seller ? template.seller : null,
								}
							: {}),
						end_link: gift ? null : template.manage_link,
						end_link_url: gift ? null : plutoniumUrl(this.config.appBaseUrl),
					}),
					attachments: invoice?.pdf ? [invoice.pdf] : undefined,
					timeZone: params.timeZone,
				},
			);
		},
		plutonium_refund_receipt: async (email, params, locale) => {
			const amountFormatted = formatMinorUnitPrice(params.amountMinor, params.currency, locale);
			return this.sendTemplatedEmail(
				email,
				'plutonium_refund_receipt',
				locale,
				{
					premiumName: params.premiumName,
					plan: params.plan,
					refundedAt: params.refundedAt,
					amountFormatted,
					paidFormatted: formatMinorUnitPrice(params.paidMinor, params.currency, locale),
					paidAt: params.paidAt,
					partial: yesNo(params.amountMinor < params.paidMinor),
					requestedFormatted: params.subscriptionEnded
						? formatMoment(params.subscriptionEnded.requestedAt, locale, params.timeZone)
						: '',
					reference: params.subscriptionEnded?.reference ?? '',
				},
				{
					slots: (template) => ({
						...summary(
							[
								[template.invoice_label, params.invoiceNumber],
								[template.product_label, template.product_value],
								[template.original_label, template.original_value],
								[template.date_label, template.date_value],
							],
							[template.refund_label, amountWithCode(params.amountMinor, params.currency, locale)],
						),
						...(params.refundUrl ? {cta_url: params.refundUrl} : {cta: null}),
						lead: params.subscriptionEnded ? template.ended_line : null,
					}),
					timeZone: params.timeZone,
				},
			);
		},
		plutonium_payment_failed: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'plutonium_payment_failed',
				locale,
				{
					premiumName: params.premiumName,
					reason: params.reason,
					attemptedAt: params.attemptedAt,
					amountFormatted: formatMinorUnitPrice(params.amountMinor, params.currency, locale),
					access: yesNo(params.accessEndsAt != null),
					accessEndsAt: params.accessEndsAt ?? params.attemptedAt,
				},
				{
					slots: (template) => ({
						...summary(
							[
								[template.invoice_label, params.invoiceNumber],
								[template.date_label, template.date_value],
							],
							[template.amount_label, amountWithCode(params.amountMinor, params.currency, locale)],
						),
						...(params.invoiceUrl ? {cta_url: params.invoiceUrl} : {cta: null}),
						end_link: template.manage_link,
						end_link_url: plutoniumUrl(this.config.appBaseUrl),
						...(params.reminder ? {subject: template.reminder_subject} : {}),
						...(params.planChange ? {intro: template.intro_change} : {}),
					}),
					timeZone: params.timeZone,
				},
			);
		},
		plutonium_renewal_notice: async (email, params, locale) => {
			const gift = params.giftEndsAt != null;
			const annual = params.variant === 'annual';
			return this.sendTemplatedEmail(
				email,
				'plutonium_renewal_notice',
				locale,
				{
					...this.upcomingChargeVariables(params, locale),
					premiumName: params.premiumName,
					gift: yesNo(gift),
					annual: yesNo(annual),
					plan: params.plan,
					giftEndsAt: params.giftEndsAt ?? params.chargeAt,
					yearStartsAt: params.yearStartsAt ?? params.chargeAt,
				},
				{
					slots: (template) => ({
						intro: paragraphsOf(
							annual ? template.annual_line : null,
							gift ? template.gift_line : null,
							gift || annual ? null : template.intro,
							template.charge_line,
							params.yearStartsAt ? template.year_line : null,
							template.after_line,
						),
						cta_url: plutoniumUrl(this.config.appBaseUrl),
						text_1: template.keep_line,
						note: params.seller ? template.seller : null,
					}),
					timeZone: params.timeZone,
				},
			);
		},
		donation_renewal_notice: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'donation_renewal_notice',
				locale,
				this.upcomingChargeVariables(params, locale),
				{
					slots: (template) => ({
						intro: paragraphsOf(template.intro, template.charge_line, template.after_line),
						cta_url: params.manageUrl,
						text_1: template.keep_line,
						note: params.seller ? template.seller : null,
					}),
					timeZone: params.timeZone,
				},
			);
		},
		plutonium_card_expiring: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'plutonium_card_expiring',
				locale,
				{...this.cardExpiringVariables(params, locale), premiumName: params.premiumName},
				{
					slots: (template) => ({
						cta_url: plutoniumUrl(this.config.appBaseUrl),
						note: params.seller ? template.seller : null,
					}),
					timeZone: params.timeZone,
				},
			);
		},
		donation_card_expiring: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'donation_card_expiring',
				locale,
				this.cardExpiringVariables(params, locale),
				{
					slots: (template) => ({cta_url: params.manageUrl, note: params.seller ? template.seller : null}),
					timeZone: params.timeZone,
				},
			);
		},
		plutonium_cancellation_confirmation: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'plutonium_cancellation_confirmation',
				locale,
				{
					...this.cancellationVariables(params, locale),
					premiumName: params.premiumName,
					plan: params.plan,
				},
				{
					slots: (template) => ({
						intro: paragraphsOf(template.intro, template.ends_line, template.charge_line),
						cta_url: plutoniumUrl(this.config.appBaseUrl),
						text_1: template.reference_line,
						note: params.seller ? template.seller : null,
					}),
					timeZone: params.timeZone,
				},
			);
		},
		donation_cancellation_confirmation: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'donation_cancellation_confirmation',
				locale,
				this.cancellationVariables(params, locale),
				{
					slots: (template) => ({
						intro: paragraphsOf(template.intro, template.ends_line, template.charge_line),
						cta_url: params.manageUrl,
						text_1: template.reference_line,
						note: params.seller ? template.seller : null,
					}),
					timeZone: params.timeZone,
				},
			);
		},
		plutonium_ended_unpaid: async (email, params, locale) => {
			const owed = ENDED_INVOICE_OWED[params.invoiceStatus];
			return this.sendTemplatedEmail(
				email,
				'plutonium_ended_unpaid',
				locale,
				{
					...sellerVariables(),
					premiumName: params.premiumName,
					numbered: yesNo(params.invoiceNumber != null),
					invoiceNumber: params.invoiceNumber ?? '',
					owed,
					amountFormatted: amountWithCode(params.amountMinor, params.currency, locale),
				},
				{
					slots: (template) => ({
						intro: paragraphsOf(template.intro, template.owed_line),
						cta_url: plutoniumUrl(this.config.appBaseUrl),
						note: params.seller ? template.seller : null,
						...(owed === 'open' && params.invoiceUrl
							? {end_link: template.invoice_link, end_link_url: params.invoiceUrl}
							: {}),
					}),
				},
			);
		},
		plutonium_gift_applied: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'plutonium_gift_applied',
				locale,
				{
					...sellerVariables(),
					...paymentMethodVariables(params.paymentMethod),
					premiumName: params.premiumName,
					plan: params.plan,
					trialEnd: params.trialEnd,
					amountFormatted: chargeFormatted(params.amount, locale),
					tax: yesNo(params.taxIncluded),
				},
				{
					slots: (template) => ({
						intro: paragraphsOf(template.intro, template.charge_line),
						cta_url: plutoniumUrl(this.config.appBaseUrl),
						note: params.seller ? template.seller : null,
					}),
					timeZone: params.timeZone,
				},
			);
		},
		billing_credit_note: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'billing_credit_note',
				locale,
				{
					...sellerVariables(),
					amountFormatted: amountWithCode(params.amountMinor, params.currency, locale),
					numbered: yesNo(params.invoiceNumber != null),
					invoiceNumber: params.invoiceNumber ?? '',
					upcoming: yesNo(params.upcomingInvoice),
				},
				{
					slots: (template) => ({
						note: params.seller ? template.seller : null,
						...(params.pdfUrl ? {end_link: template.credit_note_link, end_link_url: params.pdfUrl} : {}),
					}),
				},
			);
		},
		operator_pass_invoice: async (email, params, locale) => {
			const totals = formatTotals(params.totals, locale);
			return this.sendTemplatedEmail(
				email,
				'operator_pass_invoice',
				locale,
				{
					paidAt: params.paidAt,
					amountFormatted: totals.total,
					taxFormatted: totals.tax ?? '',
					pdf: yesNo(params.invoice.pdf != null),
					invoiceLinkUntil: params.invoice.linkUntil,
				},
				{
					slots: (template) => ({
						...summary(
							[
								[template.invoice_label, params.invoiceNumber],
								[template.date_label, template.date_value],
							],
							[template.total_label, totals.totalWithCode],
							taxLine(template, params.totals),
						),
						...invoiceSlots(template, params.invoice),
					}),
					attachments: params.invoice.pdf ? [params.invoice.pdf] : undefined,
					timeZone: params.timeZone,
				},
			);
		},
		operator_pass_refund_receipt: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'operator_pass_refund_receipt',
				locale,
				{
					orderDate: params.orderDate,
					refundedAt: params.refundedAt,
					amountFormatted: formatMinorUnitPrice(params.amountMinor, params.currency, locale),
					full: yesNo(params.full),
				},
				{
					slots: (template) => ({
						...summary(
							[
								[template.order_number_label, params.invoiceNumber],
								[template.order_date_label, template.order_date_value],
								[template.date_label, template.date_value],
							],
							[template.refund_label, amountWithCode(params.amountMinor, params.currency, locale)],
						),
					}),
					timeZone: params.timeZone,
				},
			);
		},
		operator_pass_waitlist_verification: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'operator_pass_waitlist_verification',
				locale,
				{},
				{slots: () => ({cta_url: params.confirmUrl})},
			);
		},
		operator_pass_sale_open: async (email, params, locale) => {
			return this.sendTemplatedEmail(
				email,
				'operator_pass_sale_open',
				locale,
				{
					priceFormatted: params.priceFormatted,
					priceEndsOn: getOperatorPassEarlyBirdEndsOn(params.priceRisesAt),
				},
				{
					slots: (template) => ({
						...facts([template.price_label, template.price_value]),
						points_list: `${template.included_perks}\n${template.included_bonus}`,
						cta_url: params.checkoutUrl,
						end_link: template.terms_link,
						end_link_url: getOperatorPassTermsUrl(this.config.marketingBaseUrl),
						unsubscribe_url: params.unsubscribePageUrl,
					}),
					listUnsubscribe: {url: params.unsubscribeUrl, post: true},
				},
			);
		},
	};

	private async renderTermsAttachment(locale: string | null): Promise<EmailAttachment | null> {
		const language = resolveOperatorPassTermsLanguage(locale);
		try {
			const {getOperatorPassTermsPdfFilename, renderOperatorPassTermsPdf} = await import(
				'@pkgs/email/src/operator_pass_terms/OperatorPassTermsPdf'
			);
			return {
				filename: getOperatorPassTermsPdfFilename(language),
				content: await renderOperatorPassTermsPdf(language, this.config.marketingBaseUrl),
				contentType: 'application/pdf',
			};
		} catch (error) {
			logger.error({language, error}, 'Failed to render the Operator Pass terms PDF');
			return null;
		}
	}

	async isEmailBounced(email: string): Promise<boolean> {
		return (await this.bouncedEmailChecker?.isEmailBounced(email)) ?? false;
	}

	async openBulkSession(): Promise<IBulkEmailSession> {
		const bulkProvider = this.config.enabled ? this.provider?.openBulk?.() : undefined;
		if (!bulkProvider) {
			return {
				send: (templateKey, email, params, locale) => this.send(templateKey, email, params, locale),
				close: async () => {},
			};
		}
		const pooled = new EmailService(this.config, bulkProvider, this.bouncedEmailChecker);
		return {
			send: (templateKey, email, params, locale) => pooled.send(templateKey, email, params, locale),
			close: () => bulkProvider.close(),
		};
	}

	private async sendPlainEmail(email: string, subject: string, text: string): Promise<boolean> {
		if (!this.config.enabled || !this.provider) {
			logger.info(`Email service disabled. Would have sent:\nTo: ${email}\nSubject: ${subject}\n\n${text}`);
			return true;
		}
		return this.provider.sendEmail({
			to: email,
			from: {email: this.config.fromEmail, name: this.config.fromName},
			subject,
			text,
		});
	}

	private upcomingChargeVariables(
		params: EmailSendParams['donation_renewal_notice'] | EmailSendParams['plutonium_renewal_notice'],
		locale: string | null,
	) {
		return {
			...sellerVariables(),
			...paymentMethodVariables(params.paymentMethod),
			chargeAt: params.chargeAt,
			nextFormatted: chargeFormatted(params.next ?? params.regular, locale),
			regularFormatted: chargeFormatted(params.regular, locale),
			tax: yesNo(params.taxIncluded),
			differs: yesNo(params.next != null && params.next.totalMinor !== params.regular.totalMinor),
			cancelByFormatted: formatMoment(params.chargeAt, locale, params.timeZone),
		};
	}

	private cardExpiringVariables(
		params: EmailSendParams['donation_card_expiring'] | EmailSendParams['plutonium_card_expiring'],
		locale: string | null,
	) {
		const brand = cardBrandName(params.brand);
		return {
			...sellerVariables(),
			branded: yesNo(brand != null),
			brand: brand ?? '',
			last4: params.last4,
			expiryFormatted: formatCardExpiry(params.expMonth, params.expYear, locale),
			chargeAt: params.chargeAt,
			amountFormatted: chargeFormatted(params.amount, locale),
			tax: yesNo(params.taxIncluded),
		};
	}

	private cancellationVariables(
		params:
			| EmailSendParams['donation_cancellation_confirmation']
			| EmailSendParams['plutonium_cancellation_confirmation'],
		locale: string | null,
	) {
		return {
			...sellerVariables(),
			receivedFormatted: formatMoment(params.receivedAt, locale, params.timeZone),
			endsAt: params.endsAt,
			reference: params.reference,
			unpaid: yesNo(params.unpaid != null),
			unpaidFormatted: params.unpaid ? chargeFormatted(params.unpaid, locale) : '',
		};
	}

	private sendCode<T extends EmailTemplateKey>(
		email: string,
		templateKey: T,
		locale: string | null,
		code: string,
		variables: EmailTemplateVariables[T],
		slots: (template: EmailTemplate) => EmailSlots = () => ({}),
	): Promise<boolean> {
		return this.sendTemplatedEmail(email, templateKey, locale, variables, {
			slots: (template) => ({code, ...slots(template)}),
		});
	}

	private async sendTemplatedEmail<T extends EmailTemplateKey>(
		email: string,
		templateKey: T,
		locale: string | null,
		variables: EmailTemplateVariables[T],
		options: TemplatedEmailOptions = {},
	): Promise<boolean> {
		const replyTo = resolveEmailReplyTo(this.config, templateKey);
		const result = getEmailTemplate(
			templateKey,
			locale,
			{...variables, reply: replyTo ? 'yes' : 'no'},
			this.config.productName,
			options.timeZone ?? null,
		);
		if (!result.ok) {
			logger.error({key: templateKey, locale: result.locale, error: result.error}, 'Failed to resolve email template');
			return false;
		}
		const slots = this.composeSlots(result.locale, result.value, options);
		const subject = restoreEmailLiterals(slots.subject ?? result.value.subject);
		const text = renderEmailText(slots);
		const html = this.renderHtml(templateKey, result.locale, slots);
		const attachments = options.attachments;
		if (!this.config.enabled || !this.provider) {
			logger.info(
				{
					templateKey,
					htmlBytes: html ? Buffer.byteLength(html, 'utf8') : 0,
					attachments: attachments?.map((attachment) => attachment.filename) ?? [],
				},
				`Email service disabled. Would have sent:\nTo: ${email}\nSubject: ${subject}\n\n${text}`,
			);
			return true;
		}
		if (this.bouncedEmailChecker) {
			const bounced = await this.bouncedEmailChecker.isEmailBounced(email);
			if (bounced) {
				logger.warn({email}, 'Refusing to send email to bounced address - email marked as hard bounced');
				return false;
			}
		}
		return this.provider.sendEmail({
			to: email,
			from: {email: this.config.fromEmail, name: this.config.fromName},
			...(replyTo ? {replyTo} : {}),
			subject,
			text,
			...(html ? {html} : {}),
			...(options.listUnsubscribe ? {listUnsubscribe: options.listUnsubscribe} : {}),
			...(attachments?.length ? {attachments} : {}),
		});
	}

	private composeSlots(locale: string, template: EmailTemplate, options: TemplatedEmailOptions): EmailSlots {
		const assetsUrl = `${this.config.staticCdnBaseUrl.replace(/\/+$/, '')}${EMAIL_ASSETS_PATH}`;
		return {
			...template,
			preheader: stripInlineMarkup(template.preheader ?? template.intro),
			lang: locale,
			product_name: this.config.productName,
			home_url: this.config.marketingBaseUrl,
			logo_url: this.config.brandLogo ? `${assetsUrl}/brand/v1/logo-color.png` : null,
			logo_dark_url: this.config.brandLogo ? `${assetsUrl}/brand/v1/logo-white.png` : null,
			wordmark: this.config.brandLogo ? null : this.config.productName,
			legal_address: this.config.legalAddress,
			...emailLinkUrls(this.config),
			...options.slots?.(template),
		};
	}

	private renderHtml(templateKey: EmailTemplateKey, locale: string, slots: EmailSlots): string | null {
		try {
			return renderEmailHtml(getEmailTextDirection(locale), slots, this.ownHttpOrigins, this.linkHosts);
		} catch (error) {
			logger.error({templateKey, locale, error}, 'Failed to render the HTML part, sending plaintext only');
			return null;
		}
	}
}
