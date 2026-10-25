// SPDX-License-Identifier: AGPL-3.0-or-later

import {getOperatorPassEarlyBirdEndsOn} from '@fluxer/constants/src/OperatorPassEarlyBird';
import {createLogger} from '@fluxer/logger/src/Logger';
import {formatMinorUnitPrice} from '@fluxer/number_utils/src/PriceFormatting';
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
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';
import type {EmailTemplateVariables} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
import type {EmailTemplate, EmailTemplateKey} from '@pkgs/email/src/email_i18n/EmailI18nTypes.generated';
import type {
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
	bulk_message_deletion_completed: 'privacy',
	donation_confirmation: 'support',
	donation_magic_link: 'support',
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
	operator_pass_offer_accepted: 'support',
	operator_pass_offer_ended: 'support',
	operator_pass_offer_received: 'support',
	operator_pass_perks_removed: 'appeals',
	operator_pass_purchase_confirmation: 'support',
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
		donation_confirmation: async (email, {amountCents, currency, interval, manageUrl}, locale) => {
			return this.sendTemplatedEmail(
				email,
				'donation_confirmation',
				locale,
				{amount: formatMinorUnitPrice(amountCents, currency, locale), interval},
				{
					slots: (template) => ({
						cta_url: manageUrl,
						...facts([template.amount_label, template.amount_value], [template.currency_label, currency.toUpperCase()]),
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
		gift_chargeback_notification: async (email, _params, locale) => {
			return this.sendTemplatedEmail(email, 'gift_chargeback_notification', locale, {});
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
		const subject = restoreEmailLiterals(result.value.subject);
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
			preheader: stripInlineMarkup(template.intro),
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
