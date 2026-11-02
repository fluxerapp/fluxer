// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getContentMessage, resolveContentLocale} from '@app/api/content_i18n/ContentI18n';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import {Logger} from '@app/api/Logger';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getPaymentDisputeRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {validateOperatorPassCertificateName} from '@app/api/operator_pass/OperatorPassCertificateService';
import {
	isOperatorPassPermanentlyRemoved,
	type OperatorPassEntitlementService,
	resolveOperatorPassHolder,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {
	type OperatorPassPreviewMatch,
	resolveOperatorPassPreview,
} from '@app/api/operator_pass/OperatorPassPreviewAccess';
import {
	OPERATOR_PASS_WITHDRAWAL_RULE_VERSION,
	type OperatorPassWithdrawalRuleMatch,
	resolveOperatorPassWithdrawalRule,
} from '@app/api/operator_pass/OperatorPassPrices';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {
	getOperatorPassPhase,
	getOperatorPassPriceMinor,
	getOperatorPassSchedule,
	getOperatorPassSessionExpiresAt,
	issueOperatorPassQuote,
	type OperatorPassSchedule,
	operatorPassNow,
	resolveOperatorPassCheckoutPrice,
	resolveOperatorPassCurrency,
} from '@app/api/operator_pass/OperatorPassSchedule';
import {
	hasOperatorPassWallChoice,
	normalizeOperatorPassWallChoice,
} from '@app/api/operator_pass/OperatorPassWallPresentation';
import {assertNoOpenPaymentDispute, hasOpenPaymentDispute} from '@app/api/payment_disputes/PaymentDisputeReviews';
import {getBillingBranding} from '@app/api/stripe/BillingBranding';
import {getStripeCheckoutLocale} from '@app/api/stripe/StripeCheckoutLocale';
import {assertUserCanPurchase} from '@app/api/stripe/services/StripeCheckoutService';
import {resolvePurchaseEligibility} from '@app/api/user/AccountTier';
import {shouldDisableAdaptivePricing} from '@app/api/utils/CurrencyUtils';
import {PremiumFlags} from '@fluxer/constants/src/UserConstants';
import {PurchaseEmailVerificationRequiredError} from '@fluxer/errors/src/domains/auth/EmailVerificationRequiredError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {OperatorPassAgeConfirmationRequiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassAgeConfirmationRequiredError';
import {OperatorPassDeliveryConsentRequiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassDeliveryConsentRequiredError';
import {OperatorPassEmailUndeliverableError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassEmailUndeliverableError';
import {OperatorPassNotOnSaleError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassNotOnSaleError';
import {OperatorPassPriceChangedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassPriceChangedError';
import {OperatorPassRemovedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassRemovedError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {
	OPERATOR_PASS_DELIVERY_BUSINESS_VERSION,
	OPERATOR_PASS_DELIVERY_COUNTRY_VERSION,
	OPERATOR_PASS_DELIVERY_WAIVER_VERSION,
	type OperatorPassCheckoutRequest,
	type OperatorPassCurrency,
	type OperatorPassPurchaseBlockedReason,
	type OperatorPassStateResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {
	getOperatorPassTermsUrl,
	OPERATOR_PASS_TERMS_VERSION,
} from '@pkgs/email/src/operator_pass_terms/OperatorPassTerms';
import type Stripe from 'stripe';

type CheckoutSessionCreateParams = Stripe.Checkout.SessionCreateParams;
type CheckoutLineItem = NonNullable<CheckoutSessionCreateParams['line_items']>[number];

const PASS_TAX_CODE = 'txcd_10000000';
const PREVIEW_COUPON_NAME = 'Operator Pass preview';

async function resolveSalePreview(
	schedule: OperatorPassSchedule,
	now: Date,
	clientIp: string | null,
): Promise<OperatorPassPreviewMatch | null> {
	if (getOperatorPassPhase(now, schedule) !== 'waitlist') {
		return null;
	}
	return resolveOperatorPassPreview(getKVClient(), clientIp, now);
}

function normalizeCountryCode(countryCode: string | null | undefined): string | null {
	const normalized = countryCode?.trim().toUpperCase();
	return normalized && /^[A-Z]{2}$/u.test(normalized) ? normalized : null;
}

function resolveCheckoutWithdrawalRule(
	countryCode: string | null,
	currency: OperatorPassCurrency,
): OperatorPassWithdrawalRuleMatch | null {
	return resolveOperatorPassWithdrawalRule(Config.operatorPass.withdrawalCountries, [countryCode], currency);
}

function getOperatorPassDeliveryBoxText(
	locale: string | null,
	quantity: number,
	rule: OperatorPassWithdrawalRuleMatch | null,
	businessCnpj: string | null,
): string {
	if (businessCnpj) {
		return quantity > 1
			? getContentMessage('billing.operator_pass_delivery_business_multiple', locale)
			: getContentMessage('billing.operator_pass_delivery_business', locale);
	}
	if (rule) {
		return quantity > 1
			? getContentMessage('billing.operator_pass_delivery_country_multiple_v5', locale, {days: rule.days})
			: getContentMessage('billing.operator_pass_delivery_country_v5', locale, {days: rule.days});
	}
	return quantity > 1
		? getContentMessage('billing.operator_pass_delivery_waiver_multiple', locale)
		: getContentMessage('billing.operator_pass_delivery_waiver', locale);
}

function getOperatorPassDeliveryBoxVersion(
	rule: OperatorPassWithdrawalRuleMatch | null,
	businessCnpj: string | null,
): string {
	if (businessCnpj) return OPERATOR_PASS_DELIVERY_BUSINESS_VERSION;
	return rule ? OPERATOR_PASS_DELIVERY_COUNTRY_VERSION : OPERATOR_PASS_DELIVERY_WAIVER_VERSION;
}

async function resolveCertificateNames(
	names: ReadonlyArray<string> | undefined,
	quantity: number,
	fallback: string,
): Promise<Array<string>> {
	const chosen = names && names.length > 0 ? names : [fallback];
	if (chosen.length !== 1 && chosen.length !== quantity) {
		throw InputValidationError.create('certificate_names', 'Give one name, or one name for every pass');
	}
	const normalized: Array<string> = [];
	for (const [index, name] of chosen.entries()) {
		normalized.push(await validateOperatorPassCertificateName(name, index));
	}
	return normalized.length === quantity ? normalized : Array.from({length: quantity}, () => normalized[0]!);
}

function getOperatorPassEmailProblem(user: User): 'email_unverified' | 'email_undeliverable' | null {
	if (!user.email || !user.emailVerified) {
		return 'email_unverified';
	}
	return user.emailBounced ? 'email_undeliverable' : null;
}

function assertOperatorPassEmailDeliverable(user: User): void {
	const problem = getOperatorPassEmailProblem(user);
	if (problem === 'email_unverified') {
		throw new PurchaseEmailVerificationRequiredError();
	}
	if (problem === 'email_undeliverable') {
		throw new OperatorPassEmailUndeliverableError();
	}
}

function resolveOperatorPassPurchaseBlockedReason(params: {
	phase: OperatorPassStateResponse['phase'];
	user: User;
	removed: boolean;
	paymentDisputeOpen: boolean;
}): OperatorPassPurchaseBlockedReason | null {
	const {phase, user, removed, paymentDisputeOpen} = params;
	if (phase === 'waitlist') {
		return 'not_on_sale';
	}
	if (removed) {
		return 'removed';
	}
	const eligibility = resolvePurchaseEligibility(user);
	if (!eligibility.allowed) {
		return eligibility.blocked;
	}
	const emailProblem = getOperatorPassEmailProblem(user);
	if (emailProblem) {
		return emailProblem;
	}
	if (user.premiumFlags & PremiumFlags.PURCHASE_DISABLED) {
		return 'purchase_disabled';
	}
	if (paymentDisputeOpen) {
		return 'payment_dispute_open';
	}
	return null;
}

interface OperatorPassStateParams {
	user: User | null;
	countryCode: string | null;
	clientIp: string | null;
}

interface OperatorPassCheckoutParams {
	user: User;
	request: OperatorPassCheckoutRequest;
	geoipCountryCode: string | null;
	clientIp: string | null;
	userAgent: string | null;
}

export class OperatorPassCheckoutService {
	constructor(
		private readonly stripe: Stripe | null,
		private readonly repository: OperatorPassRepository,
		private readonly entitlementService: OperatorPassEntitlementService,
	) {}

	async getState({user, countryCode, clientIp}: OperatorPassStateParams): Promise<OperatorPassStateResponse> {
		const schedule = getOperatorPassSchedule();
		const now = operatorPassNow(schedule);
		const preview = await resolveSalePreview(schedule, now, clientIp);
		const phase = getOperatorPassPhase(now, schedule, preview !== null);
		const country = normalizeCountryCode(countryCode);
		const currency = resolveOperatorPassCurrency(country);
		const rule = resolveCheckoutWithdrawalRule(country, currency);
		const priceMinor = getOperatorPassPriceMinor(phase, currency, schedule);
		const entitlement = user ? await this.entitlementService.loadState(user.id) : null;
		const holder = user && entitlement ? resolveOperatorPassHolder(entitlement, now) : null;
		const wallChosen = holder && user ? hasOperatorPassWallChoice(await this.repository.findWallEntry(user.id)) : false;
		return {
			phase,
			now: now.toISOString(),
			sale_opens_at: schedule.saleOpensAt?.toISOString() ?? null,
			price_rises_at: schedule.priceRisesAt?.toISOString() ?? null,
			currency,
			price_minor: priceMinor,
			max_quantity: Config.operatorPass.maxQuantity,
			withdrawal_rule: rule ? {country: rule.country, days: rule.days} : null,
			standard_price_minor: schedule.standardPricesMinor?.[currency] ?? null,
			quote: issueOperatorPassQuote({
				now,
				phase,
				currency,
				amountMinor: priceMinor,
				userId: user ? user.id.toString() : null,
			}),
			holder: holder
				? {
						since: holder.since.toISOString(),
						source: holder.source,
						count: Math.max(1, entitlement?.pass?.pass_count ?? 1),
						recognition_count: holder.count,
						wall_chosen: wallChosen,
					}
				: null,
			purchase_blocked_reason: user
				? resolveOperatorPassPurchaseBlockedReason({
						phase,
						user,
						removed: isOperatorPassPermanentlyRemoved(entitlement?.removal ?? null),
						paymentDisputeOpen: await hasOpenPaymentDispute(getPaymentDisputeRepository(), user.id),
					})
				: null,
		};
	}

	async createCheckoutSession({
		user,
		request,
		geoipCountryCode,
		clientIp,
		userAgent,
	}: OperatorPassCheckoutParams): Promise<{url: string; sessionId: string; expiresAt: Date}> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const quantity = request.quantity ?? 1;
		if (quantity > Config.operatorPass.maxQuantity) {
			throw InputValidationError.create('quantity', `At most ${Config.operatorPass.maxQuantity} passes per checkout`);
		}
		const schedule = getOperatorPassSchedule();
		const now = operatorPassNow(schedule);
		const preview = await resolveSalePreview(schedule, now, clientIp);
		const phase = getOperatorPassPhase(now, schedule, preview !== null);
		if (phase === 'waitlist') {
			throw new OperatorPassNotOnSaleError();
		}
		assertUserCanPurchase(user);
		assertOperatorPassEmailDeliverable(user);
		await assertNoOpenPaymentDispute(getPaymentDisputeRepository(), user.id);
		const entitlement = await this.entitlementService.loadState(user.id);
		const {removal} = entitlement;
		if (isOperatorPassPermanentlyRemoved(removal)) {
			throw new OperatorPassRemovedError();
		}
		const pricingCountryCode = normalizeCountryCode(geoipCountryCode) ?? normalizeCountryCode(request.country_code);
		const currency = resolveOperatorPassCurrency(pricingCountryCode);
		const countryRule = resolveCheckoutWithdrawalRule(pricingCountryCode, currency);
		const businessCnpj = countryRule?.country === 'BR' && request.is_business === true ? (request.cnpj ?? null) : null;
		const rule = businessCnpj ? null : countryRule;
		if (request.delivery_consent !== true) {
			throw new OperatorPassDeliveryConsentRequiredError();
		}
		if (request.age_confirmed !== true) {
			throw new OperatorPassAgeConfirmationRequiredError();
		}
		const certificateNames = await resolveCertificateNames(
			request.certificate_names,
			quantity,
			user.globalName ?? user.username,
		);
		const keepsListing =
			request.wall_opt_out == null &&
			!request.wall_name &&
			resolveOperatorPassHolder(entitlement, now) !== null &&
			hasOperatorPassWallChoice(await this.repository.findWallEntry(user.id));
		const wallChoice = keepsListing
			? null
			: normalizeOperatorPassWallChoice({name: request.wall_name, optOut: request.wall_opt_out});
		const userId = user.id.toString();
		const price = resolveOperatorPassCheckoutPrice({
			now,
			schedule,
			currency,
			userId,
			quote: request.quote,
			previewUnlocked: preview !== null,
		});
		if (price.priceChanged) {
			throw new OperatorPassPriceChangedError({
				phase: price.phase,
				currency,
				price_minor: price.amountMinor,
				quote: issueOperatorPassQuote({
					now,
					phase: price.phase,
					currency,
					amountMinor: price.amountMinor,
					userId,
				}),
			});
		}
		const locale = user.locale ?? null;
		const branding = await getBillingBranding();
		const consentAt = now;
		const amountMinor = preview ? 0 : price.amountMinor * quantity;
		const consentLocale = request.consent_locale ? resolveContentLocale(request.consent_locale) : locale;
		const deliveryText = getOperatorPassDeliveryBoxText(consentLocale, quantity, rule, businessCnpj);
		const consentVersion = getOperatorPassDeliveryBoxVersion(rule, businessCnpj);
		const metadata: Record<string, string> = {
			operator_pass: 'true',
			operator_pass_purchaser_id: userId,
			operator_pass_quantity: String(quantity),
			operator_pass_unit_amount_minor: String(price.amountMinor),
			operator_pass_phase: price.pricedPhase,
			operator_pass_amount_minor: String(amountMinor),
			operator_pass_currency: currency,
			operator_pass_quote_honored: price.quoteHonored ? 'true' : 'false',
			operator_pass_locale: locale ?? 'en-US',
			operator_pass_waiver_consent_at: consentAt.toISOString(),
			operator_pass_waiver_consent_version: consentVersion,
			operator_pass_age_confirmed: 'true',
			...(rule ? {operator_pass_withdrawal_country: rule.country} : {}),
			...(businessCnpj ? {operator_pass_cnpj: businessCnpj} : {}),
			...(preview ? {operator_pass_preview: 'true', operator_pass_preview_label: preview.label} : {}),
		};
		if (wallChoice) {
			metadata.operator_pass_wall_opt_out = wallChoice.optedOut ? 'true' : 'false';
		}
		if (request.labs_greeting !== undefined) {
			metadata.operator_pass_labs_greeting = request.labs_greeting ? 'true' : 'false';
		}
		metadata.operator_pass_terms_version = OPERATOR_PASS_TERMS_VERSION;
		const productName = branding.productName;
		const passLine: CheckoutLineItem = {
			price_data: {
				currency: currency.toLowerCase(),
				unit_amount: price.amountMinor,
				tax_behavior: 'inclusive',
				product_data: {
					name: getContentMessage('billing.operator_pass_name', locale, {product_name: productName}),
					description: getContentMessage('billing.operator_pass_description_v2', locale),
					tax_code: PASS_TAX_CODE,
				},
			},
			quantity,
		};
		const expiresAt = getOperatorPassSessionExpiresAt(price.pricedPhase, now, schedule);
		const previewCoupon = preview ? await this.createPreviewCoupon(this.stripe, metadata, expiresAt, userId) : null;
		const email = user.email;
		const checkoutParams: CheckoutSessionCreateParams = {
			...(email ? {customer_email: email} : {}),
			client_reference_id: userId,
			metadata,
			mode: 'payment',
			line_items: [passLine],
			locale: getStripeCheckoutLocale(locale),
			automatic_tax: {enabled: true},
			tax_id_collection: {enabled: true},
			billing_address_collection: request.is_business === true ? 'required' : 'auto',
			...(previewCoupon ? {discounts: [{coupon: previewCoupon}]} : {allow_promotion_codes: false}),
			success_url: `${Config.endpoints.webApp}/premium-callback?status=success&product=operator_pass&session_id={CHECKOUT_SESSION_ID}`,
			cancel_url: `${Config.endpoints.webApp}/channels/@operator-pass?status=cancel`,
			expires_at: expiresAt,
			consent_collection: {terms_of_service: 'required'},
			custom_text: {
				terms_of_service_acceptance: {message: deliveryText},
				submit: {
					message: getContentMessage('billing.operator_pass_terms_submit', locale, {
						terms_url: branding.termsUrl,
						operator_pass_terms_url: getOperatorPassTermsUrl(Config.endpoints.marketing),
					}),
				},
			},
			...(currency === 'BRL' && !preview ? {payment_method_options: {pix: {amount_includes_iof: 'always'}}} : {}),
			...(shouldDisableAdaptivePricing() ? {adaptive_pricing: {enabled: false}} : {}),
			customer_creation: 'always',
			...(preview ? {} : {payment_intent_data: {metadata}}),
		};
		let session: Stripe.Checkout.Session;
		try {
			session = await this.stripe.checkout.sessions.create(checkoutParams);
		} catch (error) {
			Logger.error({error, userId}, 'Failed to create Operator Pass checkout session');
			throw new StripeError(error instanceof Error ? error.message : 'Failed to create checkout session');
		}
		if (!session.url) {
			Logger.error({userId, sessionId: session.id}, 'Stripe checkout session missing url');
			throw new StripeError('Stripe checkout session missing url');
		}
		const purchase: OperatorPassPurchaseRow = {
			checkout_session_id: session.id,
			purchaser_id: user.id,
			quantity,
			status: 'pending',
			phase: price.pricedPhase,
			amount_minor: amountMinor,
			currency,
			quote_honored: price.quoteHonored,
			payment_intent_id: null,
			invoice_id: null,
			created_at: new Date(),
			completed_at: null,
			refunded_at: null,
			waiver_consent_at: consentAt,
			waiver_consent_version: consentVersion,
			waiver_consent_text: deliveryText,
			waiver_consent_ip: clientIp,
			waiver_consent_user_agent: userAgent?.slice(0, 512) ?? null,
			wall_name: wallChoice?.name ?? null,
			wall_opt_out: wallChoice?.optedOut ?? null,
			withdrawal_country: rule?.country ?? null,
			withdrawal_days: rule?.days ?? null,
			withdrawal_rule_version: rule ? OPERATOR_PASS_WITHDRAWAL_RULE_VERSION : null,
			fraud_warning_at: null,
			stripe_terms_consent: null,
			operator_pass_terms_version: OPERATOR_PASS_TERMS_VERSION,
			purchased_at: null,
			withdrawal_ends_at: null,
			perks_granted_at: null,
			confirmation_sent_at: null,
			withdrawn_at: null,
			withdrawal_name: null,
			withdrawal_refund_id: null,
			withdrawal_channel: null,
			withdrawal_receipts_owed: null,
			invoice_number: null,
			invoice_document_id: null,
			dispute_id: null,
			dispute_reason: null,
			dispute_status: null,
			disputed_at: null,
			dispute_closed_at: null,
			certificate_names: certificateNames,
			age_confirmed_at: consentAt,
			delivered_at: null,
			delivery_recorded_at: null,
			welcome_sent_at: null,
			welcome_skipped_at: null,
			refusal_reason: null,
			charge_id: null,
			business_buyer: null,
			holder_had_pass: null,
			labs_greeting: request.labs_greeting ?? null,
			preview_label: preview?.label ?? null,
			version: 1,
		};
		await this.repository.createPurchase(purchase);
		if (preview) {
			Logger.info(
				{userId, sessionId: session.id, previewEntryId: preview.entryId},
				'Created an Operator Pass preview checkout at no cost',
			);
		}
		return {url: session.url, sessionId: session.id, expiresAt: new Date(expiresAt * 1000)};
	}

	private async createPreviewCoupon(
		stripe: Stripe,
		metadata: Record<string, string>,
		expiresAt: number,
		userId: string,
	): Promise<string> {
		try {
			const coupon = await stripe.coupons.create({
				percent_off: 100,
				duration: 'once',
				max_redemptions: 1,
				redeem_by: expiresAt,
				name: PREVIEW_COUPON_NAME,
				metadata: {
					operator_pass_preview: 'true',
					operator_pass_preview_label: metadata.operator_pass_preview_label ?? '',
					operator_pass_purchaser_id: userId,
				},
			});
			return coupon.id;
		} catch (error) {
			Logger.error({error, userId}, 'Failed to create the Operator Pass preview coupon');
			throw new StripeError(error instanceof Error ? error.message : 'Failed to create the preview coupon');
		}
	}
}
