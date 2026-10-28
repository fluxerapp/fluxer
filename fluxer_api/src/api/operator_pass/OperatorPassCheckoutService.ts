// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getContentMessage, resolveContentLocale} from '@app/api/content_i18n/ContentI18n';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import {getDonationProductData} from '@app/api/donation/DonationProductData';
import type {DonationRepository} from '@app/api/donation/DonationRepository';
import {Logger} from '@app/api/Logger';
import {getPaymentDisputeRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {
	isOperatorPassPermanentlyRemoved,
	type OperatorPassEntitlementService,
	resolveOperatorPassHolder,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {
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
import {PremiumFlags} from '@fluxer/constants/src/UserConstants';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {DonationAmountInvalidError} from '@fluxer/errors/src/domains/donation/DonationAmountInvalidError';
import {OperatorPassAgeConfirmationRequiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassAgeConfirmationRequiredError';
import {OperatorPassCountryUnavailableError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassCountryUnavailableError';
import {OperatorPassNotOnSaleError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassNotOnSaleError';
import {OperatorPassPriceChangedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassPriceChangedError';
import {OperatorPassRemovedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassRemovedError';
import {OperatorPassWaiverRequiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWaiverRequiredError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {
	getDonationAmountConstraints,
	isDonationAmountWithinConstraints,
} from '@fluxer/schema/src/domains/donation/DonationAmountUtils';
import type {DonationCurrency} from '@fluxer/schema/src/domains/donation/DonationSchemas';
import {
	OPERATOR_PASS_COUNTRY_CONSENT_VERSION,
	OPERATOR_PASS_WAIVER_CONSENT_VERSION,
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
const DONATION_TAX_CODE = 'txcd_00000000';

const OPERATOR_PASS_DONATION_PRESETS_MINOR: Readonly<Record<OperatorPassCurrency, ReadonlyArray<number>>> = {
	USD: [500, 1000, 2500],
	EUR: [500, 1000, 2500],
	BRL: [2500, 6000, 12500],
	DKK: [3500, 7500, 15000],
	INR: [30000, 50000, 100000],
	NOK: [5000, 10000, 25000],
	PLN: [2000, 5000, 10000],
	SEK: [5000, 10000, 20000],
	TRY: [20000, 30000, 60000],
};

function toDonationCurrency(currency: OperatorPassCurrency): DonationCurrency {
	return currency.toLowerCase() as DonationCurrency;
}

function normalizeCountryCode(countryCode: string | null | undefined): string | null {
	const normalized = countryCode?.trim().toUpperCase();
	return normalized && /^[A-Z]{2}$/u.test(normalized) ? normalized : null;
}

function isOperatorPassStoppedFor(
	countryCode: string | null,
	rule: OperatorPassWithdrawalRuleMatch | null,
	stopped: ReadonlySet<string> = Config.operatorPass.stoppedCountries,
): boolean {
	return (countryCode !== null && stopped.has(countryCode)) || (rule !== null && stopped.has(rule.country));
}

function resolveCheckoutWithdrawalRule(
	countryCode: string | null,
	currency: OperatorPassCurrency,
): OperatorPassWithdrawalRuleMatch | null {
	return resolveOperatorPassWithdrawalRule(Config.operatorPass.withdrawalCountries, [countryCode], currency);
}

function getOperatorPassCheckoutBoxText(params: {
	locale: string | null;
	quantity: number;
	rule: OperatorPassWithdrawalRuleMatch | null;
}): string {
	const {locale, quantity, rule} = params;
	if (rule) {
		return quantity > 1
			? getContentMessage('billing.operator_pass_waiver_checkout_country_multiple', locale, {
					count: quantity,
					days: rule.days,
				})
			: getContentMessage('billing.operator_pass_waiver_checkout_country', locale, {days: rule.days});
	}
	return quantity > 1
		? getContentMessage('billing.operator_pass_waiver_checkout_multiple', locale, {count: quantity})
		: getContentMessage('billing.operator_pass_waiver_checkout', locale);
}

function resolveOperatorPassPurchaseBlockedReason(params: {
	phase: OperatorPassStateResponse['phase'];
	user: User;
	removed: boolean;
	countryStopped: boolean;
	paymentDisputeOpen: boolean;
}): OperatorPassPurchaseBlockedReason | null {
	const {phase, user, removed, countryStopped, paymentDisputeOpen} = params;
	if (phase === 'waitlist') {
		return 'not_on_sale';
	}
	if (countryStopped) {
		return 'country_unavailable';
	}
	if (removed) {
		return 'removed';
	}
	const eligibility = resolvePurchaseEligibility(user);
	if (!eligibility.allowed) {
		return eligibility.blocked;
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
		private readonly donationRepository: DonationRepository,
	) {}

	private async hasActiveRecurringDonation(user: User): Promise<boolean> {
		if (!user.email) {
			return false;
		}
		const donor = await this.donationRepository.findDonorByEmail(user.email);
		return donor?.hasActiveSubscription() ?? false;
	}

	async getState({user, countryCode}: OperatorPassStateParams): Promise<OperatorPassStateResponse> {
		const schedule = getOperatorPassSchedule();
		const now = operatorPassNow(schedule);
		const phase = getOperatorPassPhase(now, schedule);
		const country = normalizeCountryCode(countryCode);
		const currency = resolveOperatorPassCurrency(country);
		const rule = resolveCheckoutWithdrawalRule(country, currency);
		const priceMinor = getOperatorPassPriceMinor(phase, currency, schedule);
		const constraints = getDonationAmountConstraints(toDonationCurrency(currency));
		const entitlement = user ? await this.entitlementService.loadState(user.id) : null;
		const holder = user && entitlement ? resolveOperatorPassHolder(entitlement, now) : null;
		const wallChosen = holder && user ? hasOperatorPassWallChoice(await this.repository.findWallEntry(user.id)) : false;
		const donationPresetsMinor =
			user && (await this.hasActiveRecurringDonation(user)) ? [] : [...OPERATOR_PASS_DONATION_PRESETS_MINOR[currency]];
		return {
			phase,
			now: now.toISOString(),
			sale_opens_at: schedule.saleOpensAt?.toISOString() ?? null,
			price_rises_at: schedule.priceRisesAt?.toISOString() ?? null,
			currency,
			price_minor: priceMinor,
			max_quantity: Config.operatorPass.maxQuantity,
			withdrawal_rule: rule ? {country: rule.country, days: rule.days} : null,
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
						countryStopped: isOperatorPassStoppedFor(country, rule),
						paymentDisputeOpen: await hasOpenPaymentDispute(getPaymentDisputeRepository(), user.id),
					})
				: null,
			donation: {
				currency,
				presets_minor: donationPresetsMinor,
				min_minor: constraints.minimumAmountMinor,
				max_minor: constraints.maximumAmountMinor,
			},
		};
	}

	async createCheckoutSession({
		user,
		request,
		geoipCountryCode,
		clientIp,
		userAgent,
	}: OperatorPassCheckoutParams): Promise<string> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const quantity = request.quantity ?? 1;
		if (quantity > Config.operatorPass.maxQuantity) {
			throw InputValidationError.create('quantity', `At most ${Config.operatorPass.maxQuantity} passes per checkout`);
		}
		const schedule = getOperatorPassSchedule();
		const now = operatorPassNow(schedule);
		const phase = getOperatorPassPhase(now, schedule);
		if (phase === 'waitlist') {
			throw new OperatorPassNotOnSaleError();
		}
		assertUserCanPurchase(user);
		await assertNoOpenPaymentDispute(getPaymentDisputeRepository(), user.id);
		const entitlement = await this.entitlementService.loadState(user.id);
		const {removal} = entitlement;
		if (isOperatorPassPermanentlyRemoved(removal)) {
			throw new OperatorPassRemovedError();
		}
		const pricingCountryCode = normalizeCountryCode(geoipCountryCode) ?? normalizeCountryCode(request.country_code);
		const currency = resolveOperatorPassCurrency(pricingCountryCode);
		const rule = resolveCheckoutWithdrawalRule(pricingCountryCode, currency);
		if (isOperatorPassStoppedFor(pricingCountryCode, rule)) {
			throw new OperatorPassCountryUnavailableError();
		}
		if (request.waiver_consent !== true) {
			throw new OperatorPassWaiverRequiredError();
		}
		if (quantity > 1 && request.age_confirmed !== true) {
			throw new OperatorPassAgeConfirmationRequiredError();
		}
		const keepsListing =
			request.wall_opt_out == null &&
			!request.wall_name &&
			resolveOperatorPassHolder(entitlement, now) !== null &&
			hasOperatorPassWallChoice(await this.repository.findWallEntry(user.id));
		const wallChoice = keepsListing
			? null
			: normalizeOperatorPassWallChoice({name: request.wall_name, optOut: request.wall_opt_out});
		const userId = user.id.toString();
		const price = resolveOperatorPassCheckoutPrice({now, schedule, currency, userId, quote: request.quote});
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
		const donationAmountMinor = request.donation_amount_minor ?? null;
		if (
			donationAmountMinor !== null &&
			!isDonationAmountWithinConstraints(donationAmountMinor, toDonationCurrency(currency))
		) {
			throw new DonationAmountInvalidError();
		}
		if (donationAmountMinor !== null && (await this.hasActiveRecurringDonation(user))) {
			throw new DonationAmountInvalidError();
		}
		const locale = user.locale ?? null;
		const branding = await getBillingBranding();
		const waiverConsentAt = now;
		const consentVersion = rule ? OPERATOR_PASS_COUNTRY_CONSENT_VERSION : OPERATOR_PASS_WAIVER_CONSENT_VERSION;
		const amountMinor = price.amountMinor * quantity;
		const consentLocale = request.consent_locale ? resolveContentLocale(request.consent_locale) : locale;
		const waiverText = getOperatorPassCheckoutBoxText({locale: consentLocale, quantity, rule});
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
			operator_pass_waiver_consent_at: waiverConsentAt.toISOString(),
			operator_pass_waiver_consent_version: consentVersion,
			...(rule ? {operator_pass_withdrawal_country: rule.country} : {}),
		};
		if (wallChoice) {
			metadata.operator_pass_wall_opt_out = wallChoice.optedOut ? 'true' : 'false';
		}
		if (wallChoice?.name) {
			metadata.operator_pass_wall_name = wallChoice.name;
		}
		metadata.operator_pass_terms_version = OPERATOR_PASS_TERMS_VERSION;
		if (donationAmountMinor !== null) {
			metadata.donation_amount_minor = String(donationAmountMinor);
		}
		const productName = branding.productName;
		const passLine: CheckoutLineItem = {
			price_data: {
				currency: currency.toLowerCase(),
				unit_amount: price.amountMinor,
				tax_behavior: 'inclusive',
				product_data: {
					name: getContentMessage('billing.operator_pass_name', locale, {product_name: productName}),
					description: getContentMessage('billing.operator_pass_description', locale, {product_name: productName}),
					tax_code: PASS_TAX_CODE,
				},
			},
			quantity,
		};
		const lineItems: Array<CheckoutLineItem> = [passLine];
		if (donationAmountMinor !== null) {
			lineItems.push({
				price_data: {
					currency: currency.toLowerCase(),
					unit_amount: donationAmountMinor,
					tax_behavior: 'inclusive',
					recurring: {interval: 'month'},
					product_data: {
						...getDonationProductData({interval: 'month', locale, productName}),
						tax_code: DONATION_TAX_CODE,
					},
				},
				quantity: 1,
			});
		}
		const expiresAt = getOperatorPassSessionExpiresAt(price.pricedPhase, now, schedule);
		const email = user.email;
		const checkoutParams: CheckoutSessionCreateParams = {
			...(email ? {customer_email: email} : {}),
			client_reference_id: userId,
			metadata,
			mode: donationAmountMinor !== null ? 'subscription' : 'payment',
			line_items: lineItems,
			locale: getStripeCheckoutLocale(locale),
			automatic_tax: {enabled: true},
			tax_id_collection: {enabled: true},
			billing_address_collection: 'auto',
			allow_promotion_codes: false,
			success_url: `${Config.endpoints.webApp}/premium-callback?status=success&product=operator_pass&session_id={CHECKOUT_SESSION_ID}`,
			cancel_url: `${Config.endpoints.webApp}/channels/@operator-pass?status=cancel`,
			...(expiresAt !== undefined ? {expires_at: expiresAt} : {}),
			consent_collection: {terms_of_service: 'required'},
			custom_text: {
				terms_of_service_acceptance: {message: waiverText},
				submit: {
					message: getContentMessage('billing.operator_pass_terms_submit', locale, {
						terms_url: branding.termsUrl,
						operator_pass_terms_url: getOperatorPassTermsUrl(Config.endpoints.marketing),
					}),
				},
			},
			...(donationAmountMinor !== null
				? {
						subscription_data: {
							metadata: {
								is_donation: 'true',
								donation_email: email ?? '',
								donation_type: 'recurring',
								donation_locale: locale ?? 'en-US',
								operator_pass: 'true',
								operator_pass_purchaser_id: userId,
							},
						},
					}
				: {
						customer_creation: 'always',
						invoice_creation: {enabled: true, invoice_data: {metadata}},
						payment_intent_data: {metadata},
					}),
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
			subscription_id: null,
			created_at: new Date(),
			completed_at: null,
			refunded_at: null,
			waiver_consent_at: waiverConsentAt,
			waiver_consent_version: consentVersion,
			waiver_consent_text: waiverText,
			waiver_consent_ip: clientIp,
			waiver_consent_user_agent: userAgent?.slice(0, 512) ?? null,
			wall_name: wallChoice?.name ?? null,
			wall_opt_out: wallChoice?.optedOut ?? null,
			withdrawal_country: rule?.country ?? null,
			withdrawal_days: null,
			withdrawal_rule_version: null,
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
			invoice_number: null,
			dispute_id: null,
			dispute_reason: null,
			dispute_status: null,
			disputed_at: null,
			dispute_closed_at: null,
			version: 1,
		};
		await this.repository.createPurchase(purchase);
		return session.url;
	}
}
