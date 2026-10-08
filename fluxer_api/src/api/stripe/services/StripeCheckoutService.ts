// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {Logger} from '@app/api/Logger';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {getBillingBranding} from '@app/api/stripe/BillingBranding';
import {getEffectiveBillingConfig, isCurrentCatalogPriceId} from '@app/api/stripe/BillingConfigCache';
import type {ProductInfo, ProductRegistry} from '@app/api/stripe/ProductRegistry';
import {ensureStripeCustomer, isStripeResourceMissingError} from '@app/api/stripe/StripeCustomer';
import {getCachedStripePriceSummary, type StripePriceSummary} from '@app/api/stripe/StripePriceSummaryCache';
import {
	canProvisionPremiumFromSubscriptionStatus,
	getPremiumWillCancelFromSubscription,
} from '@app/api/stripe/StripeSubscriptionAccessPolicy';
import {
	getPrimarySubscriptionItem,
	getSubscriptionCurrentPeriodStart,
	getSubscriptionPremiumPeriodEnd,
	getSubscriptionStartDate,
} from '@app/api/stripe/StripeSubscriptionPeriod';
import {extractId} from '@app/api/stripe/StripeUtils';
import {shiftGiftExtensionPastPremiumUntil} from '@app/api/user/GiftExtensionShift';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {
	type Currency,
	getCurrencyPreferences,
	getGiftCurrencyPreferences,
	isLocalizedCurrency,
	shouldDisableAdaptivePricing,
} from '@app/api/utils/CurrencyUtils';
import {isEuEeaCountryCode} from '@fluxer/constants/src/EuropeanEconomicArea';
import {PremiumFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {PurchaseEmailVerificationRequiredError} from '@fluxer/errors/src/domains/auth/EmailVerificationRequiredError';
import {PremiumPurchaseBlockedError} from '@fluxer/errors/src/domains/payment/PremiumPurchaseBlockedError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripeInvalidProductConfigurationError} from '@fluxer/errors/src/domains/payment/StripeInvalidProductConfigurationError';
import {StripeInvalidProductError} from '@fluxer/errors/src/domains/payment/StripeInvalidProductError';
import {StripeNoPurchaseHistoryError} from '@fluxer/errors/src/domains/payment/StripeNoPurchaseHistoryError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {UnclaimedAccountCannotMakePurchasesError} from '@fluxer/errors/src/domains/user/UnclaimedAccountCannotMakePurchasesError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {CheckoutPaymentMethod} from '@fluxer/schema/src/domains/premium/GiftCodeSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type Stripe from 'stripe';

export const EU_WITHDRAWAL_WAIVER_TEXT_VERSION = '2026-04-23';

type CheckoutSessionCreateParams = Stripe.Checkout.SessionCreateParams;
type CheckoutSessionMode = CheckoutSessionCreateParams['mode'];
type CheckoutSessionPaymentMethodType = NonNullable<CheckoutSessionCreateParams['payment_method_types']>[number];
type StripeCheckoutSessionPaymentMethodOptions = NonNullable<CheckoutSessionCreateParams['payment_method_options']>;
type StripeCheckoutSessionPixOptions = NonNullable<StripeCheckoutSessionPaymentMethodOptions['pix']>;

interface CheckoutSessionPixMandateOptions {
	amount: number;
	amount_includes_iof: 'always';
	payment_schedule: 'monthly' | 'yearly';
}

interface CheckoutSessionPixOptions extends StripeCheckoutSessionPixOptions {
	mandate_options?: CheckoutSessionPixMandateOptions;
}

interface CheckoutSessionPaymentMethodOptions extends StripeCheckoutSessionPaymentMethodOptions {
	pix?: CheckoutSessionPixOptions;
}

const BLOCKING_RECURRING_SUBSCRIPTION_STATUSES: ReadonlySet<Stripe.Subscription.Status> = new Set([
	'active',
	'trialing',
	'past_due',
	'unpaid',
	'incomplete',
	'paused',
]);

export interface CreateCheckoutSessionParams {
	userId: UserID;
	priceId: string;
	isGift?: boolean;
	countryCode?: string;
	clientGeoipCountryCode?: string | null;
	purchaseGeoipCountryCode?: string | null;
	euWithdrawalWaiverAccepted?: boolean;
	paymentMethod?: CheckoutPaymentMethod;
	isBusiness?: boolean;
}

const PIX_UPI_MANDATE_HEADROOM_MULTIPLIER = 1.25;

const LOCAL_PAYMENT_METHOD_BY_CURRENCY: Partial<Record<Currency, CheckoutPaymentMethod>> = {
	BRL: 'pix',
};

interface ResolvedPriceIds {
	monthly: string | null;
	yearly: string | null;
	gift_1_month: string | null;
	gift_1_year: string | null;
	currency: Currency;
	gift_currency: Currency | null;
}

interface PriceIdsResponse extends ResolvedPriceIds {
	monthly_amount_minor: number | null;
	yearly_amount_minor: number | null;
	gift_1_month_amount_minor: number | null;
	gift_1_year_amount_minor: number | null;
}

interface EuWithdrawalWaiverContext {
	accepted: boolean;
	acceptedAt: Date | null;
	effectiveCountryCode: string | null;
	required: boolean;
}

export class StripeCheckoutService {
	constructor(
		private stripe: Stripe | null,
		private userRepository: IUserRepository,
		private productRegistry: ProductRegistry,
		private cacheService: ICacheService,
		private storeEntitlementService: StoreEntitlementService | null = null,
	) {}

	async createCheckoutSession({
		userId,
		priceId,
		isGift = false,
		countryCode,
		clientGeoipCountryCode,
		purchaseGeoipCountryCode,
		euWithdrawalWaiverAccepted,
		paymentMethod = 'card',
		isBusiness = false,
	}: CreateCheckoutSessionParams): Promise<string> {
		const {customerId, productInfo, user} = await this.prepareCheckoutContext({
			userId,
			priceId,
			isGift,
			countryCode,
			purchaseGeoipCountryCode,
		});
		const isRecurringSubscription = this.productRegistry.isRecurringSubscription(productInfo);
		const checkoutMode: CheckoutSessionMode = isRecurringSubscription ? 'subscription' : 'payment';
		const effectivePaymentMethod =
			this.resolveRequiredLocalPaymentMethod({productInfo, isGift, isRecurringSubscription}) ?? paymentMethod;
		this.assertPaymentMethodCompatibility({
			paymentMethod: effectivePaymentMethod,
			productInfo,
			isGift,
			userId,
			priceId,
		});
		const waiverContext = this.resolveEuWithdrawalWaiverContext({
			countryCode,
			clientGeoipCountryCode,
			purchaseGeoipCountryCode,
			euWithdrawalWaiverAccepted,
		});
		const paymentMethodOptions = await this.buildPaymentMethodOptions({
			productInfo,
			checkoutMode,
			paymentMethod: effectivePaymentMethod,
			priceId,
		});
		const paymentMethodTypes = this.resolvePaymentMethodTypes(effectivePaymentMethod);
		const branding = await getBillingBranding();
		const billing = getEffectiveBillingConfig();
		const checkoutMetadata = {
			user_id: userId.toString(),
			price_id: priceId,
			product_type: productInfo.type,
			is_gift: isGift ? 'true' : 'false',
			...(countryCode ? {country_code: countryCode.toUpperCase()} : {}),
			...(purchaseGeoipCountryCode ? {purchase_geoip_country_code: purchaseGeoipCountryCode.toUpperCase()} : {}),
			...(clientGeoipCountryCode ? {purchase_client_country_code: clientGeoipCountryCode.toUpperCase()} : {}),
			eu_withdrawal_waiver_required: waiverContext.required ? 'true' : 'false',
			eu_withdrawal_waiver_accepted: waiverContext.accepted ? 'true' : 'false',
			...(waiverContext.acceptedAt ? {eu_withdrawal_waiver_accepted_at: waiverContext.acceptedAt.toISOString()} : {}),
			eu_withdrawal_waiver_text_version: EU_WITHDRAWAL_WAIVER_TEXT_VERSION,
			payment_method: effectivePaymentMethod,
		};
		const checkoutParams: CheckoutSessionCreateParams = {
			customer: customerId,
			client_reference_id: userId.toString(),
			metadata: checkoutMetadata,
			...(billing.termsConsentRequired
				? {
						consent_collection: {
							terms_of_service: 'required',
						},
						custom_text: {
							terms_of_service_acceptance: {
								message: getContentMessage('billing.eu_withdrawal_waiver_checkout', user.locale, {
									product_name: branding.productName,
									premium_tier_name: branding.premiumName,
									terms_url: branding.termsUrl,
								}),
							},
						},
					}
				: {}),
			line_items: [
				{
					price: priceId,
					quantity: 1,
				},
			],
			mode: checkoutMode,
			success_url: `${Config.endpoints.webApp}/premium-callback?status=success`,
			cancel_url: `${Config.endpoints.webApp}/premium-callback?status=cancel`,
			...(checkoutMode === 'payment'
				? {
						invoice_creation: {
							enabled: true,
						},
					}
				: {}),
			automatic_tax: {
				enabled: billing.automaticTax,
			},
			tax_id_collection: {
				enabled: billing.taxIdCollection,
			},
			customer_update: {
				address: 'auto',
				name: 'auto',
			},
			billing_address_collection: isBusiness ? 'required' : 'auto',
			allow_promotion_codes: true,
			...(shouldDisableAdaptivePricing(productInfo.currency) ? {adaptive_pricing: {enabled: false}} : {}),
			...(checkoutMode === 'subscription'
				? {
						subscription_data: {
							metadata: checkoutMetadata,
						},
					}
				: {
						payment_intent_data: {
							metadata: checkoutMetadata,
						},
					}),
			...(paymentMethodTypes ? {payment_method_types: paymentMethodTypes} : {}),
			...(paymentMethodOptions ? {payment_method_options: paymentMethodOptions} : {}),
		};
		return this.createCheckoutSessionWithPaymentRecord({
			checkoutParams,
			productInfo,
			userId,
			priceId,
			isGift,
			clientGeoipCountryCode,
			purchaseGeoipCountryCode,
			waiverContext,
		});
	}

	private async prepareCheckoutContext({
		userId,
		priceId,
		isGift = false,
		countryCode,
		purchaseGeoipCountryCode,
	}: CreateCheckoutSessionParams): Promise<{
		customerId: string;
		productInfo: ProductInfo;
		user: User;
	}> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const productInfo = this.productRegistry.getProduct(priceId);
		if (!productInfo) {
			Logger.error({priceId, userId}, 'Invalid or unknown price ID');
			throw new StripeInvalidProductError();
		}
		if (productInfo.isGift !== isGift) {
			Logger.error(
				{priceId, userId, expectedIsGift: productInfo.isGift, providedIsGift: isGift},
				'Gift parameter mismatch',
			);
			throw new StripeInvalidProductConfigurationError();
		}
		const billing = getEffectiveBillingConfig();
		if (billing.catalogMode === 'operator' && !isCurrentCatalogPriceId(priceId, billing)) {
			Logger.error({priceId, userId}, 'Checkout requested for a price outside the current operator catalog');
			throw new StripeInvalidProductError();
		}
		const enforcedCountryCode = this.resolveEnforcedPricingCountryCode({countryCode, purchaseGeoipCountryCode});
		if (this.requiresCountryCodeForLocalizedCurrency(productInfo.currency) && !enforcedCountryCode) {
			Logger.error({priceId, userId, currency: productInfo.currency}, 'Localized price requested without country code');
			throw new StripeInvalidProductConfigurationError();
		}
		if (enforcedCountryCode && billing.catalogMode === 'env') {
			this.assertPriceMatchesCountryCatalog({countryCode: enforcedCountryCode, priceId, isGift, userId});
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const isRecurringSubscription = this.productRegistry.isRecurringSubscription(productInfo);
		if (user.premiumType === UserPremiumTypes.LIFETIME && isRecurringSubscription) {
			throw new PremiumPurchaseBlockedError('lifetime');
		}
		this.validateUserCanPurchase(user);
		if (isRecurringSubscription) {
			const storeEntitlement = await this.storeEntitlementService?.getActiveStoreEntitlement(user.id);
			if (storeEntitlement) {
				throw new PremiumPurchaseBlockedError('existing_subscription', {provider: storeEntitlement.provider});
			}
		}
		const customerUser = await this.ensureStripeCustomer(user);
		const customerId = customerUser.stripeCustomerId;
		if (!customerId) {
			throw new StripeError('Stripe customer id missing after customer setup');
		}
		if (isRecurringSubscription) {
			const blockingSubscription = await this.findBlockingSubscriptionForCustomer(customerId);
			const reconciledUser = await this.reconcileStripeSubscriptionId(customerUser, blockingSubscription?.id ?? null);
			if (blockingSubscription) {
				await this.repairProvisionableBlockingSubscriptionState(reconciledUser, blockingSubscription);
				throw new PremiumPurchaseBlockedError('existing_subscription', {
					subscription_status: blockingSubscription.status,
				});
			}
		}
		return {customerId, productInfo, user};
	}

	private async createCheckoutSessionWithPaymentRecord({
		checkoutParams,
		productInfo,
		userId,
		priceId,
		isGift,
		clientGeoipCountryCode,
		purchaseGeoipCountryCode,
		waiverContext,
	}: {
		checkoutParams: CheckoutSessionCreateParams;
		productInfo: ProductInfo;
		userId: UserID;
		priceId: string;
		isGift: boolean;
		clientGeoipCountryCode?: string | null;
		purchaseGeoipCountryCode?: string | null;
		waiverContext: EuWithdrawalWaiverContext;
	}): Promise<string> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		try {
			const session = await this.stripe.checkout.sessions.create(checkoutParams);
			try {
				await getBillingRepository().checkoutSessions.upsertFromStripe(session, {knownUserId: userId});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, sessionId: session.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
			}
			if (!session.url) {
				Logger.error({userId, sessionId: session.id}, 'Stripe checkout session missing url');
				throw new StripeError('Stripe checkout session missing url');
			}
			await this.userRepository.createPayment({
				checkout_session_id: session.id,
				user_id: userId,
				price_id: priceId,
				product_type: productInfo.type,
				status: 'pending',
				is_gift: isGift,
				created_at: new Date(),
				purchase_geoip_country_code: this.normalizeCountryCode(purchaseGeoipCountryCode),
				purchase_client_country_code: this.normalizeCountryCode(clientGeoipCountryCode),
				eu_withdrawal_waiver_required: waiverContext.required,
				eu_withdrawal_waiver_accepted: waiverContext.accepted,
				eu_withdrawal_waiver_accepted_at: waiverContext.acceptedAt,
				eu_withdrawal_waiver_text_version: waiverContext.required ? EU_WITHDRAWAL_WAIVER_TEXT_VERSION : null,
			});
			Logger.debug({userId, sessionId: session.id, productType: productInfo.type}, 'Checkout session created');
			return session.url;
		} catch (error: unknown) {
			Logger.error({error, userId}, 'Failed to create Stripe checkout session');
			const message = error instanceof Error ? error.message : 'Failed to create checkout session';
			throw new StripeError(message);
		}
	}

	private resolveEuWithdrawalWaiverContext({
		countryCode,
		clientGeoipCountryCode,
		purchaseGeoipCountryCode,
		euWithdrawalWaiverAccepted,
	}: Pick<
		CreateCheckoutSessionParams,
		'clientGeoipCountryCode' | 'countryCode' | 'euWithdrawalWaiverAccepted' | 'purchaseGeoipCountryCode'
	>): EuWithdrawalWaiverContext {
		const normalizedPurchaseCountryCode = this.normalizeCountryCode(purchaseGeoipCountryCode);
		const normalizedClientCountryCode = this.normalizeCountryCode(clientGeoipCountryCode);
		const normalizedPricingCountryCode = this.normalizeCountryCode(countryCode);
		const effectiveCountryCode =
			normalizedPurchaseCountryCode ?? normalizedClientCountryCode ?? normalizedPricingCountryCode ?? null;
		const required = isEuEeaCountryCode(effectiveCountryCode);
		return {
			accepted: required && euWithdrawalWaiverAccepted === true,
			acceptedAt: required && euWithdrawalWaiverAccepted === true ? new Date() : null,
			effectiveCountryCode,
			required,
		};
	}

	private normalizeCountryCode(countryCode: string | null | undefined): string | null {
		const normalized = countryCode?.trim().toUpperCase();
		return normalized && /^[A-Z]{2}$/.test(normalized) ? normalized : null;
	}

	private resolveEnforcedPricingCountryCode({
		countryCode,
		purchaseGeoipCountryCode,
	}: Pick<CreateCheckoutSessionParams, 'countryCode' | 'purchaseGeoipCountryCode'>): string | null {
		return this.normalizeCountryCode(purchaseGeoipCountryCode) ?? this.normalizeCountryCode(countryCode);
	}

	private assertPriceMatchesCountryCatalog({
		countryCode,
		priceId,
		isGift,
		userId,
	}: {
		countryCode: string;
		priceId: string;
		isGift: boolean;
		userId: UserID;
	}): void {
		const localizedPrices = this.resolveConfiguredPriceIds(countryCode);
		const allowedPriceIds = new Set(
			(isGift
				? [localizedPrices.gift_1_month, localizedPrices.gift_1_year]
				: [localizedPrices.monthly, localizedPrices.yearly]
			).filter((candidate): candidate is string => Boolean(candidate)),
		);
		if (!allowedPriceIds.has(priceId)) {
			Logger.error(
				{
					countryCode,
					priceId,
					userId,
					currency: isGift ? localizedPrices.gift_currency : localizedPrices.currency,
					isGift,
				},
				'Checkout price mismatch for country',
			);
			throw new StripeInvalidProductConfigurationError();
		}
	}

	private requiresCountryCodeForLocalizedCurrency(currency: Currency): boolean {
		return isLocalizedCurrency(currency);
	}

	private async findBlockingSubscriptionForCustomer(customerId: string): Promise<Stripe.Subscription | null> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		try {
			let startingAfter: string | undefined;
			while (true) {
				const subscriptions = await this.stripe.subscriptions.list({
					customer: customerId,
					status: 'all',
					limit: 100,
					...(startingAfter ? {starting_after: startingAfter} : {}),
				});
				const blockingSubscription = subscriptions.data.find((subscription) =>
					BLOCKING_RECURRING_SUBSCRIPTION_STATUSES.has(subscription.status),
				);
				if (blockingSubscription) {
					return blockingSubscription;
				}
				if (!subscriptions.has_more || subscriptions.data.length === 0) {
					return null;
				}
				startingAfter = subscriptions.data[subscriptions.data.length - 1]?.id;
				if (!startingAfter) {
					return null;
				}
			}
		} catch (error: unknown) {
			Logger.error({error, customerId}, 'Failed to list Stripe subscriptions for checkout guard');
			const message = error instanceof Error ? error.message : 'Failed to list Stripe subscriptions';
			throw new StripeError(message);
		}
	}

	private async reconcileStripeSubscriptionId(user: User, stripeSubscriptionId: string | null): Promise<User> {
		if (user.stripeSubscriptionId === stripeSubscriptionId) {
			return user;
		}
		const updatedUser = await this.userRepository.patchUpsert(
			user.id,
			{
				stripe_subscription_id: stripeSubscriptionId,
			},
			user.toRow(),
		);
		Logger.debug(
			{
				userId: user.id,
				oldStripeSubscriptionId: user.stripeSubscriptionId,
				newStripeSubscriptionId: stripeSubscriptionId,
			},
			'Reconciled user stripe subscription id from Stripe',
		);
		return updatedUser;
	}

	private async repairProvisionableBlockingSubscriptionState(
		user: User,
		subscription: Stripe.Subscription,
	): Promise<void> {
		if (!canProvisionPremiumFromSubscriptionStatus(subscription.status)) {
			return;
		}
		const patch: Partial<UserRow> = {};
		const subscriptionStartDate = this.getRuntimeSubscriptionStartDate(subscription);
		const premiumUntil = getSubscriptionPremiumPeriodEnd(subscription);
		const premiumBillingCycle = this.getRuntimeSubscriptionBillingCycle(subscription);
		const premiumWillCancel = getPremiumWillCancelFromSubscription(subscription);
		const customerId = extractId(subscription.customer);
		if (user.premiumType !== UserPremiumTypes.SUBSCRIPTION) {
			patch.premium_type = UserPremiumTypes.SUBSCRIPTION;
		}
		if (subscriptionStartDate && (!user.premiumSince || user.premiumSince > subscriptionStartDate)) {
			patch.premium_since = subscriptionStartDate;
		}
		if (premiumUntil && user.premiumUntil?.getTime() !== premiumUntil.getTime()) {
			patch.premium_until = premiumUntil;
			const giftEnd = shiftGiftExtensionPastPremiumUntil(
				{premiumUntil: user.premiumUntil, giftEnd: user.premiumGiftExtensionEndsAt},
				premiumUntil,
				new Date(),
				getSubscriptionCurrentPeriodStart(subscription),
			);
			if (giftEnd !== user.premiumGiftExtensionEndsAt) {
				patch.premium_gift_extension_ends_at = giftEnd;
			}
		}
		if (user.premiumWillCancel !== premiumWillCancel) {
			patch.premium_will_cancel = premiumWillCancel;
		}
		if (user.premiumGraceEndsAt) {
			patch.premium_grace_ends_at = null;
		}
		if (premiumBillingCycle && user.premiumBillingCycle !== premiumBillingCycle) {
			patch.premium_billing_cycle = premiumBillingCycle;
		}
		if (customerId && user.stripeCustomerId !== customerId) {
			patch.stripe_customer_id = customerId;
		}
		if (Object.keys(patch).length === 0) {
			return;
		}
		await this.userRepository.patchUpsert(user.id, patch, user.toRow());
		Logger.info(
			{
				userId: user.id,
				subscriptionId: subscription.id,
				patchedFields: Object.keys(patch),
			},
			'Repaired local premium state from blocking checkout subscription',
		);
	}

	private getRuntimeSubscriptionStartDate(subscription: Stripe.Subscription): Date | null {
		const runtimeSubscription = subscription as Stripe.Subscription & {
			created?: number | null;
			start_date?: number | null;
		};
		const startUnix = runtimeSubscription.start_date ?? runtimeSubscription.created ?? null;
		return typeof startUnix === 'number' ? getSubscriptionStartDate(subscription) : null;
	}

	private getRuntimeSubscriptionBillingCycle(subscription: Stripe.Subscription): 'monthly' | 'yearly' | null {
		const item = getPrimarySubscriptionItem(subscription);
		const interval = item?.price?.recurring?.interval;
		if (interval === 'month') {
			return 'monthly';
		}
		if (interval === 'year') {
			return 'yearly';
		}
		return null;
	}

	async createCustomerPortalSession(userId: UserID): Promise<string> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (!user.stripeCustomerId) {
			throw new StripeNoPurchaseHistoryError();
		}
		const portalUser = await this.ensureStripeCustomer(user);
		const customerId = portalUser.stripeCustomerId;
		if (!customerId) {
			throw new StripeNoPurchaseHistoryError();
		}
		try {
			const session = await this.stripe.billingPortal.sessions.create({
				customer: customerId,
				return_url: `${Config.endpoints.webApp}/premium-callback?status=closed-billing-portal`,
			});
			if (!session.url) {
				Logger.error({userId, customerId}, 'Stripe customer portal session missing url');
				throw new StripeError('Stripe customer portal session missing url');
			}
			return session.url;
		} catch (error: unknown) {
			Logger.error({error, userId, customerId}, 'Failed to create customer portal session');
			const message = error instanceof Error ? error.message : 'Failed to create customer portal session';
			throw new StripeError(message);
		}
	}

	async getPriceIds(countryCode?: string): Promise<PriceIdsResponse> {
		const resolvedPrices = this.resolveConfiguredPriceIds(countryCode);
		const [monthlyPrice, yearlyPrice, gift1MonthPrice, gift1YearPrice] = await Promise.all([
			this.getStripePriceSummary(resolvedPrices.monthly),
			this.getStripePriceSummary(resolvedPrices.yearly),
			this.getStripePriceSummary(resolvedPrices.gift_1_month),
			this.getStripePriceSummary(resolvedPrices.gift_1_year),
		]);
		return {
			...resolvedPrices,
			monthly_amount_minor: monthlyPrice?.unitAmountMinor ?? null,
			yearly_amount_minor: yearlyPrice?.unitAmountMinor ?? null,
			gift_1_month_amount_minor: gift1MonthPrice?.unitAmountMinor ?? null,
			gift_1_year_amount_minor: gift1YearPrice?.unitAmountMinor ?? null,
		};
	}

	validateUserCanPurchase(user: User): void {
		if (user.isUnclaimedAccount()) {
			throw new UnclaimedAccountCannotMakePurchasesError();
		}
		if (!user.emailVerified) {
			throw new PurchaseEmailVerificationRequiredError();
		}
		if (user.premiumFlags & PremiumFlags.PURCHASE_DISABLED) {
			throw new PremiumPurchaseBlockedError('purchase_disabled');
		}
	}

	private resolveConfiguredPriceIds(countryCode?: string): ResolvedPriceIds {
		const recurringCurrencyPreferences = getCurrencyPreferences(countryCode);
		const giftCurrencyPreferences = getGiftCurrencyPreferences(countryCode);
		const recurringPrices = this.resolveRecurringPriceIds(recurringCurrencyPreferences);
		const giftPrices = this.resolveGiftPriceIds(giftCurrencyPreferences);
		return {
			monthly: recurringPrices.monthly,
			yearly: recurringPrices.yearly,
			gift_1_month: giftPrices.gift_1_month,
			gift_1_year: giftPrices.gift_1_year,
			currency: recurringPrices.currency,
			gift_currency: giftPrices.gift_currency,
		};
	}

	private resolveRecurringPriceIds(
		preferredCurrencies: Array<Currency>,
	): Pick<ResolvedPriceIds, 'monthly' | 'yearly' | 'currency'> {
		for (const currency of preferredCurrencies) {
			const resolvedPrices = this.getConfiguredRecurringPriceIdsForCurrency(currency);
			if (resolvedPrices) {
				return resolvedPrices;
			}
		}
		throw new StripeError(
			`Stripe recurring price ids missing for supported currencies: ${preferredCurrencies.join(', ')}`,
		);
	}

	private resolveGiftPriceIds(
		preferredCurrencies: Array<Currency>,
	): Pick<ResolvedPriceIds, 'gift_1_month' | 'gift_1_year' | 'gift_currency'> {
		for (const currency of preferredCurrencies) {
			const resolvedPrices = this.getConfiguredGiftPriceIdsForCurrency(currency);
			if (resolvedPrices) {
				return resolvedPrices;
			}
		}
		if (Config.instance.selfHosted) {
			return {gift_1_month: null, gift_1_year: null, gift_currency: null};
		}
		throw new StripeError(`Stripe gift price ids missing for supported currencies: ${preferredCurrencies.join(', ')}`);
	}

	private getConfiguredRecurringPriceIdsForCurrency(
		currency: Currency,
	): Pick<ResolvedPriceIds, 'monthly' | 'yearly' | 'currency'> | null {
		const monthly = this.productRegistry.getRecurringSubscriptionPriceId('monthly', currency);
		const yearly = this.productRegistry.getRecurringSubscriptionPriceId('yearly', currency);
		if (!monthly || !yearly) {
			return null;
		}
		return {monthly, yearly, currency};
	}

	private getConfiguredGiftPriceIdsForCurrency(
		currency: Currency,
	): Pick<ResolvedPriceIds, 'gift_1_month' | 'gift_1_year' | 'gift_currency'> | null {
		const gift1Month = this.productRegistry.getGiftPriceId('gift_1_month', currency);
		const gift1Year = this.productRegistry.getGiftPriceId('gift_1_year', currency);
		if (!gift1Month || !gift1Year) {
			return null;
		}
		return {gift_1_month: gift1Month, gift_1_year: gift1Year, gift_currency: currency};
	}

	private async getStripePriceSummary(priceId: string | null): Promise<StripePriceSummary | null> {
		return getCachedStripePriceSummary({stripe: this.stripe, cacheService: this.cacheService, priceId});
	}

	private resolveRequiredLocalPaymentMethod({
		productInfo,
		isGift,
		isRecurringSubscription,
	}: {
		productInfo: ProductInfo;
		isGift: boolean;
		isRecurringSubscription: boolean;
	}): CheckoutPaymentMethod | null {
		if (isGift || !isRecurringSubscription) {
			return null;
		}
		if (getEffectiveBillingConfig().catalogMode === 'operator') {
			return null;
		}
		return LOCAL_PAYMENT_METHOD_BY_CURRENCY[productInfo.currency] ?? null;
	}

	private assertPaymentMethodCompatibility({
		paymentMethod,
		productInfo,
		isGift,
		userId,
		priceId,
	}: {
		paymentMethod: CheckoutPaymentMethod;
		productInfo: ProductInfo;
		isGift: boolean;
		userId: UserID;
		priceId: string;
	}): void {
		if (paymentMethod === 'card') {
			return;
		}
		if (isGift || !this.productRegistry.isRecurringSubscription(productInfo)) {
			Logger.error({paymentMethod, priceId, userId}, 'Non-card payment method only valid for recurring subscriptions');
			throw new StripeInvalidProductConfigurationError();
		}
		if (getEffectiveBillingConfig().catalogMode === 'operator') {
			Logger.error({paymentMethod, priceId, userId}, 'Non-card payment methods are unavailable for operator prices');
			throw new StripeInvalidProductConfigurationError();
		}
		if (paymentMethod === 'pix' && productInfo.currency !== 'BRL') {
			Logger.error({priceId, userId, currency: productInfo.currency}, 'Pix payment method requires a BRL price');
			throw new StripeInvalidProductConfigurationError();
		}
		if (paymentMethod === 'upi' && productInfo.currency !== 'INR') {
			Logger.error({priceId, userId, currency: productInfo.currency}, 'UPI payment method requires an INR price');
			throw new StripeInvalidProductConfigurationError();
		}
	}

	private resolvePaymentMethodTypes(
		paymentMethod: CheckoutPaymentMethod,
	): Array<CheckoutSessionPaymentMethodType> | undefined {
		if (paymentMethod === 'pix') {
			return ['pix'];
		}
		if (paymentMethod === 'upi') {
			return ['upi'];
		}
		return undefined;
	}

	private async buildPaymentMethodOptions({
		productInfo,
		checkoutMode,
		paymentMethod,
		priceId,
	}: {
		productInfo: ProductInfo;
		checkoutMode: CheckoutSessionMode;
		paymentMethod: CheckoutPaymentMethod;
		priceId: string;
	}): Promise<CheckoutSessionPaymentMethodOptions | undefined> {
		const envCatalog = getEffectiveBillingConfig().catalogMode === 'env';
		if (envCatalog && productInfo.currency === 'BRL' && checkoutMode === 'payment') {
			return {
				pix: {
					amount_includes_iof: 'always',
				},
			};
		}
		if (checkoutMode !== 'subscription') {
			return undefined;
		}
		if (paymentMethod === 'pix') {
			const mandateAmount = await this.resolveMandateAmount(priceId);
			const paymentSchedule = productInfo.billingCycle === 'yearly' ? 'yearly' : 'monthly';
			return {
				pix: {
					mandate_options: {
						amount: mandateAmount,
						amount_includes_iof: 'always',
						payment_schedule: paymentSchedule,
					},
				},
			};
		}
		if (paymentMethod === 'upi') {
			const [mandateAmount, branding] = await Promise.all([this.resolveMandateAmount(priceId), getBillingBranding()]);
			return {
				upi: {
					mandate_options: {
						amount: mandateAmount,
						amount_type: 'maximum',
						description: branding.upiMandateDescription,
					},
				},
			};
		}
		return undefined;
	}

	private async resolveMandateAmount(priceId: string): Promise<number> {
		const priceSummary = await this.getStripePriceSummary(priceId);
		if (!priceSummary?.unitAmountMinor) {
			throw new StripeError('Failed to resolve Stripe price amount for mandate configuration');
		}
		return Math.ceil(priceSummary.unitAmountMinor * PIX_UPI_MANDATE_HEADROOM_MULTIPLIER);
	}

	private async clearStaleStripeCustomer(user: User): Promise<User> {
		if (!Config.instance.selfHosted || !this.stripe || !user.stripeCustomerId) {
			return user;
		}
		const customerId = user.stripeCustomerId;
		try {
			const customer = await this.stripe.customers.retrieve(customerId);
			if (!('deleted' in customer && customer.deleted)) {
				return user;
			}
		} catch (error: unknown) {
			if (!isStripeResourceMissingError(error)) {
				Logger.warn({error, userId: user.id, customerId}, 'Failed to verify stored Stripe customer');
				return user;
			}
		}
		const patch: Partial<UserRow> = {stripe_customer_id: null};
		if (user.stripeSubscriptionId && (await this.isStripeSubscriptionMissing(user.stripeSubscriptionId))) {
			patch.stripe_subscription_id = null;
		}
		const updatedUser = await this.userRepository.patchUpsert(user.id, patch, user.toRow());
		Logger.info(
			{userId: user.id, customerId, clearedFields: Object.keys(patch)},
			'Cleared Stripe customer that no longer exists for the configured Stripe account',
		);
		return updatedUser;
	}

	private async isStripeSubscriptionMissing(subscriptionId: string): Promise<boolean> {
		if (!this.stripe) {
			return false;
		}
		try {
			await this.stripe.subscriptions.retrieve(subscriptionId);
			return false;
		} catch (error: unknown) {
			return isStripeResourceMissingError(error);
		}
	}

	private async ensureStripeCustomer(existingUser: User): Promise<User> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		return ensureStripeCustomer({
			stripe: this.stripe,
			user: await this.clearStaleStripeCustomer(existingUser),
			userRepository: this.userRepository,
			cacheService: this.cacheService,
		});
	}
}
