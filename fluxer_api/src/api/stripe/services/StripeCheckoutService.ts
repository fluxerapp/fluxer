// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
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
import type {PricingMode} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {seconds} from 'itty-time';
import type Stripe from 'stripe';
import {createUserID, type UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {getContentMessage} from '../../content_i18n/ContentI18n';
import type {UserRow} from '../../database/types/UserTypes';
import {Logger} from '../../Logger';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import type {User} from '../../models/User';
import type {IUserRepository} from '../../user/IUserRepository';
import {
	type Currency,
	getBaseCurrencyPreferences,
	getBaseGiftCurrencyPreferences,
	getCurrencyPreferences,
	getGiftCurrencyPreferences,
} from '../../utils/CurrencyUtils';
import type {ProductInfo, ProductRegistry} from '../ProductRegistry';
import {
	canProvisionPremiumFromSubscriptionStatus,
	getPremiumWillCancelFromSubscription,
} from '../StripeSubscriptionAccessPolicy';
import {
	getPrimarySubscriptionItem,
	getSubscriptionPremiumPeriodEnd,
	getSubscriptionStartDate,
} from '../StripeSubscriptionPeriod';
import {extractId} from '../StripeUtils';

const PRODUCT_NAME = 'Fluxer';
const PREMIUM_TIER_NAME = 'Plutonium';
const TERMS_URL = 'https://fluxer.app/terms';
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
	pricingMode?: PricingMode;
	paymentMethod?: CheckoutPaymentMethod;
	isBusiness?: boolean;
}

const UPI_MANDATE_DESCRIPTION = 'Fluxer Premium';
const PIX_UPI_MANDATE_HEADROOM_MULTIPLIER = 1.25;

interface ResolvedPriceIds {
	monthly: string | null;
	yearly: string | null;
	gift_1_month: string | null;
	gift_1_year: string | null;
	currency: Currency;
	gift_currency: Currency;
}

interface PriceIdsResponse extends ResolvedPriceIds {
	monthly_amount_minor: number | null;
	yearly_amount_minor: number | null;
	gift_1_month_amount_minor: number | null;
	gift_1_year_amount_minor: number | null;
}

interface StripePriceSummary {
	unitAmountMinor: number | null;
}

interface EuWithdrawalWaiverContext {
	accepted: boolean;
	acceptedAt: Date | null;
	effectiveCountryCode: string | null;
	required: boolean;
}

type LocalizedCardPreapprovalStatus = 'approved' | 'checkout_created' | 'pending' | 'rejected';
type LocalizedCardPreapprovalRejectedReason =
	| 'country_mismatch'
	| 'missing_customer'
	| 'missing_payment_method'
	| 'missing_setup_intent'
	| 'payment_method_not_card'
	| 'unknown';

interface LocalizedCardPreapprovalFlowState {
	actualCardCountry: string | null;
	approvedPaymentMethodId: string | null;
	clientGeoipCountryCode: string | null;
	countryCode: string;
	customerId: string;
	currency: Currency;
	euWithdrawalWaiverAccepted: boolean;
	finalCheckoutUrl: string | null;
	isBusiness: boolean;
	preapprovalSessionId: string;
	purchaseGeoipCountryCode: string | null;
	priceId: string;
	rejectionReason: LocalizedCardPreapprovalRejectedReason | null;
	status: LocalizedCardPreapprovalStatus;
	token: string;
	userId: string;
}

export type ContinueLocalizedCardPreapprovalResult =
	| {
			status: 'expired';
	  }
	| {
			status: 'pending';
	  }
	| {
			status: 'ready';
			url: string;
	  }
	| {
			status: 'rejected';
			reason: LocalizedCardPreapprovalRejectedReason;
			actual_country?: string | null;
	  };

export class StripeCheckoutService {
	constructor(
		private stripe: Stripe | null,
		private userRepository: IUserRepository,
		private productRegistry: ProductRegistry,
		private cacheService: ICacheService,
	) {}

	async createCheckoutSession({
		userId,
		priceId,
		isGift = false,
		countryCode,
		clientGeoipCountryCode,
		purchaseGeoipCountryCode,
		euWithdrawalWaiverAccepted,
		pricingMode = 'localized',
		paymentMethod = 'card',
		isBusiness = false,
	}: CreateCheckoutSessionParams): Promise<string> {
		const {customerId, productInfo, user} = await this.prepareCheckoutContext({
			userId,
			priceId,
			isGift,
			countryCode,
			pricingMode,
		});
		const isRecurringSubscription = this.productRegistry.isRecurringSubscription(productInfo);
		const checkoutMode: CheckoutSessionMode = isRecurringSubscription ? 'subscription' : 'payment';
		this.assertPaymentMethodCompatibility({paymentMethod, productInfo, isGift, userId, priceId});
		const waiverContext = this.resolveEuWithdrawalWaiverContext({
			countryCode,
			clientGeoipCountryCode,
			purchaseGeoipCountryCode,
			euWithdrawalWaiverAccepted,
		});
		const paymentMethodOptions = await this.buildPaymentMethodOptions({
			productInfo,
			checkoutMode,
			paymentMethod,
			priceId,
		});
		const paymentMethodTypes = this.resolvePaymentMethodTypes(paymentMethod);
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
			pricing_mode: pricingMode,
			payment_method: paymentMethod,
		};
		const checkoutParams: CheckoutSessionCreateParams = {
			customer: customerId,
			client_reference_id: userId.toString(),
			metadata: checkoutMetadata,
			consent_collection: {
				terms_of_service: 'required',
			},
			custom_text: {
				terms_of_service_acceptance: {
					message: getContentMessage('billing.eu_withdrawal_waiver_checkout', user.locale, {
						product_name: PRODUCT_NAME,
						premium_tier_name: PREMIUM_TIER_NAME,
						terms_url: TERMS_URL,
					}),
				},
			},
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
				enabled: true,
			},
			tax_id_collection: {
				enabled: true,
			},
			customer_update: {
				address: 'auto',
				name: 'auto',
			},
			billing_address_collection: isBusiness ? 'required' : 'auto',
			allow_promotion_codes: true,
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

	async createLocalizedCardPreapprovalSession({
		userId,
		priceId,
		countryCode,
		clientGeoipCountryCode,
		purchaseGeoipCountryCode,
		euWithdrawalWaiverAccepted,
		pricingMode = 'localized',
		isBusiness = false,
	}: Pick<
		CreateCheckoutSessionParams,
		| 'clientGeoipCountryCode'
		| 'countryCode'
		| 'euWithdrawalWaiverAccepted'
		| 'isBusiness'
		| 'priceId'
		| 'pricingMode'
		| 'purchaseGeoipCountryCode'
		| 'userId'
	>): Promise<string> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const normalizedCountryCode = countryCode?.trim().toUpperCase();
		if (!normalizedCountryCode) {
			Logger.error({priceId, userId}, 'Localized card preapproval requires a country code');
			throw new StripeInvalidProductConfigurationError();
		}
		if (pricingMode !== 'localized') {
			Logger.error(
				{priceId, userId, pricingMode},
				'Localized card preapproval requested for non-localized pricing mode',
			);
			throw new StripeInvalidProductConfigurationError();
		}
		const {customerId, productInfo} = await this.prepareCheckoutContext({
			userId,
			priceId,
			isGift: false,
			countryCode: normalizedCountryCode,
			pricingMode,
		});
		if (!this.requiresLocalizedCardPreapproval(productInfo)) {
			Logger.error(
				{priceId, userId, currency: productInfo.currency, countryCode: normalizedCountryCode},
				'Localized card preapproval requested for non-localized recurring price',
			);
			throw new StripeInvalidProductConfigurationError();
		}
		const waiverContext = this.resolveEuWithdrawalWaiverContext({
			countryCode: normalizedCountryCode,
			clientGeoipCountryCode,
			purchaseGeoipCountryCode,
			euWithdrawalWaiverAccepted,
		});
		const token = randomUUID();
		const checkoutParams: CheckoutSessionCreateParams = {
			customer: customerId,
			client_reference_id: userId.toString(),
			metadata: {
				user_id: userId.toString(),
				price_id: priceId,
				product_type: productInfo.type,
				country_code: normalizedCountryCode,
				...(purchaseGeoipCountryCode ? {purchase_geoip_country_code: purchaseGeoipCountryCode.toUpperCase()} : {}),
				...(clientGeoipCountryCode ? {purchase_client_country_code: clientGeoipCountryCode.toUpperCase()} : {}),
				eu_withdrawal_waiver_required: waiverContext.required ? 'true' : 'false',
				eu_withdrawal_waiver_accepted: waiverContext.accepted ? 'true' : 'false',
				...(waiverContext.acceptedAt ? {eu_withdrawal_waiver_accepted_at: waiverContext.acceptedAt.toISOString()} : {}),
				...(waiverContext.required ? {eu_withdrawal_waiver_text_version: EU_WITHDRAWAL_WAIVER_TEXT_VERSION} : {}),
				pricing_mode: pricingMode,
				setup_type: 'localized_card_preapproval',
				localized_card_preapproval_currency: productInfo.currency,
				localized_card_preapproval_token: token,
				is_business: isBusiness ? 'true' : 'false',
			},
			mode: 'setup',
			payment_method_types: ['card'],
			success_url: `${Config.endpoints.webApp}/premium-callback?status=preapproval-success&token=${encodeURIComponent(token)}`,
			cancel_url: `${Config.endpoints.webApp}/premium-callback?status=preapproval-cancel`,
			tax_id_collection: {
				enabled: true,
			},
			billing_address_collection: isBusiness ? 'required' : 'auto',
			customer_update: {
				address: 'auto',
				name: 'auto',
			},
		};
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
				Logger.error({userId, sessionId: session.id}, 'Stripe localized card preapproval session missing url');
				throw new StripeError('Stripe localized card preapproval session missing url');
			}
			await this.setLocalizedCardPreapprovalFlow(token, {
				actualCardCountry: null,
				approvedPaymentMethodId: null,
				clientGeoipCountryCode: this.normalizeCountryCode(clientGeoipCountryCode),
				countryCode: normalizedCountryCode,
				customerId,
				currency: productInfo.currency,
				euWithdrawalWaiverAccepted: waiverContext.accepted,
				finalCheckoutUrl: null,
				isBusiness,
				preapprovalSessionId: session.id,
				purchaseGeoipCountryCode: this.normalizeCountryCode(purchaseGeoipCountryCode),
				priceId,
				rejectionReason: null,
				status: 'pending',
				token,
				userId: userId.toString(),
			});
			Logger.debug(
				{userId, sessionId: session.id, countryCode: normalizedCountryCode},
				'Localized card preapproval session created',
			);
			return session.url;
		} catch (error: unknown) {
			Logger.error(
				{error, userId, countryCode: normalizedCountryCode},
				'Failed to create localized card preapproval session',
			);
			const message = error instanceof Error ? error.message : 'Failed to create localized card preapproval session';
			throw new StripeError(message);
		}
	}

	async continueLocalizedCardPreapproval(token: string): Promise<ContinueLocalizedCardPreapprovalResult> {
		const normalizedToken = token.trim();
		if (!normalizedToken) {
			return {status: 'expired'};
		}
		const flowState = await this.getLocalizedCardPreapprovalFlow(normalizedToken);
		if (!flowState) {
			return {status: 'expired'};
		}
		if (flowState.finalCheckoutUrl) {
			return {status: 'ready', url: flowState.finalCheckoutUrl};
		}
		if (flowState.status === 'pending') {
			return {status: 'pending'};
		}
		if (flowState.status === 'rejected') {
			return {
				status: 'rejected',
				reason: flowState.rejectionReason ?? 'unknown',
				actual_country: flowState.actualCardCountry,
			};
		}
		const lockKey = this.getLocalizedCardPreapprovalContinueLockKey(normalizedToken);
		const lockToken = await this.cacheService.acquireLock(
			lockKey,
			StripeCheckoutService.LOCALIZED_CARD_PREAPPROVAL_CONTINUE_LOCK_TTL_SECONDS,
		);
		if (!lockToken) {
			return {status: 'pending'};
		}
		try {
			const freshFlowState = await this.getLocalizedCardPreapprovalFlow(normalizedToken);
			if (!freshFlowState) {
				return {status: 'expired'};
			}
			if (freshFlowState.finalCheckoutUrl) {
				return {status: 'ready', url: freshFlowState.finalCheckoutUrl};
			}
			if (freshFlowState.status === 'pending') {
				return {status: 'pending'};
			}
			if (freshFlowState.status === 'rejected') {
				return {
					status: 'rejected',
					reason: freshFlowState.rejectionReason ?? 'unknown',
					actual_country: freshFlowState.actualCardCountry,
				};
			}
			if (freshFlowState.approvedPaymentMethodId && this.stripe) {
				await this.setCustomerDefaultPaymentMethod(freshFlowState.customerId, freshFlowState.approvedPaymentMethodId);
			}
			const checkoutUrl = await this.createCheckoutSession({
				userId: createUserID(BigInt(freshFlowState.userId)),
				priceId: freshFlowState.priceId,
				isGift: false,
				countryCode: freshFlowState.countryCode,
				clientGeoipCountryCode: freshFlowState.clientGeoipCountryCode,
				purchaseGeoipCountryCode: freshFlowState.purchaseGeoipCountryCode,
				euWithdrawalWaiverAccepted: freshFlowState.euWithdrawalWaiverAccepted,
				isBusiness: freshFlowState.isBusiness,
			});
			const updatedFlowState: LocalizedCardPreapprovalFlowState = {
				...freshFlowState,
				finalCheckoutUrl: checkoutUrl,
				status: 'checkout_created',
			};
			await this.setLocalizedCardPreapprovalFlow(normalizedToken, updatedFlowState);
			return {status: 'ready', url: checkoutUrl};
		} finally {
			try {
				await this.cacheService.releaseLock(lockKey, lockToken);
			} catch (error) {
				Logger.error({error, token: normalizedToken}, 'Failed to release localized card preapproval continuation lock');
			}
		}
	}

	async completeLocalizedCardPreapproval(session: Stripe.Checkout.Session): Promise<void> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const token = session.metadata?.localized_card_preapproval_token?.trim();
		if (!token) {
			Logger.error({sessionId: session.id}, 'Localized card preapproval session missing token');
			return;
		}
		const countryCode = session.metadata?.country_code?.trim().toUpperCase();
		if (!countryCode) {
			await this.rejectLocalizedCardPreapproval(session, token, 'unknown');
			return;
		}
		const setupIntentId = extractId(session.setup_intent);
		if (!setupIntentId) {
			await this.rejectLocalizedCardPreapproval(session, token, 'missing_setup_intent');
			return;
		}
		const setupIntent = await this.stripe.setupIntents.retrieve(setupIntentId, {
			expand: ['payment_method'],
		});
		const paymentMethod = setupIntent.payment_method;
		if (!paymentMethod || typeof paymentMethod === 'string') {
			await this.rejectLocalizedCardPreapproval(session, token, 'missing_payment_method');
			return;
		}
		if (paymentMethod.type !== 'card' || !paymentMethod.card) {
			await this.rejectLocalizedCardPreapproval(session, token, 'payment_method_not_card');
			return;
		}
		const cardCountry = paymentMethod.card.country?.trim().toUpperCase() ?? null;
		if (cardCountry !== countryCode) {
			await this.rejectLocalizedCardPreapproval(session, token, 'country_mismatch', cardCountry);
			return;
		}
		const flowState = await this.buildLocalizedCardPreapprovalFlowStateFromSession(session, token);
		const approvedFlowState: LocalizedCardPreapprovalFlowState = {
			...flowState,
			actualCardCountry: cardCountry,
			approvedPaymentMethodId: paymentMethod.id,
			rejectionReason: null,
			status: 'approved',
		};
		await this.setLocalizedCardPreapprovalFlow(token, approvedFlowState);
		Logger.info({sessionId: session.id, userId: flowState.userId, countryCode}, 'Localized card preapproval completed');
	}

	private async prepareCheckoutContext({
		userId,
		priceId,
		isGift = false,
		countryCode,
		pricingMode = 'localized',
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
		if (this.requiresCountryCodeForLocalizedCurrency(productInfo.currency) && !countryCode) {
			Logger.error({priceId, userId, currency: productInfo.currency}, 'Localized price requested without country code');
			throw new StripeInvalidProductConfigurationError();
		}
		if (countryCode) {
			this.assertPriceMatchesCountryCatalog({countryCode, priceId, isGift, pricingMode, userId});
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

	private assertPriceMatchesCountryCatalog({
		countryCode,
		priceId,
		isGift,
		pricingMode = 'localized',
		userId,
	}: {
		countryCode: string;
		priceId: string;
		isGift: boolean;
		pricingMode?: PricingMode;
		userId: UserID;
	}): void {
		const localizedPrices = this.resolveConfiguredPriceIds(countryCode, pricingMode);
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
					pricingMode,
				},
				'Checkout price mismatch for country',
			);
			throw new StripeInvalidProductConfigurationError();
		}
	}

	private requiresLocalizedCardPreapproval(productInfo: ProductInfo): boolean {
		return (
			this.productRegistry.isRecurringSubscription(productInfo) &&
			productInfo.currency !== 'USD' &&
			productInfo.currency !== 'EUR'
		);
	}

	private requiresCountryCodeForLocalizedCurrency(currency: Currency): boolean {
		return currency !== 'USD' && currency !== 'EUR';
	}

	private async rejectLocalizedCardPreapproval(
		session: Stripe.Checkout.Session,
		token: string,
		rejectionReason: LocalizedCardPreapprovalRejectedReason,
		actualCardCountry: string | null = null,
	): Promise<void> {
		const flowState = await this.buildLocalizedCardPreapprovalFlowStateFromSession(session, token);
		const rejectedFlowState: LocalizedCardPreapprovalFlowState = {
			...flowState,
			actualCardCountry,
			approvedPaymentMethodId: null,
			rejectionReason,
			status: 'rejected',
		};
		await this.setLocalizedCardPreapprovalFlow(token, rejectedFlowState);
		Logger.info(
			{
				sessionId: session.id,
				userId: flowState.userId,
				countryCode: flowState.countryCode,
				actualCardCountry,
				rejectionReason,
			},
			'Localized card preapproval rejected',
		);
	}

	private async buildLocalizedCardPreapprovalFlowStateFromSession(
		session: Stripe.Checkout.Session,
		token: string,
	): Promise<LocalizedCardPreapprovalFlowState> {
		const existingFlowState = await this.getLocalizedCardPreapprovalFlow(token);
		if (existingFlowState) {
			return existingFlowState;
		}
		const userId = session.metadata?.user_id?.trim();
		const priceId = session.metadata?.price_id?.trim();
		const countryCode = session.metadata?.country_code?.trim().toUpperCase();
		const currency = session.metadata?.localized_card_preapproval_currency?.trim().toUpperCase() as
			| Currency
			| undefined;
		const customerId = extractId(session.customer);
		if (!userId || !priceId || !countryCode || !currency || !customerId) {
			throw new StripeError('Localized card preapproval session missing required metadata');
		}
		return {
			actualCardCountry: null,
			approvedPaymentMethodId: null,
			clientGeoipCountryCode: this.normalizeCountryCode(session.metadata?.purchase_client_country_code),
			countryCode,
			customerId,
			currency,
			euWithdrawalWaiverAccepted: session.metadata?.eu_withdrawal_waiver_accepted === 'true',
			finalCheckoutUrl: null,
			isBusiness: session.metadata?.is_business === 'true',
			preapprovalSessionId: session.id,
			purchaseGeoipCountryCode: this.normalizeCountryCode(session.metadata?.purchase_geoip_country_code),
			priceId,
			rejectionReason: null,
			status: 'pending',
			token,
			userId,
		};
	}

	private async getLocalizedCardPreapprovalFlow(token: string): Promise<LocalizedCardPreapprovalFlowState | null> {
		return (
			(await this.cacheService.get<LocalizedCardPreapprovalFlowState>(
				this.getLocalizedCardPreapprovalFlowKey(token),
			)) ?? null
		);
	}

	private async setLocalizedCardPreapprovalFlow(
		token: string,
		flowState: LocalizedCardPreapprovalFlowState,
	): Promise<void> {
		await this.cacheService.set(
			this.getLocalizedCardPreapprovalFlowKey(token),
			flowState,
			StripeCheckoutService.LOCALIZED_CARD_PREAPPROVAL_TTL_SECONDS,
		);
	}

	private getLocalizedCardPreapprovalFlowKey(token: string): string {
		return `stripe:localized-card-preapproval:flow:${token}`;
	}

	private getLocalizedCardPreapprovalContinueLockKey(token: string): string {
		return `stripe:localized-card-preapproval:continue:${token}`;
	}

	private async setCustomerDefaultPaymentMethod(customerId: string, paymentMethodId: string): Promise<void> {
		if (!this.stripe) {
			return;
		}
		try {
			const updatedCustomer = await this.stripe.customers.update(customerId, {
				invoice_settings: {
					default_payment_method: paymentMethodId,
				},
			});
			try {
				await getBillingRepository().customers.upsertFromStripe(updatedCustomer);
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, customerId: updatedCustomer.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
			}
		} catch (error) {
			Logger.warn(
				{error, customerId, paymentMethodId},
				'Failed to set localized card preapproval default payment method',
			);
		}
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
		try {
			const session = await this.stripe.billingPortal.sessions.create({
				customer: user.stripeCustomerId,
				return_url: `${Config.endpoints.webApp}/premium-callback?status=closed-billing-portal`,
			});
			if (!session.url) {
				Logger.error({userId, customerId: user.stripeCustomerId}, 'Stripe customer portal session missing url');
				throw new StripeError('Stripe customer portal session missing url');
			}
			return session.url;
		} catch (error: unknown) {
			Logger.error({error, userId, customerId: user.stripeCustomerId}, 'Failed to create customer portal session');
			const message = error instanceof Error ? error.message : 'Failed to create customer portal session';
			throw new StripeError(message);
		}
	}

	async getPriceIds(countryCode?: string, pricingMode: PricingMode = 'localized'): Promise<PriceIdsResponse> {
		const resolvedPrices = this.resolveConfiguredPriceIds(countryCode, pricingMode);
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

	private static readonly CUSTOMER_LOCK_TTL_SECONDS = seconds('30 seconds');
	private static readonly LOCALIZED_CARD_PREAPPROVAL_CONTINUE_LOCK_TTL_SECONDS = seconds('30 seconds');
	private static readonly LOCALIZED_CARD_PREAPPROVAL_TTL_SECONDS = seconds('1 day');
	private static readonly PRICE_CACHE_TTL_SECONDS = seconds('1 hour');
	private static readonly PRICE_CACHE_PRODUCE_TIMEOUT_MS = 90000;

	private resolveConfiguredPriceIds(countryCode?: string, pricingMode: PricingMode = 'localized'): ResolvedPriceIds {
		const recurringCurrencyPreferences =
			pricingMode === 'base' ? getBaseCurrencyPreferences(countryCode) : getCurrencyPreferences(countryCode);
		const giftCurrencyPreferences =
			pricingMode === 'base' ? getBaseGiftCurrencyPreferences(countryCode) : getGiftCurrencyPreferences(countryCode);
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
		throw new StripeError(`Stripe gift price ids missing for supported currencies: ${preferredCurrencies.join(', ')}`);
	}

	private getConfiguredRecurringPriceIdsForCurrency(
		currency: Currency,
	): Pick<ResolvedPriceIds, 'monthly' | 'yearly' | 'currency'> | null {
		const prices = Config.stripe.prices;
		if (!prices) {
			return null;
		}
		switch (currency) {
			case 'EUR':
				if (!prices.monthlyEur || !prices.yearlyEur) {
					return null;
				}
				return {
					monthly: prices.monthlyEur,
					yearly: prices.yearlyEur,
					currency,
				};
			case 'BRL':
				if (!prices.monthlyBrl || !prices.yearlyBrl) {
					return null;
				}
				return {
					monthly: prices.monthlyBrl,
					yearly: prices.yearlyBrl,
					currency,
				};
			case 'INR':
				if (!prices.monthlyInr || !prices.yearlyInr) {
					return null;
				}
				return {
					monthly: prices.monthlyInr,
					yearly: prices.yearlyInr,
					currency,
				};
			case 'PLN':
				if (!prices.monthlyPln || !prices.yearlyPln) {
					return null;
				}
				return {
					monthly: prices.monthlyPln,
					yearly: prices.yearlyPln,
					currency,
				};
			case 'TRY':
				if (!prices.monthlyTry || !prices.yearlyTry) {
					return null;
				}
				return {
					monthly: prices.monthlyTry,
					yearly: prices.yearlyTry,
					currency,
				};
			case 'USD':
				if (!prices.monthlyUsd || !prices.yearlyUsd) {
					return null;
				}
				return {
					monthly: prices.monthlyUsd,
					yearly: prices.yearlyUsd,
					currency,
				};
			default:
				return null;
		}
	}

	private getConfiguredGiftPriceIdsForCurrency(
		currency: Currency,
	): Pick<ResolvedPriceIds, 'gift_1_month' | 'gift_1_year' | 'gift_currency'> | null {
		const prices = Config.stripe.prices;
		if (!prices) {
			return null;
		}
		switch (currency) {
			case 'BRL':
				if (!prices.gift1MonthBrl || !prices.gift1YearBrl) {
					return null;
				}
				return {
					gift_1_month: prices.gift1MonthBrl,
					gift_1_year: prices.gift1YearBrl,
					gift_currency: 'BRL',
				};
			case 'INR':
				if (!prices.gift1MonthInr || !prices.gift1YearInr) {
					return null;
				}
				return {
					gift_1_month: prices.gift1MonthInr,
					gift_1_year: prices.gift1YearInr,
					gift_currency: 'INR',
				};
			case 'PLN':
				if (!prices.gift1MonthPln || !prices.gift1YearPln) {
					return null;
				}
				return {
					gift_1_month: prices.gift1MonthPln,
					gift_1_year: prices.gift1YearPln,
					gift_currency: 'PLN',
				};
			case 'TRY':
				if (!prices.gift1MonthTry || !prices.gift1YearTry) {
					return null;
				}
				return {
					gift_1_month: prices.gift1MonthTry,
					gift_1_year: prices.gift1YearTry,
					gift_currency: 'TRY',
				};
			case 'EUR':
				if (!prices.gift1MonthEur || !prices.gift1YearEur) {
					return null;
				}
				return {
					gift_1_month: prices.gift1MonthEur,
					gift_1_year: prices.gift1YearEur,
					gift_currency: 'EUR',
				};
			case 'USD':
				if (!prices.gift1MonthUsd || !prices.gift1YearUsd) {
					return null;
				}
				return {
					gift_1_month: prices.gift1MonthUsd,
					gift_1_year: prices.gift1YearUsd,
					gift_currency: 'USD',
				};
			default:
				return null;
		}
	}

	private async getStripePriceSummary(priceId: string | null): Promise<StripePriceSummary | null> {
		if (!priceId || !this.stripe) {
			return null;
		}
		try {
			return await this.cacheService.getOrSet<StripePriceSummary>(
				`stripe_price_summary:${priceId}`,
				async () => {
					const price = await this.stripe!.prices.retrieve(priceId);
					return {
						unitAmountMinor: price.unit_amount ?? null,
					};
				},
				StripeCheckoutService.PRICE_CACHE_TTL_SECONDS,
				StripeCheckoutService.PRICE_CACHE_PRODUCE_TIMEOUT_MS,
			);
		} catch (error: unknown) {
			Logger.warn({error, priceId}, 'Failed to retrieve Stripe price summary');
			return null;
		}
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
		if (productInfo.currency === 'BRL' && checkoutMode === 'payment') {
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
			const mandateAmount = await this.resolveMandateAmount(priceId);
			return {
				upi: {
					mandate_options: {
						amount: mandateAmount,
						amount_type: 'maximum',
						description: UPI_MANDATE_DESCRIPTION,
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

	private async ensureStripeCustomer(user: User): Promise<User> {
		if (user.stripeCustomerId) {
			return user;
		}
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const lockKey = `stripe_customer_create_lock:${user.id}`;
		const lockToken = await this.cacheService.acquireLock(lockKey, StripeCheckoutService.CUSTOMER_LOCK_TTL_SECONDS);
		if (!lockToken) {
			const freshUser = await this.userRepository.findUnique(user.id);
			if (freshUser?.stripeCustomerId) {
				return freshUser;
			}
			throw new StripeError('Failed to acquire customer creation lock');
		}
		try {
			const freshUser = await this.userRepository.findUnique(user.id);
			if (freshUser?.stripeCustomerId) {
				return freshUser;
			}
			const customer = await this.stripe.customers.create({
				email: user.email ?? undefined,
				metadata: {
					userId: user.id.toString(),
				},
			});
			try {
				await getBillingRepository().customers.upsertFromStripe(customer, {knownUserId: user.id});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, customerId: customer.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
			}
			const updatedUser = await this.userRepository.patchUpsert(
				user.id,
				{
					stripe_customer_id: customer.id,
				},
				user.toRow(),
			);
			Logger.debug({userId: user.id, customerId: customer.id}, 'Stripe customer created');
			return updatedUser;
		} finally {
			try {
				const released = await this.cacheService.releaseLock(lockKey, lockToken);
				if (!released) {
					Logger.warn({userId: user.id, lockKey}, 'Customer creation lock token no longer matched on release');
				}
			} catch (error) {
				Logger.error({error, userId: user.id, lockKey}, 'Failed to release customer creation lock');
			}
		}
	}
}
