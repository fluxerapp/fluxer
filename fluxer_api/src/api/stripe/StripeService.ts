// SPDX-License-Identifier: AGPL-3.0-or-later

import {PremiumPurchaseBlockedError} from '@fluxer/errors/src/domains/payment/PremiumPurchaseBlockedError';
import type {
	CurrentSubscriptionPriceResponse,
	PremiumStateResponse,
	SelfServeRefundEligibilityResponse,
	SelfServeRefundResponse,
	SwitchToListPriceResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import Stripe from 'stripe';
import type {UserID} from '../BrandedTypes';
import type {BillingRepository} from '../billing/repositories/BillingRepository';
import {Config} from '../Config';
import type {GiftCodeDurationType} from '../database/types/PaymentTypes';
import type {IGuildRepositoryAggregate} from '../guild/repositories/IGuildRepositoryAggregate';
import type {GuildService} from '../guild/services/GuildService';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import type {GiftCode} from '../models/GiftCode';
import type {User} from '../models/User';
import type {IUserRepository} from '../user/IUserRepository';
import type {Currency} from '../utils/CurrencyUtils';
import {ProductRegistry} from './ProductRegistry';
import {STRIPE_API_VERSION} from './StripeApiVersion';
import {PremiumStateService} from './services/PremiumStateService';
import type {
	ContinueLocalizedCardPreapprovalResult,
	CreateCheckoutSessionParams,
} from './services/StripeCheckoutService';
import {StripeCheckoutService} from './services/StripeCheckoutService';
import {StripeGiftService} from './services/StripeGiftService';
import {StripePremiumService} from './services/StripePremiumService';
import {StripeRefundService} from './services/StripeRefundService';
import {StripeSubscriptionService} from './services/StripeSubscriptionService';

export class StripeService {
	private stripe: Stripe | null = null;
	private productRegistry: ProductRegistry;
	private checkoutService: StripeCheckoutService;
	private subscriptionService: StripeSubscriptionService;
	private giftService: StripeGiftService;
	private premiumService: StripePremiumService;
	private premiumStateService: PremiumStateService;
	private refundService: StripeRefundService;

	constructor(
		private userRepository: IUserRepository,
		private gatewayService: IGatewayService,
		private guildRepository: IGuildRepositoryAggregate,
		private guildService: GuildService,
		private cacheService: ICacheService,
		private billingRepository: BillingRepository,
	) {
		this.productRegistry = new ProductRegistry();
		if (Config.stripe.enabled && Config.stripe.secretKey) {
			this.stripe = new Stripe(Config.stripe.secretKey, {
				apiVersion: STRIPE_API_VERSION,
				httpClient: Config.dev.testModeEnabled ? Stripe.createFetchHttpClient() : undefined,
			});
		}
		this.premiumService = new StripePremiumService(
			this.userRepository,
			this.gatewayService,
			this.guildRepository,
			this.guildService,
		);
		this.premiumStateService = new PremiumStateService(
			this.userRepository,
			this.gatewayService,
			this.billingRepository,
			this.stripe,
		);
		this.checkoutService = new StripeCheckoutService(
			this.stripe,
			this.userRepository,
			this.productRegistry,
			this.cacheService,
		);
		this.subscriptionService = new StripeSubscriptionService(
			this.stripe,
			this.userRepository,
			this.productRegistry,
			this.cacheService,
			this.gatewayService,
		);
		this.giftService = new StripeGiftService(
			this.stripe,
			this.userRepository,
			this.cacheService,
			this.gatewayService,
			this.checkoutService,
			this.premiumService,
			this.subscriptionService,
		);
		this.refundService = new StripeRefundService(this.stripe, this.userRepository, this.subscriptionService);
	}

	getStripe(): Stripe | null {
		return this.stripe;
	}

	async createCheckoutSession(params: CreateCheckoutSessionParams): Promise<string> {
		try {
			return await this.checkoutService.createCheckoutSession(params);
		} catch (error) {
			const periodEndSwapUrl = await this.tryScheduleBlockedRecurringCheckoutCycleSwap(params, error);
			if (periodEndSwapUrl) {
				return periodEndSwapUrl;
			}
			throw error;
		}
	}

	private async tryScheduleBlockedRecurringCheckoutCycleSwap(
		params: CreateCheckoutSessionParams,
		error: unknown,
	): Promise<string | null> {
		if (!(error instanceof PremiumPurchaseBlockedError) || error.data?.reason !== 'existing_subscription') {
			return null;
		}
		const subscriptionStatus = error.data.subscription_status;
		if (subscriptionStatus !== 'active' && subscriptionStatus !== 'trialing') {
			return null;
		}
		const productInfo = this.productRegistry.getProduct(params.priceId);
		if (!productInfo?.billingCycle || !this.productRegistry.isRecurringSubscription(productInfo) || params.isGift) {
			return null;
		}
		const user = await this.userRepository.findUnique(params.userId);
		if (!user?.premiumBillingCycle || user.premiumBillingCycle === productInfo.billingCycle) {
			return null;
		}
		await this.subscriptionService.changeBillingCycle(params.userId, productInfo.billingCycle, 'period_end');
		return `${Config.endpoints.webApp}/premium-callback?status=success`;
	}

	async createLocalizedCardPreapprovalSession(
		params: Pick<
			CreateCheckoutSessionParams,
			| 'clientGeoipCountryCode'
			| 'countryCode'
			| 'euWithdrawalWaiverAccepted'
			| 'isBusiness'
			| 'priceId'
			| 'purchaseGeoipCountryCode'
			| 'userId'
		>,
	): Promise<string> {
		return this.checkoutService.createLocalizedCardPreapprovalSession(params);
	}

	async continueLocalizedCardPreapproval(token: string): Promise<ContinueLocalizedCardPreapprovalResult> {
		return this.checkoutService.continueLocalizedCardPreapproval(token);
	}

	async createCustomerPortalSession(userId: UserID): Promise<string> {
		return this.checkoutService.createCustomerPortalSession(userId);
	}

	async getPriceIds(countryCode?: string): Promise<{
		monthly: string | null;
		yearly: string | null;
		gift_1_month: string | null;
		gift_1_year: string | null;
		currency: Currency;
		gift_currency: Currency;
		monthly_amount_minor: number | null;
		yearly_amount_minor: number | null;
		gift_1_month_amount_minor: number | null;
		gift_1_year_amount_minor: number | null;
	}> {
		return this.checkoutService.getPriceIds(countryCode);
	}

	async getCurrentSubscriptionPrice(userId: UserID): Promise<CurrentSubscriptionPriceResponse> {
		return this.subscriptionService.getCurrentSubscriptionPrice(userId);
	}

	async cancelSubscriptionAtPeriodEnd(userId: UserID): Promise<void> {
		return this.subscriptionService.cancelSubscriptionAtPeriodEnd(userId);
	}

	async cancelSubscriptionImmediately(userId: UserID, reason?: string): Promise<void> {
		return this.subscriptionService.cancelSubscriptionImmediately(userId, reason);
	}

	async reactivateSubscription(userId: UserID): Promise<void> {
		return this.subscriptionService.reactivateSubscription(userId);
	}

	async changeSubscriptionBillingCycle(
		userId: UserID,
		billingCycle: 'monthly' | 'yearly',
		effectiveAt: 'now' | 'period_end' = 'now',
	): Promise<void> {
		return this.subscriptionService.changeBillingCycle(userId, billingCycle, effectiveAt);
	}

	async switchSubscriptionToCurrentListPrice(userId: UserID): Promise<SwitchToListPriceResponse> {
		return this.subscriptionService.switchToCurrentListPrice(userId);
	}

	async cancelPendingSubscriptionChange(userId: UserID): Promise<void> {
		return this.subscriptionService.cancelPendingSubscriptionChange(userId);
	}

	async extendSubscriptionWithGiftTrialDuration(
		user: User,
		durationType: GiftCodeDurationType,
		durationQuantity: number,
		idempotencyKey: string,
	): Promise<void> {
		return this.subscriptionService.extendSubscriptionWithGiftTrialDuration(
			user,
			durationType,
			durationQuantity,
			idempotencyKey,
		);
	}

	async getGiftCode(code: string): Promise<GiftCode> {
		return this.giftService.getGiftCode(code);
	}

	async redeemGiftCode(userId: UserID, code: string): Promise<void> {
		return this.giftService.redeemGiftCode(userId, code);
	}

	async getUserGifts(userId: UserID): Promise<Array<GiftCode>> {
		return this.giftService.getUserGifts(userId);
	}

	async endPremiumGracePeriod(userId: UserID): Promise<boolean> {
		return this.premiumService.endGracePeriod(userId);
	}

	async getPremiumState(userId: UserID, countryCode?: string): Promise<PremiumStateResponse> {
		return this.premiumStateService.getState(userId, countryCode);
	}

	async setPremiumPerksDisabled(userId: UserID, disabled: boolean): Promise<PremiumStateResponse> {
		return this.premiumStateService.setPerksDisabled(userId, disabled);
	}

	async rejoinVisionariesGuild(userId: UserID): Promise<void> {
		return this.premiumService.rejoinVisionariesGuild(userId);
	}

	async getSelfServeRefundEligibility(userId: UserID): Promise<SelfServeRefundEligibilityResponse> {
		return this.refundService.getEligibility(userId);
	}

	async refundLatestPurchase(userId: UserID): Promise<SelfServeRefundResponse> {
		return this.refundService.refundLatestPurchase(userId);
	}
}
