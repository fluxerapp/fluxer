// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BillingRepository} from '@app/api/billing/repositories/BillingRepository';
import {Config} from '@app/api/Config';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {StoreBillingRepository} from '@app/api/store_billing/StoreBillingRepository';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {getProductRegistry, type ProductRegistry} from '@app/api/stripe/ProductRegistry';
import {getStripeClient} from '@app/api/stripe/StripeClient';
import {PremiumStateService} from '@app/api/stripe/services/PremiumStateService';
import type {CreateCheckoutSessionParams} from '@app/api/stripe/services/StripeCheckoutService';
import {StripeCheckoutService} from '@app/api/stripe/services/StripeCheckoutService';
import {StripeGiftService} from '@app/api/stripe/services/StripeGiftService';
import {StripePremiumService} from '@app/api/stripe/services/StripePremiumService';
import {StripeRefundService} from '@app/api/stripe/services/StripeRefundService';
import {StripeSubscriptionService} from '@app/api/stripe/services/StripeSubscriptionService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {PremiumPurchaseBlockedError} from '@fluxer/errors/src/domains/payment/PremiumPurchaseBlockedError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type Stripe from 'stripe';

export class StripeService {
	private stripe: Stripe | null;
	private productRegistry: ProductRegistry;
	readonly checkout: StripeCheckoutService;
	readonly subscriptions: StripeSubscriptionService;
	readonly gifts: StripeGiftService;
	readonly premium: StripePremiumService;
	readonly premiumState: PremiumStateService;
	readonly refunds: StripeRefundService;

	constructor(
		private userRepository: UserRepository,
		private gatewayService: IGatewayService,
		private guildRepository: GuildRepository,
		private guildService: GuildService,
		private cacheService: ICacheService,
		private billingRepository: BillingRepository,
		private storeBillingRepository: StoreBillingRepository | null = null,
		private storeEntitlementService: StoreEntitlementService | null = null,
	) {
		this.productRegistry = getProductRegistry();
		this.stripe = getStripeClient();
		this.premium = new StripePremiumService(
			this.userRepository,
			this.gatewayService,
			this.guildRepository,
			this.guildService,
		);
		this.premiumState = new PremiumStateService(
			this.userRepository,
			this.gatewayService,
			this.billingRepository,
			this.stripe,
			this.cacheService,
			this.storeBillingRepository,
		);
		this.checkout = new StripeCheckoutService(
			this.stripe,
			this.userRepository,
			this.productRegistry,
			this.cacheService,
			this.storeEntitlementService,
		);
		this.subscriptions = new StripeSubscriptionService(
			this.stripe,
			this.userRepository,
			this.productRegistry,
			this.cacheService,
			this.gatewayService,
			this.storeEntitlementService,
		);
		this.gifts = new StripeGiftService(
			this.stripe,
			this.userRepository,
			this.cacheService,
			this.gatewayService,
			this.checkout,
			this.premium,
			this.subscriptions,
			this.storeEntitlementService,
		);
		this.refunds = new StripeRefundService(this.stripe, this.userRepository, this.subscriptions);
	}

	getStripe(): Stripe | null {
		return this.stripe;
	}

	async createCheckoutSession(params: CreateCheckoutSessionParams): Promise<string> {
		try {
			return await this.checkout.createCheckoutSession(params);
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
		await this.subscriptions.changeBillingCycle(params.userId, productInfo.billingCycle, 'period_end');
		return `${Config.endpoints.webApp}/premium-callback?status=success`;
	}
}
