/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import Stripe from 'stripe';
import type {AuthService} from '~/auth/AuthService';
import type {UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {GuildService} from '~/guild/services/GuildService';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {GiftCode} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import type {Currency} from '~/utils/CurrencyUtils';
import {ProductRegistry} from './ProductRegistry';
import type {CreateCheckoutSessionParams} from './services/StripeCheckoutService';
import {StripeCheckoutService} from './services/StripeCheckoutService';
import {StripeGiftService} from './services/StripeGiftService';
import {StripePremiumService} from './services/StripePremiumService';
import {StripeSubscriptionService} from './services/StripeSubscriptionService';
import type {HandleWebhookParams} from './services/StripeWebhookService';
import {StripeWebhookService} from './services/StripeWebhookService';

export class StripeService {
	private stripe: Stripe | null = null;
	private productRegistry: ProductRegistry;
	private checkoutService: StripeCheckoutService;
	private subscriptionService: StripeSubscriptionService;
	private giftService: StripeGiftService;
	private premiumService: StripePremiumService;
	private webhookService: StripeWebhookService;

	constructor(
		private userRepository: IUserRepository,
		private authService: AuthService,
		private gatewayService: IGatewayService,
		private emailService: IEmailService,
		private guildRepository: IGuildRepository,
		private guildService: GuildService,
		private cacheService: ICacheService,
	) {
		this.productRegistry = new ProductRegistry();

		if (Config.stripe.enabled && Config.stripe.secretKey) {
			this.stripe = new Stripe(Config.stripe.secretKey, {
				apiVersion: '2025-12-15.clover',
			});
		}

		this.premiumService = new StripePremiumService(
			this.userRepository,
			this.gatewayService,
			this.guildRepository,
			this.guildService,
		);

		this.checkoutService = new StripeCheckoutService(this.stripe, this.userRepository, this.productRegistry);

		this.subscriptionService = new StripeSubscriptionService(
			this.stripe,
			this.userRepository,
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

		this.webhookService = new StripeWebhookService(
			this.stripe,
			this.userRepository,
			this.authService,
			this.emailService,
			this.gatewayService,
			this.productRegistry,
			this.giftService,
			this.premiumService,
		);
	}

	async createCheckoutSession(params: CreateCheckoutSessionParams): Promise<string> {
		return this.checkoutService.createCheckoutSession(params);
	}

	async createCustomerPortalSession(userId: UserID): Promise<string> {
		return this.checkoutService.createCustomerPortalSession(userId);
	}

	getPriceIds(countryCode?: string): {
		monthly: string | null;
		yearly: string | null;
		visionary: string | null;
		giftVisionary: string | null;
		gift1Month: string | null;
		gift1Year: string | null;
		currency: Currency;
	} {
		return this.checkoutService.getPriceIds(countryCode);
	}

	async cancelSubscriptionAtPeriodEnd(userId: UserID): Promise<void> {
		return this.subscriptionService.cancelSubscriptionAtPeriodEnd(userId);
	}

	async reactivateSubscription(userId: UserID): Promise<void> {
		return this.subscriptionService.reactivateSubscription(userId);
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

	async getVisionarySlots(): Promise<{total: number; remaining: number}> {
		return this.premiumService.getVisionarySlots();
	}

	async rejoinVisionariesGuild(userId: UserID): Promise<void> {
		return this.premiumService.rejoinVisionariesGuild(userId);
	}

	async rejoinOperatorsGuild(userId: UserID): Promise<void> {
		return this.premiumService.rejoinOperatorsGuild(userId);
	}

	async handleWebhook(params: HandleWebhookParams): Promise<void> {
		return this.webhookService.handleWebhook(params);
	}
}
