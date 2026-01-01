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

import type Stripe from 'stripe';
import type {UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {UserFlags, UserPremiumTypes} from '~/Constants';
import {NoVisionarySlotsAvailableError, PremiumPurchaseBlockedError, StripeError, UnknownUserError} from '~/Errors';
import {Logger} from '~/Logger';
import type {User} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import {type Currency, getCurrency} from '~/utils/CurrencyUtils';
import type {ProductRegistry} from '../ProductRegistry';

const FIRST_REFUND_BLOCK_DAYS = 30;

export interface CreateCheckoutSessionParams {
	userId: UserID;
	priceId: string;
	isGift?: boolean;
}

export class StripeCheckoutService {
	constructor(
		private stripe: Stripe | null,
		private userRepository: IUserRepository,
		private productRegistry: ProductRegistry,
	) {}

	async createCheckoutSession({userId, priceId, isGift = false}: CreateCheckoutSessionParams): Promise<string> {
		if (!this.stripe) {
			throw new StripeError('Payment processing is not available');
		}

		const productInfo = this.productRegistry.getProduct(priceId);
		if (!productInfo) {
			Logger.error({priceId, userId}, 'Invalid or unknown price ID');
			throw new StripeError('Invalid product selection');
		}

		if (productInfo.isGift !== isGift) {
			Logger.error(
				{priceId, userId, expectedIsGift: productInfo.isGift, providedIsGift: isGift},
				'Gift parameter mismatch',
			);
			throw new StripeError('Invalid product configuration');
		}

		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		if (
			user.premiumType === UserPremiumTypes.LIFETIME &&
			this.productRegistry.isRecurringSubscription(productInfo) &&
			!productInfo.isGift
		) {
			throw new PremiumPurchaseBlockedError();
		}

		this.validateUserCanPurchase(user);

		if (productInfo.premiumType === UserPremiumTypes.LIFETIME) {
			const allSlots = await this.userRepository.listVisionarySlots();
			const unreservedSlots = allSlots.filter((slot) => !slot.isReserved());

			if (unreservedSlots.length === 0) {
				throw new NoVisionarySlotsAvailableError();
			}

			if (unreservedSlots.length < 10) {
				Logger.warn({remainingSlots: unreservedSlots.length, userId, isGift}, 'Visionary slots running low');
			}
		}

		const customerId = await this.ensureStripeCustomer(user);

		const checkoutMode: Stripe.Checkout.SessionCreateParams.Mode = this.productRegistry.isRecurringSubscription(
			productInfo,
		)
			? 'subscription'
			: 'payment';

		const checkoutParams: Stripe.Checkout.SessionCreateParams = {
			customer: customerId,
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
			customer_update: {
				address: 'auto',
			},
			allow_promotion_codes: true,
		};

		try {
			const session = await this.stripe.checkout.sessions.create(checkoutParams);

			await this.userRepository.createPayment({
				checkout_session_id: session.id,
				user_id: userId,
				price_id: priceId,
				product_type: productInfo.type,
				status: 'pending',
				is_gift: isGift,
				created_at: new Date(),
			});

			Logger.debug({userId, sessionId: session.id, productType: productInfo.type}, 'Checkout session created');

			return session.url!;
		} catch (error: unknown) {
			Logger.error({error, userId}, 'Failed to create Stripe checkout session');
			const message = error instanceof Error ? error.message : 'Failed to create checkout session';
			throw new StripeError(message);
		}
	}

	async createCustomerPortalSession(userId: UserID): Promise<string> {
		if (!this.stripe) {
			throw new StripeError('Payment processing is not available');
		}

		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		if (!user.stripeCustomerId) {
			throw new StripeError('No purchase history found - customer portal not available');
		}

		try {
			const session = await this.stripe.billingPortal.sessions.create({
				customer: user.stripeCustomerId,
				return_url: `${Config.endpoints.webApp}/premium-callback?status=closed-billing-portal`,
			});

			return session.url;
		} catch (error: unknown) {
			Logger.error({error, userId, customerId: user.stripeCustomerId}, 'Failed to create customer portal session');
			const message = error instanceof Error ? error.message : 'Failed to create customer portal session';
			throw new StripeError(message);
		}
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
		const currency = getCurrency(countryCode);
		const prices = Config.stripe.prices;

		if (currency === 'EUR') {
			return {
				monthly: prices?.monthlyEur ?? null,
				yearly: prices?.yearlyEur ?? null,
				visionary: prices?.visionaryEur ?? null,
				giftVisionary: prices?.giftVisionaryEur ?? null,
				gift1Month: prices?.gift1MonthEur ?? null,
				gift1Year: prices?.gift1YearEur ?? null,
				currency,
			};
		}

		return {
			monthly: prices?.monthlyUsd ?? null,
			yearly: prices?.yearlyUsd ?? null,
			visionary: prices?.visionaryUsd ?? null,
			giftVisionary: prices?.giftVisionaryUsd ?? null,
			gift1Month: prices?.gift1MonthUsd ?? null,
			gift1Year: prices?.gift1YearUsd ?? null,
			currency,
		};
	}

	validateUserCanPurchase(user: User): void {
		if (user.flags & UserFlags.PREMIUM_PURCHASE_DISABLED) {
			throw new PremiumPurchaseBlockedError();
		}

		if (user.firstRefundAt) {
			const daysSinceFirstRefund = Math.floor((Date.now() - user.firstRefundAt.getTime()) / (1000 * 60 * 60 * 24));
			if (daysSinceFirstRefund < FIRST_REFUND_BLOCK_DAYS) {
				throw new PremiumPurchaseBlockedError();
			}
		}
	}

	private async ensureStripeCustomer(user: User): Promise<string> {
		if (user.stripeCustomerId) {
			return user.stripeCustomerId;
		}

		if (!this.stripe) {
			throw new StripeError('Payment processing is not available');
		}

		const customer = await this.stripe.customers.create({
			email: user.email ?? undefined,
			metadata: {
				userId: user.id.toString(),
			},
		});

		await this.userRepository.patchUpsert(user.id, {
			stripe_customer_id: customer.id,
		});

		Logger.debug({userId: user.id, customerId: customer.id}, 'Stripe customer created');

		return customer.id;
	}
}
