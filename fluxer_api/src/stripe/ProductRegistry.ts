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

import {Config} from '~/Config';
import {UserPremiumTypes} from '~/Constants';

export enum ProductType {
	MONTHLY_SUBSCRIPTION = 'monthly_subscription',
	YEARLY_SUBSCRIPTION = 'yearly_subscription',
	VISIONARY_LIFETIME = 'visionary_lifetime',
	GIFT_1_MONTH = 'gift_1_month',
	GIFT_1_YEAR = 'gift_1_year',
	GIFT_VISIONARY = 'gift_visionary',
}

export interface ProductInfo {
	type: ProductType;
	premiumType: 1 | 2;
	durationMonths: number;
	isGift: boolean;
	billingCycle?: 'monthly' | 'yearly';
}

export class ProductRegistry {
	private products = new Map<string, ProductInfo>();

	constructor() {
		const prices = Config.stripe.prices;
		if (!prices) return;

		this.registerProduct(prices.monthlyUsd, {
			type: ProductType.MONTHLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: false,
			billingCycle: 'monthly',
		});

		this.registerProduct(prices.monthlyEur, {
			type: ProductType.MONTHLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: false,
			billingCycle: 'monthly',
		});

		this.registerProduct(prices.yearlyUsd, {
			type: ProductType.YEARLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: false,
			billingCycle: 'yearly',
		});

		this.registerProduct(prices.yearlyEur, {
			type: ProductType.YEARLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: false,
			billingCycle: 'yearly',
		});

		this.registerProduct(prices.visionaryUsd, {
			type: ProductType.VISIONARY_LIFETIME,
			premiumType: UserPremiumTypes.LIFETIME,
			durationMonths: 0,
			isGift: false,
		});

		this.registerProduct(prices.visionaryEur, {
			type: ProductType.VISIONARY_LIFETIME,
			premiumType: UserPremiumTypes.LIFETIME,
			durationMonths: 0,
			isGift: false,
		});

		this.registerProduct(prices.giftVisionaryUsd, {
			type: ProductType.VISIONARY_LIFETIME,
			premiumType: UserPremiumTypes.LIFETIME,
			durationMonths: 0,
			isGift: true,
		});

		this.registerProduct(prices.giftVisionaryEur, {
			type: ProductType.VISIONARY_LIFETIME,
			premiumType: UserPremiumTypes.LIFETIME,
			durationMonths: 0,
			isGift: true,
		});

		this.registerProduct(prices.gift1MonthUsd, {
			type: ProductType.GIFT_1_MONTH,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: true,
		});

		this.registerProduct(prices.gift1MonthEur, {
			type: ProductType.GIFT_1_MONTH,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: true,
		});

		this.registerProduct(prices.gift1YearUsd, {
			type: ProductType.GIFT_1_YEAR,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: true,
		});

		this.registerProduct(prices.gift1YearEur, {
			type: ProductType.GIFT_1_YEAR,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: true,
		});
	}

	private registerProduct(priceId: string | undefined, info: ProductInfo): void {
		if (priceId) {
			this.products.set(priceId, info);
		}
	}

	getProduct(priceId: string): ProductInfo | null {
		return this.products.get(priceId) || null;
	}

	isRecurringSubscription(info: ProductInfo): boolean {
		return !info.isGift && info.premiumType === UserPremiumTypes.SUBSCRIPTION;
	}
}
