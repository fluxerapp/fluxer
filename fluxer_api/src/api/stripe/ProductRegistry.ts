// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {Config} from '../Config';
import {Logger} from '../Logger';
import type {Currency} from '../utils/CurrencyUtils';

export enum ProductType {
	MONTHLY_SUBSCRIPTION = 'monthly_subscription',
	YEARLY_SUBSCRIPTION = 'yearly_subscription',
	GIFT_1_MONTH = 'gift_1_month',
	GIFT_1_YEAR = 'gift_1_year',
}

export type RecurringBillingCycle = 'monthly' | 'yearly';

export interface ProductInfo {
	type: ProductType;
	premiumType: 1 | 2;
	durationMonths: number;
	isGift: boolean;
	currency: Currency;
	billingCycle?: RecurringBillingCycle;
}

const LEGACY_SLOT_SHAPES: Record<string, Omit<ProductInfo, 'currency'> | undefined> = {
	monthly: {
		type: ProductType.MONTHLY_SUBSCRIPTION,
		premiumType: UserPremiumTypes.SUBSCRIPTION,
		durationMonths: 1,
		isGift: false,
		billingCycle: 'monthly',
	},
	yearly: {
		type: ProductType.YEARLY_SUBSCRIPTION,
		premiumType: UserPremiumTypes.SUBSCRIPTION,
		durationMonths: 12,
		isGift: false,
		billingCycle: 'yearly',
	},
	gift_1_month: {
		type: ProductType.GIFT_1_MONTH,
		premiumType: UserPremiumTypes.SUBSCRIPTION,
		durationMonths: 1,
		isGift: true,
	},
	gift_1_year: {
		type: ProductType.GIFT_1_YEAR,
		premiumType: UserPremiumTypes.SUBSCRIPTION,
		durationMonths: 12,
		isGift: true,
	},
};

const LEGACY_SLOT_CURRENCIES: Record<string, Currency | undefined> = {
	usd: 'USD',
	eur: 'EUR',
	brl: 'BRL',
	dkk: 'DKK',
	inr: 'INR',
	nok: 'NOK',
	pln: 'PLN',
	sek: 'SEK',
	try: 'TRY',
};

function parseLegacySlot(slot: string): ProductInfo | null {
	const separatorIndex = slot.lastIndexOf('_');
	if (separatorIndex <= 0) {
		return null;
	}
	const shape = LEGACY_SLOT_SHAPES[slot.slice(0, separatorIndex)];
	const currency = LEGACY_SLOT_CURRENCIES[slot.slice(separatorIndex + 1).toLowerCase()];
	if (!shape || !currency) {
		return null;
	}
	return {...shape, currency};
}

export class ProductRegistry {
	private products = new Map<string, ProductInfo>();

	constructor() {
		this.registerConfiguredProducts();
		this.registerLegacyProducts();
	}

	private registerConfiguredProducts(): void {
		const prices = Config.stripe.prices;
		if (!prices) return;
		this.registerProduct(prices.monthlyUsd, {
			type: ProductType.MONTHLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: false,
			currency: 'USD',
			billingCycle: 'monthly',
		});
		this.registerProduct(prices.monthlyEur, {
			type: ProductType.MONTHLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: false,
			currency: 'EUR',
			billingCycle: 'monthly',
		});
		this.registerProduct(prices.monthlyBrl, {
			type: ProductType.MONTHLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: false,
			currency: 'BRL',
			billingCycle: 'monthly',
		});
		this.registerProduct(prices.monthlyDkk, {
			type: ProductType.MONTHLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: false,
			currency: 'DKK',
			billingCycle: 'monthly',
		});
		this.registerProduct(prices.monthlyInr, {
			type: ProductType.MONTHLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: false,
			currency: 'INR',
			billingCycle: 'monthly',
		});
		this.registerProduct(prices.monthlyNok, {
			type: ProductType.MONTHLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: false,
			currency: 'NOK',
			billingCycle: 'monthly',
		});
		this.registerProduct(prices.monthlyPln, {
			type: ProductType.MONTHLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: false,
			currency: 'PLN',
			billingCycle: 'monthly',
		});
		this.registerProduct(prices.monthlySek, {
			type: ProductType.MONTHLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: false,
			currency: 'SEK',
			billingCycle: 'monthly',
		});
		this.registerProduct(prices.monthlyTry, {
			type: ProductType.MONTHLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: false,
			currency: 'TRY',
			billingCycle: 'monthly',
		});
		this.registerProduct(prices.yearlyUsd, {
			type: ProductType.YEARLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: false,
			currency: 'USD',
			billingCycle: 'yearly',
		});
		this.registerProduct(prices.yearlyEur, {
			type: ProductType.YEARLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: false,
			currency: 'EUR',
			billingCycle: 'yearly',
		});
		this.registerProduct(prices.yearlyBrl, {
			type: ProductType.YEARLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: false,
			currency: 'BRL',
			billingCycle: 'yearly',
		});
		this.registerProduct(prices.yearlyDkk, {
			type: ProductType.YEARLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: false,
			currency: 'DKK',
			billingCycle: 'yearly',
		});
		this.registerProduct(prices.yearlyInr, {
			type: ProductType.YEARLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: false,
			currency: 'INR',
			billingCycle: 'yearly',
		});
		this.registerProduct(prices.yearlyNok, {
			type: ProductType.YEARLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: false,
			currency: 'NOK',
			billingCycle: 'yearly',
		});
		this.registerProduct(prices.yearlyPln, {
			type: ProductType.YEARLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: false,
			currency: 'PLN',
			billingCycle: 'yearly',
		});
		this.registerProduct(prices.yearlySek, {
			type: ProductType.YEARLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: false,
			currency: 'SEK',
			billingCycle: 'yearly',
		});
		this.registerProduct(prices.yearlyTry, {
			type: ProductType.YEARLY_SUBSCRIPTION,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: false,
			currency: 'TRY',
			billingCycle: 'yearly',
		});
		this.registerProduct(prices.gift1MonthUsd, {
			type: ProductType.GIFT_1_MONTH,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: true,
			currency: 'USD',
		});
		this.registerProduct(prices.gift1MonthEur, {
			type: ProductType.GIFT_1_MONTH,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: true,
			currency: 'EUR',
		});
		this.registerProduct(prices.gift1MonthBrl, {
			type: ProductType.GIFT_1_MONTH,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: true,
			currency: 'BRL',
		});
		this.registerProduct(prices.gift1MonthInr, {
			type: ProductType.GIFT_1_MONTH,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: true,
			currency: 'INR',
		});
		this.registerProduct(prices.gift1MonthDkk, {
			type: ProductType.GIFT_1_MONTH,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: true,
			currency: 'DKK',
		});
		this.registerProduct(prices.gift1YearDkk, {
			type: ProductType.GIFT_1_YEAR,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: true,
			currency: 'DKK',
		});
		this.registerProduct(prices.gift1MonthNok, {
			type: ProductType.GIFT_1_MONTH,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: true,
			currency: 'NOK',
		});
		this.registerProduct(prices.gift1YearNok, {
			type: ProductType.GIFT_1_YEAR,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: true,
			currency: 'NOK',
		});
		this.registerProduct(prices.gift1MonthSek, {
			type: ProductType.GIFT_1_MONTH,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: true,
			currency: 'SEK',
		});
		this.registerProduct(prices.gift1YearSek, {
			type: ProductType.GIFT_1_YEAR,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: true,
			currency: 'SEK',
		});
		this.registerProduct(prices.gift1MonthPln, {
			type: ProductType.GIFT_1_MONTH,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: true,
			currency: 'PLN',
		});
		this.registerProduct(prices.gift1MonthTry, {
			type: ProductType.GIFT_1_MONTH,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 1,
			isGift: true,
			currency: 'TRY',
		});
		this.registerProduct(prices.gift1YearUsd, {
			type: ProductType.GIFT_1_YEAR,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: true,
			currency: 'USD',
		});
		this.registerProduct(prices.gift1YearEur, {
			type: ProductType.GIFT_1_YEAR,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: true,
			currency: 'EUR',
		});
		this.registerProduct(prices.gift1YearBrl, {
			type: ProductType.GIFT_1_YEAR,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: true,
			currency: 'BRL',
		});
		this.registerProduct(prices.gift1YearInr, {
			type: ProductType.GIFT_1_YEAR,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: true,
			currency: 'INR',
		});
		this.registerProduct(prices.gift1YearPln, {
			type: ProductType.GIFT_1_YEAR,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: true,
			currency: 'PLN',
		});
		this.registerProduct(prices.gift1YearTry, {
			type: ProductType.GIFT_1_YEAR,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			durationMonths: 12,
			isGift: true,
			currency: 'TRY',
		});
	}

	private registerLegacyProducts(): void {
		const legacyPrices = Config.stripe.legacyPrices;
		if (!legacyPrices) return;
		if (typeof legacyPrices !== 'object' || Array.isArray(legacyPrices)) {
			Logger.warn({}, 'Ignoring legacy Stripe price configuration that is not an object of slot names to price ids');
			return;
		}
		for (const [slot, priceIds] of Object.entries(legacyPrices)) {
			if (!Array.isArray(priceIds)) {
				Logger.warn({slot}, 'Ignoring legacy Stripe price slot that is not a list of price IDs');
				continue;
			}
			const info = parseLegacySlot(slot);
			if (!info) {
				Logger.warn({slot}, 'Ignoring legacy Stripe price slot with an unrecognised name or currency');
				continue;
			}
			for (const priceId of priceIds) {
				if (!priceId || this.products.has(priceId)) {
					continue;
				}
				this.products.set(priceId, info);
			}
		}
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

	getRecurringSubscriptionPriceId(billingCycle: RecurringBillingCycle, currency: string): string | null {
		const normalizedCurrency = currency.trim().toLowerCase();
		const prices = Config.stripe.prices;
		if (!prices) {
			return null;
		}
		if (normalizedCurrency === 'eur') {
			return billingCycle === 'monthly' ? (prices.monthlyEur ?? null) : (prices.yearlyEur ?? null);
		}
		if (normalizedCurrency === 'brl') {
			return billingCycle === 'monthly' ? (prices.monthlyBrl ?? null) : (prices.yearlyBrl ?? null);
		}
		if (normalizedCurrency === 'dkk') {
			return billingCycle === 'monthly' ? (prices.monthlyDkk ?? null) : (prices.yearlyDkk ?? null);
		}
		if (normalizedCurrency === 'inr') {
			return billingCycle === 'monthly' ? (prices.monthlyInr ?? null) : (prices.yearlyInr ?? null);
		}
		if (normalizedCurrency === 'nok') {
			return billingCycle === 'monthly' ? (prices.monthlyNok ?? null) : (prices.yearlyNok ?? null);
		}
		if (normalizedCurrency === 'pln') {
			return billingCycle === 'monthly' ? (prices.monthlyPln ?? null) : (prices.yearlyPln ?? null);
		}
		if (normalizedCurrency === 'sek') {
			return billingCycle === 'monthly' ? (prices.monthlySek ?? null) : (prices.yearlySek ?? null);
		}
		if (normalizedCurrency === 'try') {
			return billingCycle === 'monthly' ? (prices.monthlyTry ?? null) : (prices.yearlyTry ?? null);
		}
		if (normalizedCurrency === 'usd') {
			return billingCycle === 'monthly' ? (prices.monthlyUsd ?? null) : (prices.yearlyUsd ?? null);
		}
		return null;
	}

	getGiftPriceId(duration: 'gift_1_month' | 'gift_1_year', currency: string): string | null {
		const normalizedCurrency = currency.trim().toLowerCase();
		const prices = Config.stripe.prices;
		if (!prices) {
			return null;
		}
		if (normalizedCurrency === 'eur') {
			return duration === 'gift_1_month' ? (prices.gift1MonthEur ?? null) : (prices.gift1YearEur ?? null);
		}
		if (normalizedCurrency === 'brl') {
			return duration === 'gift_1_month' ? (prices.gift1MonthBrl ?? null) : (prices.gift1YearBrl ?? null);
		}
		if (normalizedCurrency === 'inr') {
			return duration === 'gift_1_month' ? (prices.gift1MonthInr ?? null) : (prices.gift1YearInr ?? null);
		}
		if (normalizedCurrency === 'dkk') {
			return duration === 'gift_1_month' ? (prices.gift1MonthDkk ?? null) : (prices.gift1YearDkk ?? null);
		}
		if (normalizedCurrency === 'nok') {
			return duration === 'gift_1_month' ? (prices.gift1MonthNok ?? null) : (prices.gift1YearNok ?? null);
		}
		if (normalizedCurrency === 'sek') {
			return duration === 'gift_1_month' ? (prices.gift1MonthSek ?? null) : (prices.gift1YearSek ?? null);
		}
		if (normalizedCurrency === 'pln') {
			return duration === 'gift_1_month' ? (prices.gift1MonthPln ?? null) : (prices.gift1YearPln ?? null);
		}
		if (normalizedCurrency === 'try') {
			return duration === 'gift_1_month' ? (prices.gift1MonthTry ?? null) : (prices.gift1YearTry ?? null);
		}
		if (normalizedCurrency === 'usd') {
			return duration === 'gift_1_month' ? (prices.gift1MonthUsd ?? null) : (prices.gift1YearUsd ?? null);
		}
		return null;
	}
}
