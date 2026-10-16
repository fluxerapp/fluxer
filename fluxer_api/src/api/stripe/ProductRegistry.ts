// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {
	type BillingPriceSet,
	type EffectiveBillingConfig,
	getEffectiveBillingConfig,
} from '@app/api/stripe/BillingConfigCache';
import {type Currency, getCurrencyPreferences, getGiftCurrencyPreferences} from '@app/api/utils/CurrencyUtils';

export enum ProductType {
	MONTHLY_SUBSCRIPTION = 'monthly_subscription',
	YEARLY_SUBSCRIPTION = 'yearly_subscription',
	GIFT_1_MONTH = 'gift_1_month',
	GIFT_1_YEAR = 'gift_1_year',
}

export type RecurringBillingCycle = 'monthly' | 'yearly';

export interface ProductInfo {
	type: ProductType;
	durationMonths: number;
	isGift: boolean;
	currency: Currency;
	billingCycle?: RecurringBillingCycle;
}

const LEGACY_SLOT_SHAPES: Record<string, Omit<ProductInfo, 'currency'> | undefined> = {
	monthly: {
		type: ProductType.MONTHLY_SUBSCRIPTION,
		durationMonths: 1,
		isGift: false,
		billingCycle: 'monthly',
	},
	yearly: {
		type: ProductType.YEARLY_SUBSCRIPTION,
		durationMonths: 12,
		isGift: false,
		billingCycle: 'yearly',
	},
	gift_1_month: {
		type: ProductType.GIFT_1_MONTH,
		durationMonths: 1,
		isGift: true,
	},
	gift_1_year: {
		type: ProductType.GIFT_1_YEAR,
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
	isk: 'ISK',
	nok: 'NOK',
	pln: 'PLN',
	sek: 'SEK',
	try: 'TRY',
};

const OPERATOR_CURRENCY_PATTERN = /^[A-Z]{3}$/;

const CATALOG_SLOTS: ReadonlyArray<keyof BillingPriceSet> = ['monthly', 'yearly', 'gift_1_month', 'gift_1_year'];

function parseLegacySlotCurrency(suffix: string, catalogMode: EffectiveBillingConfig['catalogMode']): Currency | null {
	if (catalogMode === 'operator') {
		const upper = suffix.toUpperCase();
		return OPERATOR_CURRENCY_PATTERN.test(upper) ? upper : null;
	}
	return LEGACY_SLOT_CURRENCIES[suffix.toLowerCase()] ?? null;
}

function parseLegacySlot(slot: string, catalogMode: EffectiveBillingConfig['catalogMode']): ProductInfo | null {
	const separatorIndex = slot.lastIndexOf('_');
	if (separatorIndex <= 0) {
		return null;
	}
	const shape = LEGACY_SLOT_SHAPES[slot.slice(0, separatorIndex)];
	const currency = parseLegacySlotCurrency(slot.slice(separatorIndex + 1), catalogMode);
	if (!shape || !currency) {
		return null;
	}
	return {...shape, currency};
}

export interface ResolvedPriceIds {
	monthly: string | null;
	yearly: string | null;
	gift_1_month: string | null;
	gift_1_year: string | null;
	currency: Currency;
	gift_currency: Currency | null;
}

export class ProductRegistry {
	private products = new Map<string, ProductInfo>();
	private readonly config: EffectiveBillingConfig;

	constructor(config: EffectiveBillingConfig = getEffectiveBillingConfig()) {
		this.config = config;
		this.registerConfiguredProducts();
		this.registerLegacyProducts();
	}

	get version(): string {
		return this.config.version;
	}

	private registerConfiguredProducts(): void {
		for (const [currency, set] of Object.entries(this.config.prices)) {
			for (const slot of CATALOG_SLOTS) {
				const shape = LEGACY_SLOT_SHAPES[slot];
				if (shape) {
					this.registerProduct(set[slot] ?? undefined, {...shape, currency});
				}
			}
		}
	}

	private registerLegacyProducts(): void {
		for (const [slot, priceIds] of Object.entries(this.config.legacyPrices)) {
			const info = parseLegacySlot(slot, this.config.catalogMode);
			if (!info) {
				Logger.warn({slot}, 'Ignoring legacy Stripe price slot with an unrecognised name or currency');
				continue;
			}
			for (const priceId of priceIds) {
				if (!priceId || this.products.has(priceId)) continue;
				this.products.set(priceId, info);
			}
		}
	}

	private registerProduct(priceId: string | undefined, info: ProductInfo): void {
		if (priceId) {
			this.products.set(priceId, info);
		}
	}

	private getPriceSet(currency: string): BillingPriceSet | null {
		return this.config.prices[currency.trim().toUpperCase()] ?? null;
	}

	getProduct(priceId: string): ProductInfo | null {
		return this.products.get(priceId) || null;
	}

	isRecurringSubscription(info: ProductInfo): boolean {
		return !info.isGift;
	}

	getRecurringSubscriptionPriceId(billingCycle: RecurringBillingCycle, currency: string): string | null {
		const set = this.getPriceSet(currency);
		if (!set) {
			return null;
		}
		return billingCycle === 'monthly' ? set.monthly : set.yearly;
	}

	getGiftPriceId(duration: 'gift_1_month' | 'gift_1_year', currency: string): string | null {
		const set = this.getPriceSet(currency);
		if (!set) {
			return null;
		}
		return duration === 'gift_1_month' ? set.gift_1_month : set.gift_1_year;
	}

	resolveLocalizedPriceIds(countryCode?: string | null): ResolvedPriceIds | null {
		const recurringCurrencyPreferences = getCurrencyPreferences(countryCode);
		const giftCurrencyPreferences = getGiftCurrencyPreferences(countryCode);
		const recurringPrices = this.resolveRecurringPriceIds(recurringCurrencyPreferences);
		const giftPrices = this.resolveGiftPriceIds(giftCurrencyPreferences);
		if (!recurringPrices) return null;
		if (!giftPrices && !Config.instance.selfHosted) return null;
		return {
			monthly: recurringPrices.monthly,
			yearly: recurringPrices.yearly,
			gift_1_month: giftPrices?.gift_1_month ?? null,
			gift_1_year: giftPrices?.gift_1_year ?? null,
			currency: recurringPrices.currency,
			gift_currency: giftPrices?.gift_currency ?? null,
		};
	}

	private resolveRecurringPriceIds(
		preferredCurrencies: Array<Currency>,
	): Pick<ResolvedPriceIds, 'monthly' | 'yearly' | 'currency'> | null {
		for (const currency of preferredCurrencies) {
			const monthly = this.getRecurringSubscriptionPriceId('monthly', currency);
			const yearly = this.getRecurringSubscriptionPriceId('yearly', currency);
			if (monthly && yearly) {
				return {monthly, yearly, currency};
			}
		}
		return null;
	}

	private resolveGiftPriceIds(
		preferredCurrencies: Array<Currency>,
	): Pick<ResolvedPriceIds, 'gift_1_month' | 'gift_1_year' | 'gift_currency'> | null {
		for (const currency of preferredCurrencies) {
			const gift1Month = this.getGiftPriceId('gift_1_month', currency);
			const gift1Year = this.getGiftPriceId('gift_1_year', currency);
			if (gift1Month && gift1Year) {
				return {gift_1_month: gift1Month, gift_1_year: gift1Year, gift_currency: currency};
			}
		}
		return null;
	}
}

let cachedRegistry: ProductRegistry | null = null;

export function getProductRegistry(config: EffectiveBillingConfig = getEffectiveBillingConfig()): ProductRegistry {
	if (cachedRegistry === null || cachedRegistry.version !== config.version) {
		cachedRegistry = new ProductRegistry(config);
	}
	return cachedRegistry;
}
