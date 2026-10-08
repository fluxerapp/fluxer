// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {
	type BillingPriceSet,
	type EffectiveBillingConfig,
	getActiveStoredBillingConfig,
	getEffectiveBillingConfig,
} from '@app/api/stripe/BillingConfigCache';
import type {Currency} from '@app/api/utils/CurrencyUtils';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';

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

	private resolveLegacyPrices(): unknown {
		if (this.config.catalogMode === 'env' && getActiveStoredBillingConfig()?.legacy_prices == null) {
			return Config.stripe.legacyPrices;
		}
		return this.config.legacyPrices;
	}

	private registerLegacyProducts(): void {
		const legacyPrices = this.resolveLegacyPrices();
		if (!legacyPrices) return;
		if (typeof legacyPrices !== 'object' || Array.isArray(legacyPrices)) {
			Logger.warn({}, 'Ignoring legacy Stripe price configuration that is not an object of slot names to price ids');
			return;
		}
		for (const [slot, priceIds] of Object.entries(legacyPrices as Record<string, unknown>)) {
			if (!Array.isArray(priceIds)) {
				Logger.warn({slot}, 'Ignoring legacy Stripe price slot that is not a list of price IDs');
				continue;
			}
			const info = parseLegacySlot(slot, this.config.catalogMode);
			if (!info) {
				Logger.warn({slot}, 'Ignoring legacy Stripe price slot with an unrecognised name or currency');
				continue;
			}
			for (const priceId of priceIds) {
				if (typeof priceId !== 'string' || !priceId || this.products.has(priceId)) {
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

	private getPriceSet(currency: string): BillingPriceSet | null {
		return this.config.prices[currency.trim().toUpperCase()] ?? null;
	}

	getProduct(priceId: string): ProductInfo | null {
		return this.products.get(priceId) || null;
	}

	isRecurringSubscription(info: ProductInfo): boolean {
		return !info.isGift && info.premiumType === UserPremiumTypes.SUBSCRIPTION;
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
}

let cachedRegistry: ProductRegistry | null = null;

export function getProductRegistry(config: EffectiveBillingConfig = getEffectiveBillingConfig()): ProductRegistry {
	if (cachedRegistry === null || cachedRegistry.version !== config.version) {
		cachedRegistry = new ProductRegistry(config);
	}
	return cachedRegistry;
}
