// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {Config} from '@app/api/Config';
import {getCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import type {BillingCatalogMode, StoredBillingConfig} from '@fluxer/schema/src/domains/admin/InstanceBillingSchemas';

export type {StoredBillingConfig} from '@fluxer/schema/src/domains/admin/InstanceBillingSchemas';

export interface BillingPriceSet {
	monthly: string | null;
	yearly: string | null;
	gift_1_month: string | null;
	gift_1_year: string | null;
}

export interface EffectiveBillingConfig {
	enabled: boolean;
	secretKey: string | null;
	webhookSecret: string | null;
	automaticTax: boolean;
	taxIdCollection: boolean;
	termsConsentRequired: boolean;
	catalogMode: BillingCatalogMode;
	defaultCurrency: string | null;
	prices: Record<string, BillingPriceSet>;
	countryCurrencies: Record<string, string>;
	legacyPrices: Record<string, Array<string>>;
	version: string;
}

type EnvPrices = NonNullable<typeof Config.stripe.prices>;
type EnvPriceKey = keyof EnvPrices;

const ENV_CATALOG: ReadonlyArray<{
	currency: string;
	monthly: EnvPriceKey;
	yearly: EnvPriceKey;
	gift_1_month: EnvPriceKey;
	gift_1_year: EnvPriceKey;
}> = [
	{
		currency: 'USD',
		monthly: 'monthlyUsd',
		yearly: 'yearlyUsd',
		gift_1_month: 'gift1MonthUsd',
		gift_1_year: 'gift1YearUsd',
	},
	{
		currency: 'EUR',
		monthly: 'monthlyEur',
		yearly: 'yearlyEur',
		gift_1_month: 'gift1MonthEur',
		gift_1_year: 'gift1YearEur',
	},
	{
		currency: 'BRL',
		monthly: 'monthlyBrl',
		yearly: 'yearlyBrl',
		gift_1_month: 'gift1MonthBrl',
		gift_1_year: 'gift1YearBrl',
	},
	{
		currency: 'DKK',
		monthly: 'monthlyDkk',
		yearly: 'yearlyDkk',
		gift_1_month: 'gift1MonthDkk',
		gift_1_year: 'gift1YearDkk',
	},
	{
		currency: 'INR',
		monthly: 'monthlyInr',
		yearly: 'yearlyInr',
		gift_1_month: 'gift1MonthInr',
		gift_1_year: 'gift1YearInr',
	},
	{
		currency: 'NOK',
		monthly: 'monthlyNok',
		yearly: 'yearlyNok',
		gift_1_month: 'gift1MonthNok',
		gift_1_year: 'gift1YearNok',
	},
	{
		currency: 'PLN',
		monthly: 'monthlyPln',
		yearly: 'yearlyPln',
		gift_1_month: 'gift1MonthPln',
		gift_1_year: 'gift1YearPln',
	},
	{
		currency: 'SEK',
		monthly: 'monthlySek',
		yearly: 'yearlySek',
		gift_1_month: 'gift1MonthSek',
		gift_1_year: 'gift1YearSek',
	},
	{
		currency: 'TRY',
		monthly: 'monthlyTry',
		yearly: 'yearlyTry',
		gift_1_month: 'gift1MonthTry',
		gift_1_year: 'gift1YearTry',
	},
];

const PREVIOUS_WEBHOOK_SECRET_TTL_MS = 24 * 60 * 60 * 1000;

let storedBillingConfig: StoredBillingConfig | null = null;
let memoizedSource: string | null = null;
let memoizedConfig: EffectiveBillingConfig | null = null;
let lastEffectiveWebhookSecret: string | null | undefined;
let previousWebhookSecrets: Array<{secret: string; expiresAt: number}> = [];

export function setStoredBillingConfig(stored: StoredBillingConfig | null): void {
	storedBillingConfig = stored;
	trackWebhookSecret();
}

export function getStoredBillingConfig(): StoredBillingConfig | null {
	return storedBillingConfig;
}

export function getActiveStoredBillingConfig(): StoredBillingConfig | null {
	return Config.instance.selfHosted ? storedBillingConfig : null;
}

function trackWebhookSecret(): void {
	if (!Config.instance.selfHosted) {
		previousWebhookSecrets = [];
		lastEffectiveWebhookSecret = undefined;
		return;
	}
	const current = computeEffectiveBillingConfig().webhookSecret;
	const now = Date.now();
	previousWebhookSecrets = previousWebhookSecrets.filter((entry) => entry.expiresAt > now && entry.secret !== current);
	if (lastEffectiveWebhookSecret && lastEffectiveWebhookSecret !== current) {
		const replaced = lastEffectiveWebhookSecret;
		previousWebhookSecrets = [
			{secret: replaced, expiresAt: now + PREVIOUS_WEBHOOK_SECRET_TTL_MS},
			...previousWebhookSecrets.filter((entry) => entry.secret !== replaced),
		];
	}
	lastEffectiveWebhookSecret = current;
}

export function getAcceptedWebhookSecrets(config: EffectiveBillingConfig = getEffectiveBillingConfig()): Array<string> {
	trackWebhookSecret();
	const now = Date.now();
	const secrets = config.webhookSecret ? [config.webhookSecret] : [];
	for (const entry of previousWebhookSecrets) {
		if (entry.expiresAt > now && !secrets.includes(entry.secret)) {
			secrets.push(entry.secret);
		}
	}
	return secrets;
}

function normalizeId(value: string | null | undefined): string | null {
	return typeof value === 'string' && value.trim().length > 0 ? value.trim() : null;
}

function buildEnvPrices(): Record<string, BillingPriceSet> {
	const env = Config.stripe.prices;
	const prices: Record<string, BillingPriceSet> = {};
	if (!env) return prices;
	for (const entry of ENV_CATALOG) {
		const set: BillingPriceSet = {
			monthly: normalizeId(env[entry.monthly]),
			yearly: normalizeId(env[entry.yearly]),
			gift_1_month: normalizeId(env[entry.gift_1_month]),
			gift_1_year: normalizeId(env[entry.gift_1_year]),
		};
		if (set.monthly || set.yearly || set.gift_1_month || set.gift_1_year) {
			prices[entry.currency] = set;
		}
	}
	return prices;
}

function buildEnvLegacyPrices(): Record<string, Array<string>> {
	const legacy: Record<string, Array<string>> = {};
	const env: unknown = Config.stripe.legacyPrices;
	if (!env || typeof env !== 'object' || Array.isArray(env)) return legacy;
	for (const [slot, ids] of Object.entries(env as Record<string, unknown>)) {
		if (Array.isArray(ids)) legacy[slot] = ids.filter((id): id is string => typeof id === 'string');
	}
	return legacy;
}

function buildOperatorPrices(stored: NonNullable<StoredBillingConfig['prices']>): Record<string, BillingPriceSet> {
	const prices: Record<string, BillingPriceSet> = {};
	for (const [currency, set] of Object.entries(stored)) {
		prices[currency] = {
			monthly: normalizeId(set.monthly),
			yearly: normalizeId(set.yearly),
			gift_1_month: normalizeId(set.gift_1_month),
			gift_1_year: normalizeId(set.gift_1_year),
		};
	}
	return prices;
}

function computeEffectiveBillingConfig(): Omit<EffectiveBillingConfig, 'version'> {
	const selfHosted = Config.instance.selfHosted;
	const stored = selfHosted ? storedBillingConfig : null;
	const operatorPrices = stored?.prices ?? null;
	const catalogMode: BillingCatalogMode = operatorPrices === null ? 'env' : 'operator';
	return {
		enabled: stored?.enabled ?? Config.stripe.enabled,
		secretKey: normalizeId(stored?.stripe_secret_key) ?? normalizeId(Config.stripe.secretKey),
		webhookSecret: normalizeId(stored?.stripe_webhook_secret) ?? normalizeId(Config.stripe.webhookSecret),
		automaticTax: selfHosted ? (stored?.automatic_tax ?? false) : true,
		taxIdCollection: selfHosted ? (stored?.tax_id_collection ?? false) : true,
		termsConsentRequired: selfHosted ? (stored?.terms_consent_required ?? false) : true,
		catalogMode,
		defaultCurrency: catalogMode === 'operator' ? (stored?.default_currency ?? null) : null,
		prices: operatorPrices === null ? buildEnvPrices() : buildOperatorPrices(operatorPrices),
		countryCurrencies: catalogMode === 'operator' ? {...(stored?.country_currencies ?? {})} : {},
		legacyPrices:
			stored?.legacy_prices != null
				? Object.fromEntries(Object.entries(stored.legacy_prices).map(([slot, ids]) => [slot, [...ids]]))
				: buildEnvLegacyPrices(),
	};
}

export function getEffectiveBillingConfig(): EffectiveBillingConfig {
	const effective = computeEffectiveBillingConfig();
	const source = JSON.stringify(effective);
	if (memoizedConfig !== null && memoizedSource === source) {
		return memoizedConfig;
	}
	const config: EffectiveBillingConfig = {
		...effective,
		version: createHash('sha256').update(source).digest('hex').slice(0, 32),
	};
	memoizedSource = source;
	memoizedConfig = config;
	return config;
}

export function hasRecurringPricePair(config: EffectiveBillingConfig = getEffectiveBillingConfig()): boolean {
	return Object.values(config.prices).some((set) => set.monthly !== null && set.yearly !== null);
}

export function getOperatorCurrencyPreferences(
	countryCode: string | null | undefined,
	config: EffectiveBillingConfig = getEffectiveBillingConfig(),
): Array<string> {
	const configured = Object.keys(config.prices);
	const country = countryCode ? countryCode.toUpperCase() : null;
	const candidates = [
		country ? config.countryCurrencies[country] : undefined,
		config.defaultCurrency ?? undefined,
		...configured,
	];
	const preferences: Array<string> = [];
	for (const currency of candidates) {
		if (currency && configured.includes(currency) && !preferences.includes(currency)) {
			preferences.push(currency);
		}
	}
	return preferences;
}

export function isPremiumTieringActive(): boolean {
	return !Config.instance.selfHosted || getCachedInstancePremiumMode() === 'mirror';
}

export function isBillingActive(config: EffectiveBillingConfig = getEffectiveBillingConfig()): boolean {
	return config.enabled && config.secretKey !== null && hasRecurringPricePair(config) && isPremiumTieringActive();
}

export function isStripeServiceable(config: EffectiveBillingConfig = getEffectiveBillingConfig()): boolean {
	if (config.secretKey === null) {
		return false;
	}
	return Config.instance.selfHosted ? isPremiumTieringActive() : config.enabled;
}

export function isCurrentCatalogPriceId(
	priceId: string,
	config: EffectiveBillingConfig = getEffectiveBillingConfig(),
): boolean {
	return Object.values(config.prices).some(
		(set) =>
			set.monthly === priceId || set.yearly === priceId || set.gift_1_month === priceId || set.gift_1_year === priceId,
	);
}
