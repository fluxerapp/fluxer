// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {
	getAcceptedWebhookSecrets,
	getEffectiveBillingConfig,
	getOperatorCurrencyPreferences,
	getStoredBillingConfig,
	isBillingActive,
	isCurrentCatalogPriceId,
	isPremiumTieringActive,
	isStripeServiceable,
	type StoredBillingConfig,
	setStoredBillingConfig,
} from '@app/api/stripe/BillingConfigCache';
import {getStripeClient} from '@app/api/stripe/StripeClient';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const ENV_PRICES = {
	monthlyUsd: 'price_monthly_usd',
	yearlyUsd: 'price_yearly_usd',
	gift1MonthUsd: 'price_gift_month_usd',
	gift1YearUsd: 'price_gift_year_usd',
	monthlyEur: 'price_monthly_eur',
	yearlyEur: 'price_yearly_eur',
	gift1MonthEur: 'price_gift_month_eur',
	gift1YearEur: 'price_gift_year_eur',
	monthlyBrl: 'price_monthly_brl',
	yearlyBrl: 'price_yearly_brl',
	gift1MonthBrl: 'price_gift_month_brl',
	gift1YearBrl: 'price_gift_year_brl',
	monthlyDkk: 'price_monthly_dkk',
	yearlyDkk: 'price_yearly_dkk',
	gift1MonthDkk: 'price_gift_month_dkk',
	gift1YearDkk: 'price_gift_year_dkk',
	monthlyInr: 'price_monthly_inr',
	yearlyInr: 'price_yearly_inr',
	gift1MonthInr: 'price_gift_month_inr',
	gift1YearInr: 'price_gift_year_inr',
	monthlyIsk: 'price_monthly_isk',
	yearlyIsk: 'price_yearly_isk',
	gift1MonthIsk: 'price_gift_month_isk',
	gift1YearIsk: 'price_gift_year_isk',
	monthlyNok: 'price_monthly_nok',
	yearlyNok: 'price_yearly_nok',
	gift1MonthNok: 'price_gift_month_nok',
	gift1YearNok: 'price_gift_year_nok',
	monthlyPln: 'price_monthly_pln',
	yearlyPln: 'price_yearly_pln',
	gift1MonthPln: 'price_gift_month_pln',
	gift1YearPln: 'price_gift_year_pln',
	monthlySek: 'price_monthly_sek',
	yearlySek: 'price_yearly_sek',
	gift1MonthSek: 'price_gift_month_sek',
	gift1YearSek: 'price_gift_year_sek',
	monthlyTry: 'price_monthly_try',
	yearlyTry: 'price_yearly_try',
	gift1MonthTry: 'price_gift_month_try',
	gift1YearTry: 'price_gift_year_try',
};

function expectedEnvCatalog(): Record<string, Record<string, string>> {
	const catalog: Record<string, Record<string, string>> = {};
	for (const currency of ['USD', 'EUR', 'BRL', 'DKK', 'INR', 'ISK', 'NOK', 'PLN', 'SEK', 'TRY']) {
		const suffix = currency.toLowerCase();
		catalog[currency] = {
			monthly: `price_monthly_${suffix}`,
			yearly: `price_yearly_${suffix}`,
			gift_1_month: `price_gift_month_${suffix}`,
			gift_1_year: `price_gift_year_${suffix}`,
		};
	}
	return catalog;
}

function operatorConfig(overrides: Partial<StoredBillingConfig> = {}): StoredBillingConfig {
	return {
		enabled: true,
		stripe_secret_key: 'sk_test_operator',
		stripe_webhook_secret: 'whsec_operator',
		automatic_tax: null,
		tax_id_collection: null,
		terms_consent_required: null,
		default_currency: 'GBP',
		prices: {
			GBP: {
				monthly: 'price_gbp_monthly',
				yearly: 'price_gbp_yearly',
				gift_1_month: 'price_gbp_gift_month',
				gift_1_year: 'price_gbp_gift_year',
			},
			CHF: {monthly: 'price_chf_monthly', yearly: 'price_chf_yearly', gift_1_month: null, gift_1_year: null},
		},
		country_currencies: {CH: 'CHF', LI: 'CHF'},
		legacy_prices: {monthly_GBP: ['price_gbp_old']},
		...overrides,
	};
}

describe('BillingConfigCache', () => {
	const stripe = Config.stripe;
	const instance = Config.instance;
	const original = {
		enabled: stripe.enabled,
		secretKey: stripe.secretKey,
		webhookSecret: stripe.webhookSecret,
		prices: stripe.prices,
		legacyPrices: stripe.legacyPrices,
		selfHosted: instance.selfHosted,
	};
	let originalPremiumMode = getCachedInstancePremiumMode();
	let originalStored = getStoredBillingConfig();

	beforeEach(() => {
		originalPremiumMode = getCachedInstancePremiumMode();
		originalStored = getStoredBillingConfig();
		instance.selfHosted = true;
		setCachedInstancePremiumMode('mirror');
		setStoredBillingConfig(null);
		stripe.prices = {...ENV_PRICES};
		stripe.legacyPrices = {monthly_try: ['price_old_try']};
	});

	afterEach(() => {
		stripe.enabled = original.enabled;
		stripe.secretKey = original.secretKey;
		stripe.webhookSecret = original.webhookSecret;
		stripe.prices = original.prices;
		stripe.legacyPrices = original.legacyPrices;
		instance.selfHosted = original.selfHosted;
		setCachedInstancePremiumMode(originalPremiumMode);
		setStoredBillingConfig(originalStored);
	});

	it('builds the env catalog for the nine hosted currencies from Config.stripe', () => {
		const config = getEffectiveBillingConfig();
		expect(config.catalogMode).toBe('env');
		expect(config.prices).toEqual(expectedEnvCatalog());
		expect(config.countryCurrencies).toEqual({});
		expect(config.defaultCurrency).toBeNull();
		expect(config.legacyPrices).toEqual({monthly_try: ['price_old_try']});
		expect(config.enabled).toBe(true);
		expect(config.secretKey).toBe('sk_test_fluxer');
		expect(config.webhookSecret).toBe('whsec_test_fluxer');
	});

	it('leaves unconfigured env currencies out of the catalog', () => {
		stripe.prices = {monthlyUsd: 'price_a', yearlyUsd: 'price_b', gift1MonthSek: 'price_c'};
		expect(getEffectiveBillingConfig().prices).toEqual({
			USD: {monthly: 'price_a', yearly: 'price_b', gift_1_month: null, gift_1_year: null},
			SEK: {monthly: null, yearly: null, gift_1_month: 'price_c', gift_1_year: null},
		});
	});

	it('reads env values at call time and changes the version when they change', () => {
		const before = getEffectiveBillingConfig();
		expect(getEffectiveBillingConfig()).toBe(before);
		stripe.prices = {...ENV_PRICES, monthlyUsd: 'price_changed'};
		const afterPrice = getEffectiveBillingConfig();
		expect(afterPrice.prices.USD?.monthly).toBe('price_changed');
		expect(afterPrice.version).not.toBe(before.version);
		stripe.webhookSecret = 'whsec_rotated';
		const afterSecret = getEffectiveBillingConfig();
		expect(afterSecret.webhookSecret).toBe('whsec_rotated');
		expect(afterSecret.version).not.toBe(afterPrice.version);
	});

	it('uses the operator catalog when stored prices are set', () => {
		setStoredBillingConfig(operatorConfig());
		const config = getEffectiveBillingConfig();
		expect(config.catalogMode).toBe('operator');
		expect(config.prices).toEqual(operatorConfig().prices);
		expect(config.defaultCurrency).toBe('GBP');
		expect(config.countryCurrencies).toEqual({CH: 'CHF', LI: 'CHF'});
		expect(config.legacyPrices).toEqual({monthly_GBP: ['price_gbp_old']});
		expect(config.secretKey).toBe('sk_test_operator');
		expect(config.webhookSecret).toBe('whsec_operator');
	});

	it('falls back to env for each stored null field', () => {
		setStoredBillingConfig({
			enabled: null,
			stripe_secret_key: null,
			stripe_webhook_secret: 'whsec_stored',
			automatic_tax: null,
			tax_id_collection: null,
			terms_consent_required: null,
			default_currency: null,
			prices: null,
			country_currencies: {SE: 'SEK'},
			legacy_prices: null,
		});
		const config = getEffectiveBillingConfig();
		expect(config.catalogMode).toBe('env');
		expect(config.enabled).toBe(true);
		expect(config.secretKey).toBe('sk_test_fluxer');
		expect(config.webhookSecret).toBe('whsec_stored');
		expect(config.prices).toEqual(expectedEnvCatalog());
		expect(config.countryCurrencies).toEqual({});
		expect(config.legacyPrices).toEqual({monthly_try: ['price_old_try']});
	});

	it('changes the version when the stored config changes', () => {
		const envVersion = getEffectiveBillingConfig().version;
		setStoredBillingConfig(operatorConfig());
		const operatorVersion = getEffectiveBillingConfig().version;
		expect(operatorVersion).not.toBe(envVersion);
		setStoredBillingConfig(operatorConfig({stripe_secret_key: 'sk_test_rotated'}));
		expect(getEffectiveBillingConfig().version).not.toBe(operatorVersion);
		setStoredBillingConfig(null);
		expect(getEffectiveBillingConfig().version).toBe(envVersion);
	});

	it('orders operator currency preferences by country, then default, then catalog order', () => {
		setStoredBillingConfig(operatorConfig());
		expect(getOperatorCurrencyPreferences('ch')).toEqual(['CHF', 'GBP']);
		expect(getOperatorCurrencyPreferences('GB')).toEqual(['GBP', 'CHF']);
		expect(getOperatorCurrencyPreferences(null)).toEqual(['GBP', 'CHF']);
		setStoredBillingConfig(operatorConfig({default_currency: 'JPY', country_currencies: {CH: 'XXX'}}));
		expect(getOperatorCurrencyPreferences('CH')).toEqual(['GBP', 'CHF']);
	});

	describe('isPremiumTieringActive and isBillingActive', () => {
		const cases: Array<{
			selfHosted: boolean;
			premiumMode: 'mirror' | 'everyone';
			stored: StoredBillingConfig | null;
			tiering: boolean;
			billing: boolean;
		}> = [
			{selfHosted: false, premiumMode: 'everyone', stored: null, tiering: true, billing: true},
			{selfHosted: false, premiumMode: 'mirror', stored: null, tiering: true, billing: true},
			{selfHosted: true, premiumMode: 'everyone', stored: null, tiering: false, billing: false},
			{selfHosted: true, premiumMode: 'mirror', stored: null, tiering: true, billing: true},
			{selfHosted: true, premiumMode: 'everyone', stored: operatorConfig(), tiering: false, billing: false},
			{selfHosted: true, premiumMode: 'mirror', stored: operatorConfig(), tiering: true, billing: true},
			{
				selfHosted: true,
				premiumMode: 'mirror',
				stored: operatorConfig({enabled: false}),
				tiering: true,
				billing: false,
			},
			{
				selfHosted: true,
				premiumMode: 'mirror',
				stored: operatorConfig({
					prices: {GBP: {monthly: 'price_gbp_monthly', yearly: null, gift_1_month: null, gift_1_year: null}},
				}),
				tiering: true,
				billing: false,
			},
			{
				selfHosted: true,
				premiumMode: 'mirror',
				stored: operatorConfig({prices: {}}),
				tiering: true,
				billing: false,
			},
			{
				selfHosted: false,
				premiumMode: 'everyone',
				stored: operatorConfig({prices: {}, enabled: false}),
				tiering: true,
				billing: true,
			},
		];

		it.each(cases)(
			'selfHosted=$selfHosted premiumMode=$premiumMode gives tiering=$tiering billing=$billing',
			({selfHosted, premiumMode, stored, tiering, billing}) => {
				instance.selfHosted = selfHosted;
				setCachedInstancePremiumMode(premiumMode);
				setStoredBillingConfig(stored);
				expect(isPremiumTieringActive()).toBe(tiering);
				expect(isBillingActive()).toBe(billing);
			},
		);

		it('is inactive without a secret key, or when env billing is disabled', () => {
			instance.selfHosted = false;
			stripe.secretKey = '';
			expect(isBillingActive()).toBe(false);
			stripe.secretKey = original.secretKey;
			stripe.enabled = false;
			expect(isBillingActive()).toBe(false);
			setStoredBillingConfig(operatorConfig({enabled: true}));
			expect(isBillingActive()).toBe(false);
			instance.selfHosted = true;
			expect(isBillingActive()).toBe(true);
		});

		it('is inactive in env mode when no recurring pair is configured', () => {
			instance.selfHosted = false;
			stripe.prices = {monthlyUsd: 'price_a', gift1MonthUsd: 'price_b', gift1YearUsd: 'price_c'};
			expect(isBillingActive()).toBe(false);
		});
	});

	describe('hosted instances', () => {
		it('ignore every stored billing field and keep the env config', () => {
			const hostedEnv = (() => {
				instance.selfHosted = false;
				return getEffectiveBillingConfig();
			})();
			setStoredBillingConfig(
				operatorConfig({
					enabled: false,
					automatic_tax: false,
					tax_id_collection: false,
					terms_consent_required: false,
				}),
			);
			const config = getEffectiveBillingConfig();
			expect(config).toBe(hostedEnv);
			expect(config.catalogMode).toBe('env');
			expect(config.enabled).toBe(true);
			expect(config.secretKey).toBe('sk_test_fluxer');
			expect(config.webhookSecret).toBe('whsec_test_fluxer');
			expect(config.prices).toEqual(expectedEnvCatalog());
			expect(config.legacyPrices).toEqual({monthly_try: ['price_old_try']});
			expect(config.automaticTax).toBe(true);
			expect(config.taxIdCollection).toBe(true);
			expect(config.termsConsentRequired).toBe(true);
			expect(isBillingActive()).toBe(true);
		});
	});

	describe('checkout flags', () => {
		it('default to off on self-hosted and follow the stored values', () => {
			setStoredBillingConfig(operatorConfig());
			let config = getEffectiveBillingConfig();
			expect(config.automaticTax).toBe(false);
			expect(config.taxIdCollection).toBe(false);
			expect(config.termsConsentRequired).toBe(false);
			setStoredBillingConfig(
				operatorConfig({automatic_tax: true, tax_id_collection: false, terms_consent_required: true}),
			);
			config = getEffectiveBillingConfig();
			expect(config.automaticTax).toBe(true);
			expect(config.taxIdCollection).toBe(false);
			expect(config.termsConsentRequired).toBe(true);
		});

		it('default to off on self-hosted without any stored config', () => {
			const config = getEffectiveBillingConfig();
			expect(config.automaticTax).toBe(false);
			expect(config.taxIdCollection).toBe(false);
			expect(config.termsConsentRequired).toBe(false);
		});
	});

	describe('isStripeServiceable and getStripeClient', () => {
		it('follows env enabled plus key on hosted, exactly like before', () => {
			instance.selfHosted = false;
			expect(isStripeServiceable()).toBe(true);
			expect(getStripeClient()).not.toBeNull();
			stripe.enabled = false;
			expect(isStripeServiceable()).toBe(false);
			expect(getStripeClient()).toBeNull();
			stripe.enabled = true;
			stripe.secretKey = '';
			expect(isStripeServiceable()).toBe(false);
			expect(getStripeClient()).toBeNull();
		});

		it('keeps servicing on self-hosted after sales are switched off or prices removed', () => {
			setStoredBillingConfig(operatorConfig({enabled: false, prices: {}}));
			expect(isBillingActive()).toBe(false);
			expect(isStripeServiceable()).toBe(true);
			expect(getStripeClient()).not.toBeNull();
		});

		it('stops servicing on self-hosted without a key or without premium tiering', () => {
			setStoredBillingConfig(operatorConfig({stripe_secret_key: null}));
			stripe.secretKey = '';
			expect(isStripeServiceable()).toBe(false);
			expect(getStripeClient()).toBeNull();
			stripe.secretKey = original.secretKey;
			setStoredBillingConfig(operatorConfig());
			setCachedInstancePremiumMode('everyone');
			expect(isStripeServiceable()).toBe(false);
			expect(getStripeClient()).toBeNull();
		});
	});

	describe('isCurrentCatalogPriceId', () => {
		it('matches current catalog ids only, never legacy ids', () => {
			setStoredBillingConfig(operatorConfig());
			expect(isCurrentCatalogPriceId('price_gbp_monthly')).toBe(true);
			expect(isCurrentCatalogPriceId('price_gbp_gift_year')).toBe(true);
			expect(isCurrentCatalogPriceId('price_chf_yearly')).toBe(true);
			expect(isCurrentCatalogPriceId('price_gbp_old')).toBe(false);
			expect(isCurrentCatalogPriceId('price_monthly_usd')).toBe(false);
		});
	});

	describe('getAcceptedWebhookSecrets', () => {
		beforeEach(() => {
			instance.selfHosted = false;
			getAcceptedWebhookSecrets();
			instance.selfHosted = true;
		});

		afterEach(() => {
			vi.restoreAllMocks();
		});

		it('remembers the previously effective env secret when a stored one replaces it', () => {
			getAcceptedWebhookSecrets();
			setStoredBillingConfig(operatorConfig({stripe_webhook_secret: 'whsec_first'}));
			expect(getAcceptedWebhookSecrets()).toEqual(['whsec_first', 'whsec_test_fluxer']);
		});

		it('remembers the previous self-hosted webhook secret for 24 hours after a change', () => {
			const start = Date.now();
			const now = vi.spyOn(Date, 'now').mockReturnValue(start);
			setStoredBillingConfig(operatorConfig({stripe_webhook_secret: 'whsec_first'}));
			expect(getAcceptedWebhookSecrets()).toEqual(['whsec_first']);
			setStoredBillingConfig(operatorConfig({stripe_webhook_secret: 'whsec_second'}));
			expect(getAcceptedWebhookSecrets()).toEqual(['whsec_second', 'whsec_first']);
			setStoredBillingConfig(operatorConfig({stripe_webhook_secret: 'whsec_second'}));
			expect(getAcceptedWebhookSecrets()).toEqual(['whsec_second', 'whsec_first']);
			now.mockReturnValue(start + 23 * 60 * 60 * 1000);
			expect(getAcceptedWebhookSecrets()).toEqual(['whsec_second', 'whsec_first']);
			now.mockReturnValue(start + 24 * 60 * 60 * 1000 + 1);
			expect(getAcceptedWebhookSecrets()).toEqual(['whsec_second']);
		});

		it('drops a remembered secret once it becomes current again', () => {
			setStoredBillingConfig(operatorConfig({stripe_webhook_secret: 'whsec_first'}));
			setStoredBillingConfig(operatorConfig({stripe_webhook_secret: 'whsec_second'}));
			setStoredBillingConfig(operatorConfig({stripe_webhook_secret: 'whsec_first'}));
			expect(getAcceptedWebhookSecrets()).toEqual(['whsec_first', 'whsec_second']);
		});

		it('accepts only the current env secret on hosted', () => {
			instance.selfHosted = false;
			expect(getAcceptedWebhookSecrets()).toEqual(['whsec_test_fluxer']);
			stripe.webhookSecret = 'whsec_rotated';
			expect(getAcceptedWebhookSecrets()).toEqual(['whsec_rotated']);
			setStoredBillingConfig(operatorConfig({stripe_webhook_secret: 'whsec_stored'}));
			expect(getAcceptedWebhookSecrets()).toEqual(['whsec_rotated']);
		});
	});
});
