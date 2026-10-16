// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {
	type EffectiveBillingConfig,
	getEffectiveBillingConfig,
	getOperatorCurrencyPreferences,
} from '@app/api/stripe/BillingConfigCache';
import {isEuEeaCountryCode} from '@fluxer/constants/src/EuropeanEconomicArea';
import type {PremiumCurrency} from '@fluxer/schema/src/domains/premium/PremiumSchemas';

export type Currency = PremiumCurrency;

export function getCurrencyPreferences(
	countryCode: string | null | undefined,
	config: EffectiveBillingConfig = getEffectiveBillingConfig(),
): Array<Currency> {
	if (config.catalogMode === 'operator') {
		return getOperatorCurrencyPreferences(countryCode, config);
	}
	return getEnvCurrencyPreferences(countryCode);
}

function getEnvCurrencyPreferences(countryCode: string | null | undefined): Array<Currency> {
	if (!countryCode) {
		return ['USD', 'EUR'];
	}
	const upperCode = countryCode.toUpperCase();
	if (upperCode === 'BR') {
		return ['BRL', 'USD', 'EUR'];
	}
	if (upperCode === 'DK' || upperCode === 'FO' || upperCode === 'GL') {
		return ['DKK', 'EUR', 'USD'];
	}
	if (upperCode === 'IN') {
		return ['INR', 'USD', 'EUR'];
	}
	if (upperCode === 'IS') {
		return ['ISK', 'EUR', 'USD'];
	}
	if (upperCode === 'NO' || upperCode === 'SJ') {
		return ['NOK', 'EUR', 'USD'];
	}
	if (upperCode === 'PL') {
		return ['PLN', 'EUR', 'USD'];
	}
	if (upperCode === 'SE') {
		return ['SEK', 'EUR', 'USD'];
	}
	if (upperCode === 'TR') {
		return ['TRY', 'USD', 'EUR'];
	}
	if (isEuEeaCountryCode(upperCode)) {
		return ['EUR', 'USD'];
	}
	return ['USD', 'EUR'];
}

const GIFT_ELIGIBLE_LOCALIZED_CURRENCIES = new Set<Currency>(['DKK', 'ISK', 'NOK', 'SEK']);

const ADAPTIVE_PRICING_DISABLED_CURRENCIES = new Set<Currency>(['DKK', 'ISK', 'NOK', 'SEK']);

const ENV_CATALOG_CURRENCIES = new Set<Currency>([
	'USD',
	'EUR',
	'BRL',
	'DKK',
	'INR',
	'ISK',
	'NOK',
	'PLN',
	'SEK',
	'TRY',
]);

const OPERATOR_CURRENCY_PATTERN = /^[A-Z]{3}$/;

export function getGiftCurrencyPreferences(
	countryCode: string | null | undefined,
	config: EffectiveBillingConfig = getEffectiveBillingConfig(),
): Array<Currency> {
	if (config.catalogMode === 'operator') {
		return getOperatorCurrencyPreferences(countryCode, config).filter((currency) => {
			const set = config.prices[currency];
			return set?.gift_1_month != null && set.gift_1_year != null;
		});
	}
	return getEnvCurrencyPreferences(countryCode).filter(
		(currency) => currency === 'USD' || currency === 'EUR' || GIFT_ELIGIBLE_LOCALIZED_CURRENCIES.has(currency),
	);
}

export function shouldDisableAdaptivePricing(currency: string): boolean {
	return !Config.instance.selfHosted && ADAPTIVE_PRICING_DISABLED_CURRENCIES.has(currency.toUpperCase());
}

export function isLocalizedCurrency(
	currency: Currency,
	config: EffectiveBillingConfig = getEffectiveBillingConfig(),
): boolean {
	return config.catalogMode === 'env' && currency !== 'USD' && currency !== 'EUR';
}

export function normalizeCatalogCurrency(
	value: string | null | undefined,
	config: EffectiveBillingConfig = getEffectiveBillingConfig(),
): Currency | null {
	const currency = value?.trim().toUpperCase();
	if (!currency) {
		return null;
	}
	if (config.catalogMode === 'operator') {
		return OPERATOR_CURRENCY_PATTERN.test(currency) ? currency : null;
	}
	return ENV_CATALOG_CURRENCIES.has(currency) ? currency : null;
}
