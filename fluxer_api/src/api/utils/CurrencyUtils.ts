// SPDX-License-Identifier: AGPL-3.0-or-later

import {isEuEeaCountryCode} from '@fluxer/constants/src/EuropeanEconomicArea';
import type {PremiumCurrency} from '@fluxer/schema/src/domains/premium/PremiumSchemas';

export type Currency = PremiumCurrency;

export function getCurrency(countryCode: string | null | undefined): Currency {
	return getCurrencyPreferences(countryCode)[0];
}

export function getCurrencyPreferences(countryCode: string | null | undefined): Array<Currency> {
	if (!countryCode) {
		return ['USD', 'EUR'];
	}
	const upperCode = countryCode.toUpperCase();
	if (upperCode === 'BR') {
		return ['BRL', 'USD', 'EUR'];
	}
	if (upperCode === 'DK') {
		return ['DKK', 'EUR', 'USD'];
	}
	if (upperCode === 'IN') {
		return ['INR', 'USD', 'EUR'];
	}
	if (upperCode === 'NO') {
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

const GIFT_ELIGIBLE_LOCALIZED_CURRENCIES = new Set<Currency>(['DKK', 'NOK', 'SEK']);

export function getGiftCurrencyPreferences(countryCode: string | null | undefined): Array<Currency> {
	return getCurrencyPreferences(countryCode).filter(
		(currency) => currency === 'USD' || currency === 'EUR' || GIFT_ELIGIBLE_LOCALIZED_CURRENCIES.has(currency),
	);
}
