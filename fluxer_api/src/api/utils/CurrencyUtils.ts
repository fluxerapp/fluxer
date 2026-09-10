// SPDX-License-Identifier: AGPL-3.0-or-later

import {isEuEeaCountryCode} from '@fluxer/constants/src/EuropeanEconomicArea';

export type Currency = 'USD' | 'EUR' | 'BRL' | 'INR' | 'PLN' | 'TRY';

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
	if (upperCode === 'IN') {
		return ['INR', 'USD', 'EUR'];
	}
	if (upperCode === 'PL') {
		return ['PLN', 'EUR', 'USD'];
	}
	if (upperCode === 'TR') {
		return ['TRY', 'USD', 'EUR'];
	}
	if (isEuEeaCountryCode(upperCode)) {
		return ['EUR', 'USD'];
	}
	return ['USD', 'EUR'];
}

export function getGiftCurrencyPreferences(countryCode: string | null | undefined): Array<Currency> {
	return getCurrencyPreferences(countryCode);
}
