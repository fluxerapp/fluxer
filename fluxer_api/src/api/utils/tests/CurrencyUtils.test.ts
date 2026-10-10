// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {
	getCurrencyPreferences,
	getGiftCurrencyPreferences,
	shouldDisableAdaptivePricing,
} from '@app/api/utils/CurrencyUtils';
import {describe, expect, it} from 'vitest';

describe('getCurrencyPreferences first choice', () => {
	describe('returns USD for non-EEA countries', () => {
		it('returns USD for United States', () => {
			expect(getCurrencyPreferences('US')[0]).toBe('USD');
		});
		it('returns USD for Canada', () => {
			expect(getCurrencyPreferences('CA')[0]).toBe('USD');
		});
		it('returns USD for United Kingdom', () => {
			expect(getCurrencyPreferences('GB')[0]).toBe('USD');
		});
		it('returns USD for Japan', () => {
			expect(getCurrencyPreferences('JP')[0]).toBe('USD');
		});
		it('returns USD for Australia', () => {
			expect(getCurrencyPreferences('AU')[0]).toBe('USD');
		});
		it('returns USD for Switzerland', () => {
			expect(getCurrencyPreferences('CH')[0]).toBe('USD');
		});
	});
	describe('handles case insensitivity', () => {
		it('returns EUR for lowercase country code', () => {
			expect(getCurrencyPreferences('de')[0]).toBe('EUR');
			expect(getCurrencyPreferences('fr')[0]).toBe('EUR');
		});
		it('returns USD for lowercase non-EEA', () => {
			expect(getCurrencyPreferences('us')[0]).toBe('USD');
			expect(getCurrencyPreferences('gb')[0]).toBe('USD');
		});
		it('handles mixed case', () => {
			expect(getCurrencyPreferences('De')[0]).toBe('EUR');
			expect(getCurrencyPreferences('dE')[0]).toBe('EUR');
		});
	});
	describe('handles null and undefined', () => {
		it('returns USD for null', () => {
			expect(getCurrencyPreferences(null)[0]).toBe('USD');
		});
		it('returns USD for undefined', () => {
			expect(getCurrencyPreferences(undefined)[0]).toBe('USD');
		});
	});
	describe('handles empty and invalid inputs', () => {
		it('returns USD for empty string', () => {
			expect(getCurrencyPreferences('')[0]).toBe('USD');
		});
		it('returns USD for invalid country code', () => {
			expect(getCurrencyPreferences('XX')[0]).toBe('USD');
			expect(getCurrencyPreferences('ZZ')[0]).toBe('USD');
		});
		it('returns USD for numeric strings', () => {
			expect(getCurrencyPreferences('12')[0]).toBe('USD');
		});
	});
	describe('covers all EEA member states', () => {
		const eeaCountries = [
			'AT',
			'BE',
			'BG',
			'HR',
			'CY',
			'CZ',
			'EE',
			'FI',
			'FR',
			'DE',
			'GR',
			'HU',
			'IE',
			'IT',
			'LV',
			'LT',
			'LU',
			'MT',
			'NL',
			'PT',
			'RO',
			'SK',
			'SI',
			'ES',
			'LI',
			'AX',
		];
		for (const country of eeaCountries) {
			it(`returns EUR for ${country}`, () => {
				expect(getCurrencyPreferences(country)[0]).toBe('EUR');
			});
		}
		it('uses local currency for Poland', () => {
			expect(getCurrencyPreferences('PL')[0]).toBe('PLN');
		});
		it('uses local currency for Sweden', () => {
			expect(getCurrencyPreferences('SE')[0]).toBe('SEK');
		});
		it('uses local currency for Denmark', () => {
			expect(getCurrencyPreferences('DK')[0]).toBe('DKK');
		});
		it('uses local currency for Norway', () => {
			expect(getCurrencyPreferences('NO')[0]).toBe('NOK');
		});
		it('uses local currency for Iceland', () => {
			expect(getCurrencyPreferences('IS')[0]).toBe('ISK');
		});
	});
	describe('maps Nordic territories to their home currency', () => {
		it('returns DKK for the Faroe Islands and Greenland', () => {
			expect(getCurrencyPreferences('FO')).toEqual(['DKK', 'EUR', 'USD']);
			expect(getCurrencyPreferences('GL')).toEqual(['DKK', 'EUR', 'USD']);
		});
		it('returns NOK for Svalbard and Jan Mayen', () => {
			expect(getCurrencyPreferences('SJ')).toEqual(['NOK', 'EUR', 'USD']);
		});
		it('returns ISK with EUR as the fallback for Iceland', () => {
			expect(getCurrencyPreferences('IS')).toEqual(['ISK', 'EUR', 'USD']);
		});
		it('returns EUR for Åland', () => {
			expect(getCurrencyPreferences('AX')).toEqual(['EUR', 'USD']);
		});
	});
});

describe('shouldDisableAdaptivePricing', () => {
	it('disables adaptive pricing for the native Nordic currencies', () => {
		for (const currency of ['SEK', 'NOK', 'DKK', 'ISK', 'sek']) {
			expect(shouldDisableAdaptivePricing(currency)).toBe(true);
		}
	});
	it('leaves adaptive pricing alone for every other currency', () => {
		for (const currency of ['USD', 'EUR', 'BRL', 'INR', 'PLN', 'TRY']) {
			expect(shouldDisableAdaptivePricing(currency)).toBe(false);
		}
	});
	it('leaves adaptive pricing alone on a self-hosted instance', () => {
		const originalSelfHosted = Config.instance.selfHosted;
		Config.instance.selfHosted = true;
		try {
			expect(shouldDisableAdaptivePricing('SEK')).toBe(false);
		} finally {
			Config.instance.selfHosted = originalSelfHosted;
		}
	});
});

describe('getGiftCurrencyPreferences', () => {
	it('never offers BRL, INR, PLN or TRY gifts', () => {
		for (const country of ['BR', 'IN', 'PL', 'TR']) {
			expect(getGiftCurrencyPreferences(country)).not.toContain(getCurrencyPreferences(country)[0]);
		}
	});
	it('offers the Nordic localized currencies for gifts', () => {
		expect(getGiftCurrencyPreferences('SE')).toEqual(['SEK', 'EUR', 'USD']);
		expect(getGiftCurrencyPreferences('DK')).toEqual(['DKK', 'EUR', 'USD']);
		expect(getGiftCurrencyPreferences('NO')).toEqual(['NOK', 'EUR', 'USD']);
		expect(getGiftCurrencyPreferences('IS')).toEqual(['ISK', 'EUR', 'USD']);
		expect(getGiftCurrencyPreferences('FO')).toEqual(['DKK', 'EUR', 'USD']);
		expect(getGiftCurrencyPreferences('SJ')).toEqual(['NOK', 'EUR', 'USD']);
	});
	it('uses EUR for other EEA countries', () => {
		expect(getGiftCurrencyPreferences('DE')).toEqual(['EUR', 'USD']);
		expect(getGiftCurrencyPreferences('PL')).toEqual(['EUR', 'USD']);
	});
	it('uses USD everywhere else', () => {
		expect(getGiftCurrencyPreferences('BR')).toEqual(['USD', 'EUR']);
		expect(getGiftCurrencyPreferences('IN')).toEqual(['USD', 'EUR']);
		expect(getGiftCurrencyPreferences('TR')).toEqual(['USD', 'EUR']);
		expect(getGiftCurrencyPreferences('US')).toEqual(['USD', 'EUR']);
	});
	it('uses USD when the country is unknown', () => {
		expect(getGiftCurrencyPreferences(null)).toEqual(['USD', 'EUR']);
		expect(getGiftCurrencyPreferences(undefined)).toEqual(['USD', 'EUR']);
	});
	it('is case insensitive', () => {
		expect(getGiftCurrencyPreferences('se')).toEqual(['SEK', 'EUR', 'USD']);
		expect(getGiftCurrencyPreferences('br')).toEqual(['USD', 'EUR']);
	});
});
