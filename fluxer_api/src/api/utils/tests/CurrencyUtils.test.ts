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
	describe('returns EUR for EEA countries', () => {
		it('returns EUR for Germany', () => {
			expect(getCurrencyPreferences('DE')[0]).toBe('EUR');
		});
		it('returns EUR for France', () => {
			expect(getCurrencyPreferences('FR')[0]).toBe('EUR');
		});
		it('returns EUR for Italy', () => {
			expect(getCurrencyPreferences('IT')[0]).toBe('EUR');
		});
		it('returns EUR for Spain', () => {
			expect(getCurrencyPreferences('ES')[0]).toBe('EUR');
		});
		it('returns EUR for Netherlands', () => {
			expect(getCurrencyPreferences('NL')[0]).toBe('EUR');
		});
		it('returns EUR for Belgium', () => {
			expect(getCurrencyPreferences('BE')[0]).toBe('EUR');
		});
		it('returns EUR for Austria', () => {
			expect(getCurrencyPreferences('AT')[0]).toBe('EUR');
		});
		it('returns EUR for Portugal', () => {
			expect(getCurrencyPreferences('PT')[0]).toBe('EUR');
		});
		it('returns EUR for Ireland', () => {
			expect(getCurrencyPreferences('IE')[0]).toBe('EUR');
		});
		it('returns EUR for Finland', () => {
			expect(getCurrencyPreferences('FI')[0]).toBe('EUR');
		});
		it('returns SEK for Sweden', () => {
			expect(getCurrencyPreferences('SE')[0]).toBe('SEK');
		});
		it('returns DKK for Denmark', () => {
			expect(getCurrencyPreferences('DK')[0]).toBe('DKK');
		});
		it('returns PLN for Poland', () => {
			expect(getCurrencyPreferences('PL')[0]).toBe('PLN');
		});
		it('returns EUR for Greece', () => {
			expect(getCurrencyPreferences('GR')[0]).toBe('EUR');
		});
		it('returns EUR for Czech Republic', () => {
			expect(getCurrencyPreferences('CZ')[0]).toBe('EUR');
		});
		it('returns EUR for Hungary', () => {
			expect(getCurrencyPreferences('HU')[0]).toBe('EUR');
		});
		it('returns EUR for Romania', () => {
			expect(getCurrencyPreferences('RO')[0]).toBe('EUR');
		});
		it('returns NOK for Norway (EEA but not EU)', () => {
			expect(getCurrencyPreferences('NO')[0]).toBe('NOK');
		});
		it('returns ISK for Iceland (EEA but not EU)', () => {
			expect(getCurrencyPreferences('IS')[0]).toBe('ISK');
		});
		it('returns EUR for Liechtenstein (EEA but not EU)', () => {
			expect(getCurrencyPreferences('LI')[0]).toBe('EUR');
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
