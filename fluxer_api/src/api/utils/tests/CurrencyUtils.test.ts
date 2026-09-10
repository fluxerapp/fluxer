// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {getCurrency, getCurrencyPreferences, getGiftCurrencyPreferences} from '../CurrencyUtils';

describe('getCurrency', () => {
	describe('returns USD for non-EEA countries', () => {
		it('returns USD for United States', () => {
			expect(getCurrency('US')).toBe('USD');
		});
		it('returns USD for Canada', () => {
			expect(getCurrency('CA')).toBe('USD');
		});
		it('returns USD for United Kingdom', () => {
			expect(getCurrency('GB')).toBe('USD');
		});
		it('returns USD for Japan', () => {
			expect(getCurrency('JP')).toBe('USD');
		});
		it('returns USD for Australia', () => {
			expect(getCurrency('AU')).toBe('USD');
		});
		it('returns USD for Switzerland', () => {
			expect(getCurrency('CH')).toBe('USD');
		});
	});
	describe('returns EUR for EEA countries', () => {
		it('returns EUR for Germany', () => {
			expect(getCurrency('DE')).toBe('EUR');
		});
		it('returns EUR for France', () => {
			expect(getCurrency('FR')).toBe('EUR');
		});
		it('returns EUR for Italy', () => {
			expect(getCurrency('IT')).toBe('EUR');
		});
		it('returns EUR for Spain', () => {
			expect(getCurrency('ES')).toBe('EUR');
		});
		it('returns EUR for Netherlands', () => {
			expect(getCurrency('NL')).toBe('EUR');
		});
		it('returns EUR for Belgium', () => {
			expect(getCurrency('BE')).toBe('EUR');
		});
		it('returns EUR for Austria', () => {
			expect(getCurrency('AT')).toBe('EUR');
		});
		it('returns EUR for Portugal', () => {
			expect(getCurrency('PT')).toBe('EUR');
		});
		it('returns EUR for Ireland', () => {
			expect(getCurrency('IE')).toBe('EUR');
		});
		it('returns EUR for Finland', () => {
			expect(getCurrency('FI')).toBe('EUR');
		});
		it('returns EUR for Sweden', () => {
			expect(getCurrency('SE')).toBe('EUR');
		});
		it('returns EUR for Denmark', () => {
			expect(getCurrency('DK')).toBe('EUR');
		});
		it('returns PLN for Poland', () => {
			expect(getCurrency('PL')).toBe('PLN');
		});
		it('returns EUR for Greece', () => {
			expect(getCurrency('GR')).toBe('EUR');
		});
		it('returns EUR for Czech Republic', () => {
			expect(getCurrency('CZ')).toBe('EUR');
		});
		it('returns EUR for Hungary', () => {
			expect(getCurrency('HU')).toBe('EUR');
		});
		it('returns EUR for Romania', () => {
			expect(getCurrency('RO')).toBe('EUR');
		});
		it('returns EUR for Norway (EEA but not EU)', () => {
			expect(getCurrency('NO')).toBe('EUR');
		});
		it('returns EUR for Iceland (EEA but not EU)', () => {
			expect(getCurrency('IS')).toBe('EUR');
		});
		it('returns EUR for Liechtenstein (EEA but not EU)', () => {
			expect(getCurrency('LI')).toBe('EUR');
		});
	});
	describe('handles case insensitivity', () => {
		it('returns EUR for lowercase country code', () => {
			expect(getCurrency('de')).toBe('EUR');
			expect(getCurrency('fr')).toBe('EUR');
		});
		it('returns USD for lowercase non-EEA', () => {
			expect(getCurrency('us')).toBe('USD');
			expect(getCurrency('gb')).toBe('USD');
		});
		it('handles mixed case', () => {
			expect(getCurrency('De')).toBe('EUR');
			expect(getCurrency('dE')).toBe('EUR');
		});
	});
	describe('handles null and undefined', () => {
		it('returns USD for null', () => {
			expect(getCurrency(null)).toBe('USD');
		});
		it('returns USD for undefined', () => {
			expect(getCurrency(undefined)).toBe('USD');
		});
	});
	describe('handles empty and invalid inputs', () => {
		it('returns USD for empty string', () => {
			expect(getCurrency('')).toBe('USD');
		});
		it('returns USD for invalid country code', () => {
			expect(getCurrency('XX')).toBe('USD');
			expect(getCurrency('ZZ')).toBe('USD');
		});
		it('returns USD for numeric strings', () => {
			expect(getCurrency('12')).toBe('USD');
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
			'DK',
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
			'SE',
			'IS',
			'LI',
			'NO',
		];
		for (const country of eeaCountries) {
			it(`returns EUR for ${country}`, () => {
				expect(getCurrency(country)).toBe('EUR');
			});
		}
		it('uses local currency for Poland', () => {
			expect(getCurrency('PL')).toBe('PLN');
		});
	});
});

describe('getGiftCurrencyPreferences', () => {
	it('never offers a localized currency for a localized market', () => {
		for (const country of ['BR', 'IN', 'PL', 'TR']) {
			expect(getGiftCurrencyPreferences(country)).not.toContain(getCurrencyPreferences(country)[0]);
		}
	});
	it('uses EUR for EEA countries', () => {
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
		expect(getGiftCurrencyPreferences('de')).toEqual(['EUR', 'USD']);
		expect(getGiftCurrencyPreferences('br')).toEqual(['USD', 'EUR']);
	});
});
