// SPDX-License-Identifier: AGPL-3.0-or-later

import {formatMinorUnitPrice, formatMinorUnitPriceWithFraction} from '@fluxer/number_utils/src/PriceFormatting';
import {describe, expect, test} from 'vitest';

describe('formatMinorUnitPrice', () => {
	test.each([
		[1234567, 'USD', 'en-US', '$12,345.67'],
		[1234567, 'BRL', 'pt-BR', 'R$ 12.345,67'],
		[1234567, 'INR', 'hi', '₹12,345.67'],
		[1234567, 'TRY', 'tr', '₺12.345,67'],
		[1234567, 'SEK', 'sv-SE', '12 345,67 kr'],
		[999, 'USD', 'en-GB', 'US$9.99'],
		[999, 'USD', 'es-ES', '9,99 US$'],
		[999, 'USD', 'es-419', 'US$9.99'],
		[999, 'USD', 'fr', '9,99 $US'],
		[999, 'USD', 'pt-BR', 'US$ 9,99'],
		[999, 'USD', 'zh-TW', 'US$9.99'],
	] as const)('formats %i %s for %s', (amount, currency, locale, expected) => {
		expect(formatMinorUnitPrice(amount, currency, locale)).toBe(expected);
	});

	test.each([
		['ar', undefined, '\u20669.99 US$\u2069'],
		['ar', 'US', '\u20669.99 $\u2069'],
		['he', undefined, '\u20669.99 $\u2069'],
	] as const)('isolates right-to-left prices for %s in %s', (locale, country, expected) => {
		expect(formatMinorUnitPrice(999, 'USD', locale, country)).toBe(expected);
	});

	test.each([
		[999, 'ISK', 'de', '10 kr'],
		[999, 'UGX', 'en-US', 'UGX 10'],
	] as const)('rounds Stripe whole-unit prices for %i %s', (amount, currency, locale, expected) => {
		expect(formatMinorUnitPrice(amount, currency, locale)).toBe(expected);
	});

	test('shows fillér only when a forint amount has them', () => {
		expect(formatMinorUnitPrice(42307, 'HUF', 'en-US')).toBe('HUF 423.07');
		expect(formatMinorUnitPrice(100000, 'HUF', 'en-US')).toBe('HUF 1,000');
	});

	test('keeps the fraction on request so a total and its tax line up', () => {
		expect(formatMinorUnitPriceWithFraction(4900, 'SEK', 'sv-SE')).toBe('49,00 kr');
		expect(formatMinorUnitPriceWithFraction(59000, 'ISK', 'de')).toBe('590 kr');
	});

	test('shows the plain symbol for the currency of the reader country', () => {
		expect(formatMinorUnitPrice(999, 'SEK', 'en-US', 'SE')).toBe('9.99 kr');
		expect(formatMinorUnitPrice(999, 'NOK', 'sv-SE', 'NO')).toBe('9,99 kr');
		expect(formatMinorUnitPrice(999, 'DKK', 'da', 'SE')).toBe('9,99 kr.');
		expect(formatMinorUnitPrice(999, 'SEK', 'en-US', null)).toBe('SEK 9.99');
		expect(formatMinorUnitPrice(999, 'USD', 'en-GB', 'GB')).toBe('US$9.99');
		expect(formatMinorUnitPrice(999, 'USD', 'en-GB', 'us')).toBe('$9.99');
		expect(formatMinorUnitPrice(999, 'NOK', 'sv', 'SE')).toBe('9,99 Nkr');
		expect(formatMinorUnitPrice(999, 'DKK', 'en', 'GL')).toBe('9.99 kr');
		expect(formatMinorUnitPrice(59000, 'ISK', 'en-US', 'IS')).toBe('590 kr');
	});

	test('drops the fraction only on whole amounts', () => {
		expect(formatMinorUnitPrice(4900, 'SEK', 'sv-SE')).toBe('49 kr');
		expect(formatMinorUnitPrice(4950, 'SEK', 'sv-SE')).toBe('49,50 kr');
		expect(formatMinorUnitPrice(19900, 'USD', 'en-US')).toBe('$199');
		expect(formatMinorUnitPrice(499, 'USD', 'en-US')).toBe('$4.99');
		expect(formatMinorUnitPrice(59000, 'ISK', 'de')).toBe('590 kr');
		expect(formatMinorUnitPrice(700, 'JPY', 'ja')).toBe('￥700');
	});

	test('qualifies the dollar only where a reader has a dollar or peso of their own', () => {
		expect(formatMinorUnitPrice(999, 'USD', 'en-GB')).toBe('US$9.99');
		expect(formatMinorUnitPrice(999, 'USD', 'pt_BR')).toBe('US$ 9,99');
		expect(formatMinorUnitPrice(999, 'USD', 'fr-CA')).toBe('9,99 $US');
		expect(formatMinorUnitPrice(999, 'USD', 'de')).toBe('9,99 $');
	});

	test('writes kronor and złoty after the amount', () => {
		expect(formatMinorUnitPrice(4900, 'NOK', 'de')).toBe('49 kr');
		expect(formatMinorUnitPrice(3500, 'DKK', 'da')).toBe('35 kr.');
		expect(formatMinorUnitPrice(1799, 'PLN', 'nl')).toBe('17,99 zł');
	});

	test('keeps the plain krona for its own readers', () => {
		expect(formatMinorUnitPrice(999, 'SEK', 'sv-SE')).toBe('9,99 kr');
		expect(formatMinorUnitPrice(999, 'DKK', 'da-DK')).toBe('9,99 kr.');
		expect(formatMinorUnitPrice(999, 'NOK', 'nb-NO')).toBe('9,99 kr');
		expect(formatMinorUnitPrice(999, 'NOK', 'nn')).toBe('9,99 kr');
		expect(formatMinorUnitPrice(999, 'NOK', 'no')).toBe('9,99 kr');
	});

	test('tells a krona apart when the reader has another krona or none', () => {
		expect(formatMinorUnitPrice(999, 'NOK', 'sv')).toBe('9,99 Nkr');
		expect(formatMinorUnitPrice(999, 'DKK', 'sv-SE')).toBe('9,99 Dkr');
		expect(formatMinorUnitPrice(59000, 'ISK', 'sv-SE')).toBe('590 ISK');
		expect(formatMinorUnitPrice(999, 'SEK', 'da')).toBe('9,99 SEK');
		expect(formatMinorUnitPrice(999, 'NOK', 'da')).toBe('9,99 NOK');
		expect(formatMinorUnitPrice(999, 'SEK', 'nb')).toBe('9,99 SEK');
		expect(formatMinorUnitPrice(999, 'DKK', 'no')).toBe('9,99 DKK');
		expect(formatMinorUnitPrice(999, 'SEK', 'en-US')).toBe('SEK 9.99');
		expect(formatMinorUnitPrice(4900, 'NOK', 'en-AU')).toBe('NOK 49');
		expect(formatMinorUnitPrice(999, 'DKK', null)).toBe('DKK 9.99');
	});

	test('uses the standard symbol for other currencies', () => {
		expect(formatMinorUnitPrice(999, 'CAD', 'en-US')).toBe('CA$9.99');
		expect(formatMinorUnitPrice(999, 'AUD', 'en-US')).toBe('A$9.99');
		expect(formatMinorUnitPrice(999, 'CAD', 'de')).toBe('9,99 CA$');
		expect(formatMinorUnitPrice(999, 'GBP', 'en-GB')).toBe('£9.99');
		expect(formatMinorUnitPrice(999, 'CHF', 'de')).toBe('9,99 CHF');
	});

	test('falls back to American English for unknown locales', () => {
		expect(formatMinorUnitPrice(19900, 'usd', null)).toBe('$199');
		expect(formatMinorUnitPrice(19950, 'EUR', 'not a locale')).toBe('€199.50');
		expect(formatMinorUnitPrice(19950, 'EUR', 'zz')).toBe('€199.50');
	});

	test('returns null without an amount or currency', () => {
		expect(formatMinorUnitPrice(null, 'USD', 'en-US')).toBeNull();
		expect(formatMinorUnitPrice(100, '', 'en-US')).toBeNull();
	});
});
