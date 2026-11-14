// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	REGIONAL_FORMAT_PATTERN,
	REGIONAL_FORMAT_SYSTEM,
	RegionalFormatLocales,
	resolveRegionalFormatLocale,
} from '@fluxer/constants/src/Locales';
import {describe, expect, test} from 'vitest';

describe('resolveRegionalFormatLocale', () => {
	test('follows the language when no regional format is set', () => {
		expect(resolveRegionalFormatLocale(null, 'en-GB', 'de-DE')).toBe('en-GB');
	});

	test('uses the system locale for the system choice', () => {
		expect(resolveRegionalFormatLocale(REGIONAL_FORMAT_SYSTEM, 'en-GB', 'de-DE')).toBe('de-DE');
	});

	test('uses a chosen region over the language', () => {
		expect(resolveRegionalFormatLocale('sv-SE', 'en-US', 'en-US')).toBe('sv-SE');
	});

	test('falls back to en-US for a system locale ICU lacks', () => {
		expect(resolveRegionalFormatLocale(REGIONAL_FORMAT_SYSTEM, 'en-GB', 'qaa')).toBe('en-US');
	});

	test('every listed region passes the API pattern', () => {
		for (const locale of RegionalFormatLocales) {
			expect(locale).toMatch(REGIONAL_FORMAT_PATTERN);
		}
		expect('system').toMatch(REGIONAL_FORMAT_PATTERN);
		expect('de').not.toMatch(REGIONAL_FORMAT_PATTERN);
		expect('de-de').not.toMatch(REGIONAL_FORMAT_PATTERN);
	});
});
