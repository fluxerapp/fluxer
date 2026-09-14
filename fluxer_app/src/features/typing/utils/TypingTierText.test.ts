// SPDX-License-Identifier: AGPL-3.0-or-later

import type {I18n} from '@lingui/core';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: {message: string}) => descriptor}));

const {getTypingTierText} = await import('@app/features/typing/utils/TypingTierText');

function i18nFor(locale: string): I18n {
	return {locale, _: (descriptor: {message: string}) => `[${locale}] ${descriptor.message}`} as unknown as I18n;
}

describe('getTypingTierText', () => {
	it('uses the translatable several people line for exactly four typists in every locale', () => {
		for (const locale of ['en-US', 'en-GB', 'de', 'ja']) {
			expect(getTypingTierText(i18nFor(locale), 4)).toBe(`[${locale}] Several people are typing...`);
		}
	});

	it.each([
		['en-US', 5, 'A handful of keyboard warriors are assembling...'],
		['en-US', 9, 'A handful of keyboard warriors are assembling...'],
		['en-GB', 10, 'A symphony of clacking keys is underway...'],
		['en-US', 14, 'A symphony of clacking keys is underway...'],
		['en-GB', 15, "It's a full-blown typing fiesta in here"],
		['en-US', 19, "It's a full-blown typing fiesta in here"],
		['en-GB', 20, "Whoa, it's a typing apocalypse"],
		['en-US', 250, "Whoa, it's a typing apocalypse"],
	])('keeps the untranslated joke line in %s for %i typists', (locale, count, line) => {
		expect(getTypingTierText(i18nFor(locale), count)).toBe(line);
	});

	it.each([
		'ar',
		'de',
		'es-419',
		'fr',
		'ja',
		'pt-BR',
		'sv-SE',
		'zh-TW',
	])('collapses every crowd to the several people line in %s', (locale) => {
		for (const count of [5, 9, 10, 14, 15, 19, 20, 250]) {
			expect(getTypingTierText(i18nFor(locale), count)).toBe(`[${locale}] Several people are typing...`);
		}
	});

	it('treats a bare language code as a non-English locale', () => {
		expect(getTypingTierText(i18nFor('en'), 12)).toBe('[en] Several people are typing...');
	});
});
