/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {describe, expect, it} from 'vitest';
import {Locales} from '~/Constants';
import {parseAcceptLanguage} from './LocaleUtils';

describe('LocaleUtils', () => {
	describe('parseAcceptLanguage', () => {
		it('should return en-US when header is null', () => {
			expect(parseAcceptLanguage(null)).toBe(Locales.EN_US);
		});

		it('should return en-US when header is undefined', () => {
			expect(parseAcceptLanguage(undefined)).toBe(Locales.EN_US);
		});

		it('should return en-US when header is empty', () => {
			expect(parseAcceptLanguage('')).toBe(Locales.EN_US);
		});

		it('should return exact match for supported locale', () => {
			expect(parseAcceptLanguage('fr')).toBe(Locales.FR);
			expect(parseAcceptLanguage('de')).toBe(Locales.DE);
			expect(parseAcceptLanguage('ja')).toBe(Locales.JA);
		});

		it('should return exact match for supported locale with region', () => {
			expect(parseAcceptLanguage('en-US')).toBe(Locales.EN_US);
			expect(parseAcceptLanguage('en-GB')).toBe(Locales.EN_GB);
			expect(parseAcceptLanguage('pt-BR')).toBe(Locales.PT_BR);
			expect(parseAcceptLanguage('es-ES')).toBe(Locales.ES_ES);
		});

		it('should handle quality values correctly', () => {
			expect(parseAcceptLanguage('en;q=0.8,fr;q=0.9')).toBe(Locales.FR);
			expect(parseAcceptLanguage('de;q=0.5,ja;q=0.9,en-US;q=0.3')).toBe(Locales.JA);
		});

		it('should handle complex Accept-Language headers', () => {
			expect(parseAcceptLanguage('fr-FR,fr;q=0.9,en-US;q=0.8,en;q=0.7')).toBe(Locales.FR);
			expect(parseAcceptLanguage('de-DE,de;q=0.9,en;q=0.8')).toBe(Locales.DE);
		});

		it('should match language code when exact locale not found', () => {
			expect(parseAcceptLanguage('en')).toBe(Locales.EN_US);
			expect(parseAcceptLanguage('es')).toBe(Locales.ES_ES);
			expect(parseAcceptLanguage('pt')).toBe(Locales.PT_BR);
			expect(parseAcceptLanguage('zh')).toBe(Locales.ZH_CN);
		});

		it('should fallback to en-US for unsupported locales', () => {
			expect(parseAcceptLanguage('xx-XX')).toBe(Locales.EN_US);
			expect(parseAcceptLanguage('invalid')).toBe(Locales.EN_US);
			expect(parseAcceptLanguage('unsupported-locale')).toBe(Locales.EN_US);
		});

		it('should handle malformed headers gracefully', () => {
			expect(parseAcceptLanguage(';;;')).toBe(Locales.EN_US);
			expect(parseAcceptLanguage('q=0.9')).toBe(Locales.EN_US);
		});

		it('should prioritize first exact match when multiple supported locales exist', () => {
			expect(parseAcceptLanguage('ja,fr,de')).toBe(Locales.JA);
			expect(parseAcceptLanguage('en-GB,en-US')).toBe(Locales.EN_GB);
		});

		it('should handle whitespace correctly', () => {
			expect(parseAcceptLanguage('  fr  ')).toBe(Locales.FR);
			expect(parseAcceptLanguage('en-US , fr ; q=0.8')).toBe(Locales.EN_US);
		});

		it('should respect quality values over order', () => {
			expect(parseAcceptLanguage('en-US;q=0.5,fr;q=1.0')).toBe(Locales.FR);
			expect(parseAcceptLanguage('de;q=0.3,ja;q=0.9,en-GB;q=0.7')).toBe(Locales.JA);
		});

		it('should handle case insensitivity for language codes', () => {
			expect(parseAcceptLanguage('EN-us')).toBe(Locales.EN_US);
			expect(parseAcceptLanguage('FR')).toBe(Locales.FR);
			expect(parseAcceptLanguage('EN-gb')).toBe(Locales.EN_GB);
			expect(parseAcceptLanguage('pt-br')).toBe(Locales.PT_BR);
		});
	});
});
