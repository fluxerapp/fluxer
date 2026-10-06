// SPDX-License-Identifier: AGPL-3.0-or-later

import {sanitizeOptionalAbsoluteUrl, sanitizeOptionalAbsoluteUrlOrNull} from '@app/api/utils/UrlSanitizer';
import {describe, expect, it} from 'vitest';

describe('sanitizeOptionalAbsoluteUrl', () => {
	it('returns undefined for nullish values', () => {
		expect(sanitizeOptionalAbsoluteUrl(undefined)).toBeUndefined();
		expect(sanitizeOptionalAbsoluteUrl(null)).toBeUndefined();
	});
	it('returns undefined for empty or whitespace-only values', () => {
		expect(sanitizeOptionalAbsoluteUrl('')).toBeUndefined();
		expect(sanitizeOptionalAbsoluteUrl('   ')).toBeUndefined();
	});
	it('returns undefined for invalid URLs', () => {
		expect(sanitizeOptionalAbsoluteUrl('not-a-valid-url')).toBeUndefined();
	});
	it('trims and normalises valid absolute URLs', () => {
		expect(sanitizeOptionalAbsoluteUrl(' https://example.com/path ')).toBe('https://example.com/path');
		expect(sanitizeOptionalAbsoluteUrl('https://example.com')).toBe('https://example.com/');
	});
});

describe('sanitizeOptionalAbsoluteUrlOrNull', () => {
	it('returns null for invalid inputs', () => {
		expect(sanitizeOptionalAbsoluteUrlOrNull(undefined)).toBeNull();
		expect(sanitizeOptionalAbsoluteUrlOrNull(null)).toBeNull();
		expect(sanitizeOptionalAbsoluteUrlOrNull('not-a-valid-url')).toBeNull();
	});
	it('returns normalised URLs for valid inputs', () => {
		expect(sanitizeOptionalAbsoluteUrlOrNull(' https://example.com/path ')).toBe('https://example.com/path');
	});
});
