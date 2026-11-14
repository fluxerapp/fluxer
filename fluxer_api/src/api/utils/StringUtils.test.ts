// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseString} from '@app/api/utils/StringUtils';
import {describe, expect, it} from 'vitest';

describe('parseString', () => {
	it('keeps family emoji intact at the webhook title truncation boundary', () => {
		expect(parseString('ab👨‍👩‍👧cdefghij', 6)).toBe('ab👨‍👩‍👧...');
		expect(parseString(`${'👨‍👩‍👧‍👦'.repeat(5)}abcdef`, 6)).toBe(`${'👨‍👩‍👧‍👦'.repeat(3)}...`);
	});
	it('keeps combining marks and flags together and decodes entities before truncating', () => {
		expect(parseString('e\u0301🇸🇪&amp;more text', 6)).toBe('e\u0301🇸🇪&...');
		expect(parseString('  &amp;🇸🇪  ', 2)).toBe('&🇸🇪');
	});
});
