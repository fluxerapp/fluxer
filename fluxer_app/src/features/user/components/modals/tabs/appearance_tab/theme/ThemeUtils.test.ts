// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	cssColorStringToHex,
	cssColorStringToNumber,
	numberToHex,
	updateCssForVariable,
} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {describe, expect, it} from 'vitest';

describe('updateCssForVariable', () => {
	it('adds a token override when the CSS is empty', () => {
		expect(updateCssForVariable('', '--background-primary', '#ff00ff')).toBe(
			':root { --background-primary: #ff00ff; }\n',
		);
	});
	it('updates an existing token override', () => {
		expect(updateCssForVariable(':root { --background-primary: #ffffff; }\n', '--background-primary', '#111111')).toBe(
			':root { --background-primary: #111111; }\n',
		);
	});
	it('removes the empty root block after the last token override is cleared', () => {
		expect(updateCssForVariable(':root { --background-primary: #ffffff; }\n', '--background-primary', null)).toBe('');
	});
	it('keeps unrelated CSS when clearing a token override', () => {
		expect(
			updateCssForVariable(
				'body { color: red; }\n:root { --background-primary: #ffffff; }\n',
				'--background-primary',
				null,
			),
		).toBe('body { color: red; }');
	});
});

describe('cssColorStringToNumber', () => {
	it('parses black as a real color', () => {
		expect(numberToHex(cssColorStringToNumber('#000000') ?? -1)).toBe('#000000');
		expect(numberToHex(cssColorStringToNumber('rgb(0, 0, 0)') ?? -1)).toBe('#000000');
		expect(numberToHex(cssColorStringToNumber('#000000 !important') ?? -1)).toBe('#000000');
	});
	it('does not turn invalid CSS values into black', () => {
		expect(cssColorStringToNumber('1px solid hsl(0, 0%, 0%)')).toBeNull();
		expect(cssColorStringToNumber('not-a-color')).toBeNull();
	});
	it('normalizes equivalent CSS colors to the same hex value', () => {
		expect(cssColorStringToHex('rgb(0, 0, 0)')).toBe('#000000');
		expect(cssColorStringToHex('#000000 !important')).toBe('#000000');
	});
});
