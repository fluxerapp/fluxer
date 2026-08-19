// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildMaskedLink,
	canWrapSelectionAsLink,
	parsePastedUrl,
} from '@app/features/lexical/composer/ComposerLinkPaste';
import {describe, expect, it} from 'vitest';

describe('parsePastedUrl', () => {
	it('accepts http and https urls', () => {
		expect(parsePastedUrl('https://fluxer.app/download')).toBe('https://fluxer.app/download');
		expect(parsePastedUrl('  http://example.test/a?b=c#d  ')).toBe('http://example.test/a?b=c#d');
	});

	it('rejects non-web protocols', () => {
		for (const value of ['javascript:alert(1)', 'data:text/html,<b>x</b>', 'file:///etc/passwd', 'mailto:a@b.test']) {
			expect(parsePastedUrl(value)).toBeNull();
		}
	});

	it('rejects prose that merely contains a url', () => {
		expect(parsePastedUrl('see https://fluxer.app for more')).toBeNull();
	});

	it('rejects angle brackets that would break the masked link', () => {
		expect(parsePastedUrl('https://fluxer.app/<script>')).toBeNull();
	});

	it('rejects empty and oversized input', () => {
		expect(parsePastedUrl('')).toBeNull();
		expect(parsePastedUrl(null)).toBeNull();
		expect(parsePastedUrl(`https://fluxer.app/${'a'.repeat(4000)}`)).toBeNull();
	});
});

describe('canWrapSelectionAsLink', () => {
	it('accepts ordinary selected words', () => {
		expect(canWrapSelectionAsLink('the download page')).toBe(true);
	});

	it('rejects blank or multiline selections', () => {
		expect(canWrapSelectionAsLink('   ')).toBe(false);
		expect(canWrapSelectionAsLink('first\nsecond')).toBe(false);
	});

	it('rejects selections containing markdown link brackets', () => {
		expect(canWrapSelectionAsLink('already [linked]')).toBe(false);
	});

	it('rejects replacing a url with a url', () => {
		expect(canWrapSelectionAsLink('https://example.test')).toBe(false);
	});
});

describe('buildMaskedLink', () => {
	it('uses the escaped destination form so parentheses survive', () => {
		expect(buildMaskedLink('wiki', 'https://en.wikipedia.org/wiki/Foo_(bar)')).toBe(
			'[wiki](<https://en.wikipedia.org/wiki/Foo_(bar)>)',
		);
	});
});
