// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildMaskedLink,
	canWrapSelectionAsLink,
	parsePastedUrl,
	resolvePastedLinkInsertion,
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

describe('resolvePastedLinkInsertion', () => {
	it('wraps a selection when a bare url is pasted over it', () => {
		expect(resolvePastedLinkInsertion('https://fluxer.app/download', 'the download page')).toBe(
			'[the download page](<https://fluxer.app/download>)',
		);
	});

	it('wraps a message link the same way a plain url is wrapped', () => {
		const messageLink = 'https://fluxer.app/channels/1234567890/9876543210/1122334455';
		expect(resolvePastedLinkInsertion(messageLink, 'this message')).toBe(`[this message](<${messageLink}>)`);
	});

	it('declines when the pasted text is not a url', () => {
		expect(resolvePastedLinkInsertion('just some text', 'selected')).toBeNull();
	});

	it('declines when the selection cannot be wrapped', () => {
		expect(resolvePastedLinkInsertion('https://fluxer.app', '   ')).toBeNull();
		expect(resolvePastedLinkInsertion('https://fluxer.app', 'https://example.test')).toBeNull();
	});

	it('declines a non-web protocol so pasting cannot forge a link', () => {
		expect(resolvePastedLinkInsertion('javascript:alert(1)', 'click me')).toBeNull();
	});
});
