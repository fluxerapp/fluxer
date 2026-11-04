// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getOperatorPassWallQuoteProblem,
	normalizeOperatorPassWallQuote,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassWallQuote';
import {describe, expect, it} from 'vitest';

describe('normalizeOperatorPassWallQuote', () => {
	it('trims, applies NFC and treats blank input as no quote', () => {
		expect(normalizeOperatorPassWallQuote('  Cafe\u0301 ')).toBe('Caf\u00E9');
		expect(normalizeOperatorPassWallQuote(' \t ')).toBeNull();
		expect(normalizeOperatorPassWallQuote(null)).toBeNull();
	});
});

describe('getOperatorPassWallQuoteProblem', () => {
	it('accepts plain text, emoji sequences and joiners inside words', () => {
		for (const quote of [
			'Chat should belong to the people who use it.',
			'Built by friends, for friends \u{1F3F3}\uFE0F\u200D\u{1F308}',
			'\u{1F469}\u{1F3FD}\u200D\u{1F4BB} shipping late at night',
			'Rock on \u{1F3F4}\u{E0067}\u{E0062}\u{E0073}\u{E0063}\u{E0074}\u{E007F}',
			'\u0645\u06CC\u200C\u062E\u0648\u0627\u0647\u0645',
			'e.g. this, i.e. that',
		]) {
			expect(getOperatorPassWallQuoteProblem(quote), quote).toBeNull();
		}
	});

	it('counts graphemes, so an emoji is one character', () => {
		expect(
			getOperatorPassWallQuoteProblem('\u{1F468}\u200D\u{1F469}\u200D\u{1F467}\u200D\u{1F466}'.repeat(80)),
		).toBeNull();
		expect(getOperatorPassWallQuoteProblem('a'.repeat(80))).toBeNull();
		expect(getOperatorPassWallQuoteProblem('a'.repeat(81))).toBe('too_long');
	});

	it('rejects line breaks', () => {
		for (const quote of ['one\ntwo', 'one\r\ntwo', 'one\u2028two']) {
			expect(getOperatorPassWallQuoteProblem(quote)).toBe('line_break');
		}
	});

	it('rejects bidi controls, zero-width characters, hidden tags and stacked marks', () => {
		for (const quote of [
			'abc\u202Edcba',
			'abc\u2066x\u2069',
			'right\u200Fmark',
			'zero\u200Bwidth',
			'word\u2060joiner',
			'\uFEFFbom',
			'stray\u200D joiner',
			'tag\u{E0041}\u{E0042}smuggle',
			'bell\u0007',
			`Z${'\u0301'.repeat(14)}`,
			'filler\u3164',
			'x\u2800y',
			'grapheme\u034Fjoiner',
			'khmer\u17B4aq',
			'khmer\u17B5aa',
		]) {
			expect(getOperatorPassWallQuoteProblem(quote), JSON.stringify(quote)).toBe('characters');
		}
	});

	it('rejects links and contact handles', () => {
		for (const quote of [
			'see https://example.org',
			'HTTP://EXAMPLE.ORG',
			'www.example',
			'visit example.com today',
			'shop at b\u00FCcher.de',
			'mail me@example.org',
			'ping @someone',
			'join us at discord.gg/abc',
			'invite/abc123',
			'xn--bcher-kva.example',
			'example\u3002com',
			'example\uFF61com',
			'example\u2024com',
			'example\uFE52com',
			'www\u3002x',
			'192.168.0.1',
			'1.1.1.1/x',
		]) {
			expect(getOperatorPassWallQuoteProblem(quote), quote).toBe('link');
		}
	});
});
