// SPDX-License-Identifier: AGPL-3.0-or-later

import {canonicalizeUrl, extractUrlCandidates} from '@app/api/utils/UrlNormalizer';
import {describe, expect, it} from 'vitest';

describe('canonicalizeUrl', () => {
	it('lowercases scheme and host', () => {
		expect(canonicalizeUrl('HTTPS://EXAMPLE.COM/Path')).toBe('https://example.com/path');
	});
	it('adds trailing slash on bare host', () => {
		const result = canonicalizeUrl('https://example.com');
		expect(result).toBe('https://example.com/');
	});
	it('preserves path', () => {
		expect(canonicalizeUrl('https://example.com/foo/bar')).toBe('https://example.com/foo/bar');
	});
	it('strips utm_source tracking param', () => {
		expect(canonicalizeUrl('https://example.com/?utm_source=twitter')).toBe('https://example.com/');
	});
	it('strips utm_medium tracking param', () => {
		expect(canonicalizeUrl('https://example.com/?utm_medium=social')).toBe('https://example.com/');
	});
	it('strips utm_campaign tracking param', () => {
		expect(canonicalizeUrl('https://example.com/?utm_campaign=spring')).toBe('https://example.com/');
	});
	it('strips fbclid tracking param', () => {
		expect(canonicalizeUrl('https://example.com/?fbclid=abc123')).toBe('https://example.com/');
	});
	it('strips gclid tracking param', () => {
		expect(canonicalizeUrl('https://example.com/?gclid=xyz')).toBe('https://example.com/');
	});
	it('strips mc_cid tracking param', () => {
		expect(canonicalizeUrl('https://example.com/?mc_cid=abc')).toBe('https://example.com/');
	});
	it('strips mc_eid tracking param', () => {
		expect(canonicalizeUrl('https://example.com/?mc_eid=def')).toBe('https://example.com/');
	});
	it('strips msclkid tracking param', () => {
		expect(canonicalizeUrl('https://example.com/?msclkid=bing')).toBe('https://example.com/');
	});
	it('strips _ga tracking param', () => {
		expect(canonicalizeUrl('https://example.com/?_ga=123')).toBe('https://example.com/');
	});
	it('strips _gl tracking param', () => {
		expect(canonicalizeUrl('https://example.com/?_gl=456')).toBe('https://example.com/');
	});
	it('strips igshid tracking param', () => {
		expect(canonicalizeUrl('https://example.com/?igshid=abc')).toBe('https://example.com/');
	});
	it('strips ref_src and ref_url tracking params', () => {
		expect(canonicalizeUrl('https://example.com/?ref_src=twsrc&ref_url=https://t.co')).toBe('https://example.com/');
	});
	it('strips multiple tracking params but keeps non-tracking params', () => {
		expect(canonicalizeUrl('https://example.com/?page=1&utm_source=tw&fbclid=abc&sort=asc')).toBe(
			'https://example.com/?page=1&sort=asc',
		);
	});
	it('sorts remaining query params alphabetically', () => {
		expect(canonicalizeUrl('https://example.com/?z=1&a=2&m=3')).toBe('https://example.com/?a=2&m=3&z=1');
	});
	it('strips fragment', () => {
		expect(canonicalizeUrl('https://example.com/page#section')).toBe('https://example.com/page');
	});
	it('strips fragment along with tracking params', () => {
		expect(canonicalizeUrl('https://example.com/?utm_source=tw#top')).toBe('https://example.com/');
	});
	it('strips default port 80 for http', () => {
		expect(canonicalizeUrl('http://example.com:80/path')).toBe('http://example.com/path');
	});
	it('strips default port 443 for https', () => {
		expect(canonicalizeUrl('https://example.com:443/path')).toBe('https://example.com/path');
	});
	it('keeps non-default ports', () => {
		const result = canonicalizeUrl('https://example.com:8443/path');
		expect(result).toContain(':8443');
	});
	it('converts IDN to punycode', () => {
		const result = canonicalizeUrl('https://xn--nxasmq6b.example.com/');
		expect(result).not.toBeNull();
		expect(result).toContain('xn--');
	});
	it('rejects mailto scheme', () => {
		expect(canonicalizeUrl('mailto:user@example.com')).toBeNull();
	});
	it('rejects ftp scheme', () => {
		expect(canonicalizeUrl('ftp://example.com/file')).toBeNull();
	});
	it('rejects tel scheme', () => {
		expect(canonicalizeUrl('tel:+1234567890')).toBeNull();
	});
	it('rejects invalid URLs', () => {
		expect(canonicalizeUrl('not-a-url')).toBeNull();
	});
	it('rejects empty string', () => {
		expect(canonicalizeUrl('')).toBeNull();
	});
	it('rejects whitespace-only string', () => {
		expect(canonicalizeUrl('   ')).toBeNull();
	});
	it('rejects URLs with embedded whitespace', () => {
		expect(canonicalizeUrl('https://example .com/')).toBeNull();
	});
	it('rejects URLs with null byte control characters', () => {
		expect(canonicalizeUrl('https://example.com/\x00')).toBeNull();
	});
	it('rejects URLs with embedded newlines', () => {
		expect(canonicalizeUrl('https://exam\nple.com/')).toBeNull();
	});
	it('rejects URLs with embedded tabs', () => {
		expect(canonicalizeUrl('https://exa\tmple.com/')).toBeNull();
	});
	it('trims leading/trailing whitespace before parsing', () => {
		expect(canonicalizeUrl('  https://example.com/  ')).toBe('https://example.com/');
	});
});

describe('extractUrlCandidates', () => {
	it('extracts https:// URL from text', () => {
		const result = extractUrlCandidates('visit https://example.com/page for info');
		expect(result).toHaveLength(1);
		expect(result[0]).toBe('https://example.com/page');
	});
	it('extracts http:// URL from text', () => {
		const result = extractUrlCandidates('see http://example.com/page');
		expect(result).toHaveLength(1);
		expect(result[0]).toBe('http://example.com/page');
	});
	it('extracts bare domain.tld/path URLs and adds http scheme', () => {
		const result = extractUrlCandidates('check out example.com/page');
		expect(result).toHaveLength(1);
		expect(result[0]).toBe('http://example.com/page');
	});
	it('handles URLs in parentheses', () => {
		const result = extractUrlCandidates('(https://example.com/page)');
		expect(result).toHaveLength(1);
		expect(result[0]).toBe('https://example.com/page');
	});
	it('handles URLs in quotes', () => {
		const result = extractUrlCandidates('"https://example.com/page"');
		expect(result).toHaveLength(1);
		expect(result[0]).toBe('https://example.com/page');
	});
	it('strips trailing punctuation', () => {
		const result = extractUrlCandidates('visit https://example.com/page.');
		expect(result).toHaveLength(1);
		expect(result[0]).toBe('https://example.com/page');
	});
	it('strips trailing comma', () => {
		const result = extractUrlCandidates('see https://example.com/page, then');
		expect(result).toHaveLength(1);
		expect(result[0]).toBe('https://example.com/page');
	});
	it('strips trailing exclamation mark', () => {
		const result = extractUrlCandidates('wow https://example.com/page!');
		expect(result).toHaveLength(1);
		expect(result[0]).toBe('https://example.com/page');
	});
	it('does not match full email addresses as URLs', () => {
		const result = extractUrlCandidates('contact support@help.co');
		for (const url of result) {
			expect(url).not.toContain('@');
		}
	});
	it('extracts multiple URLs from one string', () => {
		const result = extractUrlCandidates('see https://example.com and https://other.org/page for details');
		expect(result).toHaveLength(2);
	});
	it('returns empty array for empty string', () => {
		expect(extractUrlCandidates('')).toEqual([]);
	});
	it('returns empty array for null-ish input', () => {
		expect(extractUrlCandidates(null)).toEqual([]);
		expect(extractUrlCandidates(undefined)).toEqual([]);
	});
	it('returns empty array for text with no URLs', () => {
		expect(extractUrlCandidates('hello world no links here')).toEqual([]);
	});
});
