// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseUrlDomainEntry, UrlHostRuleSet} from '@app/api/utils/UrlHostRules';
import {describe, expect, it} from 'vitest';

function rules(entries: Array<[string, boolean]>): UrlHostRuleSet {
	const set = new UrlHostRuleSet();
	for (const [value, matchSubdomains] of entries) {
		set.add(value, matchSubdomains);
	}
	return set;
}

describe('parseUrlDomainEntry', () => {
	it('canonicalizes a plain domain', () => {
		expect(parseUrlDomainEntry(' Shop.Example.COM. ')).toEqual({ok: true, value: 'shop.example.com', pattern: false});
	});

	it('stores an internationalized domain in punycode', () => {
		expect(parseUrlDomainEntry('bücher.example')).toEqual({ok: true, value: 'xn--bcher-kva.example', pattern: false});
	});

	it('keeps an exact entry for a domain under a shared hosting suffix', () => {
		expect(parseUrlDomainEntry('onrender.com')).toEqual({ok: true, value: 'onrender.com', pattern: false});
	});

	it('rejects malformed plain domains', () => {
		for (const value of ['localhost', 'bad_label.example.com', '-lead.example.com', 'a..example.com', 'a b.com']) {
			expect(parseUrlDomainEntry(value).ok).toBe(false);
		}
	});

	it('canonicalizes a pattern and collapses repeated wildcards', () => {
		expect(parseUrlDomainEntry('**Shop**.OnRender.com.')).toEqual({
			ok: true,
			value: '*shop*.onrender.com',
			pattern: true,
		});
	});

	it('accepts patterns under a private shared hosting suffix', () => {
		expect(parseUrlDomainEntry('*shop*.github.io').ok).toBe(true);
		expect(parseUrlDomainEntry('shop-*.example.co.uk').ok).toBe(true);
	});

	it('rejects patterns that are too broad', () => {
		for (const value of [
			'*',
			'*.com',
			'*shop*.com',
			'shop*.co.uk',
			'*.example.com',
			'*ab*.example.com',
			'a*b.example.com',
		]) {
			expect(parseUrlDomainEntry(value).ok).toBe(false);
		}
	});

	it('rejects wildcards outside the leftmost label', () => {
		expect(parseUrlDomainEntry('shop.*.example.com').ok).toBe(false);
		expect(parseUrlDomainEntry('*shop*.example*.com').ok).toBe(false);
	});

	it('rejects wildcard labels with unsupported characters or too many wildcards', () => {
		expect(parseUrlDomainEntry('*sh?p*.example.com').ok).toBe(false);
		expect(parseUrlDomainEntry('*sh_p*.example.com').ok).toBe(false);
		expect(parseUrlDomainEntry('*a*b*c*d*.example.com').ok).toBe(false);
	});

	it('rejects a pattern without a domain', () => {
		expect(parseUrlDomainEntry('*shop*').ok).toBe(false);
	});
});

describe('UrlHostRuleSet', () => {
	it('matches an exact domain', () => {
		const set = rules([['shop.example.com', false]]);
		expect(set.matches('shop.example.com')).toBe(true);
		expect(set.matches('www.shop.example.com')).toBe(false);
		expect(set.matches('example.com')).toBe(false);
	});

	it('matches subdomains only when the entry covers them', () => {
		const set = rules([['shop.example.com', true]]);
		expect(set.matches('shop.example.com')).toBe(true);
		expect(set.matches('a.b.shop.example.com')).toBe(true);
		expect(set.matches('myshop.example.com')).toBe(false);
	});

	it('normalizes the checked host', () => {
		const set = rules([['shop.example.com', true]]);
		expect(set.matches('SHOP.Example.com.')).toBe(true);
		expect(set.matches('www.shop。example。com')).toBe(true);
		expect(rules([['xn--bcher-kva.example', false]]).matches('Bücher.example')).toBe(true);
	});

	it('matches a pattern against the label left of its suffix', () => {
		const set = rules([['*shop*.onrender.com', false]]);
		expect(set.matches('shop.onrender.com')).toBe(true);
		expect(set.matches('best-shop-2.onrender.com')).toBe(true);
		expect(set.matches('SHOPPING.onrender.com')).toBe(true);
		expect(set.matches('store.onrender.com')).toBe(false);
	});

	it('never matches the bare suffix of a pattern', () => {
		const set = rules([['*shop*.onrender.com', true]]);
		expect(set.matches('onrender.com')).toBe(false);
		expect(set.matches('com')).toBe(false);
		expect(set.matches('shop.com')).toBe(false);
		expect(set.matches('shop.onrender.com.evil.example')).toBe(false);
		expect(set.matches('shoponrender.com')).toBe(false);
	});

	it('applies anchored pattern segments', () => {
		const set = rules([
			['shop-*.example.com', false],
			['*-store.example.org', false],
			['a*b*c.example.net', false],
		]);
		expect(set.matches('shop-1.example.com')).toBe(true);
		expect(set.matches('myshop-1.example.com')).toBe(false);
		expect(set.matches('big-store.example.org')).toBe(true);
		expect(set.matches('big-store2.example.org')).toBe(false);
		expect(set.matches('axxbyyc.example.net')).toBe(true);
		expect(set.matches('abc.example.net')).toBe(true);
		expect(set.matches('acb.example.net')).toBe(false);
	});

	it('extends a pattern to deeper subdomains only when the entry covers them', () => {
		const exact = rules([['*shop*.onrender.com', false]]);
		const covering = rules([['*shop*.onrender.com', true]]);
		expect(exact.matches('www.shop.onrender.com')).toBe(false);
		expect(covering.matches('www.shop.onrender.com')).toBe(true);
		expect(covering.matches('shop.www.onrender.com')).toBe(false);
	});

	it('matches internationalized labels through their punycode form', () => {
		const set = rules([['*shop*.onrender.com', false]]);
		expect(set.matches('shop-ü.onrender.com')).toBe(true);
	});

	it('removes domains and patterns', () => {
		const set = rules([
			['shop.example.com', true],
			['*shop*.onrender.com', true],
			['*store*.onrender.com', true],
		]);
		set.remove('SHOP.example.com');
		set.remove('**shop*.onrender.com');
		expect(set.matches('shop.example.com')).toBe(false);
		expect(set.matches('shop.onrender.com')).toBe(false);
		expect(set.matches('store.onrender.com')).toBe(true);
		expect(set.size).toEqual({domains: 0, patterns: 1});
	});

	it('replaces a pattern when it is added again', () => {
		const set = rules([
			['*shop*.onrender.com', false],
			['*shop*.onrender.com', true],
		]);
		expect(set.size).toEqual({domains: 0, patterns: 1});
		expect(set.matches('www.shop.onrender.com')).toBe(true);
	});

	it('ignores invalid stored patterns', () => {
		const set = rules([['*.com', true]]);
		expect(set.size).toEqual({domains: 0, patterns: 0});
		expect(set.matches('anything.com')).toBe(false);
	});
});
