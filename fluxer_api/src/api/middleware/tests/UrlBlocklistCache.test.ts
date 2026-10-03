// SPDX-License-Identifier: AGPL-3.0-or-later

import {urlBlocklistCache} from '@app/api/middleware/UrlBlocklistCache';
import {afterEach, describe, expect, it} from 'vitest';

describe('urlBlocklistCache link matching', () => {
	afterEach(() => {
		urlBlocklistCache.resetForTesting();
	});

	it('blocks a masked markdown link whose target a pattern covers', () => {
		urlBlocklistCache.addDomain('*shop*.onrender.com', true);
		expect(urlBlocklistCache.containsBannedLink('[open the store](https://shop-2.onrender.com)')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('[open the store](<https://www.shop.onrender.com/x>)')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('[https://docs.onrender.com](https://shop.onrender.com)')).toBe(true);
	});

	it('blocks autolinks and bare links', () => {
		urlBlocklistCache.addDomain('*shop*.onrender.com', false);
		expect(urlBlocklistCache.containsBannedLink('<https://myshop.onrender.com/path>')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('visit myshop.onrender.com today')).toBe(true);
	});

	it('normalizes the link target before matching', () => {
		urlBlocklistCache.addDomain('*shop*.onrender.com', false);
		const variants = [
			'https://user:pass@Shop.OnRender.com:8443/x',
			'https://login@shop.onrender.com',
			'https://shop.onrender.com./',
			'https://shop%2Eonrender%2Ecom/',
			'https://shop。onrender。com/',
			'https://shop-ü.onrender.com/',
		];
		for (const text of variants) {
			expect(urlBlocklistCache.containsBannedLink(text), text).toBe(true);
		}
	});

	it('leaves the bare suffix and unrelated hosts alone', () => {
		urlBlocklistCache.addDomain('*shop*.onrender.com', true);
		expect(urlBlocklistCache.containsBannedLink('https://onrender.com/docs')).toBe(false);
		expect(urlBlocklistCache.containsBannedLink('[docs](https://docs.onrender.com)')).toBe(false);
		expect(urlBlocklistCache.containsBannedLink('the shop is closed')).toBe(false);
	});

	it('covers subdomains of a domain entry only when it is flagged to', () => {
		urlBlocklistCache.addDomain('shop.example.com', true);
		urlBlocklistCache.addDomain('store.example.com', false);
		expect(urlBlocklistCache.containsBannedLink('https://www.shop.example.com')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('https://store.example.com')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('https://www.store.example.com')).toBe(false);
	});

	it('stops matching after removal', () => {
		urlBlocklistCache.addDomain('*shop*.onrender.com', true);
		urlBlocklistCache.removeDomain('*shop*.onrender.com');
		expect(urlBlocklistCache.containsBannedLink('https://shop.onrender.com')).toBe(false);
	});

	it('applies domain rules to a single URL', () => {
		urlBlocklistCache.addDomain('*shop*.onrender.com', false);
		expect(urlBlocklistCache.isUrlOrDomainBanned('https://shop.onrender.com/checkout')).toBe(true);
		expect(urlBlocklistCache.isUrlOrDomainBanned('https://docs.onrender.com/')).toBe(false);
	});
});
