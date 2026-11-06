// SPDX-License-Identifier: AGPL-3.0-or-later

import {urlBlocklistCache} from '@app/api/middleware/UrlBlocklistCache';
import {canonicalizeUrl} from '@app/api/utils/UrlNormalizer';
import {afterEach, describe, expect, it} from 'vitest';

describe('url blocklist text matching', () => {
	afterEach(() => {
		urlBlocklistCache.resetForTesting();
	});

	it('matches blocked hosts written without a scheme', () => {
		urlBlocklistCache.addDomain('xn--mnchen-3ya.de', true);
		urlBlocklistCache.addDomain('127.0.0.1', true);
		urlBlocklistCache.addDomain('blocked.example', true);
		expect(urlBlocklistCache.containsBannedLink('see münchen.de now')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('see 127.0.0.1 now')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('see blocked。example now')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('see blocked.example. now')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('mail me at user@blocked.example')).toBe(false);
		expect(urlBlocklistCache.containsBannedLink('version 127.1 is out')).toBe(false);
		expect(urlBlocklistCache.containsBannedLink('see allowed.example and 10.0.0.1')).toBe(false);
	});

	it('matches every spelling of a blocked exact url', () => {
		urlBlocklistCache.addExactUrl(canonicalizeUrl('https://blocked.example/phish?x=1')!);
		urlBlocklistCache.addExactUrl(canonicalizeUrl('http://1.2.3.4:8080/path')!);
		for (const text of [
			'see https://blocked.example/phish?x=1 now',
			'see https://blocked.example./phish?x=1 now',
			'see https://anyone@blocked.example/phish?x=1 now',
			'see https://anyone:pw@BLOCKED.example./phish?x=1, now',
			'see http://1.2.3.4:8080/path now',
		]) {
			expect(urlBlocklistCache.containsBannedLink(text)).toBe(true);
		}
		expect(urlBlocklistCache.isUrlBanned('https://anyone@blocked.example./phish?x=1')).toBe(true);
		expect(urlBlocklistCache.containsBannedLink('see https://blocked.example/other now')).toBe(false);
	});

	it('scans long link-like text quickly', () => {
		urlBlocklistCache.addDomain('blocked.example', true);
		const started = performance.now();
		for (const text of [
			'a.'.repeat(100_000),
			'a'.repeat(200_000),
			`a.b/${','.repeat(200_000)}x`,
			'ü.'.repeat(100_000),
			`https://${'a.'.repeat(100_000)}x`,
		]) {
			expect(urlBlocklistCache.containsBannedLink(`${text}!`)).toBe(false);
		}
		expect(performance.now() - started).toBeLessThan(500);
	});
});
