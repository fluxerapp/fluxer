// SPDX-License-Identifier: AGPL-3.0-or-later

import {ipBanCache} from '@app/api/middleware/IpBanMiddleware';
import {beforeEach, describe, expect, it} from 'vitest';

beforeEach(() => {
	ipBanCache.resetCaches();
});

describe('IpBanCache', () => {
	it('blocks IPv4-mapped IPv6 when a single IPv4 address is banned', () => {
		ipBanCache.ban('127.0.0.1');
		expect(ipBanCache.isBanned('127.0.0.1')).toBe(true);
		expect(ipBanCache.isBanned('::ffff:7f00:1')).toBe(true);
	});
	it('blocks IPv4-mapped IPv6 when an IPv4 range is banned', () => {
		ipBanCache.ban('127.0.0.0/24');
		expect(ipBanCache.isBanned('127.0.0.5')).toBe(true);
		expect(ipBanCache.isBanned('::ffff:7f00:5')).toBe(true);
	});
	it('blocks IPv4 clients when the IPv4-mapped IPv6 range is banned', () => {
		ipBanCache.ban('::ffff:0:0/96');
		expect(ipBanCache.isBanned('::ffff:127.0.0.1')).toBe(true);
		expect(ipBanCache.isBanned('127.0.0.1')).toBe(true);
	});
	it('blocks rotated IPv6 privacy addresses from the same /64 when a single IPv6 address is banned', () => {
		ipBanCache.ban('2a01:e0a:d10:95b0:8f54:410e:f290:1c66');
		expect(ipBanCache.isBanned('2a01:e0a:d10:95b0:01e4:53a8:d0dd:7733')).toBe(true);
	});
	it('keeps IPv4 bans exact rather than widening to /24', () => {
		ipBanCache.ban('203.0.113.42');
		expect(ipBanCache.isBanned('203.0.113.42')).toBe(true);
		expect(ipBanCache.isBanned('203.0.113.43')).toBe(false);
	});
	it('reports temporary global IP bans distinctly', () => {
		ipBanCache.banTemp('203.0.113.50', 86400);
		const match = ipBanCache.getMatch('203.0.113.50');
		expect(match?.kind).toBe('temporary_24h');
		expect(match?.expiresAt).toBeInstanceOf(Date);
	});
	it('reports permanent global IP bans distinctly', () => {
		ipBanCache.ban('203.0.113.51');
		const match = ipBanCache.getMatch('203.0.113.51');
		expect(match?.kind).toBe('permanent');
		expect(match?.expiresAt).toBe(null);
	});
});
