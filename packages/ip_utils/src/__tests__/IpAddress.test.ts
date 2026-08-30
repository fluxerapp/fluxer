// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getIpNetworkKey,
	getSameIpDecisionKey,
	getSubnet,
	isLoopbackIpAddress,
	isPublicIpAddress,
	isSameIpDecisionMatch,
	isValidIp,
	maskIpForDisplay,
	normalizeIpString,
	parseIpAddress,
} from '@fluxer/ip_utils/src/IpAddress';
import {describe, expect, it} from 'vitest';

describe('normalizeIpString', () => {
	describe('ipv4 addresses', () => {
		it('normalizes standard values', () => {
			expect(normalizeIpString('192.168.1.1')).toBe('192.168.1.1');
			expect(normalizeIpString('10.0.0.1')).toBe('10.0.0.1');
			expect(normalizeIpString('172.16.0.1')).toBe('172.16.0.1');
		});
		it('trims whitespace', () => {
			expect(normalizeIpString('  192.168.1.1  ')).toBe('192.168.1.1');
			expect(normalizeIpString('\t10.0.0.1\n')).toBe('10.0.0.1');
		});
	});
	describe('ipv6 addresses', () => {
		it('normalizes standard values', () => {
			expect(normalizeIpString('2001:db8::1')).toBe('2001:db8::1');
			expect(normalizeIpString('::1')).toBe('::1');
			expect(normalizeIpString('::')).toBe('::');
		});
		it('strips brackets and zone identifiers', () => {
			expect(normalizeIpString('[2001:db8::1]')).toBe('2001:db8::1');
			expect(normalizeIpString('fe80::1%eth0')).toBe('fe80::1');
			expect(normalizeIpString('[fe80::1%en0]')).toBe('fe80::1');
		});
		it('normalizes case and compact form', () => {
			expect(normalizeIpString('2001:DB8::1')).toBe('2001:db8::1');
			expect(normalizeIpString('2001:0db8:0000:0000:0000:0000:0000:0001')).toBe('2001:db8::1');
		});
		it('normalizes ipv4-mapped ipv6 addresses', () => {
			expect(normalizeIpString('::ffff:192.0.2.1')).toBe('::ffff:c000:201');
		});
	});
	describe('edge cases', () => {
		it('handles empty and invalid values', () => {
			expect(normalizeIpString('')).toBe('');
			expect(normalizeIpString('   ')).toBe('');
			expect(normalizeIpString('not-an-ip')).toBe('not-an-ip');
		});
		it('does not strip brackets from host-port formats', () => {
			expect(normalizeIpString('[2001:db8::1]:8080')).toBe('[2001:db8::1]:8080');
		});
		it('returns input value when url parsing fails', () => {
			const OriginalURL = globalThis.URL;
			globalThis.URL = class extends OriginalURL {
				constructor(input: string | URL, base?: string | URL) {
					if (typeof input === 'string' && input.includes('[2001:db8::ffff]')) {
						throw new Error('Simulated URL parsing failure');
					}
					super(input, base);
				}
			} as typeof URL;
			try {
				expect(normalizeIpString('2001:db8::ffff')).toBe('2001:db8::ffff');
			} finally {
				globalThis.URL = OriginalURL;
			}
		});
	});
});

describe('parseIpAddress', () => {
	it('parses valid ip values', () => {
		expect(parseIpAddress('192.168.1.1')).toEqual({
			raw: '192.168.1.1',
			normalized: '192.168.1.1',
			family: 'ipv4',
		});
		expect(parseIpAddress('[2001:DB8::1]')).toEqual({
			raw: '[2001:DB8::1]',
			normalized: '2001:db8::1',
			family: 'ipv6',
		});
	});
	it('returns null for invalid values', () => {
		expect(parseIpAddress('not-an-ip')).toBeNull();
		expect(parseIpAddress('')).toBeNull();
	});
});

describe('isValidIp', () => {
	it('accepts valid values', () => {
		expect(isValidIp('192.168.1.1')).toBe(true);
		expect(isValidIp('[::1]')).toBe(true);
		expect(isValidIp('fe80::1%eth0')).toBe(true);
	});
	it('rejects invalid values', () => {
		expect(isValidIp('256.256.256.256')).toBe(false);
		expect(isValidIp('gggg::1')).toBe(false);
		expect(isValidIp('example.com')).toBe(false);
	});
});

describe('maskIpForDisplay', () => {
	it('masks the host segment of IPv4 addresses', () => {
		expect(maskIpForDisplay('203.0.113.42')).toBe('203.0.113.x');
	});
	it('shows only the leading prefix for IPv6 addresses', () => {
		expect(maskIpForDisplay('2a01:e0a:d10:95b0:8f54:410e:f290:1c66')).toBe('2a01:e0a:d10:95b0::/64');
	});
	it('treats IPv4-mapped IPv6 addresses as IPv4 addresses', () => {
		expect(maskIpForDisplay('::ffff:192.0.2.123')).toBe('192.0.2.x');
	});
	it('returns null for invalid values', () => {
		expect(maskIpForDisplay('not-an-ip')).toBeNull();
	});
});

describe('isLoopbackIpAddress', () => {
	it('accepts IPv4 loopback addresses', () => {
		expect(isLoopbackIpAddress('127.0.0.1')).toBe(true);
		expect(isLoopbackIpAddress('127.0.0.2')).toBe(true);
		expect(isLoopbackIpAddress('127.255.255.254')).toBe(true);
	});
	it('accepts IPv6 loopback and its IPv4-mapped form', () => {
		expect(isLoopbackIpAddress('::1')).toBe(true);
		expect(isLoopbackIpAddress('[::1]')).toBe(true);
		expect(isLoopbackIpAddress('0:0:0:0:0:0:0:1')).toBe(true);
		expect(isLoopbackIpAddress('::ffff:127.0.0.1')).toBe(true);
		expect(isLoopbackIpAddress('::ffff:7f00:1')).toBe(true);
	});
	it('rejects non-loopback addresses', () => {
		expect(isLoopbackIpAddress('8.8.8.8')).toBe(false);
		expect(isLoopbackIpAddress('10.0.0.5')).toBe(false);
		expect(isLoopbackIpAddress('172.18.0.4')).toBe(false);
		expect(isLoopbackIpAddress('192.168.1.1')).toBe(false);
		expect(isLoopbackIpAddress('128.0.0.1')).toBe(false);
		expect(isLoopbackIpAddress('126.255.255.255')).toBe(false);
		expect(isLoopbackIpAddress('::2')).toBe(false);
		expect(isLoopbackIpAddress('fe80::1')).toBe(false);
		expect(isLoopbackIpAddress('::ffff:8.8.8.8')).toBe(false);
	});
	it('rejects invalid values', () => {
		expect(isLoopbackIpAddress('not-an-ip')).toBe(false);
		expect(isLoopbackIpAddress('')).toBe(false);
	});
});

describe('isPublicIpAddress', () => {
	it('accepts public unicast addresses', () => {
		expect(isPublicIpAddress('8.8.8.8')).toBe(true);
		expect(isPublicIpAddress('2606:4700:4700::1111')).toBe(true);
	});
	it('rejects loopback, private, and reserved IPv4 addresses', () => {
		expect(isPublicIpAddress('127.0.0.1')).toBe(false);
		expect(isPublicIpAddress('10.0.0.5')).toBe(false);
		expect(isPublicIpAddress('172.16.0.5')).toBe(false);
		expect(isPublicIpAddress('192.168.0.5')).toBe(false);
		expect(isPublicIpAddress('100.64.0.5')).toBe(false);
		expect(isPublicIpAddress('198.51.100.42')).toBe(false);
	});
	it('rejects IPv4-mapped loopback and reserved IPv6 addresses', () => {
		expect(isPublicIpAddress('::ffff:127.0.0.1')).toBe(false);
		expect(isPublicIpAddress('::1')).toBe(false);
		expect(isPublicIpAddress('::2')).toBe(false);
		expect(isPublicIpAddress('fc00::1')).toBe(false);
		expect(isPublicIpAddress('fe80::1')).toBe(false);
		expect(isPublicIpAddress('2001:db8::1')).toBe(false);
	});
	it('rejects invalid values', () => {
		expect(isPublicIpAddress('not-an-ip')).toBe(false);
	});
});

describe('getIpNetworkKey', () => {
	it('keeps IPv4 trust keys exact for auth lookups', () => {
		expect(
			getIpNetworkKey('203.0.113.42', {
				ipv4PrefixLength: 'exact',
				ipv6PrefixLength: 64,
			}),
		).toBe('203.0.113.42');
	});
	it('normalizes IPv6 trust keys to /64 prefixes', () => {
		expect(
			getIpNetworkKey('2a01:e0a:d10:95b0:8f54:410e:f290:1c66', {
				ipv4PrefixLength: 'exact',
				ipv6PrefixLength: 64,
			}),
		).toBe('2a01:e0a:d10:95b0::/64');
	});
	it('treats IPv4-mapped IPv6 addresses using IPv4 rules', () => {
		expect(
			getIpNetworkKey('::ffff:192.0.2.1', {
				ipv4PrefixLength: 'exact',
				ipv6PrefixLength: 64,
			}),
		).toBe('192.0.2.1');
		expect(
			getIpNetworkKey('::ffff:192.0.2.1', {
				ipv4PrefixLength: 24,
				ipv6PrefixLength: 64,
			}),
		).toBe('192.0.2.0/24');
	});
	it('returns null for invalid values', () => {
		expect(
			getIpNetworkKey('not-an-ip', {
				ipv4PrefixLength: 'exact',
				ipv6PrefixLength: 64,
			}),
		).toBeNull();
	});
});

describe('getSubnet', () => {
	it('returns /24 for IPv4 and /48 for IPv6', () => {
		expect(getSubnet('203.0.113.42')).toBe('203.0.113.0/24');
		expect(getSubnet('2a01:e0a:d10:95b0:8f54:410e:f290:1c66')).toBe('2a01:e0a:d10::/48');
	});
});

describe('getSameIpDecisionKey', () => {
	it('uses exact IPv4 and /64 IPv6 matching', () => {
		expect(getSameIpDecisionKey('203.0.113.42')).toBe('203.0.113.42');
		expect(getSameIpDecisionKey('2a01:e0a:d10:95b0:8f54:410e:f290:1c66')).toBe('2a01:e0a:d10:95b0::/64');
	});
});

describe('isSameIpDecisionMatch', () => {
	it('matches IPv6 addresses that share a /64', () => {
		expect(isSameIpDecisionMatch('2a01:e0a:d10:95b0:8f54:410e:f290:1c66', '2a01:e0a:d10:95b0:1e4:53a8:d0dd:7733')).toBe(
			true,
		);
	});
	it('keeps IPv4 matching exact', () => {
		expect(isSameIpDecisionMatch('203.0.113.10', '203.0.113.10')).toBe(true);
		expect(isSameIpDecisionMatch('203.0.113.10', '203.0.113.11')).toBe(false);
	});
	it('falls back to normalized exact comparison for invalid values', () => {
		expect(isSameIpDecisionMatch(' not-an-ip ', 'not-an-ip')).toBe(true);
	});
});
