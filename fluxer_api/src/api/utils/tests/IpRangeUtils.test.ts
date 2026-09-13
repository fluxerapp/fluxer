// SPDX-License-Identifier: AGPL-3.0-or-later

import {isValidIpOrRange, parseIpBanEntry, tryParseSingleIp} from '@app/api/utils/IpRangeUtils';
import {describe, expect, it} from 'vitest';

describe('parseIpBanEntry', () => {
	describe('single IPv4 addresses', () => {
		it('parses a simple IPv4 address', () => {
			const result = parseIpBanEntry('192.168.1.1');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('single');
			expect(result?.family).toBe('ipv4');
			expect(result?.canonical).toBe('192.168.1.1');
		});
		it('parses IPv4 address 0.0.0.0', () => {
			const result = parseIpBanEntry('0.0.0.0');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('single');
			expect(result?.canonical).toBe('0.0.0.0');
		});
		it('parses IPv4 address 255.255.255.255', () => {
			const result = parseIpBanEntry('255.255.255.255');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('single');
			expect(result?.canonical).toBe('255.255.255.255');
		});
		it('rejects IPv4 with leading zeros (octal ambiguity)', () => {
			const result = parseIpBanEntry('192.168.001.001');
			expect(result).toBeNull();
		});
	});
	describe('single IPv6 addresses', () => {
		it('parses a full IPv6 address', () => {
			const result = parseIpBanEntry('2001:0db8:85a3:0000:0000:8a2e:0370:7334');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('single');
			expect(result?.family).toBe('ipv6');
		});
		it('parses a compressed IPv6 address', () => {
			const result = parseIpBanEntry('2001:db8::1');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('single');
			expect(result?.family).toBe('ipv6');
		});
		it('parses IPv6 loopback address', () => {
			const result = parseIpBanEntry('::1');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('single');
			expect(result?.family).toBe('ipv6');
		});
		it('parses IPv6 unspecified address', () => {
			const result = parseIpBanEntry('::');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('single');
			expect(result?.family).toBe('ipv6');
		});
		it('parses IPv6 address with zone identifier stripped', () => {
			const result = parseIpBanEntry('fe80::1%eth0');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('single');
			expect(result?.family).toBe('ipv6');
		});
	});
	describe('IPv4-mapped IPv6 addresses', () => {
		it('parses hex IPv4-mapped IPv6 as IPv4', () => {
			const result = parseIpBanEntry('::ffff:7f00:1');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('single');
			expect(result?.family).toBe('ipv4');
			expect(result?.canonical).toBe('127.0.0.1');
		});
		it('parses dotted IPv4-mapped IPv6 as IPv4', () => {
			const result = parseIpBanEntry('::ffff:127.0.0.1');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('single');
			expect(result?.family).toBe('ipv4');
			expect(result?.canonical).toBe('127.0.0.1');
		});
	});
	describe('IPv4 CIDR ranges', () => {
		it('parses /32 (single host)', () => {
			const result = parseIpBanEntry('192.168.1.1/32');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
			if (result?.type === 'range') {
				expect(result.family).toBe('ipv4');
				expect(result.prefixLength).toBe(32);
				expect(result.start).toBe(result.end);
			}
		});
		it('parses /24 (256 hosts)', () => {
			const result = parseIpBanEntry('192.168.1.0/24');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
			if (result?.type === 'range') {
				expect(result.family).toBe('ipv4');
				expect(result.prefixLength).toBe(24);
				expect(result.end - result.start).toBe(255n);
			}
		});
		it('parses /16 (65536 hosts)', () => {
			const result = parseIpBanEntry('192.168.0.0/16');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
			if (result?.type === 'range') {
				expect(result.family).toBe('ipv4');
				expect(result.prefixLength).toBe(16);
				expect(result.end - result.start).toBe(65535n);
			}
		});
		it('parses /8 (class A)', () => {
			const result = parseIpBanEntry('10.0.0.0/8');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
			if (result?.type === 'range') {
				expect(result.prefixLength).toBe(8);
			}
		});
		it('parses /0 (all addresses)', () => {
			const result = parseIpBanEntry('0.0.0.0/0');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
			if (result?.type === 'range') {
				expect(result.prefixLength).toBe(0);
			}
		});
		it('normalizes network address in CIDR', () => {
			const result = parseIpBanEntry('192.168.1.100/24');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
			if (result?.type === 'range') {
				expect(result.canonical).toBe('192.168.1.0/24');
			}
		});
	});
	describe('IPv6 CIDR ranges', () => {
		it('parses /128 (single host)', () => {
			const result = parseIpBanEntry('2001:db8::1/128');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
			if (result?.type === 'range') {
				expect(result.family).toBe('ipv6');
				expect(result.prefixLength).toBe(128);
				expect(result.start).toBe(result.end);
			}
		});
		it('parses /64 (common subnet)', () => {
			const result = parseIpBanEntry('2001:db8::/64');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
			if (result?.type === 'range') {
				expect(result.family).toBe('ipv6');
				expect(result.prefixLength).toBe(64);
			}
		});
		it('parses /48 (site prefix)', () => {
			const result = parseIpBanEntry('2001:db8:abcd::/48');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
			if (result?.type === 'range') {
				expect(result.prefixLength).toBe(48);
			}
		});
		it('parses /0 (all addresses)', () => {
			const result = parseIpBanEntry('::/0');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
			if (result?.type === 'range') {
				expect(result.prefixLength).toBe(0);
			}
		});
	});
	describe('IPv4-mapped IPv6 CIDR ranges', () => {
		it('maps IPv4-mapped /96 to IPv4 /0', () => {
			const result = parseIpBanEntry('::ffff:0:0/96');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
			if (result?.type === 'range') {
				expect(result.family).toBe('ipv4');
				expect(result.prefixLength).toBe(0);
				expect(result.canonical).toBe('0.0.0.0/0');
			}
		});
		it('maps IPv4-mapped /120 to IPv4 /24', () => {
			const result = parseIpBanEntry('::ffff:192.0.2.0/120');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
			if (result?.type === 'range') {
				expect(result.family).toBe('ipv4');
				expect(result.prefixLength).toBe(24);
				expect(result.canonical).toBe('192.0.2.0/24');
			}
		});
	});
	describe('invalid inputs', () => {
		it('returns null for empty string', () => {
			expect(parseIpBanEntry('')).toBeNull();
		});
		it('returns null for whitespace only', () => {
			expect(parseIpBanEntry('   ')).toBeNull();
		});
		it('returns null for invalid IPv4', () => {
			expect(parseIpBanEntry('256.256.256.256')).toBeNull();
			expect(parseIpBanEntry('192.168.1')).toBeNull();
			expect(parseIpBanEntry('192.168.1.1.1')).toBeNull();
		});
		it('returns null for invalid CIDR prefix', () => {
			expect(parseIpBanEntry('192.168.1.0/33')).toBeNull();
			expect(parseIpBanEntry('192.168.1.0/-1')).toBeNull();
			expect(parseIpBanEntry('2001:db8::/129')).toBeNull();
		});
		it('returns null for random text', () => {
			expect(parseIpBanEntry('not-an-ip')).toBeNull();
			expect(parseIpBanEntry('hello world')).toBeNull();
		});
		it('returns null for missing prefix after slash', () => {
			expect(parseIpBanEntry('192.168.1.0/')).toBeNull();
		});
	});
	describe('edge cases', () => {
		it('handles whitespace around IP', () => {
			const result = parseIpBanEntry('  192.168.1.1  ');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('single');
		});
		it('handles whitespace around CIDR', () => {
			const result = parseIpBanEntry('  192.168.1.0/24  ');
			expect(result).not.toBeNull();
			expect(result?.type).toBe('range');
		});
		it('handles bracketed IPv6', () => {
			const result = parseIpBanEntry('[2001:db8::1]');
			expect(result).not.toBeNull();
			expect(result?.family).toBe('ipv6');
		});
	});
});

describe('tryParseSingleIp', () => {
	it('returns parsed result for single IPv4', () => {
		const result = tryParseSingleIp('192.168.1.1');
		expect(result).not.toBeNull();
		expect(result?.type).toBe('single');
	});
	it('returns parsed result for single IPv6', () => {
		const result = tryParseSingleIp('2001:db8::1');
		expect(result).not.toBeNull();
		expect(result?.type).toBe('single');
	});
	it('returns null for CIDR range', () => {
		expect(tryParseSingleIp('192.168.1.0/24')).toBeNull();
	});
	it('returns null for invalid input', () => {
		expect(tryParseSingleIp('invalid')).toBeNull();
	});
});

describe('isValidIpOrRange', () => {
	it('returns true for valid single IPv4', () => {
		expect(isValidIpOrRange('192.168.1.1')).toBe(true);
	});
	it('returns true for valid single IPv6', () => {
		expect(isValidIpOrRange('2001:db8::1')).toBe(true);
	});
	it('returns true for valid IPv4 CIDR', () => {
		expect(isValidIpOrRange('192.168.1.0/24')).toBe(true);
	});
	it('returns true for valid IPv6 CIDR', () => {
		expect(isValidIpOrRange('2001:db8::/32')).toBe(true);
	});
	it('returns false for invalid input', () => {
		expect(isValidIpOrRange('')).toBe(false);
		expect(isValidIpOrRange('invalid')).toBe(false);
		expect(isValidIpOrRange('256.256.256.256')).toBe(false);
	});
});
