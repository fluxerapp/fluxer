// SPDX-License-Identifier: AGPL-3.0-or-later

import {getConfig} from '@app/api/Config';
import {isIpBanExempt, resetIpBanExemptionsForTesting} from '@app/api/risk/IpBanExemptions';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

describe('isIpBanExempt', () => {
	let originalExemptIps: Array<string>;

	beforeEach(() => {
		const config = getConfig();
		originalExemptIps = config.ipBanExemptIps;
		config.ipBanExemptIps = ['198.51.100.7', '2001:db8:6::', '2001:db8:1200:1000::/56', '203.0.113.0/24'];
		resetIpBanExemptionsForTesting();
	});

	afterEach(() => {
		getConfig().ipBanExemptIps = originalExemptIps;
		resetIpBanExemptionsForTesting();
	});

	it('matches a bare IPv4 address exactly', () => {
		expect(isIpBanExempt('198.51.100.7')).toBe(true);
		expect(isIpBanExempt('198.51.100.8')).toBe(false);
	});

	it('matches a bare IPv6 address on its /64', () => {
		expect(isIpBanExempt('2001:db8:6::abcd')).toBe(true);
		expect(isIpBanExempt('2001:db8:7::1')).toBe(false);
	});

	it('matches every address inside a CIDR range', () => {
		expect(isIpBanExempt('2001:db8:1200:1000::1')).toBe(true);
		expect(isIpBanExempt('2001:db8:1200:10ff:ffff:ffff:ffff:ffff')).toBe(true);
		expect(isIpBanExempt('2001:db8:1200:1100::1')).toBe(false);
		expect(isIpBanExempt('2001:db8:1200:fff::1')).toBe(false);
		expect(isIpBanExempt('203.0.113.200')).toBe(true);
		expect(isIpBanExempt('::ffff:203.0.113.200')).toBe(true);
		expect(isIpBanExempt('203.0.114.1')).toBe(false);
	});

	it('does not match empty or unparsable input', () => {
		expect(isIpBanExempt(null)).toBe(false);
		expect(isIpBanExempt('')).toBe(false);
		expect(isIpBanExempt('not-an-ip')).toBe(false);
	});
});
