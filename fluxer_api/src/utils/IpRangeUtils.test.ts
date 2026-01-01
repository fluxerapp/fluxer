/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {describe, expect, test} from 'vitest';
import {isValidIpOrRange, parseIpBanEntry, tryParseSingleIp} from './IpRangeUtils';

const IPv4Value = BigInt('0xc0a80001');
const IPv4RangeStart = BigInt('0xc0a80100');
const IPv4RangeEnd = BigInt('0xc0a801ff');
const IPv6Value = BigInt('0x20010db8000000000000000000000001');
const IPv6RangeStart = BigInt('0x20010db8000000000000000000000000');
const IPv6HostSpan = (1n << 96n) - 1n;

describe('IpRangeUtils', () => {
	test('parse single IPv4 address', () => {
		const parsed = parseIpBanEntry(' 192.168.0.1 ');
		expect(parsed).not.toBeNull();
		expect(parsed?.type).toBe('single');
		if (parsed?.type !== 'single') return;
		expect(parsed?.family).toBe('ipv4');
		expect(parsed?.canonical).toBe('192.168.0.1');
		expect(parsed.value).toBe(IPv4Value);
	});

	test('parse single IPv6 address with zone', () => {
		const parsed = tryParseSingleIp('2001:db8::1%eth0');
		expect(parsed).not.toBeNull();
		expect(parsed?.canonical).toBe('2001:0db8:0000:0000:0000:0000:0000:0001');
		expect(parsed?.value).toBe(IPv6Value);
	});

	test('parse IPv4 range and trim host bits', () => {
		const parsed = parseIpBanEntry('192.168.1.5/24');
		expect(parsed?.type).toBe('range');
		if (parsed?.type !== 'range') return;
		expect(parsed.family).toBe('ipv4');
		expect(parsed.canonical).toBe('192.168.1.0/24');
		expect(parsed.prefixLength).toBe(24);
		expect(parsed.start).toBe(IPv4RangeStart);
		expect(parsed.end).toBe(IPv4RangeEnd);
	});

	test('parse IPv6 range and compute host span', () => {
		const parsed = parseIpBanEntry('2001:db8::/32');
		expect(parsed?.type).toBe('range');
		if (parsed?.type !== 'range') return;
		expect(parsed.family).toBe('ipv6');
		expect(parsed.canonical).toBe('2001:0db8:0000:0000:0000:0000:0000:0000/32');
		expect(parsed.prefixLength).toBe(32);
		expect(parsed.start).toBe(IPv6RangeStart);
		expect(parsed.end - parsed.start).toBe(IPv6HostSpan);
	});

	test('invalid input returns null and isValidIpOrRange rejects it', () => {
		expect(parseIpBanEntry('300.1.1.1')).toBeNull();
		expect(parseIpBanEntry('10.0.0.0/33')).toBeNull();
		expect(isValidIpOrRange('10.0.0.0/33')).toBe(false);
		expect(isValidIpOrRange('something')).toBe(false);
	});
});
