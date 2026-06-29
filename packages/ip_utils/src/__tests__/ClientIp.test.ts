// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	extractClientIp,
	extractClientIpDetails,
	extractClientIpDetailsFromHeaders,
	extractClientIpFromHeaders,
	MissingClientIpError,
	requireClientIp,
	resolveClientIpHeaderName,
} from '@fluxer/ip_utils/src/ClientIp';
import {describe, expect, it} from 'vitest';

describe('extractClientIp', () => {
	it('extracts ip from configured header when trusted', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Real-Ip': '192.168.1.1'},
		});
		expect(extractClientIp(request, {trustClientIpHeader: true, clientIpHeaderName: 'x-real-ip'})).toBe('192.168.1.1');
	});
	it('prioritises configured header', () => {
		const request = new Request('http://example.com', {
			headers: {
				'X-Real-Ip': '203.0.113.40',
				'X-Forwarded-For': '203.0.113.60',
			},
		});
		expect(extractClientIp(request, {trustClientIpHeader: true, clientIpHeaderName: 'x-real-ip'})).toBe('203.0.113.40');
	});
	it('uses default header name (x-forwarded-for) when clientIpHeaderName is not specified', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '203.0.113.50'},
		});
		expect(extractClientIp(request, {trustClientIpHeader: true})).toBe('203.0.113.50');
	});
	it('returns null when configured header is missing', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '192.168.1.3'},
		});
		expect(extractClientIp(request, {trustClientIpHeader: true, clientIpHeaderName: 'x-real-ip'})).toBeNull();
	});
	it('extracts first hop from x-forwarded-for', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '192.168.1.1, 203.0.113.10'},
		});
		expect(extractClientIp(request, {trustClientIpHeader: true})).toBe('192.168.1.1');
	});
	it('extracts first hop from any configured header with comma-separated values', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '192.168.1.1, 203.0.113.10'},
		});
		expect(extractClientIp(request, {trustClientIpHeader: true, clientIpHeaderName: 'x-forwarded-for'})).toBe(
			'192.168.1.1',
		);
	});
	it('returns null when trust is disabled', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '192.168.1.1'},
		});
		expect(extractClientIp(request, {trustClientIpHeader: false})).toBeNull();
		expect(extractClientIp(request)).toBeNull();
	});
	it('returns null when no options are provided', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '192.168.1.1'},
		});
		expect(extractClientIp(request)).toBeNull();
	});
	it('returns null for missing or invalid headers', () => {
		expect(extractClientIp(new Request('http://example.com'), {trustClientIpHeader: true})).toBeNull();
		expect(
			extractClientIp(
				new Request('http://example.com', {
					headers: {'X-Forwarded-For': ''},
				}),
				{trustClientIpHeader: true},
			),
		).toBeNull();
		expect(
			extractClientIp(
				new Request('http://example.com', {
					headers: {'X-Forwarded-For': 'not-an-ip'},
				}),
				{trustClientIpHeader: true},
			),
		).toBeNull();
		expect(
			extractClientIp(
				new Request('http://example.com', {
					headers: {'X-Forwarded-For': 'not-an-ip, 203.0.113.10'},
				}),
				{trustClientIpHeader: true},
			),
		).toBeNull();
	});
	it('normalizes bracketed and zoned ipv6', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '[fe80::1%eth0]'},
		});
		expect(extractClientIp(request, {trustClientIpHeader: true})).toBe('fe80::1');
	});
});

describe('resolveClientIpHeaderName', () => {
	it('defaults to x-forwarded-for when no header configured', () => {
		expect(resolveClientIpHeaderName()).toBe('x-forwarded-for');
	});
	it('returns the configured header name normalised to lowercase', () => {
		expect(resolveClientIpHeaderName('x-client-ip')).toBe('x-client-ip');
		expect(resolveClientIpHeaderName('X-Real-Ip')).toBe('x-real-ip');
	});
});

describe('extractClientIpDetails', () => {
	it('returns extracted ip and source', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '203.0.113.50'},
		});
		expect(extractClientIpDetails(request, {trustClientIpHeader: true})).toEqual({
			ip: '203.0.113.50',
			source: 'client-ip-header',
			ipVersion: 'ipv4',
		});
	});
	it('returns null when trust is disabled', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '203.0.113.50'},
		});
		expect(extractClientIpDetails(request, {trustClientIpHeader: false})).toBeNull();
	});
});

describe('extractClientIpFromHeaders', () => {
	it('extracts from node-style headers with configured header name', () => {
		const headers = {
			'x-real-ip': '192.168.1.1',
		};
		expect(extractClientIpFromHeaders(headers, {trustClientIpHeader: true, clientIpHeaderName: 'x-real-ip'})).toBe(
			'192.168.1.1',
		);
	});
	it('supports case-insensitive keys and array values', () => {
		const headers = {
			'X-FORWARDED-FOR': ['203.0.113.50'],
		};
		expect(extractClientIpFromHeaders(headers, {trustClientIpHeader: true})).toBe('203.0.113.50');
		expect(extractClientIpDetailsFromHeaders(headers, {trustClientIpHeader: true})).toEqual({
			ip: '203.0.113.50',
			source: 'client-ip-header',
			ipVersion: 'ipv4',
		});
	});
	it('returns null when trust is disabled', () => {
		expect(extractClientIpFromHeaders({'x-forwarded-for': '192.168.1.1'})).toBeNull();
	});
	it('returns null for invalid inputs', () => {
		expect(extractClientIpFromHeaders({}, {trustClientIpHeader: true})).toBeNull();
		expect(extractClientIpFromHeaders({'x-forwarded-for': 'not-an-ip'}, {trustClientIpHeader: true})).toBeNull();
	});
});

describe('requireClientIp', () => {
	it('returns ip when present and trusted', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '192.168.1.1'},
		});
		expect(requireClientIp(request, {trustClientIpHeader: true})).toBe('192.168.1.1');
	});
	it('throws typed error when missing', () => {
		const request = new Request('http://example.com');
		expect(() => requireClientIp(request, {trustClientIpHeader: true})).toThrow(MissingClientIpError);
		expect(() => requireClientIp(request, {trustClientIpHeader: true})).toThrow('Client IP header is required');
	});
});
