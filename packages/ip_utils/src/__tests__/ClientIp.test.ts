// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	extractClientIp,
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
	it('ignores the header and uses the peer address when trust is disabled', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '192.168.1.1'},
		});
		expect(extractClientIp(request, {trustClientIpHeader: false, peerAddress: '203.0.113.7'})).toBe('203.0.113.7');
		expect(extractClientIp(request, {peerAddress: '203.0.113.7'})).toBe('203.0.113.7');
	});
	it('uses the peer address when trust is disabled and no header is sent', () => {
		const request = new Request('http://example.com');
		expect(extractClientIp(request, {trustClientIpHeader: false, peerAddress: '2001:db8::7'})).toBe('2001:db8::7');
	});
	it('unmaps an ipv4-mapped peer address', () => {
		const request = new Request('http://example.com');
		expect(extractClientIp(request, {peerAddress: '::ffff:203.0.113.7'})).toBe('203.0.113.7');
	});
	it('returns null when trust is disabled and there is no usable peer address', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '192.168.1.1'},
		});
		expect(extractClientIp(request)).toBeNull();
		expect(extractClientIp(request, {trustClientIpHeader: false, peerAddress: null})).toBeNull();
		expect(extractClientIp(request, {trustClientIpHeader: false, peerAddress: 'not-an-ip'})).toBeNull();
	});
	it('ignores the peer address when the header is trusted', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '198.51.100.4'},
		});
		expect(extractClientIp(request, {trustClientIpHeader: true, peerAddress: '10.0.0.2'})).toBe('198.51.100.4');
		expect(
			extractClientIp(new Request('http://example.com'), {trustClientIpHeader: true, peerAddress: '10.0.0.2'}),
		).toBeNull();
	});
	it('unmaps an ipv4-mapped header value', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '::ffff:198.51.100.4, 10.0.0.1'},
		});
		expect(extractClientIp(request, {trustClientIpHeader: true})).toBe('198.51.100.4');
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

describe('requireClientIp', () => {
	it('returns ip when present and trusted', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '192.168.1.1'},
		});
		expect(requireClientIp(request, {trustClientIpHeader: true})).toBe('192.168.1.1');
	});
	it('returns the peer address when the header is not trusted', () => {
		const request = new Request('http://example.com', {
			headers: {'X-Forwarded-For': '192.168.1.1'},
		});
		expect(requireClientIp(request, {trustClientIpHeader: false, peerAddress: '203.0.113.7'})).toBe('203.0.113.7');
	});
	it('throws typed error when missing', () => {
		const request = new Request('http://example.com');
		expect(() => requireClientIp(request, {trustClientIpHeader: true})).toThrow(MissingClientIpError);
		expect(() => requireClientIp(request, {trustClientIpHeader: true})).toThrow('Client IP is required');
		expect(() => requireClientIp(request, {trustClientIpHeader: false})).toThrow(MissingClientIpError);
	});
});
