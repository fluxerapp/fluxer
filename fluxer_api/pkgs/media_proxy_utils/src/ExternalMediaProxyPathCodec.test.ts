// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildExternalMediaProxyPath,
	buildOpaqueExternalMediaProxyPath,
	reconstructOriginalUrl,
} from '@pkgs/media_proxy_utils/src/ExternalMediaProxyPathCodec';
import {describe, expect, it} from 'vitest';

const ROUND_TRIP_URLS = [
	'https://static.klipy.com/ii/c8/28/HkAKKCzZ.webp',
	'https://static.klipy.com/ii/HkAKKCzZ.webp?v=query_param&goes=here',
	'https://example.com:8443/a.png',
	'https://avatars.githubusercontent.com/u/241303489?v=4',
	'http://example.com/plain.gif',
	'https://example.com/deep/nested/path/to/file.jpeg',
	'https://example.com/file.png?a=1&b=2&c=3',
	'https://example.com/spaced%20name.png',
	'https://example.com/unicode/%C3%A5%C3%A4%C3%B6.png',
	'https://example.com/file.png?redirect=https%3A%2F%2Fother.example%2Fx.png',
	'https://sub.domain.example.co.uk/a/b.webp',
];

describe('buildExternalMediaProxyPath', () => {
	it('emits the plain path shape with the extension last', () => {
		expect(buildExternalMediaProxyPath('https://static.klipy.com/ii/c8/28/HkAKKCzZ.webp')).toBe(
			'https/static.klipy.com/ii/c8/28/HkAKKCzZ.webp',
		);
	});

	it('puts an encoded query, leading question mark included, ahead of the protocol', () => {
		expect(buildExternalMediaProxyPath('https://static.klipy.com/ii/HkAKKCzZ.webp?v=query_param&goes=here')).toBe(
			'%3Fv%3Dquery_param%26goes%3Dhere/https/static.klipy.com/ii/HkAKKCzZ.webp',
		);
	});

	it('keeps a non default port on the host segment', () => {
		expect(buildExternalMediaProxyPath('https://example.com:8443/a.png')).toBe('https/example.com:8443/a.png');
	});

	it('preserves the http scheme', () => {
		expect(buildExternalMediaProxyPath('http://example.com/a.gif')).toBe('http/example.com/a.gif');
	});

	it('handles a root url with no path', () => {
		expect(buildExternalMediaProxyPath('https://example.com/')).toBe('https/example.com');
	});

	it('never emits the v2 prefix any more', () => {
		for (const url of ROUND_TRIP_URLS) {
			expect(buildExternalMediaProxyPath(url).startsWith('v2/')).toBe(false);
		}
	});

	it('ends in the source file extension so extension based cdn caching applies', () => {
		for (const [url, ext] of [
			['https://example.com/a.webp', '.webp'],
			['https://example.com/a.png?x=1', '.png'],
			['https://example.com/a/b/c.jpeg', '.jpeg'],
		] as const) {
			expect(buildExternalMediaProxyPath(url).endsWith(ext)).toBe(true);
		}
	});

	it('rejects a url it cannot parse', () => {
		expect(() => buildExternalMediaProxyPath('not a url')).toThrow();
	});
});

describe('reconstructOriginalUrl', () => {
	it('round trips every supported shape', () => {
		for (const url of ROUND_TRIP_URLS) {
			expect(reconstructOriginalUrl(buildExternalMediaProxyPath(url))).toBe(url);
		}
	});

	it('decodes an externally produced path verbatim', () => {
		expect(
			reconstructOriginalUrl('%3Fv%3Dquery_param%26goes%3Dhere/https/static.klipy.com/ii/c8/28/HkAKKCzZ.webp'),
		).toBe('https://static.klipy.com/ii/c8/28/HkAKKCzZ.webp?v=query_param&goes=here');
	});

	it('does not double the question mark when the query segment carries one', () => {
		const decoded = reconstructOriginalUrl('%3Fa%3D1/https/example.com/x.png');
		expect(decoded).toBe('https://example.com/x.png?a=1');
		expect(decoded).not.toContain('??');
	});

	it('still accepts a query segment without a leading question mark', () => {
		expect(reconstructOriginalUrl('a%3D1/https/example.com/x.png')).toBe('https://example.com/x.png?a=1');
	});

	it('still decodes v2 paths so links already sent keep working', () => {
		expect(reconstructOriginalUrl(buildOpaqueExternalMediaProxyPath('https://example.com/a.png?x=1'))).toBe(
			'https://example.com/a.png?x=1',
		);
	});

	it('rejects a path that has no host after the protocol', () => {
		expect(() => reconstructOriginalUrl('https')).toThrow();
	});

	it('rejects an empty v2 payload', () => {
		expect(() => reconstructOriginalUrl('v2/')).toThrow();
	});
});
