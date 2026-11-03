// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildExternalMediaProxyPath} from '@pkgs/media_proxy_utils/src/ExternalMediaProxyPathCodec';
import {describe, expect, it} from 'vitest';

describe('buildExternalMediaProxyPath', () => {
	it.each([
		['https://example.com/deep/nested/path/to/file.jpeg', 'https/example.com/deep/nested/path/to/file.jpeg'],
		['https://example.com/spaced%20name.png', 'https/example.com/spaced%2520name.png'],
		[
			'https://example.com/unicode/%C3%A5%C3%A4%C3%B6.png',
			'https/example.com/unicode/%25C3%25A5%25C3%25A4%25C3%25B6.png',
		],
		[
			'https://example.com/file.png?redirect=https%3A%2F%2Fother.example%2Fx.png',
			'%3Fredirect%3Dhttps%253A%252F%252Fother.example%252Fx.png/https/example.com/file.png',
		],
	])('preserves encoder wire shape for %s', (url, path) => {
		expect(buildExternalMediaProxyPath(url)).toBe(path);
	});
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

	it('rejects a url it cannot parse', () => {
		expect(() => buildExternalMediaProxyPath('not a url')).toThrow();
	});
});
