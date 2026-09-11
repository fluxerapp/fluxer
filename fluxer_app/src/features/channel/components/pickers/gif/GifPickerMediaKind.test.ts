// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {resolvesToVideo} from './GifPickerMediaKind';

const OPAQUE_PROXY_SRC =
	'https://media.test/external/sig/v2/aHR0cHM6Ly9zdGF0aWMua2xpcHkuY29tL2lpLzhjLzZhL2E4LzVsU2U1SFBCLndlYm0';
const PLAIN_PROXY_SRC = 'https://media.test/external/sig/https/static.klipy.com/ii/8c/6a/a8/5lSe5HPB.webm';

describe('resolvesToVideo', () => {
	it('trusts a stored video content type when no url states its kind', () => {
		expect(resolvesToVideo('video/webm', OPAQUE_PROXY_SRC, OPAQUE_PROXY_SRC)).toBe(true);
	});

	it('trusts a stored image content type when no url states its kind', () => {
		expect(resolvesToVideo('image/gif', OPAQUE_PROXY_SRC, OPAQUE_PROXY_SRC)).toBe(false);
	});

	it('falls back to an image when nothing states the kind', () => {
		expect(resolvesToVideo('', OPAQUE_PROXY_SRC, OPAQUE_PROXY_SRC)).toBe(false);
	});

	it('reads the extension off a plain proxy path', () => {
		expect(resolvesToVideo('', PLAIN_PROXY_SRC, null)).toBe(true);
	});

	it('reads the extension off the media source when the proxy path is opaque', () => {
		expect(resolvesToVideo('', OPAQUE_PROXY_SRC, 'https://static.klipy.com/ii/8c/6a/a8/5lSe5HPB.webm')).toBe(true);
	});

	it('keeps images as images when the proxy path states its kind', () => {
		expect(resolvesToVideo('', 'https://media.test/external/sig/https/cdn.test/a.gif', null)).toBe(false);
	});

	it('ignores a query string when reading the extension', () => {
		expect(resolvesToVideo('', 'https://media.test/external/sig/https/cdn.test/a.mp4?width=200', null)).toBe(true);
	});
});
