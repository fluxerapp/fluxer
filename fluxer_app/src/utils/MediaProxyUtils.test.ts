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

import {afterEach, describe, expect, test} from 'vitest';
import {buildMediaProxyURL, stripMediaProxyParams} from './MediaProxyUtils';

const ELECTRON_MEDIA_PROXY_BASE = 'http://127.0.0.1:21867/media?token=test-token';

const setElectronMediaProxy = () => {
	Object.defineProperty(window, 'electron', {
		value: {
			getMediaProxyUrl: () => ELECTRON_MEDIA_PROXY_BASE,
		},
		writable: true,
		configurable: true,
	});
};

const clearElectron = () => {
	Object.defineProperty(window, 'electron', {
		value: undefined,
		writable: true,
		configurable: true,
	});
};

afterEach(() => {
	clearElectron();
});

describe('MediaProxyUtils (Electron media proxy wrapping)', () => {
	test('does not wrap when Electron API is missing', () => {
		const url = 'https://example.com/media.png';
		expect(buildMediaProxyURL(url)).toBe(url);
	});

	test('wraps custom-host media URLs when Electron media proxy is available', () => {
		setElectronMediaProxy();

		const target = 'https://example.com/media.png';
		const result = buildMediaProxyURL(target);

		const parsed = new URL(result);
		expect(parsed.origin).toBe('http://127.0.0.1:21867');
		expect(parsed.pathname).toBe('/media');
		expect(parsed.searchParams.get('target')).toBe(target);
	});

	test('does not wrap Fluxer-hosted URLs that are already allowed by the default CSP', () => {
		setElectronMediaProxy();

		const url = 'https://fluxerusercontent.com/media.png';
		expect(buildMediaProxyURL(url)).toBe(url);
	});

	test('appends params onto the wrapped target URL (not the wrapper URL)', () => {
		setElectronMediaProxy();

		const base = 'https://example.com/media.png';
		const first = buildMediaProxyURL(base, {width: 100});
		const second = buildMediaProxyURL(first, {format: 'webp'});

		const parsed = new URL(second);
		const target = parsed.searchParams.get('target');
		expect(target).toBeTruthy();

		const targetUrl = new URL(target ?? '');
		expect(targetUrl.searchParams.get('width')).toBe('100');
		expect(targetUrl.searchParams.get('format')).toBe('webp');
	});

	test('stripMediaProxyParams removes params from wrapped target URLs', () => {
		setElectronMediaProxy();

		const base = 'https://example.com/media.png';
		const proxied = buildMediaProxyURL(base, {width: 100, format: 'webp', quality: 'high', animated: true});
		const stripped = stripMediaProxyParams(proxied);

		const parsed = new URL(stripped);
		const target = parsed.searchParams.get('target');
		expect(target).toBeTruthy();

		const targetUrl = new URL(target ?? '');
		expect(targetUrl.searchParams.get('width')).toBeNull();
		expect(targetUrl.searchParams.get('format')).toBeNull();
		expect(targetUrl.searchParams.get('quality')).toBeNull();
		expect(targetUrl.searchParams.get('animated')).toBeNull();
	});
});
