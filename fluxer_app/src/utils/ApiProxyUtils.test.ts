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
import {isCustomInstanceUrl, isElectronApiProxyUrl, wrapUrlWithElectronApiProxy} from './ApiProxyUtils';

const ELECTRON_API_PROXY_BASE = 'http://127.0.0.1:21862/proxy';

const setElectronApiProxy = () => {
	Object.defineProperty(window, 'electron', {
		value: {
			getApiProxyUrl: () => ELECTRON_API_PROXY_BASE,
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

describe('ApiProxyUtils (Electron API proxy wrapping)', () => {
	test('detects custom instances outside the default list', () => {
		const url = 'https://self-hosted.example/api';
		expect(isCustomInstanceUrl(url)).toBe(true);
	});

	test('ignores default Fluxer hosts', () => {
		const url = 'https://api.fluxer.app/v1';
		expect(isCustomInstanceUrl(url)).toBe(false);
	});

	test('recognizes the electron proxy URL (including query params)', () => {
		setElectronApiProxy();

		const wrapped = `${ELECTRON_API_PROXY_BASE}?target=https://self-hosted.example/api`;
		expect(isElectronApiProxyUrl(ELECTRON_API_PROXY_BASE)).toBe(true);
		expect(isElectronApiProxyUrl(wrapped)).toBe(true);
		expect(isElectronApiProxyUrl('https://self-hosted.example/api')).toBe(false);
	});

	test('wraps custom hosts but skips already wrapped URLs', () => {
		setElectronApiProxy();

		const target = 'https://self-hosted.example/api';
		const wrapped = wrapUrlWithElectronApiProxy(target);
		const parsed = new URL(wrapped);
		expect(parsed.origin).toBe('http://127.0.0.1:21862');
		expect(parsed.pathname).toBe('/proxy');
		expect(parsed.searchParams.get('target')).toBe(target);

		const alreadyWrapped = wrapUrlWithElectronApiProxy(wrapped);
		expect(alreadyWrapped).toBe(wrapped);
	});
});
