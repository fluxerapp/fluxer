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

import {describe, expect, it} from 'vitest';
import {OAuth2RedirectURICreateType, OAuth2RedirectURIUpdateType} from './OAuth2RedirectURI';

const loopbackRedirects = [
	'https://example.com/callback',
	'https://example.com/callback?foo=bar',
	'http://localhost:3000/callback',
	'http://127.0.0.1/callback',
	'http://[::1]/callback',
	'http://foo.localhost/callback',
];

const deniedProtocols = [
	'javascript://example.com/%0Aalert(1)',
	'data://example.com/text',
	'file://example.com/etc/passwd',
	'vbscript://example.com/code',
	'ftp://example.com/file',
	'ws://example.com/socket',
	'wss://example.com/socket',
	'custom://example.com/path',
];

describe('OAuth2 redirect URI validation', () => {
	describe('create redirect URI type', () => {
		it('allows secure redirect URIs', () => {
			for (const redirect of loopbackRedirects) {
				const result = OAuth2RedirectURICreateType.safeParse(redirect);
				expect(result.success).toBe(true);
			}
		});

		it('rejects non-localhost http hosts', () => {
			const result = OAuth2RedirectURICreateType.safeParse('http://example.com/callback');
			expect(result.success).toBe(false);
		});

		for (const entry of deniedProtocols) {
			it(`rejects ${entry.split('://')[0]} protocols`, () => {
				const result = OAuth2RedirectURICreateType.safeParse(entry);
				expect(result.success).toBe(false);
			});
		}
	});

	describe('update redirect URI type', () => {
		it('allows http redirects for all hosts', () => {
			const result = OAuth2RedirectURIUpdateType.safeParse('http://example.com/callback');
			expect(result.success).toBe(true);
		});

		for (const redirect of loopbackRedirects) {
			it(`still allows ${redirect} redirects`, () => {
				const result = OAuth2RedirectURIUpdateType.safeParse(redirect);
				expect(result.success).toBe(true);
			});
		}

		for (const entry of deniedProtocols) {
			it(`rejects ${entry.split('://')[0]} protocols`, () => {
				const result = OAuth2RedirectURIUpdateType.safeParse(entry);
				expect(result.success).toBe(false);
			});
		}
	});
});
