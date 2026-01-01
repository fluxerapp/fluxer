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

import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import {extractURLs, idnaEncodeURL, isFluxerAppExcludedURL, isValidURL} from './UnfurlerUtils';

const loggerMocks = vi.hoisted(() => ({
	loggerErrorMock: vi.fn<(context: Record<string, unknown>, message: string) => void>(),
}));

const configMock = vi.hoisted(() => ({
	Config: {
		endpoints: {
			webApp: 'https://web.fluxer.app',
		},
		hosts: {
			invite: 'fluxer.gg',
			gift: 'fluxer.gift',
			marketing: 'fluxer.app',
			unfurlIgnored: [],
		},
	},
}));

vi.mock('~/Logger', () => ({
	Logger: {
		error: loggerMocks.loggerErrorMock,
	},
}));

vi.mock('~/Config', () => configMock);

const {loggerErrorMock} = loggerMocks;

describe('UnfurlerUtils', () => {
	beforeEach(() => {
		vi.clearAllMocks();
	});

	afterEach(() => {
		vi.clearAllMocks();
	});

	describe('idnaEncodeURL', () => {
		it('should encode international domain names', () => {
			const url = 'https://测试.example.com/path';
			const result = idnaEncodeURL(url);

			expect(result).toContain('xn--');
			expect(result).toMatch(/^https:\/\/.*example\.com\/path$/);
		});

		it('should handle regular ASCII URLs', () => {
			const url = 'https://example.com/path?query=value';
			const result = idnaEncodeURL(url);

			expect(result).toBe('https://example.com/path?query=value');
		});

		it('should remove username and password from URLs', () => {
			const url = 'https://user:pass@example.com/path';
			const result = idnaEncodeURL(url);

			expect(result).toBe('https://example.com/path');
			expect(result).not.toContain('user');
			expect(result).not.toContain('pass');
		});

		it('should convert hostname to lowercase', () => {
			const url = 'https://EXAMPLE.COM/Path';
			const result = idnaEncodeURL(url);

			expect(result).toBe('https://example.com/Path');
		});

		it('should handle invalid URLs gracefully', () => {
			const invalidUrl = 'not-a-url';
			const result = idnaEncodeURL(invalidUrl);

			expect(result).toBe('');
			expect(loggerErrorMock).toHaveBeenCalled();
			const call = loggerErrorMock.mock.calls.at(-1);
			expect(call).toBeDefined();
			if (!call) return;
			const [context, message] = call;
			expect(message).toBe('Failed to encode URL');
			expect(context).toHaveProperty('error');
			expect((context as {error: unknown}).error).toBeInstanceOf(Error);
		});

		it('should handle URLs with ports', () => {
			const url = 'https://example.com:8080/path';
			const result = idnaEncodeURL(url);

			expect(result).toBe('https://example.com:8080/path');
		});

		it('should handle URLs with fragments', () => {
			const url = 'https://example.com/path#fragment';
			const result = idnaEncodeURL(url);

			expect(result).toBe('https://example.com/path#fragment');
		});
	});

	describe('isValidURL', () => {
		it('should return true for valid HTTP URLs', () => {
			expect(isValidURL('http://example.com')).toBe(true);
			expect(isValidURL('http://example.com/path')).toBe(true);
			expect(isValidURL('http://localhost:3000')).toBe(true);
		});

		it('should return true for valid HTTPS URLs', () => {
			expect(isValidURL('https://example.com')).toBe(true);
			expect(isValidURL('https://example.com/path?query=value')).toBe(true);
			expect(isValidURL('https://subdomain.example.com')).toBe(true);
		});

		it('should return false for non-HTTP/HTTPS protocols', () => {
			expect(isValidURL('ftp://example.com')).toBe(false);
			expect(isValidURL('file:///path/to/file')).toBe(false);
			expect(isValidURL('javascript:alert("xss")')).toBe(false);
			expect(isValidURL('data:text/plain;base64,SGVsbG8=')).toBe(false);
		});

		it('should return false for invalid URLs', () => {
			expect(isValidURL('not-a-url')).toBe(false);
			expect(isValidURL('')).toBe(false);
			expect(isValidURL('example.com')).toBe(false);
			expect(isValidURL('://example.com')).toBe(false);
		});

		it('should handle malformed URLs', () => {
			expect(isValidURL('http://')).toBe(false);
			expect(isValidURL('https://')).toBe(false);
			expect(isValidURL('http://.')).toBe(true);
		});
	});

	describe('isFluxerAppExcludedURL', () => {
		it('should return true for invite shortlink domain', () => {
			expect(isFluxerAppExcludedURL('https://fluxer.gg/abc123')).toBe(true);
			expect(isFluxerAppExcludedURL('https://fluxer.gg/')).toBe(true);
			expect(isFluxerAppExcludedURL('http://fluxer.gg/test')).toBe(true);
		});

		it('should return true for gift shortlink domain', () => {
			expect(isFluxerAppExcludedURL('https://fluxer.gift/xyz789')).toBe(true);
			expect(isFluxerAppExcludedURL('https://fluxer.gift/')).toBe(true);
			expect(isFluxerAppExcludedURL('http://fluxer.gift/test')).toBe(true);
		});

		it('should return true for all web app URLs', () => {
			expect(isFluxerAppExcludedURL('https://web.fluxer.app/')).toBe(true);
			expect(isFluxerAppExcludedURL('https://web.fluxer.app/login')).toBe(true);
			expect(isFluxerAppExcludedURL('https://web.fluxer.app/channels/@me')).toBe(true);
			expect(isFluxerAppExcludedURL('https://web.fluxer.app/any/path')).toBe(true);
		});

		it('should return false for marketing site URLs', () => {
			expect(isFluxerAppExcludedURL('https://fluxer.app/')).toBe(false);
			expect(isFluxerAppExcludedURL('https://fluxer.app/about')).toBe(false);
			expect(isFluxerAppExcludedURL('https://fluxer.app/blog')).toBe(false);
			expect(isFluxerAppExcludedURL('https://fluxer.app/docs')).toBe(false);
		});

		it('should treat marketing channel paths as Fluxer app URLs', () => {
			expect(isFluxerAppExcludedURL('https://fluxer.app/channels/@me')).toBe(true);
			expect(isFluxerAppExcludedURL('https://fluxer.app/channels/general')).toBe(true);
		});

		it('should return false for different hosts', () => {
			expect(isFluxerAppExcludedURL('https://example.com/login')).toBe(false);
			expect(isFluxerAppExcludedURL('https://other.com/invite/abc')).toBe(false);
		});

		it('should handle URLs with query strings and fragments', () => {
			expect(isFluxerAppExcludedURL('https://web.fluxer.app/login?redirect=/home')).toBe(true);
			expect(isFluxerAppExcludedURL('https://fluxer.gg/abc?ref=email')).toBe(true);
			expect(isFluxerAppExcludedURL('https://fluxer.gift/xyz#promo')).toBe(true);
		});

		it('should handle invalid URLs gracefully', () => {
			expect(isFluxerAppExcludedURL('not-a-url')).toBe(false);
			expect(isFluxerAppExcludedURL('')).toBe(false);
		});
	});

	describe('extractURLs', () => {
		it('should extract valid URLs from text', () => {
			const text = 'Check out https://example.com and http://test.org for more info';
			const result = extractURLs(text);

			expect(result).toContain('https://example.com/');
			expect(result).toContain('http://test.org/');
			expect(result).toHaveLength(2);
		});

		it('should ignore URLs in code blocks', () => {
			const text = 'Visit https://example.com but ignore `https://code.example.com` in code';
			const result = extractURLs(text);

			expect(result).toContain('https://example.com/');
			expect(result).not.toContain('https://code.example.com/');
		});

		it('should ignore URLs in multiline code blocks', () => {
			const text = `
				Check https://example.com
				\`\`\`
				const url = 'https://code.example.com';
				\`\`\`
				Also visit https://test.org
			`;
			const result = extractURLs(text);

			expect(result).toContain('https://example.com/');
			expect(result).toContain('https://test.org/');
			expect(result).not.toContain('https://code.example.com/');
		});

		it('should extract URLs from markdown links', () => {
			const text = 'Check out [Example](https://example.com) and [Test](http://test.org)';
			const result = extractURLs(text);

			expect(result).toContain('https://example.com/');
			expect(result).toContain('http://test.org/');
		});

		it('should ignore URLs in angle brackets', () => {
			const text = 'Visit https://example.com but ignore <https://ignored.com>';
			const result = extractURLs(text);

			expect(result).toContain('https://example.com/');
			expect(result).not.toContain('https://ignored.com/');
		});

		it('should extract URLs wrapped in spoiler markers', () => {
			const text = 'Hidden link: ||https://google.com|| is still a URL';
			const result = extractURLs(text);

			expect(result).toContain('https://google.com/');
			expect(result).toHaveLength(1);
		});

		it('should filter out invite URLs', () => {
			const text = 'Visit https://example.com but not https://fluxer.gg/test123';
			const result = extractURLs(text);

			expect(result).toContain('https://example.com/');
			expect(result).not.toContain('https://fluxer.gg/test123');
		});

		it('should deduplicate URLs', () => {
			const text = 'Visit https://example.com and https://example.com again';
			const result = extractURLs(text);

			expect(result).toEqual(['https://example.com/']);
		});

		it('should limit to 5 URLs', () => {
			const text = Array.from({length: 10}, (_, i) => `https://example${i}.com`).join(' ');
			const result = extractURLs(text);

			expect(result).toHaveLength(5);
		});

		it('should handle URLs with international domains', () => {
			const text = 'Visit https://测试.example.com for testing';
			const result = extractURLs(text);

			expect(result).toHaveLength(1);
			expect(result[0]).toMatch(/^https:\/\/.*example\.com\/$/);
		});

		it('should filter out invalid URLs', () => {
			const text = 'Visit https://example.com and ftp://invalid.com';
			const result = extractURLs(text);

			expect(result).toContain('https://example.com/');
			expect(result).not.toContain('ftp://invalid.com');
		});

		it('should handle empty input', () => {
			expect(extractURLs('')).toEqual([]);
			expect(extractURLs('   ')).toEqual([]);
		});

		it('should handle text with no URLs', () => {
			const text = 'This is just plain text with no URLs';
			const result = extractURLs(text);

			expect(result).toEqual([]);
		});

		it('should handle complex mixed content', () => {
			const text = `
				Check out https://example.com for docs.

				Code example:
				\`\`\`javascript
				fetch('https://api.example.com/data')
				\`\`\`

				Also see [GitHub](https://github.com/user/repo) and ignore <https://spam.com>.

				Visit \`https://inline-code.com\` but not that.

				Don't forget https://fluxer.gg/abc123 (invite link).

				Final link: https://final.example.org
			`;
			const result = extractURLs(text);

			expect(result).toContain('https://example.com/');
			expect(result).toContain('https://github.com/user/repo');
			expect(result).toContain('https://final.example.org/');
			expect(result).not.toContain('https://api.example.com/data');
			expect(result).not.toContain('https://inline-code.com');
			expect(result).not.toContain('https://spam.com');
			expect(result).not.toContain('https://fluxer.gg/abc123');
		});

		it('should filter out all web app URLs', () => {
			const text = `
				Visit https://example.com and https://web.fluxer.app/login
				Check out https://web.fluxer.app/channels/@me/123
				Also see https://fluxer.app/about which is allowed
			`;
			const result = extractURLs(text);

			expect(result).toContain('https://example.com/');
			expect(result).toContain('https://fluxer.app/about');
			expect(result).not.toContain('https://web.fluxer.app/login');
			expect(result).not.toContain('https://web.fluxer.app/channels/@me/123');
		});

		it('should filter invite and gift shortlinks', () => {
			const excludedUrls = ['https://fluxer.gg/code', 'https://fluxer.gift/code', 'https://web.fluxer.app/any/path'];

			excludedUrls.forEach((url) => {
				const result = extractURLs(`Visit ${url} and https://example.com`);
				expect(result).not.toContain(url);
				expect(result).toContain('https://example.com/');
			});
		});
	});
});
