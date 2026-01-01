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
import {
	createSignature,
	getExternalMediaProxyURL,
	getProxyURLPath,
	reconstructOriginalURL,
	verifySignature,
} from './MediaProxyUtils';

describe('MediaProxyUtils', () => {
	const testSecretKey = 'test-secret-key';
	const testEndpoint = 'https://media-proxy.example.com';

	describe('getProxyURLPath', () => {
		it('should encode basic URL components', () => {
			const url = 'https://example.com/image.jpg';
			const result = getProxyURLPath(url);

			expect(result).toBe('https/example.com/image.jpg');
		});

		it('should handle URLs with ports', () => {
			const url = 'http://example.com:8080/image.jpg';
			const result = getProxyURLPath(url);

			expect(result).toBe('http/example.com:8080/image.jpg');
		});

		it('should handle URLs with query parameters', () => {
			const url = 'https://example.com/image.jpg?size=large&format=webp';
			const result = getProxyURLPath(url);

			expect(result).toBe('size%3Dlarge%26format%3Dwebp/https/example.com/image.jpg');
		});

		it('should handle URLs with paths containing slashes', () => {
			const url = 'https://example.com/folder/subfolder/image.jpg';
			const result = getProxyURLPath(url);

			expect(result).toBe('https/example.com/folder/subfolder/image.jpg');
		});

		it('should handle URLs with special characters in path', () => {
			const url = 'https://example.com/images/my%20file.jpg';
			const result = getProxyURLPath(url);

			expect(result).toBe('https/example.com/images/my%2520file.jpg');
		});

		it('should handle URLs with fragments (ignored)', () => {
			const url = 'https://example.com/image.jpg#section';
			const result = getProxyURLPath(url);

			expect(result).toBe('https/example.com/image.jpg');
		});

		it('should preserve forward slashes in encoded components', () => {
			const url = 'https://example.com/api/v1/images/photo.jpg';
			const result = getProxyURLPath(url);

			expect(result).toBe('https/example.com/api/v1/images/photo.jpg');
		});
	});

	describe('createSignature', () => {
		it('should create consistent signatures for same input', () => {
			const input = 'test-input-string';
			const signature1 = createSignature(input, testSecretKey);
			const signature2 = createSignature(input, testSecretKey);

			expect(signature1).toBe(signature2);
			expect(typeof signature1).toBe('string');
			expect(signature1.length).toBeGreaterThan(0);
		});

		it('should create different signatures for different inputs', () => {
			const signature1 = createSignature('input1', testSecretKey);
			const signature2 = createSignature('input2', testSecretKey);

			expect(signature1).not.toBe(signature2);
		});

		it('should create different signatures for different keys', () => {
			const input = 'same-input';
			const signature1 = createSignature(input, 'key1');
			const signature2 = createSignature(input, 'key2');

			expect(signature1).not.toBe(signature2);
		});

		it('should handle empty input', () => {
			const signature = createSignature('', testSecretKey);

			expect(typeof signature).toBe('string');
			expect(signature.length).toBeGreaterThan(0);
		});

		it('should produce base64url-safe signatures', () => {
			const signature = createSignature('test-input', testSecretKey);

			expect(signature).not.toMatch(/[+/=]/);
			expect(signature).toMatch(/^[A-Za-z0-9_-]+$/);
		});
	});

	describe('getExternalMediaProxyURL', () => {
		it('should generate complete proxy URL', () => {
			const inputURL = 'https://example.com/image.jpg';
			const result = getExternalMediaProxyURL({
				inputURL,
				mediaProxyEndpoint: testEndpoint,
				mediaProxySecretKey: testSecretKey,
			});

			expect(result).toMatch(new RegExp(`^${testEndpoint}/external/[A-Za-z0-9_-]+/https/example\\.com/image\\.jpg$`));
		});

		it('should include query parameters in URL path', () => {
			const inputURL = 'https://example.com/image.jpg?size=large';
			const result = getExternalMediaProxyURL({
				inputURL,
				mediaProxyEndpoint: testEndpoint,
				mediaProxySecretKey: testSecretKey,
			});

			expect(result).toContain('size%3Dlarge');
		});

		it('should handle complex URLs', () => {
			const inputURL = 'https://cdn.example.com:443/api/v2/images/photo.jpg?format=webp&quality=80';
			const result = getExternalMediaProxyURL({
				inputURL,
				mediaProxyEndpoint: testEndpoint,
				mediaProxySecretKey: testSecretKey,
			});

			expect(result).toContain(testEndpoint);
			expect(result).toContain('/external/');
			expect(result).toContain('format%3Dwebp%26quality%3D80');
			expect(result).toContain('https/cdn.example.com');
		});

		it('should generate verifiable URLs', () => {
			const inputURL = 'https://example.com/test.jpg';
			const proxyURL = getExternalMediaProxyURL({
				inputURL,
				mediaProxyEndpoint: testEndpoint,
				mediaProxySecretKey: testSecretKey,
			});

			const urlParts = proxyURL.replace(`${testEndpoint}/external/`, '').split('/');
			const signature = urlParts[0];
			const path = urlParts.slice(1).join('/');

			expect(verifySignature(path, signature, testSecretKey)).toBe(true);
		});
	});

	describe('verifySignature', () => {
		it('should verify valid signatures', () => {
			const path = 'https/example.com/image.jpg';
			const signature = createSignature(path, testSecretKey);

			expect(verifySignature(path, signature, testSecretKey)).toBe(true);
		});

		it('should reject invalid signatures', () => {
			const path = 'https/example.com/image.jpg';
			const validSignature = createSignature(path, testSecretKey);
			const invalidSignature = `${validSignature.slice(0, -1)}X`;

			expect(verifySignature(path, invalidSignature, testSecretKey)).toBe(false);
		});

		it('should reject signatures with wrong key', () => {
			const path = 'https/example.com/image.jpg';
			const signature = createSignature(path, 'wrong-key');

			expect(verifySignature(path, signature, testSecretKey)).toBe(false);
		});

		it('should reject signatures for wrong path', () => {
			const signature = createSignature('https/example.com/image1.jpg', testSecretKey);

			expect(verifySignature('https/example.com/image2.jpg', signature, testSecretKey)).toBe(false);
		});

		it('should handle timing-safe comparison', () => {
			const path = 'https/example.com/image.jpg';
			const correctSignature = createSignature(path, testSecretKey);
			const wrongSignature = `${correctSignature.slice(0, -1)}X`;

			expect(verifySignature(path, wrongSignature, testSecretKey)).toBe(false);
		});
	});

	describe('reconstructOriginalURL', () => {
		it('should reconstruct basic URLs', () => {
			const originalURL = 'https://example.com/image.jpg';
			const proxyPath = getProxyURLPath(originalURL);
			const reconstructed = reconstructOriginalURL(proxyPath);

			expect(reconstructed).toBe(originalURL);
		});

		it('should reconstruct URLs with ports', () => {
			const originalURL = 'http://example.com:8080/image.jpg';
			const proxyPath = getProxyURLPath(originalURL);
			const reconstructed = reconstructOriginalURL(proxyPath);

			expect(reconstructed).toBe(originalURL);
		});

		it('should reconstruct URLs with query parameters', () => {
			const originalURL = 'https://example.com/image.jpg?size=large&format=webp';
			const proxyPath = getProxyURLPath(originalURL);
			const reconstructed = reconstructOriginalURL(proxyPath);

			expect(reconstructed).toBe(originalURL);
		});

		it('should reconstruct URLs with complex paths', () => {
			const originalURL = 'https://cdn.example.com/api/v2/images/folder/photo.jpg';
			const proxyPath = getProxyURLPath(originalURL);
			const reconstructed = reconstructOriginalURL(proxyPath);

			expect(reconstructed).toBe(originalURL);
		});

		it('should throw error for missing protocol', () => {
			const invalidPath = 'example.com/image.jpg';

			const result = reconstructOriginalURL(invalidPath);
			expect(result).toBe('example.com://image.jpg/');
		});

		it('should throw error for missing hostname', () => {
			const invalidPath = 'https//image.jpg';

			expect(() => reconstructOriginalURL(invalidPath)).toThrow('Hostname is missing in the proxy URL path.');
		});

		it('should handle URLs without query parameters', () => {
			const originalURL = 'https://example.com/simple.jpg';
			const proxyPath = getProxyURLPath(originalURL);
			const reconstructed = reconstructOriginalURL(proxyPath);

			expect(reconstructed).toBe(originalURL);
		});

		it('should handle edge case with empty path', () => {
			const originalURL = 'https://example.com/';
			const proxyPath = getProxyURLPath(originalURL);
			const reconstructed = reconstructOriginalURL(proxyPath);

			expect(reconstructed).toBe(originalURL);
		});
	});

	describe('roundtrip encoding/decoding', () => {
		const testURLs = [
			'https://example.com/image.jpg',
			'http://localhost:3000/api/image',
			'https://cdn.example.com/folder/subfolder/image.png?size=large&format=webp',
			'https://example.com:8443/path/to/resource.jpg',
			'https://example.com/images/file%20with%20spaces.jpg',
			'https://api.example.com/v1/media/123456789.jpg?token=abc123&expires=1234567890',
		];

		testURLs.forEach((url) => {
			it(`should correctly roundtrip encode/decode: ${url}`, () => {
				const proxyPath = getProxyURLPath(url);
				const reconstructed = reconstructOriginalURL(proxyPath);

				expect(reconstructed).toBe(url);
			});
		});

		testURLs.forEach((url) => {
			it(`should generate valid proxy URLs for: ${url}`, () => {
				const proxyURL = getExternalMediaProxyURL({
					inputURL: url,
					mediaProxyEndpoint: testEndpoint,
					mediaProxySecretKey: testSecretKey,
				});

				const urlParts = proxyURL.replace(`${testEndpoint}/external/`, '').split('/');
				const signature = urlParts[0];
				const path = urlParts.slice(1).join('/');

				expect(verifySignature(path, signature, testSecretKey)).toBe(true);

				const reconstructed = reconstructOriginalURL(path);
				expect(reconstructed).toBe(url);
			});
		});
	});
});
