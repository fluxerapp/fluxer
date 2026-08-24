// SPDX-License-Identifier: AGPL-3.0-or-later

import {cacheHeaders} from '@fluxer/hono/src/middleware/CacheHeaders';
import {Hono} from 'hono';
import {describe, expect, test} from 'vitest';

describe('CacheHeaders Middleware', () => {
	describe('static content caching', () => {
		test('sets long-lived cache for CSS files', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/styles.css', () => {
				return new Response('body { color: red; }', {
					headers: {'Content-Type': 'text/css'},
				});
			});
			const response = await app.request('/styles.css');
			expect(response.headers.get('Cache-Control')).toBe('public, max-age=31536000');
		});
		test('sets long-lived cache for JavaScript files', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/app.js', () => {
				return new Response('console.log("hello");', {
					headers: {'Content-Type': 'application/javascript'},
				});
			});
			const response = await app.request('/app.js');
			expect(response.headers.get('Cache-Control')).toBe('public, max-age=31536000');
		});
		test('sets long-lived cache for images', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/logo.png', (c) => {
				c.header('Content-Type', 'image/png');
				return c.body(new Uint8Array([1, 2, 3]));
			});
			const response = await app.request('/logo.png');
			expect(response.headers.get('Cache-Control')).toBe('public, max-age=31536000');
		});
		test('sets long-lived cache for fonts', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/font.woff2', (c) => {
				c.header('Content-Type', 'font/woff2');
				return c.body(new Uint8Array([1, 2, 3]));
			});
			const response = await app.request('/font.woff2');
			expect(response.headers.get('Cache-Control')).toBe('public, max-age=31536000');
		});
		test('sets long-lived cache for application/font-woff2', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/font.woff2', (c) => {
				c.header('Content-Type', 'application/font-woff2');
				return c.body(new Uint8Array([1, 2, 3]));
			});
			const response = await app.request('/font.woff2');
			expect(response.headers.get('Cache-Control')).toBe('public, max-age=31536000');
		});
		test('sets long-lived cache for video files', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/video.mp4', (c) => {
				c.header('Content-Type', 'video/mp4');
				return c.body(new Uint8Array([1, 2, 3]));
			});
			const response = await app.request('/video.mp4');
			expect(response.headers.get('Cache-Control')).toBe('public, max-age=31536000');
		});
		test('sets long-lived cache for audio files', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/audio.mp3', (c) => {
				c.header('Content-Type', 'audio/mpeg');
				return c.body(new Uint8Array([1, 2, 3]));
			});
			const response = await app.request('/audio.mp3');
			expect(response.headers.get('Cache-Control')).toBe('public, max-age=31536000');
		});
	});
	describe('default caching', () => {
		test('sets no-cache for JSON responses', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/api/data', (c) => c.json({ok: true}));
			const response = await app.request('/api/data');
			expect(response.headers.get('Cache-Control')).toBe('no-cache');
		});
		test('sets no-cache for HTML responses', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/', (c) => {
				c.header('Content-Type', 'text/html');
				return c.html('<html><body>Hello</body></html>');
			});
			const response = await app.request('/');
			expect(response.headers.get('Cache-Control')).toBe('no-cache');
		});
		test('sets no-cache for text/plain responses', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/readme.txt', (c) => c.text('Hello World'));
			const response = await app.request('/readme.txt');
			expect(response.headers.get('Cache-Control')).toBe('no-cache');
		});
		test('sets no-cache when Content-Type is not set', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/test', (c) => c.body('data'));
			const response = await app.request('/test');
			expect(response.headers.get('Cache-Control')).toBe('no-cache');
		});
	});
	describe('existing Cache-Control header', () => {
		test('does not override existing Cache-Control header', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/test', (c) => {
				c.header('Cache-Control', 'private, max-age=3600');
				return c.json({ok: true});
			});
			const response = await app.request('/test');
			expect(response.headers.get('Cache-Control')).toBe('private, max-age=3600');
		});
		test('respects existing no-store header', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/sensitive', (c) => {
				c.header('Cache-Control', 'no-store');
				return c.json({secret: 'data'});
			});
			const response = await app.request('/sensitive');
			expect(response.headers.get('Cache-Control')).toBe('no-store');
		});
	});
	describe('custom options', () => {
		test('uses custom staticCacheControl', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders({staticCacheControl: 'public, max-age=86400'}));
			app.get('/image.png', () => {
				return new Response(new Uint8Array([1, 2, 3]), {
					headers: {'Content-Type': 'image/png'},
				});
			});
			const response = await app.request('/image.png');
			expect(response.headers.get('Cache-Control')).toBe('public, max-age=86400');
		});
		test('uses custom defaultCacheControl', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders({defaultCacheControl: 'no-store'}));
			app.get('/api/data', (c) => c.json({ok: true}));
			const response = await app.request('/api/data');
			expect(response.headers.get('Cache-Control')).toBe('no-store');
		});
		test('uses both custom options together', async () => {
			const app = new Hono();
			app.use(
				'*',
				cacheHeaders({
					staticCacheControl: 'public, max-age=7200',
					defaultCacheControl: 'private, no-cache',
				}),
			);
			app.get('/style.css', () => {
				return new Response('body {}', {
					headers: {'Content-Type': 'text/css'},
				});
			});
			app.get('/api/data', (c) => c.json({ok: true}));
			const cssResponse = await app.request('/style.css');
			expect(cssResponse.headers.get('Cache-Control')).toBe('public, max-age=7200');
			const jsonResponse = await app.request('/api/data');
			expect(jsonResponse.headers.get('Cache-Control')).toBe('private, no-cache');
		});
	});
	describe('content type variations', () => {
		test('handles image/jpeg', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/photo.jpg', (c) => {
				c.header('Content-Type', 'image/jpeg');
				return c.body(new Uint8Array([1, 2, 3]));
			});
			const response = await app.request('/photo.jpg');
			expect(response.headers.get('Cache-Control')).toBe('public, max-age=31536000');
		});
		test('handles image/gif', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/animation.gif', (c) => {
				c.header('Content-Type', 'image/gif');
				return c.body(new Uint8Array([1, 2, 3]));
			});
			const response = await app.request('/animation.gif');
			expect(response.headers.get('Cache-Control')).toBe('public, max-age=31536000');
		});
		test('handles image/svg+xml', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/icon.svg', () => {
				return new Response('<svg></svg>', {
					headers: {'Content-Type': 'image/svg+xml'},
				});
			});
			const response = await app.request('/icon.svg');
			expect(response.headers.get('Cache-Control')).toBe('public, max-age=31536000');
		});
		test('handles video/webm', async () => {
			const app = new Hono();
			app.use('*', cacheHeaders());
			app.get('/video.webm', (c) => {
				c.header('Content-Type', 'video/webm');
				return c.body(new Uint8Array([1, 2, 3]));
			});
			const response = await app.request('/video.webm');
			expect(response.headers.get('Cache-Control')).toBe('public, max-age=31536000');
		});
	});
});
