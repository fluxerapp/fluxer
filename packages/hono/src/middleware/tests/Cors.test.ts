// SPDX-License-Identifier: AGPL-3.0-or-later

import {cors} from '@fluxer/hono/src/middleware/Cors';
import {Hono} from 'hono';
import {describe, expect, test} from 'vitest';

describe('CORS Middleware', () => {
	describe('wildcard origin', () => {
		test('sets Access-Control-Allow-Origin to * for wildcard origins', async () => {
			const app = new Hono();
			app.use('*', cors({origins: '*'}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				headers: {origin: 'https://example.com'},
			});
			expect(response.status).toBe(200);
			expect(response.headers.get('Access-Control-Allow-Origin')).toBe('*');
		});
		test('does not set Vary header for wildcard origins', async () => {
			const app = new Hono();
			app.use('*', cors({origins: '*'}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				headers: {origin: 'https://example.com'},
			});
			expect(response.headers.get('Vary')).toBeNull();
		});
	});
	describe('specific origins', () => {
		test('allows requests from whitelisted origin', async () => {
			const app = new Hono();
			app.use('*', cors({origins: ['https://allowed.com', 'https://also-allowed.com']}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				headers: {origin: 'https://allowed.com'},
			});
			expect(response.status).toBe(200);
			expect(response.headers.get('Access-Control-Allow-Origin')).toBe('https://allowed.com');
			expect(response.headers.get('Vary')).toBe('Origin');
		});
		test('does not set origin header for non-whitelisted origin', async () => {
			const app = new Hono();
			app.use('*', cors({origins: ['https://allowed.com']}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				headers: {origin: 'https://not-allowed.com'},
			});
			expect(response.status).toBe(200);
			expect(response.headers.get('Access-Control-Allow-Origin')).toBeNull();
			expect(response.headers.get('Vary')).toBe('Origin');
		});
		test('does not set origin header when no origin header in request', async () => {
			const app = new Hono();
			app.use('*', cors({origins: ['https://allowed.com']}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test');
			expect(response.status).toBe(200);
			expect(response.headers.get('Access-Control-Allow-Origin')).toBeNull();
		});
	});
	describe('credentials', () => {
		test('sets Access-Control-Allow-Credentials when credentials is true', async () => {
			const app = new Hono();
			app.use('*', cors({credentials: true}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test');
			expect(response.headers.get('Access-Control-Allow-Credentials')).toBe('true');
		});
		test('does not set Access-Control-Allow-Credentials when credentials is false', async () => {
			const app = new Hono();
			app.use('*', cors({credentials: false}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test');
			expect(response.headers.get('Access-Control-Allow-Credentials')).toBeNull();
		});
	});
	describe('preflight requests (OPTIONS)', () => {
		test('responds to preflight with 204 status', async () => {
			const app = new Hono();
			app.use('*', cors());
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				method: 'OPTIONS',
				headers: {origin: 'https://example.com'},
			});
			expect(response.status).toBe(204);
		});
		test('sets Access-Control-Allow-Methods header on preflight', async () => {
			const app = new Hono();
			app.use('*', cors({methods: ['GET', 'POST', 'PUT']}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				method: 'OPTIONS',
			});
			expect(response.headers.get('Access-Control-Allow-Methods')).toBe('GET, POST, PUT');
		});
		test('uses default methods when not specified', async () => {
			const app = new Hono();
			app.use('*', cors());
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				method: 'OPTIONS',
			});
			const methods = response.headers.get('Access-Control-Allow-Methods');
			expect(methods).toContain('GET');
			expect(methods).toContain('POST');
			expect(methods).toContain('PUT');
			expect(methods).toContain('DELETE');
			expect(methods).toContain('OPTIONS');
		});
		test('sets Access-Control-Allow-Headers header on preflight', async () => {
			const app = new Hono();
			app.use('*', cors({allowedHeaders: ['Content-Type', 'Authorization', 'X-Custom-Header']}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				method: 'OPTIONS',
			});
			expect(response.headers.get('Access-Control-Allow-Headers')).toBe('Content-Type, Authorization, X-Custom-Header');
		});
		test('sets Access-Control-Max-Age header on preflight', async () => {
			const app = new Hono();
			app.use('*', cors({maxAge: 3600}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				method: 'OPTIONS',
			});
			expect(response.headers.get('Access-Control-Max-Age')).toBe('3600');
		});
		test('uses default maxAge of 86400', async () => {
			const app = new Hono();
			app.use('*', cors());
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				method: 'OPTIONS',
			});
			expect(response.headers.get('Access-Control-Max-Age')).toBe('86400');
		});
	});
	describe('exposed headers', () => {
		test('sets Access-Control-Expose-Headers when exposedHeaders is provided', async () => {
			const app = new Hono();
			app.use('*', cors({exposedHeaders: ['X-Custom-Header', 'X-Another-Header']}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test');
			expect(response.headers.get('Access-Control-Expose-Headers')).toBe('X-Custom-Header, X-Another-Header');
		});
		test('does not set Access-Control-Expose-Headers when empty', async () => {
			const app = new Hono();
			app.use('*', cors({exposedHeaders: []}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test');
			expect(response.headers.get('Access-Control-Expose-Headers')).toBeNull();
		});
		test('sets exposed headers on both preflight and regular requests', async () => {
			const app = new Hono();
			app.use('*', cors({exposedHeaders: ['X-Custom-Header']}));
			app.get('/test', (c) => c.json({ok: true}));
			const preflightResponse = await app.request('/test', {method: 'OPTIONS'});
			expect(preflightResponse.headers.get('Access-Control-Expose-Headers')).toBe('X-Custom-Header');
			const regularResponse = await app.request('/test');
			expect(regularResponse.headers.get('Access-Control-Expose-Headers')).toBe('X-Custom-Header');
		});
	});
	describe('enabled option', () => {
		test('skips CORS when enabled is false', async () => {
			const app = new Hono();
			app.use('*', cors({enabled: false}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				headers: {origin: 'https://example.com'},
			});
			expect(response.status).toBe(200);
			expect(response.headers.get('Access-Control-Allow-Origin')).toBeNull();
		});
		test('processes CORS when enabled is true', async () => {
			const app = new Hono();
			app.use('*', cors({enabled: true, origins: '*'}));
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				headers: {origin: 'https://example.com'},
			});
			expect(response.headers.get('Access-Control-Allow-Origin')).toBe('*');
		});
		test('enabled defaults to true', async () => {
			const app = new Hono();
			app.use('*', cors());
			app.get('/test', (c) => c.json({ok: true}));
			const response = await app.request('/test', {
				headers: {origin: 'https://example.com'},
			});
			expect(response.headers.get('Access-Control-Allow-Origin')).toBe('*');
		});
	});
	describe('integration with routes', () => {
		test('works with multiple routes', async () => {
			const app = new Hono();
			app.use('*', cors({origins: '*'}));
			app.get('/api/users', (c) => c.json({users: []}));
			app.post('/api/users', (c) => c.json({created: true}));
			const getResponse = await app.request('/api/users');
			expect(getResponse.headers.get('Access-Control-Allow-Origin')).toBe('*');
			const postResponse = await app.request('/api/users', {method: 'POST'});
			expect(postResponse.headers.get('Access-Control-Allow-Origin')).toBe('*');
		});
	});
});
