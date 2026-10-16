// SPDX-License-Identifier: AGPL-3.0-or-later

import {cors} from '@fluxer/hono/src/middleware/Cors';
import {Hono} from 'hono';
import {describe, expect, test} from 'vitest';

describe('CORS Middleware', () => {
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
	});
});
