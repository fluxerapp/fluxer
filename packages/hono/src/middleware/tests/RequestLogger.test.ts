// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RequestLogData} from '@fluxer/hono/src/middleware/RequestLogger';
import {requestLogger} from '@fluxer/hono/src/middleware/RequestLogger';
import {Hono} from 'hono';
import {describe, expect, test} from 'vitest';

function createApp() {
	const entries: Array<RequestLogData> = [];
	const routes = new Hono();
	routes.use(requestLogger({log: (data) => entries.push(data), skip: ['/_health']}));
	routes.get('/_health', (c) => c.text('OK'));
	routes.get('/auth/reset/:token', (c) => c.json({ok: true}));
	routes.post('/webhooks/:webhook_id/:token', (c) => c.body(null, 204));
	routes.get('/blocked/:code', (c) => c.text('unreachable'));
	routes.use('/limited/*', async (c) => c.text('limited', 429));
	routes.get('/limited/:code', (c) => c.text('unreachable'));
	const app = new Hono();
	app.route('/v1', routes);
	app.route('/', routes);
	return {app, entries};
}

describe('RequestLogger Middleware', () => {
	test('logs the matched route pattern instead of the request path', async () => {
		const {app, entries} = createApp();
		await app.request('/v1/auth/reset/abc123');
		await app.request('/webhooks/111/def456', {method: 'POST'});
		expect(entries).toEqual([
			expect.objectContaining({method: 'GET', path: '/v1/auth/reset/:token', status: 200}),
			expect.objectContaining({method: 'POST', path: '/webhooks/:webhook_id/:token', status: 204}),
		]);
	});
	test('logs the route pattern when middleware ends the request early', async () => {
		const {app, entries} = createApp();
		await app.request('/v1/limited/ghi789');
		expect(entries).toEqual([expect.objectContaining({path: '/v1/limited/:code', status: 429})]);
	});
	test('does not log raw segments for unmatched routes', async () => {
		const {app, entries} = createApp();
		await app.request('/v1/unknown/jkl012');
		expect(entries.length).toBeGreaterThan(0);
		for (const entry of entries) {
			expect(entry.status).toBe(404);
			expect(entry.path).not.toContain('jkl012');
		}
	});
	test('skips configured paths', async () => {
		const {app, entries} = createApp();
		await app.request('/_health');
		expect(entries).toEqual([]);
	});
});
