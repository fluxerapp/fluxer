// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createMetricsMiddleware,
	createRegisteredMetricsHandler,
	registerCounter,
} from '@fluxer/hono/src/middleware/Metrics';
import {Hono} from 'hono';
import {describe, expect, test} from 'vitest';

function requestMetrics(app: Hono, remoteAddress = '127.0.0.1') {
	return app.request('/_metrics', undefined, {incoming: {socket: {remoteAddress}}});
}

function createTestApp() {
	const {middleware, metricsHandler} = createMetricsMiddleware('test');
	const app = new Hono();
	app.use('*', middleware);
	app.get('/_metrics', metricsHandler);
	return {app};
}

describe('Metrics Middleware', () => {
	describe('loopback restriction', () => {
		test('serves metrics to an IPv4 loopback peer', async () => {
			const {app} = createTestApp();
			const res = await requestMetrics(app, '127.0.0.1');
			expect(res.status).toBe(200);
		});

		test('serves metrics to any 127.0.0.0/8 peer', async () => {
			const {app} = createTestApp();
			const res = await requestMetrics(app, '127.0.0.2');
			expect(res.status).toBe(200);
		});

		test('serves metrics to an IPv6 loopback peer', async () => {
			const {app} = createTestApp();
			const res = await requestMetrics(app, '::1');
			expect(res.status).toBe(200);
		});

		test('serves metrics to an IPv4-mapped loopback peer', async () => {
			const {app} = createTestApp();
			const res = await requestMetrics(app, '::ffff:127.0.0.1');
			expect(res.status).toBe(200);
		});

		test('rejects a public peer', async () => {
			const {app} = createTestApp();
			const res = await requestMetrics(app, '8.8.8.8');
			expect(res.status).toBe(403);
			expect(await res.text()).toBe('FORBIDDEN');
		});

		test('rejects a container network peer', async () => {
			const {app} = createTestApp();
			const res = await requestMetrics(app, '172.18.0.4');
			expect(res.status).toBe(403);
		});

		test('rejects a request with no peer address', async () => {
			const {app} = createTestApp();
			const res = await app.request('/_metrics', undefined, {incoming: {socket: {}}});
			expect(res.status).toBe(403);
		});

		test('rejects a request with no node bindings', async () => {
			const {app} = createTestApp();
			const res = await app.request('/_metrics', undefined, {});
			expect(res.status).toBe(403);
		});

		test('ignores a forged x-forwarded-for header', async () => {
			const {app} = createTestApp();
			const res = await app.request(
				'/_metrics',
				{headers: {'x-forwarded-for': '127.0.0.1'}},
				{incoming: {socket: {remoteAddress: '203.0.113.9'}}},
			);
			expect(res.status).toBe(403);
		});
	});

	describe('registered metrics', () => {
		test('serves only registered metrics from the standalone handler', async () => {
			registerCounter('fluxer_test_standalone_total', 'Standalone counter').inc();
			const app = new Hono();
			app.get('/_metrics', createRegisteredMetricsHandler());
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(res.status).toBe(200);
			expect(res.headers.get('Content-Type')).toBe('text/plain; version=0.0.4; charset=utf-8');
			expect(body).toContain('fluxer_test_standalone_total 1');
			expect(body).not.toContain('_http_requests_total');
			expect((await requestMetrics(app, '8.8.8.8')).status).toBe(403);
		});
	});
});
