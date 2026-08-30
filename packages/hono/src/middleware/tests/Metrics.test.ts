// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMetricsMiddleware} from '@fluxer/hono/src/middleware/Metrics';
import {Hono} from 'hono';
import {describe, expect, test} from 'vitest';

function requestMetrics(app: Hono, remoteAddress = '127.0.0.1') {
	return app.request('/_metrics', undefined, {incoming: {socket: {remoteAddress}}});
}

function createTestApp() {
	const {middleware, metricsHandler, state} = createMetricsMiddleware('test');
	const app = new Hono();
	app.use('*', middleware);
	app.get('/_metrics', metricsHandler);
	app.get('/_health', (c) => c.text('OK'));
	app.get('/_healthz', (c) => c.text('OK'));
	app.get('/users', (c) => c.json({users: []}, 200));
	app.post('/users', (c) => c.json({created: true}, 201));
	app.get('/error', (c) => c.json({error: 'Server Error'}, 500));
	app.get('/bad', (c) => c.json({error: 'Bad Request'}, 400));
	return {app, state};
}

describe('Metrics Middleware', () => {
	describe('request counting', () => {
		test('increments request counter on each request', async () => {
			const {app} = createTestApp();
			await app.request('/users');
			await app.request('/users');
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).toContain('fluxer_test_http_requests_total{method="GET",status="2xx"} 2');
		});

		test('tracks different methods separately', async () => {
			const {app} = createTestApp();
			await app.request('/users');
			await app.request('/users', {method: 'POST'});
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).toContain('fluxer_test_http_requests_total{method="GET",status="2xx"} 1');
			expect(body).toContain('fluxer_test_http_requests_total{method="POST",status="2xx"} 1');
		});

		test('tracks different status classes separately', async () => {
			const {app} = createTestApp();
			await app.request('/users');
			await app.request('/bad');
			await app.request('/error');
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).toContain('status="2xx"');
			expect(body).toContain('status="4xx"');
			expect(body).toContain('status="5xx"');
		});
	});

	describe('error counting', () => {
		test('increments error counter on 5xx responses', async () => {
			const {app} = createTestApp();
			await app.request('/error');
			await app.request('/error');
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).toContain('fluxer_test_http_errors_total{method="GET"} 2');
		});

		test('does not count 4xx as errors', async () => {
			const {app} = createTestApp();
			await app.request('/bad');
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).not.toContain('fluxer_test_http_errors_total{method="GET"}');
		});
	});

	describe('duration histogram', () => {
		test('records request duration', async () => {
			const {app} = createTestApp();
			await app.request('/users');
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).toContain('fluxer_test_http_request_duration_seconds_count 1');
			expect(body).toContain('fluxer_test_http_request_duration_seconds_sum');
			expect(body).toContain('fluxer_test_http_request_duration_seconds_bucket{le="0.005"}');
			expect(body).toContain('fluxer_test_http_request_duration_seconds_bucket{le="+Inf"} 1');
		});

		test('accumulates across multiple requests', async () => {
			const {app} = createTestApp();
			await app.request('/users');
			await app.request('/users');
			await app.request('/users');
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).toContain('fluxer_test_http_request_duration_seconds_count 3');
		});
	});

	describe('uptime gauge', () => {
		test('reports uptime in seconds', async () => {
			const {app} = createTestApp();
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).toContain('# TYPE fluxer_test_uptime_seconds gauge');
			expect(body).toMatch(/fluxer_test_uptime_seconds \d/);
		});
	});

	describe('path skipping', () => {
		test('skips /_health requests', async () => {
			const {app} = createTestApp();
			await app.request('/_health');
			await app.request('/_health');
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).not.toContain('method="GET",status="2xx"');
		});

		test('skips /_healthz requests', async () => {
			const {app} = createTestApp();
			await app.request('/_healthz');
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).not.toContain('method="GET",status="2xx"');
		});

		test('skips /_metrics requests', async () => {
			const {app} = createTestApp();
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).not.toContain('method="GET",status="2xx"');
		});

		test('does not skip normal paths', async () => {
			const {app} = createTestApp();
			await app.request('/users');
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).toContain('method="GET",status="2xx"');
		});
	});

	describe('metrics endpoint', () => {
		test('returns correct content type', async () => {
			const {app} = createTestApp();
			const res = await requestMetrics(app);
			expect(res.headers.get('Content-Type')).toBe('text/plain; version=0.0.4; charset=utf-8');
		});

		test('returns 200 status', async () => {
			const {app} = createTestApp();
			const res = await requestMetrics(app);
			expect(res.status).toBe(200);
		});

		test('includes HELP and TYPE annotations', async () => {
			const {app} = createTestApp();
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).toContain('# HELP fluxer_test_http_requests_total Total HTTP requests');
			expect(body).toContain('# TYPE fluxer_test_http_requests_total counter');
			expect(body).toContain('# HELP fluxer_test_http_request_duration_seconds HTTP request duration in seconds');
			expect(body).toContain('# TYPE fluxer_test_http_request_duration_seconds histogram');
			expect(body).toContain('# HELP fluxer_test_http_errors_total Total HTTP 5xx errors');
			expect(body).toContain('# TYPE fluxer_test_http_errors_total counter');
			expect(body).toContain('# HELP fluxer_test_uptime_seconds Process uptime in seconds');
			expect(body).toContain('# TYPE fluxer_test_uptime_seconds gauge');
		});

		test('renders default counter value when no requests made', async () => {
			const {app} = createTestApp();
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).toContain('fluxer_test_http_requests_total 0');
			expect(body).toContain('fluxer_test_http_errors_total 0');
		});
	});

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

	describe('service name prefix', () => {
		test('uses provided service name in metric names', async () => {
			const {middleware, metricsHandler} = createMetricsMiddleware('gateway');
			const app = new Hono();
			app.use('*', middleware);
			app.get('/_metrics', metricsHandler);
			app.get('/test', (c) => c.json({ok: true}));
			await app.request('/test');
			const res = await requestMetrics(app);
			const body = await res.text();
			expect(body).toContain('fluxer_gateway_http_requests_total');
			expect(body).toContain('fluxer_gateway_http_request_duration_seconds');
			expect(body).toContain('fluxer_gateway_http_errors_total');
			expect(body).toContain('fluxer_gateway_uptime_seconds');
		});
	});
});
