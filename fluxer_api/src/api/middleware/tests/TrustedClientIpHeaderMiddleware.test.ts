// SPDX-License-Identifier: AGPL-3.0-or-later

import {Hono} from 'hono';
import {describe, expect, it} from 'vitest';
import type {ILogger} from '../../ILogger';
import type {HonoEnv} from '../../types/HonoEnv';
import {TrustedClientIpHeaderMiddleware} from '../TrustedClientIpHeaderMiddleware';

class MockLogger implements ILogger {
	trace(_msgOrObject: string | object, _msg?: string): void {}

	debug(_msgOrObject: string | object, _msg?: string): void {}

	info(_msgOrObject: string | object, _msg?: string): void {}

	warn(_msgOrObject: string | object, _msg?: string): void {}

	error(_msgOrObject: string | object, _msg?: string): void {}

	fatal(_msgOrObject: string | object, _msg?: string): void {}

	child(_bindings: Record<string, unknown>): ILogger {
		return this;
	}
}

function createApp(clientIpHeaderName = 'x-real-ip'): Hono<HonoEnv> {
	const app = new Hono<HonoEnv>();
	app.use(
		'*',
		TrustedClientIpHeaderMiddleware({
			enabled: true,
			logger: new MockLogger(),
			trustClientIpHeader: true,
			clientIpHeaderName,
		}),
	);
	app.get('/v1/messages', (ctx) => ctx.text('ok'));
	return app;
}

describe('TrustedClientIpHeaderMiddleware', () => {
	it('accepts requests with a valid client IP header', async () => {
		const app = createApp();
		const response = await app.request('http://localhost/v1/messages', {
			headers: {'x-real-ip': '203.0.113.10'},
		});
		expect(response.status).toBe(200);
	});
	it('accepts requests when the IP header is absent (passthrough)', async () => {
		const app = createApp();
		const response = await app.request('http://localhost/v1/messages');
		expect(response.status).toBe(200);
	});
	it('rejects requests with an invalid client IP header value', async () => {
		const app = createApp();
		const response = await app.request('http://localhost/v1/messages', {
			headers: {'x-real-ip': 'not-an-ip'},
		});
		expect(response.status).toBe(403);
	});
	it('accepts requests when the IP header holds only whitespace (passthrough)', async () => {
		const app = createApp();
		const response = await app.request('http://localhost/v1/messages', {
			headers: {'x-real-ip': '   '},
		});
		expect(response.status).toBe(200);
	});
	it('rejects x-forwarded-for with an empty first hop', async () => {
		const app = createApp('x-forwarded-for');
		const response = await app.request('http://localhost/v1/messages', {
			headers: {'x-forwarded-for': ', 10.0.0.1'},
		});
		expect(response.status).toBe(403);
	});
	it('accepts x-forwarded-for with multiple hops and a valid first hop', async () => {
		const app = createApp('x-forwarded-for');
		const response = await app.request('http://localhost/v1/messages', {
			headers: {'x-forwarded-for': '203.0.113.10, 10.0.0.1'},
		});
		expect(response.status).toBe(200);
	});
});
