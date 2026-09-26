// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {RequireClientIpMiddleware} from '@app/api/middleware/RequireClientIpMiddleware';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import type {ClientIpResolution} from '@app/api/utils/RequestClientIp';
import {AppErrorHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {Hono} from 'hono';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

interface Harness {
	request: (headers: Record<string, string>) => Promise<Response>;
	resolutions: Array<ClientIpResolution | undefined>;
}

function createHarness(path = 'http://localhost/v1/messages'): Harness {
	const resolutions: Array<ClientIpResolution | undefined> = [];
	const app = new Hono<HonoEnv>();
	app.use(RequireClientIpMiddleware());
	app.get('/v1/messages', (ctx) => {
		resolutions.push(ctx.get('clientIpResolution'));
		return ctx.text('ok');
	});
	app.get('/_health', (ctx) => ctx.text('OK'));
	app.get('/internal/rpc', (ctx) => ctx.text('OK'));
	app.onError(AppErrorHandler);
	return {
		request: async (headers) => app.request(path, {headers}),
		resolutions,
	};
}

describe('RequireClientIpMiddleware', () => {
	let previousTestModeEnabled: boolean;
	let previousTrustClientIpHeader: boolean;

	beforeEach(() => {
		previousTestModeEnabled = Config.dev.testModeEnabled;
		previousTrustClientIpHeader = Config.proxy.trust_client_ip_header;
		Config.dev.testModeEnabled = false;
		Config.proxy.trust_client_ip_header = true;
	});

	afterEach(() => {
		Config.dev.testModeEnabled = previousTestModeEnabled;
		Config.proxy.trust_client_ip_header = previousTrustClientIpHeader;
	});

	it('rejects an unparsable client ip header with 403 FORBIDDEN', async () => {
		const harness = createHarness();
		const response = await harness.request({'x-forwarded-for': 'not-an-ip'});
		expect(response.status).toBe(403);
		expect(await response.json()).toMatchObject({code: 'FORBIDDEN'});
	});

	it('rejects a request when the header is not trusted with 403', async () => {
		Config.proxy.trust_client_ip_header = false;
		const harness = createHarness();
		const response = await harness.request({'x-forwarded-for': '203.0.113.10'});
		expect(response.status).toBe(403);
		expect(await response.json()).toMatchObject({code: 'FORBIDDEN'});
	});

	it('rejects a request with no client ip header with 403 FORBIDDEN', async () => {
		const harness = createHarness();
		const response = await harness.request({});
		expect(response.status).toBe(403);
		expect(await response.json()).toMatchObject({code: 'FORBIDDEN'});
	});

	it('passes a parsable header and caches the resolution', async () => {
		const harness = createHarness();
		const response = await harness.request({'x-forwarded-for': '203.0.113.10, 10.0.0.1'});
		expect(response.status).toBe(200);
		expect(harness.resolutions[0]?.ip).toBe('203.0.113.10');
	});

	it('leaves exempt paths alone', async () => {
		const harness = createHarness('http://localhost/_health');
		const response = await harness.request({});
		expect(response.status).toBe(200);
	});

	it('leaves internal service to service calls alone', async () => {
		const harness = createHarness('http://localhost/internal/rpc');
		const response = await harness.request({});
		expect(response.status).toBe(200);
	});

	it('passes every request through in test mode', async () => {
		Config.dev.testModeEnabled = true;
		const harness = createHarness();
		const response = await harness.request({'x-forwarded-for': 'not-an-ip'});
		expect(response.status).toBe(200);
	});
});
