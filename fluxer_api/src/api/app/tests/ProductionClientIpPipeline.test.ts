// SPDX-License-Identifier: AGPL-3.0-or-later

import {configureMiddleware} from '@app/api/app/MiddlewarePipeline';
import {Config} from '@app/api/Config';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {AppErrorHandler, AppNotFoundHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {Hono} from 'hono';
import {afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const CLIENT_IP_HEADER_NAME = 'x-real-ip';

function createProductionApp(): Hono<HonoEnv> {
	const routes = new Hono<HonoEnv>({strict: true});
	configureMiddleware(routes, {
		logger: new NoopLogger(),
		nodeEnv: 'production',
		corsOrigins: ['https://web.fluxer.app'],
		trustClientIpHeader: true,
		clientIpHeaderName: CLIENT_IP_HEADER_NAME,
		maxInflightRequests: 100,
		torExitBlockingEnabled: false,
	});
	routes.onError(AppErrorHandler);
	routes.notFound(AppNotFoundHandler);
	routes.post('/internal/rpc', (ctx) => ctx.json({ok: true}));
	routes.get('/connections/bluesky/jwks.json', (ctx) => ctx.json({keys: []}));
	routes.get('/users/@me', (ctx) => ctx.json({ok: true}));
	const app = new Hono<HonoEnv>({strict: true});
	app.route('/v1', routes);
	app.route('/', routes);
	app.onError(AppErrorHandler);
	app.notFound(AppNotFoundHandler);
	return app;
}

describe('client ip requirements across the production middleware pipeline', () => {
	let previousTestModeEnabled: boolean;
	let previousTrustClientIpHeader: boolean;
	let previousClientIpHeader: string;

	beforeAll(() => {
		setInjectedWorkerService(new NoopWorkerService());
	});

	beforeEach(() => {
		previousTestModeEnabled = Config.dev.testModeEnabled;
		previousTrustClientIpHeader = Config.proxy.trust_client_ip_header;
		previousClientIpHeader = Config.proxy.client_ip_header;
		Config.dev.testModeEnabled = false;
		Config.proxy.trust_client_ip_header = true;
		Config.proxy.client_ip_header = CLIENT_IP_HEADER_NAME;
	});

	afterEach(() => {
		Config.dev.testModeEnabled = previousTestModeEnabled;
		Config.proxy.trust_client_ip_header = previousTrustClientIpHeader;
		Config.proxy.client_ip_header = previousClientIpHeader;
	});

	it('serves the internal rpc route without a client ip header', async () => {
		const app = createProductionApp();
		const response = await app.request('http://api:8080/internal/rpc', {
			method: 'POST',
			headers: {'content-type': 'application/json'},
			body: '{}',
		});
		expect(response.status).toBe(200);
	});

	it('serves the internal rpc route with a client ip header', async () => {
		const app = createProductionApp();
		const response = await app.request('http://api:8080/internal/rpc', {
			method: 'POST',
			headers: {'content-type': 'application/json', [CLIENT_IP_HEADER_NAME]: '203.0.113.10'},
			body: '{}',
		});
		expect(response.status).toBe(200);
	});

	it('serves an exempt public route without a client ip header', async () => {
		const app = createProductionApp();
		const response = await app.request('http://api:8080/connections/bluesky/jwks.json');
		expect(response.status).toBe(200);
	});

	it('still rejects a non exempt route without a client ip header', async () => {
		const app = createProductionApp();
		const response = await app.request('http://api:8080/users/@me');
		expect(response.status).toBe(403);
		expect(await response.json()).toMatchObject({code: 'FORBIDDEN'});
	});
});
