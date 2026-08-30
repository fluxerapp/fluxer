// SPDX-License-Identifier: AGPL-3.0-or-later

import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';
import {IpBannedError} from '@fluxer/errors/src/domains/moderation/IpBannedError';
import {Hono} from 'hono';
import {beforeEach, describe, expect, it} from 'vitest';
import {NoopLogger} from '../../test/mocks/NoopLogger';
import type {HonoEnv} from '../../types/HonoEnv';
import type {ClientIpResolution} from '../../utils/RequestClientIp';
import {IpBanMiddleware, ipBanCache} from '../IpBanMiddleware';
import {torExitListCache} from '../TorExitListCache';
import {TorExitMiddleware} from '../TorExitMiddleware';
import {TrustedClientIpHeaderMiddleware} from '../TrustedClientIpHeaderMiddleware';

interface Pipeline {
	request: (headers: Record<string, string>) => Promise<Response>;
	resolutions: Array<ClientIpResolution | undefined>;
	errors: Array<unknown>;
}

function createPipeline(clientIpHeaderName = 'x-forwarded-for'): Pipeline {
	const resolutions: Array<ClientIpResolution | undefined> = [];
	const errors: Array<unknown> = [];
	const app = new Hono<HonoEnv>();
	app.use(IpBanMiddleware);
	app.use(async (ctx, next) => {
		resolutions.push(ctx.get('clientIpResolution'));
		await next();
	});
	app.use(
		TrustedClientIpHeaderMiddleware({
			enabled: true,
			logger: new NoopLogger(),
			trustClientIpHeader: true,
			clientIpHeaderName,
		}),
	);
	app.use(TorExitMiddleware);
	app.get('/v1/messages', (ctx) => {
		resolutions.push(ctx.get('clientIpResolution'));
		return ctx.text('ok');
	});
	app.onError((error) => {
		errors.push(error);
		return new Response('error', {status: 403});
	});
	return {
		request: async (headers) => app.request('http://localhost/v1/messages', {headers}),
		resolutions,
		errors,
	};
}

beforeEach(() => {
	ipBanCache.resetCaches();
	torExitListCache.clearForTesting();
});

describe('client ip resolution across the request pipeline', () => {
	it('resolves once and shares that resolution with every later middleware', async () => {
		const pipeline = createPipeline();
		const response = await pipeline.request({'x-forwarded-for': '203.0.113.10, 10.0.0.1'});
		expect(response.status).toBe(200);
		expect(pipeline.resolutions).toHaveLength(2);
		expect(pipeline.resolutions[0]?.ip).toBe('203.0.113.10');
		expect(pipeline.resolutions[1]).toBe(pipeline.resolutions[0]);
	});
	it('still blocks a banned client ip', async () => {
		ipBanCache.ban('203.0.113.20');
		const pipeline = createPipeline();
		const response = await pipeline.request({'x-forwarded-for': '203.0.113.20'});
		expect(response.status).toBe(403);
		expect(pipeline.errors[0]).toBeInstanceOf(IpBannedError);
	});
	it('still blocks a tor exit client ip', async () => {
		torExitListCache.seedForTesting(['203.0.113.30']);
		const pipeline = createPipeline();
		const response = await pipeline.request({'x-forwarded-for': '203.0.113.30'});
		expect(response.status).toBe(403);
		expect(pipeline.errors[0]).toBeInstanceOf(IpBannedError);
	});
	it('rejects a malformed client ip header after the ban check saw no address', async () => {
		const pipeline = createPipeline();
		const response = await pipeline.request({'x-forwarded-for': 'not-an-ip'});
		expect(response.status).toBe(403);
		expect(pipeline.resolutions[0]?.ip).toBe(null);
		expect(pipeline.errors[0]).toBeInstanceOf(ForbiddenError);
		expect(pipeline.errors[0]).not.toBeInstanceOf(IpBannedError);
	});
	it('passes requests through when no client ip header is present', async () => {
		const pipeline = createPipeline();
		const response = await pipeline.request({});
		expect(response.status).toBe(200);
		expect(pipeline.resolutions[0]?.ip).toBe(null);
		expect(pipeline.resolutions[1]).toBe(pipeline.resolutions[0]);
	});
	it('keeps ban checks on the configured header when the trusted header check uses another one', async () => {
		ipBanCache.ban('198.51.100.7');
		const pipeline = createPipeline('x-real-ip');
		const response = await pipeline.request({'x-forwarded-for': '203.0.113.10', 'x-real-ip': '198.51.100.7'});
		expect(response.status).toBe(200);
		expect(pipeline.resolutions[0]?.ip).toBe('203.0.113.10');
		expect(pipeline.resolutions[1]?.ip).toBe('203.0.113.10');
	});
	it('rejects an invalid trusted header even when the configured header carries a valid address', async () => {
		const pipeline = createPipeline('x-real-ip');
		const response = await pipeline.request({'x-forwarded-for': '203.0.113.10', 'x-real-ip': 'not-an-ip'});
		expect(response.status).toBe(403);
		expect(pipeline.errors[0]).toBeInstanceOf(ForbiddenError);
	});
});
