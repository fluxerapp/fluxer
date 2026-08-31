// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppErrorHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {Logger} from '@fluxer/logger/src/Logger';
import {Hono} from 'hono';
import {describe, expect, test, vi} from 'vitest';
import type {HonoEnv} from '../../types/HonoEnv';
import {ConcurrencyLimitMiddleware} from '../ConcurrencyLimitMiddleware';

interface Deferred {
	promise: Promise<void>;
	resolve: () => void;
}

function createDeferred(): Deferred {
	let resolve: () => void = () => undefined;
	const promise = new Promise<void>((resolveFn) => {
		resolve = resolveFn;
	});
	return {promise, resolve};
}

function buildApp(maxInflightRequests: number) {
	const release = createDeferred();
	const entered = createDeferred();
	const boomError = new Error('boom');
	const routes = new Hono<HonoEnv>({strict: true});
	routes.use(ConcurrencyLimitMiddleware({maxInflightRequests}));
	routes.get('/_health', (ctx) => ctx.text('OK'));
	routes.get('/_metrics', (ctx) => ctx.text('metrics'));
	routes.get('/fast', (ctx) => ctx.text('fast'));
	routes.get('/slow', async (ctx) => {
		entered.resolve();
		await release.promise;
		return ctx.text('slow');
	});
	routes.get('/boom', () => {
		throw boomError;
	});
	routes.onError(AppErrorHandler);
	const app = new Hono<HonoEnv>({strict: true});
	app.route('/v1', routes);
	app.route('/', routes);
	app.onError(AppErrorHandler);
	return {app, release, entered, boomError};
}

describe('ConcurrencyLimitMiddleware', () => {
	test('sheds with 503 once the in-flight ceiling is reached', async () => {
		const {app, release, entered} = buildApp(1);
		const errorLoggerSpy = vi.spyOn(Logger.prototype, 'error').mockImplementation(() => {});
		const inflight = app.request('/slow');
		try {
			await entered.promise;

			const shed = await app.request('/fast');

			expect(shed.status).toBe(503);
			expect(shed.headers.get('Retry-After')).toBe('1');
			expect(await shed.json()).toMatchObject({code: 'SERVICE_UNAVAILABLE'});
			expect(errorLoggerSpy).toHaveBeenCalledTimes(1);
			expect(errorLoggerSpy).toHaveBeenCalledWith(
				{
					err: expect.any(ServiceUnavailableError),
					method: 'GET',
					path: '/fast',
					requestId: undefined,
					status: 503,
				},
				'Request failed',
			);

			release.resolve();
			expect((await inflight).status).toBe(200);
		} finally {
			release.resolve();
			errorLoggerSpy.mockRestore();
		}
	});

	test('admits probe paths while shedding everything else', async () => {
		const {app, release, entered} = buildApp(1);
		const errorLoggerSpy = vi.spyOn(Logger.prototype, 'error').mockImplementation(() => {});
		const inflight = app.request('/slow');
		try {
			await entered.promise;

			expect((await app.request('/_health')).status).toBe(200);
			expect((await app.request('/_metrics')).status).toBe(200);
			expect((await app.request('/v1/_health')).status).toBe(200);
			expect((await app.request('/fast')).status).toBe(503);
			expect(errorLoggerSpy).toHaveBeenCalledTimes(1);
			expect(errorLoggerSpy).toHaveBeenCalledWith(
				{
					err: expect.any(ServiceUnavailableError),
					method: 'GET',
					path: '/fast',
					requestId: undefined,
					status: 503,
				},
				'Request failed',
			);

			release.resolve();
			await inflight;
		} finally {
			release.resolve();
			errorLoggerSpy.mockRestore();
		}
	});

	test('releases the slot when the handler throws', async () => {
		const {app, boomError} = buildApp(1);
		const errorLoggerSpy = vi.spyOn(Logger.prototype, 'error').mockImplementation(() => {});
		try {
			for (let attempt = 0; attempt < 3; attempt += 1) {
				expect((await app.request('/boom')).status).toBe(500);
			}
			expect((await app.request('/fast')).status).toBe(200);
			expect(errorLoggerSpy).toHaveBeenCalledTimes(3);
			for (let callIndex = 1; callIndex <= 3; callIndex += 1) {
				expect(errorLoggerSpy).toHaveBeenNthCalledWith(
					callIndex,
					{err: boomError, method: 'GET', path: '/boom', requestId: undefined, status: 500},
					'Unhandled error occurred',
				);
			}
		} finally {
			errorLoggerSpy.mockRestore();
		}
	});

	test('releases the slot taken by a shed request', async () => {
		const {app, release, entered} = buildApp(1);
		const errorLoggerSpy = vi.spyOn(Logger.prototype, 'error').mockImplementation(() => {});
		const inflight = app.request('/slow');
		try {
			await entered.promise;

			expect((await app.request('/fast')).status).toBe(503);
			expect((await app.request('/fast')).status).toBe(503);
			expect(errorLoggerSpy).toHaveBeenCalledTimes(2);
			for (let callIndex = 1; callIndex <= 2; callIndex += 1) {
				expect(errorLoggerSpy).toHaveBeenNthCalledWith(
					callIndex,
					{
						err: expect.any(ServiceUnavailableError),
						method: 'GET',
						path: '/fast',
						requestId: undefined,
						status: 503,
					},
					'Request failed',
				);
			}

			release.resolve();
			await inflight;

			expect((await app.request('/fast')).status).toBe(200);
		} finally {
			release.resolve();
			errorLoggerSpy.mockRestore();
		}
	});
});
