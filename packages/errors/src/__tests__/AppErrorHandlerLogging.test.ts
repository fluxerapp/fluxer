// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import {AppErrorHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {ServiceUnavailableError} from '@fluxer/errors/src/HttpErrors';
import type {BaseHonoEnv} from '@fluxer/hono_types/src/HonoTypes';
import {Hono} from 'hono';
import {beforeEach, describe, expect, it, vi} from 'vitest';

type LogCall = [Record<string, unknown>, string | undefined];

const logCalls = vi.hoisted(() => ({
	debug: [] as Array<LogCall>,
	warn: [] as Array<LogCall>,
	error: [] as Array<LogCall>,
}));

vi.mock('@fluxer/logger/src/Logger', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@fluxer/logger/src/Logger')>();
	const record = (bucket: Array<LogCall>) => (obj: Record<string, unknown>, msg?: string) => {
		bucket.push([obj, msg]);
	};
	return {
		...actual,
		createLogger: () => ({
			trace: () => {},
			debug: record(logCalls.debug),
			info: () => {},
			warn: record(logCalls.warn),
			error: record(logCalls.error),
			fatal: () => {},
		}),
	};
});

function createApp(): Hono<BaseHonoEnv> {
	const app = new Hono<BaseHonoEnv>();
	app.onError(AppErrorHandler);
	return app;
}

describe('AppErrorHandler logging', () => {
	beforeEach(() => {
		logCalls.debug.length = 0;
		logCalls.warn.length = 0;
		logCalls.error.length = 0;
	});

	it('logs 5xx FluxerErrors with the underlying cause', async () => {
		const cause = new Error('connect ECONNREFUSED 127.0.0.1:9000');
		const app = createApp();
		app.use('*', async (ctx, next) => {
			ctx.set('requestId', 'req-1');
			await next();
		});
		app.post('/messages', () => {
			throw new ServiceUnavailableError({
				message: 'Attachment storage is temporarily unavailable',
				cause,
			});
		});
		const response = await app.request('/messages', {method: 'POST'});
		expect(response.status).toBe(503);
		expect(logCalls.error).toHaveLength(1);
		const [details, message] = logCalls.error[0]!;
		expect(message).toBe('Request failed');
		expect(details.status).toBe(503);
		expect(details.method).toBe('POST');
		expect(details.path).toBe('/messages');
		expect(details.requestId).toBe('req-1');
		const loggedError = details.err as ServiceUnavailableError;
		expect(loggedError.message).toBe('Attachment storage is temporarily unavailable');
		expect(loggedError.code).toBe(APIErrorCodes.SERVICE_UNAVAILABLE);
		expect(loggedError.cause).toBe(cause);
	});

	it('logs 4xx FluxerErrors at debug rather than error', async () => {
		const app = createApp();
		app.get('/thing', () => {
			throw new BadRequestError({code: APIErrorCodes.BAD_REQUEST});
		});
		const response = await app.request('/thing');
		expect(response.status).toBe(400);
		expect(logCalls.error).toHaveLength(0);
		expect(logCalls.debug).toHaveLength(1);
		const [details, message] = logCalls.debug[0]!;
		expect(message).toBe('Request rejected');
		expect(details.status).toBe(400);
	});

	it('still reports unexpected errors as unhandled', async () => {
		const app = createApp();
		app.get('/thing', () => {
			throw new Error('boom');
		});
		const response = await app.request('/thing');
		expect(response.status).toBe(500);
		expect(logCalls.error).toHaveLength(1);
		expect(logCalls.error[0]![1]).toBe('Unhandled error occurred');
	});
});
