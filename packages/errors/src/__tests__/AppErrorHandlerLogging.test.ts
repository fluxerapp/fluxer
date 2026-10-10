// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {AppErrorHandler, type BaseHonoEnv} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {ServiceUnavailableError} from '@fluxer/errors/src/HttpErrors';
import {Hono} from 'hono';
import {beforeEach, describe, expect, it, vi} from 'vitest';

type LogCall = [Record<string, unknown>, string | undefined];

const logCalls = vi.hoisted(() => ({
	error: [] as Array<LogCall>,
}));

vi.mock('@fluxer/logger/src/Logger', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@fluxer/logger/src/Logger')>();
	return {
		...actual,
		createLogger: () => ({
			trace: () => {},
			debug: () => {},
			info: () => {},
			warn: () => {},
			error: (obj: Record<string, unknown>, msg?: string) => {
				logCalls.error.push([obj, msg]);
			},
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

	it('logs the matched route pattern instead of the request path', async () => {
		const app = createApp();
		app.get('/reset/:token', () => {
			throw new ServiceUnavailableError({message: 'unavailable'});
		});
		const response = await app.request('/reset/abc123');
		expect(response.status).toBe(503);
		expect(logCalls.error).toHaveLength(1);
		expect(logCalls.error[0]![0].path).toBe('/reset/:token');
	});
});
