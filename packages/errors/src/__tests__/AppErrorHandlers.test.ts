// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Locales} from '@fluxer/constants/src/Locales';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import {AppErrorHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {getErrorMessage} from '@fluxer/errors/src/i18n/ErrorI18n';
import type {BaseHonoEnv} from '@fluxer/hono_types/src/HonoTypes';
import {Logger} from '@fluxer/logger/src/Logger';
import {Hono} from 'hono';
import {afterEach, describe, expect, it, vi} from 'vitest';

afterEach(() => {
	vi.restoreAllMocks();
});

function createApp(error: Error, requestLocale?: string, requestId?: string): Hono<BaseHonoEnv> {
	const app = new Hono<BaseHonoEnv>();
	app.onError(AppErrorHandler);
	app.use('*', async (ctx, next) => {
		ctx.set('requestLocale', requestLocale);
		ctx.set('requestId', requestId);
		await next();
	});
	app.get('/test', () => {
		throw error;
	});
	return app;
}

describe('AppErrorHandler i18n fallbacks', () => {
	it.each([
		{
			name: 'uses Accept-Language when middleware locale is missing',
			acceptLanguage: 'fr-CA,fr;q=0.9,en;q=0.8',
			requestLocale: undefined,
			requestId: undefined,
			message: 'Erreur interne du serveur.',
		},
		{
			name: 'prefers the middleware locale over Accept-Language',
			acceptLanguage: 'fr',
			requestLocale: Locales.EN_US,
			requestId: 'request-123',
			message: 'Internal server error.',
		},
		{
			name: 'defaults to English without a locale or language header',
			acceptLanguage: undefined,
			requestLocale: undefined,
			requestId: undefined,
			message: 'Internal server error.',
		},
	])('$name and logs unexpected errors with request metadata', async ({
		acceptLanguage,
		requestLocale,
		requestId,
		message,
	}) => {
		const errorLogger = vi.spyOn(Logger.prototype, 'error').mockImplementation(() => {});
		const error = new Error('Private internal failure');
		const headers = new Headers();
		if (acceptLanguage !== undefined) {
			headers.set('accept-language', acceptLanguage);
		}
		const response = await createApp(error, requestLocale, requestId).request('/test', {headers});
		expect(response.status).toBe(500);
		expect(await response.json()).toEqual({code: APIErrorCodes.INTERNAL_SERVER_ERROR, message});
		expect(errorLogger).toHaveBeenCalledExactlyOnceWith(
			{err: error, status: 500, method: 'GET', path: '/test', requestId},
			'Unhandled error occurred',
		);
	});

	it('localizes FluxerError responses without an i18n service and logs them as expected rejections', async () => {
		const errorLogger = vi.spyOn(Logger.prototype, 'error').mockImplementation(() => {});
		const debugLogger = vi.spyOn(Logger.prototype, 'debug').mockImplementation(() => {});
		const error = new BadRequestError({code: APIErrorCodes.BAD_REQUEST});
		const response = await createApp(error).request('/test', {headers: {'accept-language': 'fr'}});
		expect(response.status).toBe(400);
		expect(await response.json()).toEqual({
			code: APIErrorCodes.BAD_REQUEST,
			message: getErrorMessage('http.bad_request', 'fr'),
		});
		expect(errorLogger).not.toHaveBeenCalled();
		expect(debugLogger).toHaveBeenCalledExactlyOnceWith(
			{err: error, status: 400, method: 'GET', path: '/test', requestId: undefined},
			'Request rejected',
		);
	});
});
