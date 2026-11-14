// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Locales} from '@fluxer/constants/src/Locales';
import {AppErrorHandler, type BaseHonoEnv} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {Hono} from 'hono';
import {describe, expect, it} from 'vitest';

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
	])('$name without leaking the internal message', async ({acceptLanguage, requestLocale, requestId, message}) => {
		const error = new Error('Private internal failure');
		const headers = new Headers();
		if (acceptLanguage !== undefined) {
			headers.set('accept-language', acceptLanguage);
		}
		const response = await createApp(error, requestLocale, requestId).request('/test', {headers});
		expect(response.status).toBe(500);
		expect(await response.json()).toEqual({code: APIErrorCodes.INTERNAL_SERVER_ERROR, message});
	});
});
