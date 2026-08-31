// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Locales} from '@fluxer/constants/src/Locales';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import {AppErrorHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {getErrorMessage} from '@fluxer/errors/src/i18n/ErrorI18n';
import type {BaseHonoEnv} from '@fluxer/hono_types/src/HonoTypes';
import {Logger} from '@fluxer/logger/src/Logger';
import {Hono} from 'hono';
import {describe, expect, it, vi} from 'vitest';

interface ErrorResponse {
	code: string;
	message: string;
}

function createApp(): Hono<BaseHonoEnv> {
	const app = new Hono<BaseHonoEnv>();
	app.onError(AppErrorHandler);
	return app;
}

describe('AppErrorHandler i18n fallbacks', () => {
	it('localizes unexpected errors from Accept-Language when middleware locale is missing', async () => {
		const errorLoggerSpy = vi.spyOn(Logger.prototype, 'error').mockImplementation(() => {});
		const error = new Error('boom');
		const app = createApp();
		app.get('/test', () => {
			throw error;
		});
		try {
			const response = await app.request('/test', {
				headers: {
					'accept-language': 'fr-CA,fr;q=0.9,en;q=0.8',
				},
			});
			expect(response.status).toBe(500);
			const body = (await response.json()) as ErrorResponse;
			expect(body.code).toBe(APIErrorCodes.INTERNAL_SERVER_ERROR);
			expect(body.message).toBe('Erreur interne du serveur.');
			expect(errorLoggerSpy).toHaveBeenCalledTimes(1);
			expect(errorLoggerSpy).toHaveBeenCalledWith(
				{err: error, status: 500, method: 'GET', path: '/test', requestId: undefined},
				'Unhandled error occurred',
			);
		} finally {
			errorLoggerSpy.mockRestore();
		}
	});
	it('localizes FluxerError responses without errorI18nService in context', async () => {
		const app = createApp();
		app.get('/test', () => {
			throw new BadRequestError({code: APIErrorCodes.BAD_REQUEST});
		});
		const response = await app.request('/test', {
			headers: {
				'accept-language': 'fr',
			},
		});
		expect(response.status).toBe(400);
		const body = (await response.json()) as ErrorResponse;
		expect(body.code).toBe(APIErrorCodes.BAD_REQUEST);
		expect(body.message).toBe(getErrorMessage('http.bad_request', 'fr'));
	});
	it('prefers requestLocale context over Accept-Language header', async () => {
		const errorLoggerSpy = vi.spyOn(Logger.prototype, 'error').mockImplementation(() => {});
		const error = new Error('boom');
		const app = createApp();
		app.use('*', async (ctx, next) => {
			ctx.set('requestLocale', Locales.EN_US);
			await next();
		});
		app.get('/test', () => {
			throw error;
		});
		try {
			const response = await app.request('/test', {
				headers: {
					'accept-language': 'fr',
				},
			});
			expect(response.status).toBe(500);
			const body = (await response.json()) as ErrorResponse;
			expect(body.code).toBe(APIErrorCodes.INTERNAL_SERVER_ERROR);
			expect(body.message).toBe('Internal server error.');
			expect(errorLoggerSpy).toHaveBeenCalledTimes(1);
			expect(errorLoggerSpy).toHaveBeenCalledWith(
				{err: error, status: 500, method: 'GET', path: '/test', requestId: undefined},
				'Unhandled error occurred',
			);
		} finally {
			errorLoggerSpy.mockRestore();
		}
	});
	it('returns 500 for unexpected errors with request metadata', async () => {
		const errorLoggerSpy = vi.spyOn(Logger.prototype, 'error').mockImplementation(() => {});
		const error = new Error('boom');
		const app = createApp();
		app.get('/test', () => {
			throw error;
		});
		try {
			const response = await app.request('/test');
			expect(response.status).toBe(500);
			const body = (await response.json()) as ErrorResponse;
			expect(body.code).toBe(APIErrorCodes.INTERNAL_SERVER_ERROR);
			expect(errorLoggerSpy).toHaveBeenCalledTimes(1);
			expect(errorLoggerSpy).toHaveBeenCalledWith(
				{err: error, status: 500, method: 'GET', path: '/test', requestId: undefined},
				'Unhandled error occurred',
			);
		} finally {
			errorLoggerSpy.mockRestore();
		}
	});
});
