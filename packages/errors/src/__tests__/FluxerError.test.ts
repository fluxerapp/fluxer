// SPDX-License-Identifier: AGPL-3.0-or-later

import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {HTTPException} from 'hono/http-exception';
import {describe, expect, it} from 'vitest';

describe('FluxerError', () => {
	it.each([
		{
			name: 'defaults the message to the code',
			create: () => new FluxerError({code: 'BAD_REQUEST', status: 400}),
			status: 400,
			body: {code: 'BAD_REQUEST', message: 'BAD_REQUEST'},
		},
		{
			name: 'uses a custom message',
			create: () => new FluxerError({code: 'SERVER_ERROR', message: 'Custom error message', status: 500}),
			status: 500,
			body: {code: 'SERVER_ERROR', message: 'Custom error message'},
		},
		{
			name: 'preserves an intentionally empty message',
			create: () => new FluxerError({code: 'FORBIDDEN', message: '', status: 403}),
			status: 403,
			body: {code: 'FORBIDDEN', message: ''},
		},
		{
			name: 'includes field error data',
			create: () =>
				new FluxerError({
					code: 'VALIDATION_ERROR',
					message: 'Validation failed',
					status: 400,
					data: {errors: [{path: 'email', code: 'INVALID', message: 'Invalid email'}]},
				}),
			status: 400,
			body: {
				code: 'VALIDATION_ERROR',
				message: 'Validation failed',
				errors: [{path: 'email', code: 'INVALID', message: 'Invalid email'}],
			},
		},
	])('$name in JSON serialization and the HTTP response', async ({create, status, body}) => {
		const error = create();
		const response = error.getResponse();
		expect(error.status).toBe(status);
		expect(error.toJSON()).toEqual(body);
		expect(response.status).toBe(status);
		expect(response.headers.get('Content-Type')).toBe('application/json');
		expect(await response.json()).toEqual(body);
	});

	it('preserves exception metadata and headers without leaking them into the body', async () => {
		const cause = new Error('Original error');
		const headers = {'Retry-After': '60', 'X-RateLimit-Reset': '1234567890'};
		const data = {retry_after: 60};
		const messageVariables = {retryAfter: 60};
		const error = new FluxerError({
			code: 'RATE_LIMITED',
			status: 429,
			cause,
			headers,
			data,
			messageVariables,
		});
		expect(error).toBeInstanceOf(Error);
		expect(error).toBeInstanceOf(HTTPException);
		expect(error.name).toBe('FluxerError');
		expect(error.code).toBe('RATE_LIMITED');
		expect(error.message).toBe('RATE_LIMITED');
		expect(error.cause).toBe(cause);
		expect(error.data).toEqual(data);
		expect(error.headers).toEqual(headers);
		expect(error.messageVariables).toEqual(messageVariables);

		const response = error.getResponse();
		expect(response.status).toBe(429);
		expect(response.headers.get('Content-Type')).toBe('application/json');
		expect(response.headers.get('Retry-After')).toBe('60');
		expect(response.headers.get('X-RateLimit-Reset')).toBe('1234567890');
		const body = {code: 'RATE_LIMITED', message: 'RATE_LIMITED', retry_after: 60};
		expect(error.toJSON()).toEqual(body);
		expect(await response.json()).toEqual(body);
	});
});
