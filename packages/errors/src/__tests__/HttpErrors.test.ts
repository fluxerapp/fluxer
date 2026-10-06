// SPDX-License-Identifier: AGPL-3.0-or-later

import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {
	BadGatewayError,
	BadRequestError,
	ConflictError,
	ForbiddenError,
	GatewayTimeoutError,
	GoneError,
	InternalServerError,
	MethodNotAllowedError,
	NotFoundError,
	NotImplementedError,
	ServiceUnavailableError,
	UnauthorizedError,
} from '@fluxer/errors/src/HttpErrors';
import {describe, expect, it} from 'vitest';

describe.each([
	['BAD_REQUEST', BadRequestError, HttpStatus.BAD_REQUEST, 'Bad Request'],
	['UNAUTHORIZED', UnauthorizedError, HttpStatus.UNAUTHORIZED, 'Unauthorized'],
	['FORBIDDEN', ForbiddenError, HttpStatus.FORBIDDEN, 'Forbidden'],
	['NOT_FOUND', NotFoundError, HttpStatus.NOT_FOUND, 'Not Found'],
	['METHOD_NOT_ALLOWED', MethodNotAllowedError, HttpStatus.METHOD_NOT_ALLOWED, 'Method Not Allowed'],
	['CONFLICT', ConflictError, HttpStatus.CONFLICT, 'Conflict'],
	['GONE', GoneError, HttpStatus.GONE, 'Gone'],
	['INTERNAL_SERVER_ERROR', InternalServerError, HttpStatus.INTERNAL_SERVER_ERROR, 'Internal Server Error'],
	['NOT_IMPLEMENTED', NotImplementedError, HttpStatus.NOT_IMPLEMENTED, 'Not Implemented'],
	['BAD_GATEWAY', BadGatewayError, HttpStatus.BAD_GATEWAY, 'Bad Gateway'],
	['SERVICE_UNAVAILABLE', ServiceUnavailableError, HttpStatus.SERVICE_UNAVAILABLE, 'Service Unavailable'],
	['GATEWAY_TIMEOUT', GatewayTimeoutError, HttpStatus.GATEWAY_TIMEOUT, 'Gateway Timeout'],
] as const)('%s', (code, ErrorClass, status, message) => {
	it('serializes the default error with its HTTP status', async () => {
		const error = new ErrorClass();
		const response = error.getResponse();

		expect(error).toBeInstanceOf(FluxerError);
		expect(error).toBeInstanceOf(Error);
		expect(error).toMatchObject({name: ErrorClass.name, status, code, message});
		expect(error.toJSON()).toEqual({code, message});
		expect(response.status).toBe(status);
		expect(response.headers.get('Content-Type')).toBe('application/json');
		expect(await response.json()).toEqual({code, message});
	});

	it('forwards overrides without leaking the cause into the response', async () => {
		const cause = new Error('Upstream operation failed');
		const error = new ErrorClass({
			code: 'CUSTOM_ERROR',
			message: 'Request could not be completed',
			data: {field: 'email'},
			headers: {'X-Error-Type': 'validation'},
			cause,
		});
		const response = error.getResponse();

		expect(error.status).toBe(status);
		expect(error.cause).toBe(cause);
		expect(error.data).toEqual({field: 'email'});
		expect(response.headers.get('X-Error-Type')).toBe('validation');
		expect(await response.json()).toEqual({
			code: 'CUSTOM_ERROR',
			message: 'Request could not be completed',
			field: 'email',
		});
	});

	it('keeps the default message when only the code changes', () => {
		expect(new ErrorClass({code: 'CUSTOM_ERROR'}).toJSON()).toEqual({code: 'CUSTOM_ERROR', message});
	});

	it('keeps the default code when only the message changes', () => {
		expect(new ErrorClass({message: 'Custom message'}).toJSON()).toEqual({code, message: 'Custom message'});
	});

	it('does not replace explicitly empty overrides with defaults', () => {
		expect(new ErrorClass({code: '', message: ''}).toJSON()).toEqual({code: '', message: ''});
	});
});
