// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InvalidPhoneNumberError} from '@fluxer/errors/src/domains/auth/InvalidPhoneNumberError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {InternalServerError} from '@fluxer/errors/src/domains/core/InternalServerError';
import {NotFoundError} from '@fluxer/errors/src/domains/core/NotFoundError';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {describe, expect, it} from 'vitest';

interface ErrorResponse {
	code: string;
	message: string;
	[key: string]: unknown;
}

describe('Domain Errors', () => {
	describe('core domain errors', () => {
		describe('BadRequestError', () => {
			it('should create error with required code', () => {
				const error = new BadRequestError({code: APIErrorCodes.INVALID_REQUEST});
				expect(error.status).toBe(400);
				expect(error.code).toBe(APIErrorCodes.INVALID_REQUEST);
				expect(error.message).toBe(APIErrorCodes.INVALID_REQUEST);
			});
			it('should allow custom message', () => {
				const error = new BadRequestError({
					code: APIErrorCodes.INVALID_FORM_BODY,
					message: 'Custom bad request message',
				});
				expect(error.message).toBe('Custom bad request message');
			});
			it('should include data', () => {
				const error = new BadRequestError({
					code: APIErrorCodes.INVALID_REQUEST,
					data: {field: 'test'},
				});
				expect(error.data).toEqual({field: 'test'});
			});
			it('should include headers', () => {
				const error = new BadRequestError({
					code: APIErrorCodes.INVALID_REQUEST,
					headers: {'X-Custom': 'value'},
				});
				expect(error.headers).toEqual({'X-Custom': 'value'});
			});
			it('should include messageVariables for i18n', () => {
				const error = new BadRequestError({
					code: APIErrorCodes.INVALID_REQUEST,
					messageVariables: {count: 5},
				});
				expect(error.messageVariables).toEqual({count: 5});
			});
			it('should be instance of FluxerError', () => {
				const error = new BadRequestError({code: APIErrorCodes.INVALID_REQUEST});
				expect(error).toBeInstanceOf(FluxerError);
			});
		});
		describe('NotFoundError', () => {
			it('should create error with status 404', () => {
				const error = new NotFoundError({code: APIErrorCodes.UNKNOWN_USER});
				expect(error.status).toBe(404);
				expect(error.code).toBe(APIErrorCodes.UNKNOWN_USER);
				expect(error.message).toBe(APIErrorCodes.UNKNOWN_USER);
			});
			it('should allow messageVariables for i18n', () => {
				const error = new NotFoundError({
					code: APIErrorCodes.UNKNOWN_USER,
					messageVariables: {userId: '12345'},
				});
				expect(error.messageVariables).toEqual({userId: '12345'});
			});
			it('should be instance of FluxerError', () => {
				const error = new NotFoundError({code: APIErrorCodes.UNKNOWN_USER});
				expect(error).toBeInstanceOf(FluxerError);
			});
		});
		describe('ForbiddenError', () => {
			it('should create error with status 403', () => {
				const error = new ForbiddenError({code: APIErrorCodes.ACCESS_DENIED});
				expect(error.status).toBe(403);
				expect(error.code).toBe(APIErrorCodes.ACCESS_DENIED);
				expect(error.message).toBe(APIErrorCodes.ACCESS_DENIED);
			});
			it('should be instance of FluxerError', () => {
				const error = new ForbiddenError({code: APIErrorCodes.ACCESS_DENIED});
				expect(error).toBeInstanceOf(FluxerError);
			});
		});
		describe('InternalServerError', () => {
			it('should create error with status 500', () => {
				const error = new InternalServerError({code: APIErrorCodes.GENERAL_ERROR});
				expect(error.status).toBe(500);
				expect(error.code).toBe(APIErrorCodes.GENERAL_ERROR);
				expect(error.message).toBe(APIErrorCodes.GENERAL_ERROR);
			});
			it('should be instance of FluxerError', () => {
				const error = new InternalServerError({code: APIErrorCodes.GENERAL_ERROR});
				expect(error).toBeInstanceOf(FluxerError);
			});
		});
		describe('InputValidationError', () => {
			it('should create error with validation errors', () => {
				const error = new InputValidationError([{path: 'email', message: 'Invalid email format'}]);
				expect(error.status).toBe(400);
				expect(error.code).toBe(APIErrorCodes.INVALID_FORM_BODY);
				expect(error.data).toEqual({
					errors: [{path: 'email', message: 'Invalid email format'}],
				});
			});
			it('should support localized errors', () => {
				const error = new InputValidationError(
					[{path: 'name', message: ValidationErrorCodes.EMAIL_IS_REQUIRED}],
					[{path: 'name', code: ValidationErrorCodes.EMAIL_IS_REQUIRED}],
				);
				expect(error.localizedErrors).toEqual([{path: 'name', code: ValidationErrorCodes.EMAIL_IS_REQUIRED}]);
				expect(error.getLocalizedErrors()).toEqual([{path: 'name', code: ValidationErrorCodes.EMAIL_IS_REQUIRED}]);
			});
			it('should return null for localizedErrors when not provided', () => {
				const error = new InputValidationError([{path: 'field', message: 'error'}]);
				expect(error.localizedErrors).toBeNull();
				expect(error.getLocalizedErrors()).toBeNull();
			});
			it('should create from single field using static method', () => {
				const error = InputValidationError.create('username', 'Username is required');
				expect(error).toBeInstanceOf(InputValidationError);
				expect(error.data).toEqual({
					errors: [{path: 'username', message: 'Username is required'}],
				});
			});
			it('should create from multiple fields using static method', () => {
				const error = InputValidationError.createMultiple([
					{path: 'email', message: 'Invalid email'},
					{path: 'password', message: 'Password too short'},
				]);
				expect(error.data).toEqual({
					errors: [
						{path: 'email', message: 'Invalid email'},
						{path: 'password', message: 'Password too short'},
					],
				});
			});
			it('should create from error code using static method', () => {
				const error = InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_IS_REQUIRED, {maxLength: 255});
				expect(error.localizedErrors).toEqual([
					{path: 'email', code: ValidationErrorCodes.EMAIL_IS_REQUIRED, variables: {maxLength: 255}},
				]);
			});
			it('should create from multiple error codes using static method', () => {
				const error = InputValidationError.fromCodes([
					{path: 'email', code: ValidationErrorCodes.EMAIL_IS_REQUIRED},
					{path: 'name', code: ValidationErrorCodes.STRING_LENGTH_INVALID, variables: {max: 100}},
				]);
				expect(error.localizedErrors).toHaveLength(2);
			});
		});
	});
	describe('auth domain errors', () => {
		describe('InvalidPhoneNumberError', () => {
			it('should have correct code from APIErrorCodes', () => {
				const error = new InvalidPhoneNumberError();
				expect(error.code).toBe(APIErrorCodes.INVALID_PHONE_NUMBER);
				expect(error.status).toBe(HttpStatus.BAD_REQUEST);
			});
			it('should be instance of BadRequestError', () => {
				const error = new InvalidPhoneNumberError();
				expect(error).toBeInstanceOf(BadRequestError);
			});
			it('should be instance of FluxerError', () => {
				const error = new InvalidPhoneNumberError();
				expect(error).toBeInstanceOf(FluxerError);
			});
		});
	});
	describe('channel domain errors', () => {
		describe('UnknownChannelError', () => {
			it('should have correct code from APIErrorCodes', () => {
				const error = new UnknownChannelError();
				expect(error.code).toBe(APIErrorCodes.UNKNOWN_CHANNEL);
				expect(error.status).toBe(HttpStatus.NOT_FOUND);
			});
			it('should be instance of NotFoundError', () => {
				const error = new UnknownChannelError();
				expect(error).toBeInstanceOf(NotFoundError);
			});
			it('should be instance of FluxerError', () => {
				const error = new UnknownChannelError();
				expect(error).toBeInstanceOf(FluxerError);
			});
		});
		describe('UnknownMessageError', () => {
			it('should have correct code from APIErrorCodes', () => {
				const error = new UnknownMessageError();
				expect(error.code).toBe(APIErrorCodes.UNKNOWN_MESSAGE);
				expect(error.status).toBe(HttpStatus.NOT_FOUND);
			});
			it('should be instance of NotFoundError', () => {
				const error = new UnknownMessageError();
				expect(error).toBeInstanceOf(NotFoundError);
			});
		});
	});
	describe('error response generation', () => {
		it('should generate correct JSON response for domain errors', async () => {
			const error = new UnknownChannelError();
			const response = error.getResponse();
			expect(response.status).toBe(404);
			expect(response.headers.get('Content-Type')).toBe('application/json');
			const body = (await response.json()) as ErrorResponse;
			expect(body.code).toBe(APIErrorCodes.UNKNOWN_CHANNEL);
		});
		it('should include data in response', async () => {
			const error = new BadRequestError({
				code: APIErrorCodes.INVALID_REQUEST,
				data: {field: 'test', reason: 'invalid'},
			});
			const response = error.getResponse();
			const body = (await response.json()) as ErrorResponse;
			expect(body).toEqual({
				code: APIErrorCodes.INVALID_REQUEST,
				message: APIErrorCodes.INVALID_REQUEST,
				field: 'test',
				reason: 'invalid',
			});
		});
		it('should include custom headers in response', async () => {
			const error = new ForbiddenError({
				code: APIErrorCodes.ACCESS_DENIED,
				headers: {'X-Permission-Required': 'admin'},
			});
			const response = error.getResponse();
			expect(response.headers.get('X-Permission-Required')).toBe('admin');
		});
	});
	describe('error serialization', () => {
		it('should serialize domain errors to JSON correctly', () => {
			const error = new UnknownChannelError();
			const json = error.toJSON();
			expect(json).toEqual({
				code: APIErrorCodes.UNKNOWN_CHANNEL,
				message: APIErrorCodes.UNKNOWN_CHANNEL,
			});
		});
		it('should include data in JSON serialization', () => {
			const error = new BadRequestError({
				code: APIErrorCodes.INVALID_REQUEST,
				data: {extra: 'info'},
			});
			const json = error.toJSON();
			expect(json).toEqual({
				code: APIErrorCodes.INVALID_REQUEST,
				message: APIErrorCodes.INVALID_REQUEST,
				extra: 'info',
			});
		});
	});
	describe('error inheritance chain', () => {
		it('should maintain correct prototype chain', () => {
			const error = new InvalidPhoneNumberError();
			expect(error).toBeInstanceOf(InvalidPhoneNumberError);
			expect(error).toBeInstanceOf(BadRequestError);
			expect(error).toBeInstanceOf(FluxerError);
			expect(error).toBeInstanceOf(Error);
		});
		it('should be catchable at any level of the chain', () => {
			const error = new UnknownChannelError();
			try {
				throw error;
			} catch (e) {
				if (e instanceof NotFoundError) {
					expect(e.code).toBe(APIErrorCodes.UNKNOWN_CHANNEL);
				}
			}
			try {
				throw error;
			} catch (e) {
				if (e instanceof FluxerError) {
					expect(e.status).toBe(404);
				}
			}
			try {
				throw error;
			} catch (e) {
				if (e instanceof Error) {
					expect(e).toBeInstanceOf(UnknownChannelError);
				}
			}
		});
	});
});
