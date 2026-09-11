// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InvalidPhoneNumberError} from '@fluxer/errors/src/domains/auth/InvalidPhoneNumberError';
import {PhoneCountryNotSupportedError} from '@fluxer/errors/src/domains/auth/PhoneCountryNotSupportedError';
import {PhoneInboundVerificationRequiredError} from '@fluxer/errors/src/domains/auth/PhoneInboundVerificationRequiredError';
import {PhoneLookupUnavailableError} from '@fluxer/errors/src/domains/auth/PhoneLookupUnavailableError';
import {PhoneNumberNotInServiceError} from '@fluxer/errors/src/domains/auth/PhoneNumberNotInServiceError';
import {PhoneNumberNotMobileError} from '@fluxer/errors/src/domains/auth/PhoneNumberNotMobileError';
import {PhoneVerificationNeedsReviewError} from '@fluxer/errors/src/domains/auth/PhoneVerificationNeedsReviewError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {InternalServerError} from '@fluxer/errors/src/domains/core/InternalServerError';
import {NotFoundError} from '@fluxer/errors/src/domains/core/NotFoundError';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {describe, expect, it} from 'vitest';

describe.each([
	[APIErrorCodes.INVALID_REQUEST, BadRequestError, HttpStatus.BAD_REQUEST],
	[APIErrorCodes.ACCESS_DENIED, ForbiddenError, HttpStatus.FORBIDDEN],
	[APIErrorCodes.UNKNOWN_USER, NotFoundError, HttpStatus.NOT_FOUND],
	[APIErrorCodes.GENERAL_ERROR, InternalServerError, HttpStatus.INTERNAL_SERVER_ERROR],
] as const)('%s', (code, ErrorClass, status) => {
	it('uses the domain code as its default message', async () => {
		const error = new ErrorClass({code});
		const response = error.getResponse();

		expect(error).toBeInstanceOf(FluxerError);
		expect(error).toMatchObject({status, code, message: code});
		expect(response.status).toBe(status);
		expect(response.headers.get('Content-Type')).toBe('application/json');
		expect(error.toJSON()).toEqual({code, message: code});
		expect(await response.json()).toEqual({code, message: code});
	});

	it('preserves data, headers, and localization variables', async () => {
		const error = new ErrorClass({
			code,
			data: {field: 'email'},
			headers: {'X-Permission-Required': 'admin'},
			messageVariables: {count: 5},
		});
		const response = error.getResponse();

		expect(error.messageVariables).toEqual({count: 5});
		expect(response.headers.get('X-Permission-Required')).toBe('admin');
		expect(await response.json()).toEqual({code, message: code, field: 'email'});
	});
});

it.each([
	BadRequestError,
	ForbiddenError,
	NotFoundError,
])('preserves custom messages for client errors', (ErrorClass) => {
	const error = new ErrorClass({code: APIErrorCodes.INVALID_REQUEST, message: 'Custom error message'});

	expect(error.toJSON()).toEqual({code: APIErrorCodes.INVALID_REQUEST, message: 'Custom error message'});
});

describe.each([
	[APIErrorCodes.INVALID_PHONE_NUMBER, InvalidPhoneNumberError, BadRequestError, HttpStatus.BAD_REQUEST],
	[APIErrorCodes.PHONE_COUNTRY_NOT_SUPPORTED, PhoneCountryNotSupportedError, BadRequestError, HttpStatus.BAD_REQUEST],
	[
		APIErrorCodes.PHONE_INBOUND_VERIFICATION_REQUIRED,
		PhoneInboundVerificationRequiredError,
		BadRequestError,
		HttpStatus.BAD_REQUEST,
	],
	[APIErrorCodes.PHONE_LOOKUP_UNAVAILABLE, PhoneLookupUnavailableError, BadRequestError, HttpStatus.BAD_REQUEST],
	[APIErrorCodes.PHONE_NUMBER_NOT_IN_SERVICE, PhoneNumberNotInServiceError, BadRequestError, HttpStatus.BAD_REQUEST],
	[APIErrorCodes.PHONE_NUMBER_NOT_MOBILE, PhoneNumberNotMobileError, BadRequestError, HttpStatus.BAD_REQUEST],
	[
		APIErrorCodes.PHONE_VERIFICATION_NEEDS_REVIEW,
		PhoneVerificationNeedsReviewError,
		BadRequestError,
		HttpStatus.BAD_REQUEST,
	],
	[APIErrorCodes.UNKNOWN_CHANNEL, UnknownChannelError, NotFoundError, HttpStatus.NOT_FOUND],
	[APIErrorCodes.UNKNOWN_MESSAGE, UnknownMessageError, NotFoundError, HttpStatus.NOT_FOUND],
] as const)('%s', (code, ErrorClass, BaseClass, status) => {
	it('preserves the domain inheritance and response contract', async () => {
		const error = new ErrorClass();
		const response = error.getResponse();

		expect(error).toBeInstanceOf(ErrorClass);
		expect(error).toBeInstanceOf(BaseClass);
		expect(error).toBeInstanceOf(FluxerError);
		expect(error).toBeInstanceOf(Error);
		expect(error).toMatchObject({code, status});
		expect(error.toJSON()).toEqual({code, message: code});
		expect(response.status).toBe(status);
		expect(await response.json()).toEqual({code, message: code});
	});
});

describe('InputValidationError', () => {
	const errors = [
		{path: 'email', message: 'Invalid email'},
		{path: 'password', message: 'Password too short'},
	];

	it.each([
		['constructor', () => new InputValidationError(errors), errors],
		['createMultiple', () => InputValidationError.createMultiple(errors), errors],
		['create', () => InputValidationError.create('email', 'Invalid email'), [errors[0]]],
	] as const)('%s retains every field error in the response', async (_name, createError, expectedErrors) => {
		const error = createError();
		const response = error.getResponse();

		expect(error).toBeInstanceOf(InputValidationError);
		expect(error.data).toEqual({errors: expectedErrors});
		expect(error.localizedErrors).toBeNull();
		expect(error.getLocalizedErrors()).toBeNull();
		expect(response.status).toBe(HttpStatus.BAD_REQUEST);
		expect(await response.json()).toEqual({
			code: APIErrorCodes.INVALID_FORM_BODY,
			message: APIErrorCodes.INVALID_FORM_BODY,
			errors: expectedErrors,
		});
	});

	it('retains explicitly supplied localization metadata separately from field messages', () => {
		const localizedErrors = [{path: 'email', code: ValidationErrorCodes.EMAIL_IS_REQUIRED}];
		const error = new InputValidationError(errors, localizedErrors);

		expect(error.data).toEqual({errors});
		expect(error.localizedErrors).toEqual(localizedErrors);
		expect(error.getLocalizedErrors()).toEqual(localizedErrors);
	});

	it('creates a localized field error with interpolation variables', () => {
		const code = ValidationErrorCodes.STRING_LENGTH_INVALID;
		const variables = {min: 1, max: 100};
		const error = InputValidationError.fromCode('name', code, variables);

		expect(error.data).toEqual({errors: [{path: 'name', code, message: code}]});
		expect(error.getLocalizedErrors()).toEqual([{path: 'name', code, variables}]);
	});

	it('preserves all localized codes, paths, and variables in order', () => {
		const localizedErrors = [
			{path: 'email', code: ValidationErrorCodes.EMAIL_IS_REQUIRED},
			{path: 'name', code: ValidationErrorCodes.STRING_LENGTH_INVALID, variables: {max: 100}},
		];
		const error = InputValidationError.fromCodes(localizedErrors);

		expect(error.getLocalizedErrors()).toEqual(localizedErrors);
		expect(error.data).toEqual({
			errors: [
				{path: 'email', code: ValidationErrorCodes.EMAIL_IS_REQUIRED, message: ValidationErrorCodes.EMAIL_IS_REQUIRED},
				{
					path: 'name',
					code: ValidationErrorCodes.STRING_LENGTH_INVALID,
					message: ValidationErrorCodes.STRING_LENGTH_INVALID,
				},
			],
		});
	});
});
