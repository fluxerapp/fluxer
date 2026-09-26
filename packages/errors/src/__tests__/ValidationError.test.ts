// SPDX-License-Identifier: AGPL-3.0-or-later

import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {ValidationError} from '@fluxer/errors/src/ValidationError';
import {describe, expect, it} from 'vitest';

describe('ValidationError', () => {
	it.each([
		{name: 'single field', errors: [{path: 'email', code: 'INVALID_EMAIL', message: 'Invalid email format'}]},
		{
			name: 'nested and array fields',
			errors: [
				{path: 'user.email', code: 'REQUIRED', message: "Email can't be empty"},
				{path: 'address.street', code: 'REQUIRED', message: 'Street is required'},
				{path: 'items[0].name', code: 'REQUIRED', message: 'Item name is required'},
				{path: 'items[1].quantity', code: 'MIN', message: 'Quantity must be at least 1'},
			],
		},
		{name: 'empty field list', errors: []},
	])('preserves $name errors through construction and serialization', async ({errors}) => {
		const error = new ValidationError({errors});
		const body = {code: 'VALIDATION_ERROR', message: 'Validation failed', errors};
		expect(error).toBeInstanceOf(FluxerError);
		expect(error.name).toBe('ValidationError');
		expect(error.status).toBe(HttpStatus.BAD_REQUEST);
		expect(error.code).toBe(body.code);
		expect(error.message).toBe(body.message);
		expect(error.errors).toEqual(errors);
		expect(error.data).toEqual({errors});
		expect(error.toJSON()).toEqual(body);
		const response = error.getResponse();
		expect(response.status).toBe(HttpStatus.BAD_REQUEST);
		expect(response.headers.get('Content-Type')).toBe('application/json');
		expect(await response.json()).toEqual(body);
	});

	it.each([
		{options: {code: 'CUSTOM_VALIDATION'}, code: 'CUSTOM_VALIDATION', message: 'Validation failed'},
		{options: {message: 'Input validation failed'}, code: 'VALIDATION_ERROR', message: 'Input validation failed'},
		{
			options: {code: 'CUSTOM_VALIDATION', message: 'Input validation failed'},
			code: 'CUSTOM_VALIDATION',
			message: 'Input validation failed',
		},
	])('applies code/message overrides $options without changing field errors', async ({options, code, message}) => {
		const errors = [{path: 'name', code: 'REQUIRED', message: 'Name is required'}];
		const error = new ValidationError({...options, errors});
		const body = {code, message, errors};
		expect(error.code).toBe(code);
		expect(error.message).toBe(message);
		expect(error.toJSON()).toEqual(body);
		expect(await error.getResponse().json()).toEqual(body);
	});

	it('fromPath constructs one field error with the default response contract', () => {
		const error = ValidationError.fromPath('username', 'TAKEN', 'Username is already taken');
		expect(error).toBeInstanceOf(ValidationError);
		expect(error.toJSON()).toEqual({
			code: 'VALIDATION_ERROR',
			message: 'Validation failed',
			errors: [{path: 'username', code: 'TAKEN', message: 'Username is already taken'}],
		});
	});

	it.each([
		{errors: []},
		{
			errors: [
				{path: 'email', code: 'REQUIRED', message: 'Email is required'},
				{path: 'password', code: 'WEAK', message: 'Password is too weak'},
			],
		},
	])('fromPaths preserves the supplied fields $errors', ({errors}) => {
		const error = ValidationError.fromPaths(errors);
		expect(error).toBeInstanceOf(ValidationError);
		expect(error.errors).toEqual(errors);
		expect(error.toJSON()).toEqual({code: 'VALIDATION_ERROR', message: 'Validation failed', errors});
	});
});
