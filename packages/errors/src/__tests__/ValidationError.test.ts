// SPDX-License-Identifier: AGPL-3.0-or-later

import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {ValidationError} from '@fluxer/errors/src/ValidationError';
import {describe, expect, it} from 'vitest';

interface ValidationErrorBody {
	code: string;
	message: string;
	errors: Array<{
		path: string;
		code: string;
		message: string;
	}>;
}

describe('ValidationError', () => {
	describe('constructor', () => {
		it('should create error with single path error', () => {
			const error = new ValidationError({
				errors: [{path: 'email', code: 'INVALID_EMAIL', message: 'Invalid email format'}],
			});
			expect(error.status).toBe(HttpStatus.BAD_REQUEST);
			expect(error.code).toBe('VALIDATION_ERROR');
			expect(error.message).toBe('Validation failed');
			expect(error.name).toBe('ValidationError');
			expect(error.errors).toEqual([{path: 'email', code: 'INVALID_EMAIL', message: 'Invalid email format'}]);
		});
		it('should create error with multiple path errors', () => {
			const pathErrors = [
				{path: 'email', code: 'REQUIRED', message: 'Email is required'},
				{path: 'password', code: 'TOO_SHORT', message: 'Password must be at least 8 characters'},
				{path: 'username', code: 'INVALID_CHARS', message: 'Username contains invalid characters'},
			];
			const error = new ValidationError({errors: pathErrors});
			expect(error.errors).toHaveLength(3);
			expect(error.errors).toEqual(pathErrors);
		});
		it('should allow custom code', () => {
			const error = new ValidationError({
				code: 'INVALID_FORM_BODY',
				errors: [{path: 'name', code: 'REQUIRED', message: 'Name is required'}],
			});
			expect(error.code).toBe('INVALID_FORM_BODY');
		});
		it('should allow custom message', () => {
			const error = new ValidationError({
				message: 'Input validation failed',
				errors: [{path: 'name', code: 'REQUIRED', message: 'Name is required'}],
			});
			expect(error.message).toBe('Input validation failed');
		});
		it('should be instance of FluxerError', () => {
			const error = new ValidationError({
				errors: [{path: 'field', code: 'CODE', message: 'message'}],
			});
			expect(error).toBeInstanceOf(FluxerError);
		});
	});
	describe('getResponse', () => {
		it('should return JSON response with errors array', async () => {
			const error = new ValidationError({
				errors: [{path: 'email', code: 'INVALID', message: 'Invalid email'}],
			});
			const response = error.getResponse();
			expect(response.status).toBe(400);
			expect(response.headers.get('Content-Type')).toBe('application/json');
			const body = await response.json();
			expect(body).toEqual({
				code: 'VALIDATION_ERROR',
				message: 'Validation failed',
				errors: [{path: 'email', code: 'INVALID', message: 'Invalid email'}],
			});
		});
		it('should include multiple errors in response', async () => {
			const pathErrors = [
				{path: 'email', code: 'REQUIRED', message: 'Email is required'},
				{path: 'password', code: 'TOO_SHORT', message: 'Password too short'},
			];
			const error = new ValidationError({errors: pathErrors});
			const response = error.getResponse();
			const body = (await response.json()) as ValidationErrorBody;
			expect(body.errors).toHaveLength(2);
			expect(body.errors).toEqual(pathErrors);
		});
		it('should include custom code and message in response', async () => {
			const error = new ValidationError({
				code: 'CUSTOM_VALIDATION',
				message: 'Custom validation message',
				errors: [{path: 'field', code: 'CODE', message: 'message'}],
			});
			const response = error.getResponse();
			const body = (await response.json()) as ValidationErrorBody;
			expect(body.code).toBe('CUSTOM_VALIDATION');
			expect(body.message).toBe('Custom validation message');
		});
	});
	describe('fromPath static method', () => {
		it('should create ValidationError from single path', () => {
			const error = ValidationError.fromPath('username', 'TAKEN', 'Username is already taken');
			expect(error).toBeInstanceOf(ValidationError);
			expect(error.errors).toEqual([{path: 'username', code: 'TAKEN', message: 'Username is already taken'}]);
		});
		it('should have default code and message', () => {
			const error = ValidationError.fromPath('field', 'code', 'message');
			expect(error.code).toBe('VALIDATION_ERROR');
			expect(error.message).toBe('Validation failed');
		});
	});
	describe('fromPaths static method', () => {
		it('should create ValidationError from multiple paths', () => {
			const pathErrors = [
				{path: 'email', code: 'REQUIRED', message: 'Email is required'},
				{path: 'password', code: 'WEAK', message: 'Password is too weak'},
			];
			const error = ValidationError.fromPaths(pathErrors);
			expect(error).toBeInstanceOf(ValidationError);
			expect(error.errors).toEqual(pathErrors);
		});
		it('should handle empty array', () => {
			const error = ValidationError.fromPaths([]);
			expect(error.errors).toEqual([]);
		});
	});
	describe('error data structure', () => {
		it('should store errors in data property', () => {
			const pathErrors = [{path: 'test', code: 'TEST', message: 'Test error'}];
			const error = new ValidationError({errors: pathErrors});
			expect(error.data).toEqual({errors: pathErrors});
		});
		it('should serialize correctly with toJSON', () => {
			const error = new ValidationError({
				errors: [{path: 'field', code: 'CODE', message: 'message'}],
			});
			const json = error.toJSON();
			expect(json).toEqual({
				code: 'VALIDATION_ERROR',
				message: 'Validation failed',
				errors: [{path: 'field', code: 'CODE', message: 'message'}],
			});
		});
	});
	describe('edge cases', () => {
		it('should handle paths with special characters', () => {
			const error = new ValidationError({
				errors: [{path: 'user.email', code: 'INVALID', message: "Email can't be empty"}],
			});
			expect(error.errors[0].path).toBe('user.email');
			expect(error.errors[0].message).toBe("Email can't be empty");
		});
		it('should handle nested field paths', () => {
			const error = new ValidationError({
				errors: [
					{path: 'address.street', code: 'REQUIRED', message: 'Street is required'},
					{path: 'address.city', code: 'REQUIRED', message: 'City is required'},
					{path: 'address.zip', code: 'INVALID_FORMAT', message: 'Invalid ZIP code format'},
				],
			});
			expect(error.errors).toHaveLength(3);
			expect(error.errors[0].path).toBe('address.street');
		});
		it('should handle array index field paths', () => {
			const error = new ValidationError({
				errors: [
					{path: 'items[0].name', code: 'REQUIRED', message: 'Item name is required'},
					{path: 'items[1].quantity', code: 'MIN', message: 'Quantity must be at least 1'},
				],
			});
			expect(error.errors).toHaveLength(2);
			expect(error.errors[0].path).toBe('items[0].name');
		});
	});
});
