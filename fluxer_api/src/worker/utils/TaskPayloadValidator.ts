/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

type FieldRequirement = 'required' | 'optional';

interface FieldValidator {
	type: 'string' | 'number' | 'boolean' | 'object' | 'array';
	requirement: FieldRequirement;
	min?: number;
	max?: number;
	validator?: (value: unknown) => boolean;
}

interface TaskPayloadSchema {
	[key: string]: FieldValidator;
}

class PayloadValidationError extends Error {
	constructor(
		public readonly field: string,
		message: string,
	) {
		super(`Invalid payload: ${field} - ${message}`);
		this.name = 'PayloadValidationError';
	}
}

function validateField(field: string, value: unknown, validator: FieldValidator): void {
	if (value === undefined || value === null) {
		if (validator.requirement === 'required') {
			throw new PayloadValidationError(field, 'is required');
		}
		return;
	}

	const actualType = Array.isArray(value) ? 'array' : typeof value;
	if (actualType !== validator.type) {
		throw new PayloadValidationError(field, `must be ${validator.type}, got ${actualType}`);
	}

	if (validator.type === 'number' && typeof value === 'number') {
		if (validator.min !== undefined && value < validator.min) {
			throw new PayloadValidationError(field, `must be at least ${validator.min}`);
		}
		if (validator.max !== undefined && value > validator.max) {
			throw new PayloadValidationError(field, `must be at most ${validator.max}`);
		}
	}

	if (validator.validator && !validator.validator(value)) {
		throw new PayloadValidationError(field, 'failed custom validation');
	}
}

export function validatePayload<T>(payload: unknown, schema: TaskPayloadSchema): T {
	if (typeof payload !== 'object' || payload === null) {
		throw new PayloadValidationError('payload', 'must be an object');
	}

	const data = payload as Record<string, unknown>;

	for (const [field, validator] of Object.entries(schema)) {
		validateField(field, data[field], validator);
	}

	return payload as T;
}

export const CommonFields = {
	userId: (requirement: FieldRequirement = 'required'): FieldValidator => ({
		type: 'string',
		requirement,
	}),

	guildId: (requirement: FieldRequirement = 'required'): FieldValidator => ({
		type: 'string',
		requirement,
	}),

	channelId: (requirement: FieldRequirement = 'required'): FieldValidator => ({
		type: 'string',
		requirement,
	}),

	messageId: (requirement: FieldRequirement = 'required'): FieldValidator => ({
		type: 'string',
		requirement,
	}),

	timestamp: (requirement: FieldRequirement = 'optional'): FieldValidator => ({
		type: 'number',
		requirement,
	}),

	days: (requirement: FieldRequirement = 'required', min = 0, max = 7): FieldValidator => ({
		type: 'number',
		requirement,
		min,
		max,
	}),

	limit: (requirement: FieldRequirement = 'optional', max = 1000): FieldValidator => ({
		type: 'number',
		requirement,
		min: 1,
		max,
	}),

	deletionReasonCode: (requirement: FieldRequirement = 'required'): FieldValidator => ({
		type: 'number',
		requirement,
	}),

	boolean: (requirement: FieldRequirement = 'optional'): FieldValidator => ({
		type: 'boolean',
		requirement,
	}),

	stringArray: (requirement: FieldRequirement = 'optional'): FieldValidator => ({
		type: 'array',
		requirement,
		validator: (value) => Array.isArray(value) && value.every((v) => typeof v === 'string'),
	}),
};
