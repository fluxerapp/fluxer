// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {beforeAll, describe, expect, it} from 'vitest';
import {z} from 'zod';
import {initializeFluxerErrorMap} from './ZodErrorMap';

function firstIssueMessage(schema: z.ZodType, value: unknown): string | undefined {
	const result = schema.safeParse(value);
	return result.success ? undefined : result.error.issues[0]?.message;
}

describe('ZodErrorMap', () => {
	beforeAll(() => {
		initializeFluxerErrorMap();
	});

	it('maps a date below its minimum to INVALID_FORMAT', () => {
		expect(
			firstIssueMessage(z.date().min(new Date('2000-01-01T00:00:00.000Z')), new Date('1999-12-31T00:00:00.000Z')),
		).toBe(ValidationErrorCodes.INVALID_FORMAT);
	});

	it('maps a date above its maximum to INVALID_FORMAT', () => {
		expect(
			firstIssueMessage(z.date().max(new Date('2000-01-01T00:00:00.000Z')), new Date('2000-01-02T00:00:00.000Z')),
		).toBe(ValidationErrorCodes.INVALID_FORMAT);
	});

	it('maps both numeric bounds to INVALID_FORMAT', () => {
		expect(firstIssueMessage(z.number().min(1), 0)).toBe(ValidationErrorCodes.INVALID_FORMAT);
		expect(firstIssueMessage(z.number().max(1), 2)).toBe(ValidationErrorCodes.INVALID_FORMAT);
	});

	it('maps a string longer than its maximum to CONTENT_EXCEEDS_MAX_LENGTH', () => {
		expect(firstIssueMessage(z.string().max(1), 'ab')).toBe(ValidationErrorCodes.CONTENT_EXCEEDS_MAX_LENGTH);
	});

	it('maps a string shorter than its minimum to INVALID_FORMAT', () => {
		expect(firstIssueMessage(z.string().min(2), 'a')).toBe(ValidationErrorCodes.INVALID_FORMAT);
	});
});
