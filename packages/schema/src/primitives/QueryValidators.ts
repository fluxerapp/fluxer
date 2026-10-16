// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {IsoTimestampStringType} from '@fluxer/schema/src/primitives/DateValidators';
import {z} from 'zod';

const TRUE_VALUES = ['true', 'True', '1'];
const INTEGER_STRING_REGEX = /^[+-]?\d+$/;
export const QueryBooleanType = z
	.string()
	.trim()
	.optional()
	.default('false')
	.transform((value) => TRUE_VALUES.includes(value));

export function createQueryIntegerType({defaultValue = 0, minValue = 0, maxValue = 2147483647} = {}) {
	return z
		.string()
		.trim()
		.optional()
		.default(defaultValue.toString())
		.transform((value, ctx) => {
			const num = Number(value);
			if (!INTEGER_STRING_REGEX.test(value) || !Number.isSafeInteger(num) || num < minValue || num > maxValue) {
				ctx.addIssue({
					code: 'custom',
					message: ValidationErrorCodes.VALUE_MUST_BE_INTEGER_IN_RANGE,
					params: {minValue, maxValue},
				});
				return z.NEVER;
			}
			return num;
		});
}

export const DateTimeType = z.union([
	IsoTimestampStringType.transform((value) => new Date(value)),
	z
		.number()
		.int()
		.min(0)
		.max(8640000000000000)
		.transform((value) => new Date(value)),
]);
