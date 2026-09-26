import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {z} from 'zod';

const ISO_TIMESTAMP_REGEX = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+-]\d{2}:?\d{2})?$/;

export const CalendarDateType = z.iso.date();

export const IsoTimestampStringType = z
	.string()
	.regex(ISO_TIMESTAMP_REGEX, ValidationErrorCodes.INVALID_ISO_TIMESTAMP)
	.refine(
		(value) => CalendarDateType.safeParse(value.slice(0, 10)).success && !Number.isNaN(Date.parse(value)),
		ValidationErrorCodes.INVALID_ISO_TIMESTAMP,
	);
