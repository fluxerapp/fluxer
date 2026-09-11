import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {z} from 'zod';

export const APIErrorCodeSchema = z.enum(APIErrorCodes).describe('Known error codes returned by API operations');

export const ValidationErrorItem = z.object({
	path: z.string().describe('Field path that failed validation'),
	code: z.string().optional().describe('Machine-readable validation error code'),
	message: z.string().describe('Human-readable validation error message'),
});

export type ValidationErrorItem = z.infer<typeof ValidationErrorItem>;

export const ErrorResponse = z.looseObject({
	code: z.string().describe('Machine-readable error code'),
	message: z.string().describe('Human-readable error message'),
	errors: z.array(ValidationErrorItem).optional().describe('Field-specific validation errors'),
});

export type ErrorResponse = z.infer<typeof ErrorResponse>;

export const RateLimitMetadata = z.object({
	retry_after: z.number().describe('Seconds to wait before retrying'),
	global: z.boolean().describe('Whether this is a global rate limit'),
});

export type RateLimitMetadata = z.infer<typeof RateLimitMetadata>;

export const ThrottledErrorResponse = ErrorResponse.extend(RateLimitMetadata.partial().shape);

export type ThrottledErrorResponse = z.infer<typeof ThrottledErrorResponse>;
