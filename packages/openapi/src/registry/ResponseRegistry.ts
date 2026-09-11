// SPDX-License-Identifier: AGPL-3.0-or-later
import type {OpenAPIResponse, OpenAPISchema} from '@fluxer/openapi/src/OpenAPITypes';
import {ErrorResponse, ThrottledErrorResponse} from '@fluxer/schema/src/domains/common/ErrorSchemas';
import {z} from 'zod';

function toErrorSchema(schema: z.ZodType): OpenAPISchema {
	const jsonSchema = z.toJSONSchema(schema, {io: 'output', target: 'openapi-3.0'});
	delete jsonSchema.$schema;
	return jsonSchema;
}

export const ERROR_SCHEMA = toErrorSchema(ErrorResponse);
export const THROTTLED_ERROR_SCHEMA = toErrorSchema(ThrottledErrorResponse);
const RATE_LIMIT_HEADERS: Record<
	string,
	{
		description: string;
		schema: OpenAPISchema;
	}
> = {
	'X-RateLimit-Limit': {
		description: 'The number of requests that can be made in the current window',
		schema: {type: 'integer'},
	},
	'X-RateLimit-Remaining': {
		description: 'The number of remaining requests that can be made',
		schema: {type: 'integer'},
	},
	'X-RateLimit-Reset': {
		description: 'Unix timestamp when the rate limit resets',
		schema: {type: 'integer'},
	},
	'Retry-After': {
		description: 'Number of seconds to wait before retrying (only on 429)',
		schema: {type: 'integer'},
	},
};
const COMMON_RESPONSES: Record<string, OpenAPIResponse> = {
	'400': {
		description: 'Bad Request - The request was malformed or contained invalid data',
		content: {
			'application/json': {
				schema: {$ref: '#/components/schemas/Error'},
			},
		},
	},
	'401': {
		description: 'Unauthorized - Authentication is required or the token is invalid',
		content: {
			'application/json': {
				schema: {$ref: '#/components/schemas/Error'},
			},
		},
	},
	'403': {
		description: 'Forbidden - You do not have permission to perform this action',
		content: {
			'application/json': {
				schema: {$ref: '#/components/schemas/Error'},
			},
		},
	},
	'404': {
		description: 'Not Found - The requested resource was not found',
		content: {
			'application/json': {
				schema: {$ref: '#/components/schemas/Error'},
			},
		},
	},
	'429': {
		description: 'Too Many Requests - You are being rate limited',
		content: {
			'application/json': {
				schema: {$ref: '#/components/schemas/ThrottledError'},
			},
		},
		headers: {
			'Retry-After': RATE_LIMIT_HEADERS['Retry-After'],
			'X-RateLimit-Limit': RATE_LIMIT_HEADERS['X-RateLimit-Limit'],
			'X-RateLimit-Remaining': RATE_LIMIT_HEADERS['X-RateLimit-Remaining'],
			'X-RateLimit-Reset': RATE_LIMIT_HEADERS['X-RateLimit-Reset'],
		},
	},
	'500': {
		description: 'Internal Server Error - An unexpected error occurred',
		content: {
			'application/json': {
				schema: {$ref: '#/components/schemas/Error'},
			},
		},
	},
};
export function getErrorResponses(requiresAuth: boolean): Record<string, OpenAPIResponse> {
	const responses: Record<string, OpenAPIResponse> = {
		'400': COMMON_RESPONSES['400'],
		'429': COMMON_RESPONSES['429'],
		'500': COMMON_RESPONSES['500'],
	};
	if (requiresAuth) {
		responses['401'] = COMMON_RESPONSES['401'];
		responses['403'] = COMMON_RESPONSES['403'];
	}
	return responses;
}
