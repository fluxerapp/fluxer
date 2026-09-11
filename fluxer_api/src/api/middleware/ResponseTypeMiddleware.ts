// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {InternalServerError} from '@fluxer/errors/src/domains/core/InternalServerError';
import {createLogger} from '@fluxer/logger/src/Logger';
import type {Context, MiddlewareHandler} from 'hono';
import type {ZodType} from 'zod';
import {Config} from '../Config';
import type {HonoEnv} from '../types/HonoEnv';

const responseValidationLogger = createLogger('response_validation');

function stringifyValidatedResponse(value: unknown): string {
	return JSON.stringify(value, (_key, item) => (typeof item === 'bigint' ? item.toString() : item));
}

class ResponseValidationError extends InternalServerError {
	constructor(
		public readonly validationErrors: Array<{
			path: string;
			message: string;
		}>,
	) {
		const errorsDescription = validationErrors.map((e) => `${e.path}: ${e.message}`).join(', ');
		super({
			code: APIErrorCodes.RESPONSE_VALIDATION_ERROR,
			messageVariables: {errors: errorsDescription},
		});
	}
}

async function validateAndRewriteResponse(ctx: Context<HonoEnv>, schema: ZodType): Promise<void> {
	const response = ctx.res;
	const contentType = response.headers.get('content-type');
	if (!contentType?.includes('application/json')) {
		return;
	}
	if (response.status >= 400) {
		return;
	}
	const clonedResponse = response.clone();
	let body: unknown;
	try {
		body = await clonedResponse.json();
	} catch {
		return;
	}
	const result = schema.safeParse(body);
	if (!result.success) {
		const validationErrors = result.error.issues.map((issue) => ({
			path: issue.path.join('.') || 'root',
			message: issue.message,
		}));
		const errorContext = {
			method: ctx.req.method,
			path: ctx.req.path,
			status: response.status,
			validationErrors,
			body,
		};
		const responseValidationError = new ResponseValidationError(validationErrors);
		responseValidationLogger.error(errorContext, 'Response validation failed');
		throw responseValidationError;
	}
	ctx.res = new Response(stringifyValidatedResponse(result.data), {
		status: response.status,
		statusText: response.statusText,
		headers: response.headers,
	});
}

export function ResponseType<T extends ZodType>(
	schema: T,
	options?: {
		skipValidation?: boolean;
		allowNoContent?: boolean;
	},
): MiddlewareHandler<HonoEnv> {
	const {skipValidation = false, allowNoContent = false} = options ?? {};
	return async (ctx, next) => {
		ctx.set('responseSchema', schema);
		await next();
		if (skipValidation || !Config.dev.validateResponses) {
			return;
		}
		if (allowNoContent && ctx.res.status === 204) {
			return;
		}
		await validateAndRewriteResponse(ctx, schema);
	};
}

type SecurityScheme = 'botToken' | 'oauth2Token' | 'bearerToken' | 'sessionToken' | 'adminApiKey';

export interface OpenAPIRouteMetadata {
	operationId: string;
	summary: string;
	description: string;
	responseSchema: ZodType | null;
	responseContentType?: string;
	requestSchema?: ZodType;
	requestFormSchema?: ZodType;
	requestBodyRequired?: boolean;
	statusCode?: number | Array<number>;
	bodylessStatusCodes?: Array<number>;
	security?: SecurityScheme | Array<SecurityScheme>;
	tags: string | Array<string>;
	deprecated?: boolean;
	externalDocs?: {
		url: string;
		description?: string;
	};
}

interface OpenAPIOptions {
	description: string;
	responseContentType?: string;
}

function validateOperationId(operationId: string): void {
	if (!/^[a-z][a-z0-9_]*$/.test(operationId)) {
		throw new Error(
			`Invalid operationId "${operationId}". Must be snake_case (lowercase letters, numbers, and underscores only, starting with a letter).`,
		);
	}
}

function toArray<T>(value: T | Array<T>): Array<T> {
	return Array.isArray(value) ? value : [value];
}

export function OpenAPI(metadata: OpenAPIRouteMetadata): MiddlewareHandler<HonoEnv>;
export function OpenAPI(
	operationId: string,
	summary: string,
	responseSchema: ZodType | null,
	options: OpenAPIOptions,
): MiddlewareHandler<HonoEnv>;
export function OpenAPI(
	operationIdOrMetadata: string | OpenAPIRouteMetadata,
	summary?: string,
	responseSchema?: ZodType | null,
	options?: OpenAPIOptions,
): MiddlewareHandler<HonoEnv> {
	let metadata: OpenAPIRouteMetadata;
	if (typeof operationIdOrMetadata === 'string') {
		if (!options?.description) {
			throw new Error(
				`Missing description for OpenAPI route ${operationIdOrMetadata}. The description field is required.`,
			);
		}
		if (responseSchema === undefined) {
			throw new Error(
				`Missing responseSchema for OpenAPI route ${operationIdOrMetadata}. The responseSchema field is required (use null for no-content responses).`,
			);
		}
		metadata = {
			operationId: operationIdOrMetadata,
			summary: summary!,
			description: options.description,
			responseSchema,
			responseContentType: options.responseContentType,
			tags: [],
		};
	} else {
		metadata = operationIdOrMetadata;
	}
	validateOperationId(metadata.operationId);
	const {statusCode, security, tags, bodylessStatusCodes, responseContentType} = metadata;
	const schema = metadata.responseSchema;
	const fullMetadata: OpenAPIRouteMetadata = {
		...metadata,
		statusCode: statusCode === undefined ? undefined : toArray(statusCode),
		security: security === undefined ? undefined : toArray(security),
		tags: toArray(tags),
	};
	const mediaType = responseContentType?.split(';', 1)[0]?.trim().toLowerCase();
	const hasJsonResponse = mediaType === undefined || mediaType === 'application/json';
	return async (ctx, next) => {
		ctx.set('openapiMetadata', fullMetadata);
		ctx.set('responseSchema', schema);
		await next();
		if (!schema || !hasJsonResponse || !Config.dev.validateResponses || bodylessStatusCodes?.includes(ctx.res.status)) {
			return;
		}
		await validateAndRewriteResponse(ctx, schema);
	};
}
