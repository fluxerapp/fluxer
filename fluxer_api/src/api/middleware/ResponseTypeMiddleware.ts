// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {InternalServerError} from '@fluxer/errors/src/domains/core/InternalServerError';
import {resolveRoutePattern} from '@fluxer/errors/src/error_handling/RoutePattern';
import {createLogger} from '@fluxer/logger/src/Logger';
import type {Context, MiddlewareHandler} from 'hono';
import type {ZodType} from 'zod';

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
			path: resolveRoutePattern(ctx),
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

type SecurityScheme = 'botToken' | 'oauth2Token' | 'bearerToken' | 'sessionToken' | 'adminApiKey';

export interface OpenAPIRouteMetadata {
	operationId: string;
	summary: string;
	description: string;
	responseSchema: ZodType | null;
	acceptedResponseSchema?: ZodType;
	responseContentType?: string;
	requestSchema?: ZodType;
	requestFormSchema?: ZodType;
	requestBodyRequired?: boolean;
	statusCode?: number | Array<number>;
	bodylessStatusCodes?: Array<number>;
	errorStatusCodes?: Array<number>;
	security?: SecurityScheme | Array<SecurityScheme>;
	tags: string | Array<string>;
	deprecated?: boolean;
	externalDocs?: {
		url: string;
		description?: string;
	};
	experiment?: string;
}

function validateOperationId(operationId: string): void {
	if (!/^[a-z][a-z0-9_]*$/.test(operationId)) {
		throw new Error(
			`Invalid operationId "${operationId}". Must be snake_case (lowercase letters, numbers, and underscores only, starting with a letter).`,
		);
	}
}

export function OpenAPI(metadata: OpenAPIRouteMetadata): MiddlewareHandler<HonoEnv> {
	validateOperationId(metadata.operationId);
	const {bodylessStatusCodes, responseContentType} = metadata;
	const schema = metadata.responseSchema;
	const mediaType = responseContentType?.split(';', 1)[0]?.trim().toLowerCase();
	const hasJsonResponse = mediaType === undefined || mediaType === 'application/json';
	return async (ctx, next) => {
		await next();
		const statusSchema =
			ctx.res.status === 202 && metadata.acceptedResponseSchema ? metadata.acceptedResponseSchema : schema;
		if (
			!statusSchema ||
			!hasJsonResponse ||
			!Config.dev.validateResponses ||
			bodylessStatusCodes?.includes(ctx.res.status)
		) {
			return;
		}
		await validateAndRewriteResponse(ctx, statusSchema);
	};
}
