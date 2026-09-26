// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CacheHeadersOptions} from '@fluxer/hono/src/middleware/CacheHeaders';
import {cacheHeaders} from '@fluxer/hono/src/middleware/CacheHeaders';
import type {CorsOptions} from '@fluxer/hono/src/middleware/Cors';
import {cors} from '@fluxer/hono/src/middleware/Cors';
import type {ErrorHandlerOptions} from '@fluxer/hono/src/middleware/ErrorHandler';
import {createErrorHandler} from '@fluxer/hono/src/middleware/ErrorHandler';
import type {RateLimitOptions, RateLimitService} from '@fluxer/hono/src/middleware/RateLimit';
import {rateLimit} from '@fluxer/hono/src/middleware/RateLimit';
import type {RequestIdOptions} from '@fluxer/hono/src/middleware/RequestId';
import {requestId} from '@fluxer/hono/src/middleware/RequestId';
import type {LogFunction, RequestLoggerOptions} from '@fluxer/hono/src/middleware/RequestLogger';
import {requestLogger} from '@fluxer/hono/src/middleware/RequestLogger';
import {fluxerVersionHeader} from '@fluxer/hono/src/middleware/VersionHeader';
import type {Env, Hono, MiddlewareHandler} from 'hono';

interface MiddlewareStackOptions {
	requestId?: RequestIdOptions;
	cors?: CorsOptions;
	cacheHeaders?: CacheHeadersOptions | false;
	logger?: Omit<RequestLoggerOptions, 'log'> & {
		log?: LogFunction;
	};
	rateLimit?: RateLimitOptions & {
		service?: RateLimitService;
	};
	errorHandler?: ErrorHandlerOptions;
	customMiddleware?: Array<MiddlewareHandler>;
}

interface ApplyMiddlewareStackOptions extends MiddlewareStackOptions {
	skipRequestId?: boolean;
	skipCors?: boolean;
	skipCacheHeaders?: boolean;
	skipLogger?: boolean;
	skipRateLimit?: boolean;
	skipErrorHandler?: boolean;
}

function createStandardMiddlewareStack(options: MiddlewareStackOptions = {}): Array<MiddlewareHandler> {
	const stack: Array<MiddlewareHandler> = [fluxerVersionHeader()];
	if (options.requestId) {
		stack.push(requestId(options.requestId));
	}
	if (options.cors && options.cors.enabled !== false) {
		stack.push(cors(options.cors));
	}
	if (options.cacheHeaders !== false) {
		stack.push(cacheHeaders(options.cacheHeaders ?? {}));
	}
	if (options.logger?.log) {
		stack.push(
			requestLogger({
				log: options.logger.log,
				skip: options.logger.skip,
			}),
		);
	}
	if (options.rateLimit && options.rateLimit.enabled !== false && options.rateLimit.service) {
		stack.push(rateLimit(options.rateLimit));
	}
	if (options.customMiddleware) {
		stack.push(...options.customMiddleware);
	}
	return stack;
}

function buildStackOptions(options: ApplyMiddlewareStackOptions): MiddlewareStackOptions {
	return {
		requestId: options.skipRequestId ? undefined : options.requestId,
		cors: options.skipCors ? undefined : options.cors,
		cacheHeaders: options.skipCacheHeaders ? false : options.cacheHeaders,
		logger: options.skipLogger ? undefined : options.logger,
		rateLimit: options.skipRateLimit ? undefined : options.rateLimit,
		customMiddleware: options.customMiddleware,
	};
}

export function applyMiddlewareStack<E extends Env = Env>(
	app: Hono<E>,
	options: ApplyMiddlewareStackOptions = {},
): void {
	const stack = createStandardMiddlewareStack(buildStackOptions(options));
	for (const middleware of stack) {
		app.use('*', middleware);
	}
	if (!options.skipErrorHandler) {
		const errorHandler = createErrorHandler(options.errorHandler ?? {});
		app.onError(errorHandler);
	}
}
