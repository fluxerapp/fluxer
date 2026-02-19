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

/** @jsxRuntime automatic */
/** @jsxImportSource hono/jsx */

import {CdnEndpoints} from '@fluxer/constants/src/CdnEndpoints';
import type {HttpStatusCode} from '@fluxer/constants/src/HttpConstants';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {createErrorHandler} from '@fluxer/errors/src/ErrorHandler';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {applyMiddlewareStack} from '@fluxer/hono/src/middleware/MiddlewareStack';
import type {MetricsCollector} from '@fluxer/hono_types/src/MetricsTypes';
import type {TracingOptions} from '@fluxer/hono_types/src/TracingTypes';
import {extractClientIp} from '@fluxer/ip_utils/src/ClientIp';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {MarketingConfig} from '@fluxer/marketing/src/MarketingConfig';
import {cacheHeadersMiddleware} from '@fluxer/marketing/src/middleware/CacheHeadersMiddleware';
import {marketingCsrfMiddleware} from '@fluxer/marketing/src/middleware/Csrf';
import type {IRateLimitService} from '@fluxer/rate_limit/src/IRateLimitService';
import {captureException} from '@fluxer/sentry/src/Sentry';
import {ErrorPage} from '@fluxer/ui/src/pages/ErrorPage';
import type {Context, Hono, ErrorHandler as HonoErrorHandler} from 'hono';

export interface ApplyMarketingMiddlewareStackOptions {
	app: Hono;
	config: MarketingConfig;
	logger: LoggerInterface;
	rateLimitService?: IRateLimitService | null;
	metricsCollector?: MetricsCollector;
	tracing?: TracingOptions;
}

export function applyMarketingMiddlewareStack(options: ApplyMarketingMiddlewareStackOptions): void {
	applyMiddlewareStack(options.app, {
		requestId: {},
		tracing: options.tracing,
		metrics: options.metricsCollector
			? {
					enabled: true,
					collector: options.metricsCollector,
					skipPaths: ['/_health', '/static'],
				}
			: undefined,
		logger: {
			log: (data) => {
				options.logger.debug(
					{
						method: data.method,
						path: data.path,
						status: data.status,
						durationMs: data.durationMs,
					},
					'Request completed',
				);
			},
			skip: ['/_health', '/static'],
		},
		rateLimit: options.config.rateLimit
			? {
					enabled: true,
					service: options.rateLimitService ?? undefined,
					maxAttempts: options.config.rateLimit.limit,
					windowMs: options.config.rateLimit.windowMs,
					skipPaths: ['/_health', '/static'],
					keyGenerator: (req) => extractClientIp(req) ?? 'unknown',
				}
			: undefined,
		customMiddleware: [cacheHeadersMiddleware(), marketingCsrfMiddleware],
		skipErrorHandler: true,
	});

	options.app.onError(createMarketingErrorHandler(options.logger, options.config));
}

const KNOWN_HTTP_STATUS_CODES: Array<HttpStatusCode> = Object.values(HttpStatus);

function createMarketingErrorHandler(logger: LoggerInterface, config: MarketingConfig): HonoErrorHandler {
	const homeUrl = config.basePath || '/';

	return createErrorHandler({
		includeStack: config.env === 'development',
		logError: (error, c) => {
			if (!(error instanceof FluxerError)) {
				captureException(error);
			}

			logger.error(
				{
					error: error.message,
					stack: error.stack,
					path: c.req.path,
					method: c.req.method,
				},
				'Request error',
			);
		},
		customHandler: (error, c) => {
			const status = getStatus(error) ?? 500;
			return renderMarketingError(c, status, homeUrl);
		},
	});
}

function getStatus(error: Error): number | null {
	const statusValue = Reflect.get(error, 'status');
	return typeof statusValue === 'number' ? statusValue : null;
}

function renderMarketingError(c: Context, status: number, homeUrl: string): Response | Promise<Response> {
	const statusCode = isHttpStatusCode(status) ? status : HttpStatus.INTERNAL_SERVER_ERROR;
	c.status(statusCode);
	return c.html(
		<ErrorPage
			statusCode={statusCode}
			title="Something went wrong"
			description="An unexpected error occurred. Please try again later."
			staticCdnEndpoint={CdnEndpoints.STATIC}
			homeUrl={homeUrl}
			homeLabel="Go home"
		/>,
	);
}

function isHttpStatusCode(value: number): value is HttpStatusCode {
	for (const statusCode of KNOWN_HTTP_STATUS_CODES) {
		if (statusCode === value) {
			return true;
		}
	}
	return false;
}
