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
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {captureException} from '@fluxer/sentry/src/Sentry';
import {ErrorPage} from '@fluxer/ui/src/pages/ErrorPage';
import type {Context, ErrorHandler} from 'hono';

const KNOWN_HTTP_STATUS_CODES: Array<HttpStatusCode> = Object.values(HttpStatus);

export function createAdminErrorHandler(
	logger: LoggerInterface,
	includeStack: boolean,
	basePath: string,
): ErrorHandler {
	const homeUrl = basePath || '/';

	return createErrorHandler({
		includeStack,
		logError: (error, c) => {
			const isExpectedError = error instanceof Error && 'isExpected' in error && error.isExpected;

			if (!(error instanceof FluxerError || isExpectedError)) {
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
			if (status === 404) {
				return renderNotFound(c, homeUrl);
			}
			return renderError(c, status, homeUrl);
		},
	});
}

function getStatus(error: Error): number | null {
	const statusValue = Reflect.get(error, 'status');
	return typeof statusValue === 'number' ? statusValue : null;
}

function renderNotFound(c: Context, homeUrl: string): Response | Promise<Response> {
	c.status(404);
	return c.html(
		<ErrorPage
			statusCode={404}
			title="Page not found"
			description="The page you are looking for does not exist or has been moved."
			staticCdnEndpoint={CdnEndpoints.STATIC}
			homeUrl={homeUrl}
			homeLabel="Go to admin"
		/>,
	);
}

function renderError(c: Context, status: number, homeUrl: string): Response | Promise<Response> {
	const statusCode = isHttpStatusCode(status) ? status : HttpStatus.INTERNAL_SERVER_ERROR;
	c.status(statusCode);
	return c.html(
		<ErrorPage
			statusCode={statusCode}
			title="Something went wrong"
			description="An unexpected error occurred. Please try again later."
			staticCdnEndpoint={CdnEndpoints.STATIC}
			homeUrl={homeUrl}
			homeLabel="Go to admin"
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
