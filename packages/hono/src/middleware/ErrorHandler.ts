// SPDX-License-Identifier: AGPL-3.0-or-later

import {createErrorHandler as createFluxerErrorHandler} from '@fluxer/errors/src/ErrorHandler';
import {resolveRoutePattern} from '@fluxer/errors/src/error_handling/RoutePattern';
import type {Context, ErrorHandler} from 'hono';

export interface ErrorHandlerOptions {
	includeStack?: boolean;
	logger?: (error: Error, context: Context) => void;
	captureException?: (error: Error, context?: Record<string, unknown>) => void;
}

export function createErrorHandler(options: ErrorHandlerOptions = {}): ErrorHandler {
	const {includeStack = false, logger, captureException} = options;
	const logError =
		logger || captureException
			? (error: Error, context: Context) => {
					if (logger) {
						logger(error, context);
					}
					if (captureException) {
						captureException(error, {
							path: resolveRoutePattern(context),
							method: context.req.method,
							status: context.res?.status,
						});
					}
				}
			: undefined;
	return createFluxerErrorHandler({
		includeStack,
		logError,
	});
}
