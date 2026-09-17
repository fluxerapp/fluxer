// SPDX-License-Identifier: AGPL-3.0-or-later

import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {getApiErrorCodeForStatus} from '@fluxer/errors/src/error_handling/ErrorIntrospection';
import {createJsonErrorResponse, createXmlErrorResponse} from '@fluxer/errors/src/error_handling/ErrorResponse';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import type {Context, ErrorHandler} from 'hono';
import {HTTPException} from 'hono/http-exception';

type ResponseFormat = 'json' | 'xml';

interface ErrorHandlerOptions {
	logError?: (error: Error, context: Context) => void;
	includeStack?: boolean;
	responseFormat?: ResponseFormat;
	customHandler?: (error: Error, context: Context) => Response | Promise<Response> | undefined;
}

export function createErrorHandler(options: ErrorHandlerOptions = {}): ErrorHandler {
	const {logError, includeStack = false, responseFormat = 'json', customHandler} = options;
	return async (error: Error, c: Context): Promise<Response> => {
		if (logError) {
			logError(error, c);
		}
		if (customHandler) {
			const customResponse = await customHandler(error, c);
			if (customResponse) {
				return customResponse;
			}
		}
		if (error instanceof FluxerError) {
			return error.getResponse();
		}
		if (error instanceof HTTPException) {
			const status = error.status;
			const code = getApiErrorCodeForStatus(status);
			const message = error.message || 'An error occurred';
			if (responseFormat === 'xml') {
				return createXmlErrorResponse(status, code, message);
			}
			return createJsonErrorResponse({
				status,
				code,
				message,
				data: includeStack ? {stack: error.stack} : undefined,
			});
		}
		const status = HttpStatus.INTERNAL_SERVER_ERROR;
		const message = includeStack ? error.message : 'Something went wrong. Please try again later.';
		if (responseFormat === 'xml') {
			return createXmlErrorResponse(status, 'INTERNAL_SERVER_ERROR', message);
		}
		return createJsonErrorResponse({
			status,
			code: 'INTERNAL_SERVER_ERROR',
			message,
			data: includeStack ? {stack: error.stack} : undefined,
		});
	};
}
