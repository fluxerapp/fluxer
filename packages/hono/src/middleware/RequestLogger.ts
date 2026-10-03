// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveRoutePattern} from '@fluxer/errors/src/error_handling/RoutePattern';
import {matchesAnyPathPattern} from '@fluxer/hono/src/middleware/utils/PathMatchers';
import type {MiddlewareHandler} from 'hono';

export interface RequestLogData {
	method: string;
	path: string;
	status: number;
	durationMs: number;
}

interface RequestInfoLogger {
	info(obj: Record<string, unknown>, msg: string): void;
}

export type LogFunction = (data: RequestLogData) => void;

export interface RequestLoggerOptions {
	log: LogFunction;
	skip?: Array<string>;
}

export function createInfoRequestLogger(logger: RequestInfoLogger): LogFunction {
	return (data) => {
		logger.info(
			{
				method: data.method,
				path: data.path,
				status: data.status,
				durationMs: data.durationMs,
			},
			'Request completed',
		);
	};
}

export function requestLogger(options: RequestLoggerOptions): MiddlewareHandler {
	const {log, skip = []} = options;
	return async (c, next) => {
		if (matchesAnyPathPattern(c.req.path, skip)) {
			return next();
		}
		const startTime = Date.now();
		const method = c.req.method;
		await next();
		const durationMs = Date.now() - startTime;
		const status = c.res.status;
		log({method, path: resolveRoutePattern(c), status, durationMs});
	};
}
