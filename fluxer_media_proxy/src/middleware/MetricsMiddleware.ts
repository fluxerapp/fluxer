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

import type {MiddlewareHandler} from 'hono';
import type {ErrorContext, ErrorType} from '~/lib/MediaTypes';
import * as metrics from '~/lib/MetricsClient';

const getRouteFromPath = (path: string): string | null => {
	if (path === '/_health') return null;
	if (path.startsWith('/avatars/')) return 'avatars';
	if (path.startsWith('/icons/')) return 'icons';
	if (path.startsWith('/banners/')) return 'banners';
	if (path.startsWith('/emojis/')) return 'emojis';
	if (path.startsWith('/stickers/')) return 'stickers';
	if (path.startsWith('/attachments/')) return 'attachments';
	if (path.startsWith('/external/')) return 'external';
	if (path.startsWith('/guilds/')) return 'guild_assets';
	return 'other';
};

const getErrorTypeFromStatus = (status: number): ErrorType => {
	switch (status) {
		case 400:
			return 'bad_request';
		case 401:
			return 'unauthorized';
		case 403:
			return 'forbidden';
		case 404:
			return 'not_found';
		case 408:
			return 'timeout';
		case 413:
			return 'payload_too_large';
		default:
			if (status >= 500 && status < 600) {
				return 'upstream_5xx';
			}
			return 'other';
	}
};

export const metricsMiddleware: MiddlewareHandler = async (ctx, next) => {
	const start = Date.now();
	let errorType: ErrorType | undefined;
	let errorSource: string | undefined;

	try {
		await next();
	} catch (error) {
		if (error instanceof Error) {
			const message = error.message.toLowerCase();
			if (message.includes('timeout') || message.includes('timed out') || message.includes('etimedout')) {
				errorType = 'timeout';
				errorSource = 'network';
			} else if (message.includes('econnrefused') || message.includes('econnreset') || message.includes('enotfound')) {
				errorType = 'upstream_5xx';
				errorSource = 'network';
			}
		}
		throw error;
	} finally {
		const duration = Date.now() - start;
		const route = getRouteFromPath(ctx.req.path);

		if (route !== null) {
			const status = ctx.res.status;

			metrics.histogram({
				name: 'media_proxy.latency',
				dimensions: {route},
				valueMs: duration,
			});

			metrics.counter({
				name: 'media_proxy.request',
				dimensions: {route, status: String(status)},
			});

			if (status >= 400) {
				const errorContext = ctx.get('metricsErrorContext') as ErrorContext | undefined;
				const finalErrorType = errorContext?.errorType ?? errorType ?? getErrorTypeFromStatus(status);
				const finalErrorSource = errorContext?.errorSource ?? errorSource ?? 'handler';

				metrics.counter({
					name: 'media_proxy.failure',
					dimensions: {
						route,
						status: String(status),
						error_type: finalErrorType,
						error_source: finalErrorSource,
					},
				});
			} else {
				metrics.counter({
					name: 'media_proxy.success',
					dimensions: {route, status: String(status)},
				});
			}

			const contentLength = ctx.res.headers.get('content-length');
			if (contentLength) {
				const bytes = Number.parseInt(contentLength, 10);
				if (!Number.isNaN(bytes)) {
					metrics.counter({
						name: 'media_proxy.bytes',
						dimensions: {route},
						value: bytes,
					});
				}
			}
		}
	}
};
