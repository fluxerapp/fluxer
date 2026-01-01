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

import {createMiddleware} from 'hono/factory';
import type {HonoEnv} from '~/App';
import {getMetricsService} from '~/infrastructure/MetricsService';

function normalizePath(path: string): string {
	return path
		.replace(/\/\d{17,20}/g, '/:id')
		.replace(/\/[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}/gi, '/:uuid')
		.replace(/\/[A-Za-z0-9_-]{6,}/g, (match) => {
			if (match.match(/^\/[0-9]+$/)) {
				return '/:id';
			}
			return match;
		});
}

export const MetricsMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const metrics = getMetricsService();
	if (!metrics.isEnabled()) {
		await next();
		return;
	}

	const start = performance.now();
	const method = ctx.req.method;
	const path = new URL(ctx.req.url).pathname;

	try {
		await next();
	} finally {
		const durationMs = performance.now() - start;
		const normalizedPath = normalizePath(path);
		const status = ctx.res.status;

		metrics.histogram({
			name: 'api.latency',
			dimensions: {
				method,
				path: normalizedPath,
				status: String(status),
			},
			valueMs: durationMs,
		});

		if (status >= 200 && status < 300) {
			metrics.counter({
				name: 'api.request.2xx',
				dimensions: {
					method,
					path: normalizedPath,
					status: String(status),
				},
				value: 1,
			});
		} else if (status >= 400 && status < 500) {
			metrics.counter({
				name: 'api.request.4xx',
				dimensions: {
					method,
					path: normalizedPath,
					status: String(status),
				},
				value: 1,
			});
		} else if (status >= 500 && status < 600) {
			metrics.counter({
				name: 'api.request.5xx',
				dimensions: {
					method,
					path: normalizedPath,
					status: String(status),
				},
				value: 1,
			});
		}
	}
});
