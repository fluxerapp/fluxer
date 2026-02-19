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

import type {ILogger} from '@fluxer/api/src/ILogger';
import {AuditLogMiddleware} from '@fluxer/api/src/middleware/AuditLogMiddleware';
import {ConcurrencyLimitMiddleware} from '@fluxer/api/src/middleware/ConcurrencyLimitMiddleware';
import {GuildAvailabilityMiddleware} from '@fluxer/api/src/middleware/GuildAvailabilityMiddleware';
import {IpBanMiddleware} from '@fluxer/api/src/middleware/IpBanMiddleware';
import {LocaleMiddleware} from '@fluxer/api/src/middleware/LocaleMiddleware';
import {MetricsMiddleware} from '@fluxer/api/src/middleware/MetricsMiddleware';
import {RequestCacheMiddleware} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import {RequireXForwardedForMiddleware} from '@fluxer/api/src/middleware/RequireXForwardedForMiddleware';
import {ServiceMiddleware} from '@fluxer/api/src/middleware/ServiceMiddleware';
import {UserMiddleware} from '@fluxer/api/src/middleware/UserMiddleware';
import type {HonoApp, HonoEnv} from '@fluxer/api/src/types/HonoEnv';
import {InvalidApiOriginError} from '@fluxer/errors/src/domains/core/InvalidApiOriginError';
import {applyMiddlewareStack} from '@fluxer/hono/src/middleware/MiddlewareStack';
import {createServiceTelemetry} from '@fluxer/hono/src/middleware/TelemetryAdapters';
import {formatTraceparent, getActiveSpan} from '@fluxer/telemetry/src/Tracing';
import type {Context as HonoContext} from 'hono';

export interface MiddlewarePipelineOptions {
	logger: ILogger;
	nodeEnv: string;
	corsOrigins: Array<string>;
	setSentryUser?: (user: {id?: string; username?: string; email?: string; ip_address?: string}) => void;
	isTelemetryActive?: () => boolean;
}

const TRACEPARENT_HEADER = 'traceparent';
function attachTraceparentHeader(ctx: HonoContext<HonoEnv>): void {
	const span = getActiveSpan();
	if (!span) {
		return;
	}

	const traceparent = formatTraceparent(span);
	if (traceparent) {
		ctx.header(TRACEPARENT_HEADER, traceparent);
	}
}

export function configureMiddleware(routes: HonoApp, options: MiddlewarePipelineOptions): void {
	const {logger, nodeEnv, corsOrigins, setSentryUser, isTelemetryActive} = options;

	const requestTelemetry = createServiceTelemetry({
		serviceName: 'fluxer-api',
		skipPaths: ['/_health', '/internal/telemetry'],
	});

	applyMiddlewareStack(routes, {
		requestId: {},
		cors: {origins: corsOrigins},
		tracing: requestTelemetry.tracing,
		metrics: {
			enabled: true,
			collector: requestTelemetry.metricsCollector,
			skipPaths: ['/_health', '/internal/telemetry'],
		},
		logger: {
			log: (data) => {
				logger.info(
					{
						method: data.method,
						path: data.path,
						status: data.status,
						durationMs: data.durationMs,
					},
					'Request completed',
				);
			},
			skip: ['/_health'],
		},
		skipErrorHandler: true,
	});

	if (nodeEnv === 'production') {
		routes.use('*', async (ctx, next) => {
			const host = ctx.req.header('host');
			if (ctx.req.method !== 'GET' && (host === 'web.fluxer.app' || host === 'web.canary.fluxer.app')) {
				const origin = ctx.req.header('origin');
				if (!origin || origin !== `https://${host}`) {
					throw new InvalidApiOriginError();
				}
			}
			await next();
		});
	}

	routes.use(IpBanMiddleware);
	routes.use(ConcurrencyLimitMiddleware);
	routes.use(MetricsMiddleware);
	routes.use(AuditLogMiddleware);
	routes.use(RequireXForwardedForMiddleware());
	routes.use(RequestCacheMiddleware);
	routes.use(ServiceMiddleware);
	routes.use(UserMiddleware);
	routes.use(GuildAvailabilityMiddleware);
	routes.use(LocaleMiddleware);

	routes.use('*', async (ctx, next) => {
		attachTraceparentHeader(ctx);
		await next();
	});

	if (setSentryUser) {
		routes.use('*', async (ctx, next) => {
			const user = ctx.get('user');
			const clientIp = ctx.req.header('X-Forwarded-For')?.split(',')[0]?.trim();

			setSentryUser({
				id: user?.id.toString(),
				username: user?.username,
				email: user?.email ?? undefined,
				ip_address: clientIp,
			});

			return next();
		});
	}

	routes.get('/_health', async (ctx) => ctx.text('OK'));

	if (isTelemetryActive) {
		routes.get('/internal/telemetry', async (ctx) => {
			return ctx.json({
				telemetry_enabled: isTelemetryActive(),
				service: 'fluxer_api',
				timestamp: new Date().toISOString(),
			});
		});
	}
}
