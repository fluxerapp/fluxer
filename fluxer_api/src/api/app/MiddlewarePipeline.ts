// SPDX-License-Identifier: AGPL-3.0-or-later

import {Headers as HttpHeaders} from '@fluxer/constants/src/Headers';
import {InvalidApiOriginError} from '@fluxer/errors/src/domains/core/InvalidApiOriginError';
import {cors} from '@fluxer/hono/src/middleware/Cors';
import {applyMiddlewareStack} from '@fluxer/hono/src/middleware/MiddlewareStack';
import {createInfoRequestLogger, requestLogger} from '@fluxer/hono/src/middleware/RequestLogger';
import {resolveClientIpHeaderName} from '@fluxer/ip_utils/src/ClientIp';
import type {ILogger} from '../ILogger';
import {ClientErrorAbuseSignalMiddleware} from '../middleware/AbusiveIpAutoBanner';
import {AuditLogMiddleware} from '../middleware/AuditLogMiddleware';
import ContentFilterMiddleware from '../middleware/ContentFilterMiddleware';
import {GuildAvailabilityMiddleware} from '../middleware/GuildAvailabilityMiddleware';
import {IpBanMiddleware} from '../middleware/IpBanMiddleware';
import {LocaleMiddleware} from '../middleware/LocaleMiddleware';
import {RequestCacheMiddleware} from '../middleware/RequestCacheMiddleware';
import {RequireClientIpMiddleware} from '../middleware/RequireClientIpMiddleware';
import {ServiceMiddleware} from '../middleware/ServiceMiddleware';
import {TorExitMiddleware} from '../middleware/TorExitMiddleware';
import {TrustedClientIpHeaderMiddleware} from '../middleware/TrustedClientIpHeaderMiddleware';
import {UserMiddleware} from '../middleware/UserMiddleware';
import type {HonoApp} from '../types/HonoEnv';

interface MiddlewarePipelineOptions {
	logger: ILogger;
	nodeEnv: string;
	corsOrigins: Array<string>;
	trustClientIpHeader: boolean;
	clientIpHeaderName?: string;
}

export function configureMiddleware(routes: HonoApp, options: MiddlewarePipelineOptions): void {
	const {logger, nodeEnv, corsOrigins, trustClientIpHeader, clientIpHeaderName} = options;
	const resolvedHeader = resolveClientIpHeaderName(clientIpHeaderName);
	routes.use('/webhooks/:webhook_id/:token', cors({origins: '*'}));
	routes.use('/webhooks/:webhook_id/:token/messages/:message_id', cors({origins: '*'}));
	applyMiddlewareStack(routes, {
		requestId: {},
		cors: {origins: corsOrigins, exposedHeaders: [HttpHeaders.X_FLUXER_VERSION]},
		skipLogger: true,
		skipErrorHandler: true,
	});
	routes.get('/_health', async (ctx) => ctx.text('OK'));
	routes.use(IpBanMiddleware);
	routes.use(
		requestLogger({
			log: createInfoRequestLogger(logger),
			skip: ['/_health'],
		}),
	);
	routes.use(ClientErrorAbuseSignalMiddleware);
	routes.use(RequestCacheMiddleware);
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
	if (trustClientIpHeader) {
		routes.use(
			TrustedClientIpHeaderMiddleware({
				enabled: true,
				logger,
				trustClientIpHeader,
				clientIpHeaderName: resolvedHeader,
			}),
		);
	}
	routes.use(TorExitMiddleware);
	routes.use(AuditLogMiddleware);
	routes.use(
		RequireClientIpMiddleware({
			requiredHeaders: [resolvedHeader],
		}),
	);
	routes.use(ServiceMiddleware);
	routes.use(UserMiddleware);
	routes.use(ContentFilterMiddleware);
	routes.use(GuildAvailabilityMiddleware);
	routes.use(LocaleMiddleware);
}
