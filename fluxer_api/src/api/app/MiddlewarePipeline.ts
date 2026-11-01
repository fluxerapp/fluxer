// SPDX-License-Identifier: AGPL-3.0-or-later

import {ActivityContextMiddleware} from '@app/api/infrastructure/activity/ActivityMeta';
import {AuditLogMiddleware} from '@app/api/middleware/AuditLogMiddleware';
import {ClientFeaturesMiddleware} from '@app/api/middleware/ClientFeaturesMiddleware';
import {ConcurrencyLimitMiddleware} from '@app/api/middleware/ConcurrencyLimitMiddleware';
import ContentFilterMiddleware from '@app/api/middleware/ContentFilterMiddleware';
import {DateOfBirthGateMiddleware} from '@app/api/middleware/DateOfBirthGateMiddleware';
import {GuildAvailabilityMiddleware} from '@app/api/middleware/GuildAvailabilityMiddleware';
import {GuildVisitorRequestContextMiddleware} from '@app/api/middleware/GuildVisitorRequestContextMiddleware';
import {IpBanMiddleware} from '@app/api/middleware/IpBanMiddleware';
import {LocaleMiddleware} from '@app/api/middleware/LocaleMiddleware';
import {RequestBodyLimitMiddleware} from '@app/api/middleware/RequestBodyLimitMiddleware';
import {RequestCacheMiddleware} from '@app/api/middleware/RequestCacheMiddleware';
import {RequestErrorTelemetry} from '@app/api/middleware/RequestErrorTelemetry';
import {RequireClientIpMiddleware} from '@app/api/middleware/RequireClientIpMiddleware';
import {ServiceMiddleware} from '@app/api/middleware/ServiceMiddleware';
import {TrustedClientIpHeaderMiddleware} from '@app/api/middleware/TrustedClientIpHeaderMiddleware';
import {UserMiddleware} from '@app/api/middleware/UserMiddleware';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {PeerAddressMiddleware} from '@app/api/utils/RequestClientIp';
import {embeddedWorkerStalledComponents} from '@app/api/worker/EmbeddedWorker';
import {Headers as HttpHeaders} from '@fluxer/constants/src/Headers';
import {InvalidApiOriginError} from '@fluxer/errors/src/domains/core/InvalidApiOriginError';
import {cors} from '@fluxer/hono/src/middleware/Cors';
import {applyMiddlewareStack} from '@fluxer/hono/src/middleware/MiddlewareStack';
import {createInfoRequestLogger, requestLogger} from '@fluxer/hono/src/middleware/RequestLogger';
import {resolveClientIpHeaderName} from '@fluxer/ip_utils/src/ClientIp';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';

const WEB_HOST_ALLOWED_ORIGINS: ReadonlyMap<string, ReadonlySet<string>> = new Map([
	['web.fluxer.app', new Set(['https://web.fluxer.app', 'https://fluxer.com'])],
	['web.canary.fluxer.app', new Set(['https://web.canary.fluxer.app', 'https://canary.fluxer.com'])],
]);

interface MiddlewarePipelineOptions {
	logger: LoggerInterface;
	nodeEnv: string;
	corsOrigins: Array<string>;
	trustClientIpHeader: boolean;
	clientIpHeaderName?: string;
	maxInflightRequests: number;
}

export function configureMiddleware(routes: HonoApp, options: MiddlewarePipelineOptions): void {
	const {logger, nodeEnv, corsOrigins, trustClientIpHeader, clientIpHeaderName, maxInflightRequests} = options;
	const resolvedHeader = resolveClientIpHeaderName(clientIpHeaderName);
	routes.use(PeerAddressMiddleware);
	routes.use('/webhooks/:webhook_id/:token', cors({origins: '*'}));
	routes.use('/webhooks/:webhook_id/:token/messages/:message_id', cors({origins: '*'}));
	routes.use(
		'/.well-known/fluxer',
		cors({
			origins: '*',
			methods: ['GET', 'HEAD', 'OPTIONS'],
			allowedHeaders: [
				HttpHeaders.ACCEPT,
				HttpHeaders.CONTENT_TYPE,
				HttpHeaders.IF_MODIFIED_SINCE,
				HttpHeaders.IF_NONE_MATCH,
			],
			exposedHeaders: [HttpHeaders.ETAG, HttpHeaders.LAST_MODIFIED],
		}),
	);
	applyMiddlewareStack(routes, {
		requestId: {},
		cors: {
			origins: corsOrigins,
			allowedHeaders: [
				HttpHeaders.CONTENT_TYPE,
				HttpHeaders.AUTHORIZATION,
				HttpHeaders.X_REQUESTED_WITH,
				HttpHeaders.ACCEPT_LANGUAGE,
				HttpHeaders.X_REQUEST_ID,
				HttpHeaders.IF_NONE_MATCH,
				HttpHeaders.X_AUDIT_LOG_REASON,
				HttpHeaders.X_CAPTCHA_ID,
				HttpHeaders.X_CAPTCHA_TOKEN,
				HttpHeaders.X_FLUXER_CAPABILITIES,
				HttpHeaders.X_FLUXER_CLIENT_INSTALLATION_ID,
				HttpHeaders.X_FLUXER_FEATURES,
				HttpHeaders.X_FLUXER_PLATFORM,
				HttpHeaders.X_FLUXER_SUDO_MODE_JWT,
			],
			exposedHeaders: [HttpHeaders.X_FLUXER_VERSION, HttpHeaders.ETAG],
		},
	});
	routes.use(ConcurrencyLimitMiddleware({maxInflightRequests}));
	routes.get('/_health', async (ctx) =>
		embeddedWorkerStalledComponents().length > 0 ? ctx.text('Worker stalled', 503) : ctx.text('OK'),
	);
	routes.use(IpBanMiddleware);
	routes.use(
		requestLogger({
			log: createInfoRequestLogger(logger),
			skip: ['/_health'],
		}),
	);
	routes.use(RequestErrorTelemetry);
	routes.use(RequestBodyLimitMiddleware);
	routes.use(RequestCacheMiddleware);
	routes.use(ClientFeaturesMiddleware);
	if (nodeEnv === 'production') {
		routes.use('*', async (ctx, next) => {
			const allowedOrigins = WEB_HOST_ALLOWED_ORIGINS.get(ctx.req.header('host') ?? '');
			if (ctx.req.method !== 'GET' && allowedOrigins !== undefined) {
				const origin = ctx.req.header('origin');
				if (!origin || !allowedOrigins.has(origin)) {
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
	routes.use(AuditLogMiddleware);
	routes.use(RequireClientIpMiddleware());
	routes.use(ActivityContextMiddleware);
	routes.use(ServiceMiddleware);
	routes.use(UserMiddleware);
	routes.use(DateOfBirthGateMiddleware);
	routes.use(GuildVisitorRequestContextMiddleware);
	routes.use(ContentFilterMiddleware);
	routes.use(GuildAvailabilityMiddleware);
	routes.use(LocaleMiddleware);
}
