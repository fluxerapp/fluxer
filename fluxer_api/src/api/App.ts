// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppErrorHandler, AppNotFoundHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {IpBannedError} from '@fluxer/errors/src/domains/moderation/IpBannedError';
import {resolveErrorStatus} from '@fluxer/errors/src/error_handling/ErrorIntrospection';
import {createMetricsMiddleware} from '@fluxer/hono/src/middleware/Metrics';
import {LOCKED_DOWN_PERMISSIONS_POLICY, securityHeaders} from '@fluxer/hono/src/middleware/SecurityHeaders';
import {setIsDevelopment} from '@fluxer/schema/src/primitives/UrlValidators';
import type {Context} from 'hono';
import {Hono} from 'hono';
import {createInitializer, createShutdown} from './app/APILifecycle';
import {registerControllers} from './app/ControllerRegistry';
import {configureMiddleware} from './app/MiddlewarePipeline';
import type {APIConfig} from './config/APIConfig';
import type {ILogger} from './ILogger';
import {recordHttpClientError} from './middleware/AbusiveIpAutoBanner';
import type {HonoApp, HonoEnv} from './types/HonoEnv';

interface CreateAPIAppOptions {
	config: APIConfig;
	logger: ILogger;
}

interface APIAppResult {
	app: HonoApp;
	initialize: () => Promise<void>;
	shutdown: () => Promise<void>;
}

function AbuseAwareAppErrorHandler(err: Error, ctx: Context<HonoEnv>): Response | Promise<Response> {
	if (!(err instanceof IpBannedError)) {
		const status = resolveErrorStatus(err);
		if (status !== null && !ctx.get('user')) {
			recordHttpClientError(ctx.req.raw, status);
		}
	}
	return AppErrorHandler(err, ctx);
}

export async function createAPIApp(options: CreateAPIAppOptions): Promise<APIAppResult> {
	const {config, logger} = options;
	const shutdownApiLifecycle = createShutdown(config, logger);
	setIsDevelopment(config.nodeEnv === 'development');
	const routes = new Hono<HonoEnv>({strict: true});
	configureMiddleware(routes, {
		logger,
		nodeEnv: config.nodeEnv,
		corsOrigins: [config.endpoints.webApp, config.endpoints.marketing],
		trustClientIpHeader: config.proxy.trust_client_ip_header,
		clientIpHeaderName: config.proxy.client_ip_header,
	});
	routes.onError(AbuseAwareAppErrorHandler);
	routes.notFound(AppNotFoundHandler);
	registerControllers(routes, config);
	const app = new Hono<HonoEnv>({strict: true});
	const {middleware: metricsMiddleware, metricsHandler} = createMetricsMiddleware('api');
	app.use('*', metricsMiddleware);
	app.get('/_metrics', metricsHandler);
	app.use(
		'*',
		securityHeaders({
			contentSecurityPolicy: "default-src 'none'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'",
			permissionsPolicy: LOCKED_DOWN_PERMISSIONS_POLICY,
		}),
	);
	app.route('/v1', routes);
	app.route('/', routes);
	app.onError(AbuseAwareAppErrorHandler);
	app.notFound(AppNotFoundHandler);
	return {
		app,
		initialize: createInitializer(config, logger),
		shutdown: shutdownApiLifecycle,
	};
}
