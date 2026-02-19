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

import {createRequireAuth} from '@fluxer/admin/src/middleware/Auth';
import {csrfMiddleware, initializeCsrf} from '@fluxer/admin/src/middleware/Csrf';
import {createAdminErrorHandler} from '@fluxer/admin/src/middleware/ErrorHandler';
import {resolveAdminPublicDir} from '@fluxer/admin/src/PublicDir';
import {createAdminRoutes} from '@fluxer/admin/src/routes/Admin';
import {createAuthRoutes} from '@fluxer/admin/src/routes/Auth';
import {createBansRoutes} from '@fluxer/admin/src/routes/Bans';
import {createCodesRoutes} from '@fluxer/admin/src/routes/Codes';
import {createDiscoveryRoutes} from '@fluxer/admin/src/routes/Discovery';
import {createGuildsRoutes} from '@fluxer/admin/src/routes/Guilds';
import {createMessagesRoutes} from '@fluxer/admin/src/routes/Messages';
import {createReportsRoutes} from '@fluxer/admin/src/routes/Reports';
import type {RouteFactory} from '@fluxer/admin/src/routes/RouteTypes';
import {createSystemRoutes} from '@fluxer/admin/src/routes/System';
import {createUsersRoutes} from '@fluxer/admin/src/routes/Users';
import {createVisionarySlotsRoutes} from '@fluxer/admin/src/routes/VisionarySlots';
import {createVoiceRoutes} from '@fluxer/admin/src/routes/Voice';
import type {AppVariables} from '@fluxer/admin/src/types/App';
import type {AdminConfig} from '@fluxer/admin/src/types/Config';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import {KVCacheProvider} from '@fluxer/cache/src/providers/KVCacheProvider';
import {cacheHeaders} from '@fluxer/hono/src/middleware/CacheHeaders';
import {applyMiddlewareStack} from '@fluxer/hono/src/middleware/MiddlewareStack';
import {normalizeEndpointOrigin, validateOutboundEndpointUrl} from '@fluxer/hono/src/security/OutboundEndpoint';
import type {MetricsCollector} from '@fluxer/hono_types/src/MetricsTypes';
import type {TracingOptions} from '@fluxer/hono_types/src/TracingTypes';
import type {IKVProvider} from '@fluxer/kv_client/src/IKVProvider';
import {KVClient} from '@fluxer/kv_client/src/KVClient';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {throwKVRequiredError} from '@fluxer/rate_limit/src/KVRequiredError';
import {RateLimitService} from '@fluxer/rate_limit/src/RateLimitService';
import {serveStatic} from '@hono/node-server/serve-static';
import {Hono} from 'hono';

export interface CreateAdminAppOptions {
	config: AdminConfig;
	logger: LoggerInterface;
	assetVersion?: string;
	metricsCollector?: MetricsCollector;
	tracing?: TracingOptions;
	cacheService?: ICacheService;
	kvProvider?: IKVProvider | null;
}

export interface AdminAppResult {
	app: Hono<{Variables: AppVariables}>;
	shutdown: () => void;
}

export function createAdminApp(options: CreateAdminAppOptions): AdminAppResult {
	const {logger, assetVersion = Date.now().toString(), metricsCollector, tracing} = options;
	const config = normalizeAdminSecurityConfig(options.config);

	const app = new Hono<{Variables: AppVariables}>();
	const publicDir = resolveAdminPublicDir();

	let rateLimitService: RateLimitService | null = null;

	initializeCsrf(config.secretKeyBase, config.env === 'production');

	app.use(
		'/static/*',
		serveStatic({
			root: publicDir,
			rewriteRequestPath: (path: string) => toRelativeStaticPath(stripLeadingBasePath(path, config.basePath)),
			onNotFound: (_path) => {
				logger.error(
					{
						publicDir,
						cwd: process.cwd(),
					},
					'Admin static asset not found (expected packages/admin/public/static/app.css to exist)',
				);
			},
		}),
	);

	const kvProvider = options.kvProvider ?? createHttpKVProvider(config, logger);

	if (!kvProvider) {
		throwKVRequiredError({
			serviceName: 'Admin panel',
			configPath: 'config.kvUrl',
			fluxerServerHint: 'kvProvider is passed in as an option',
		});
	}

	const cacheService = options.cacheService ?? new KVCacheProvider({client: kvProvider});
	rateLimitService = new RateLimitService(cacheService);

	applyMiddlewareStack(app, {
		requestId: {},
		tracing,
		metrics: metricsCollector
			? {
					enabled: true,
					collector: metricsCollector,
					skipPaths: ['/_health', '/robots.txt'],
				}
			: undefined,
		logger: {
			log: (data) => {
				logger.debug(
					{
						method: data.method,
						path: data.path,
						status: data.status,
						durationMs: data.durationMs,
					},
					'Request completed',
				);
			},
			skip: ['/_health', '/robots.txt'],
		},
		rateLimit: rateLimitService
			? {
					enabled: true,
					service: rateLimitService,
					maxAttempts: 100,
					windowMs: 60000,
					skipPaths: ['/_health', '/robots.txt'],
				}
			: undefined,
		customMiddleware: [cacheHeaders(), csrfMiddleware],
		skipErrorHandler: true,
	});

	app.use('*', async (c, next) => {
		if (c.req.query('sh') === '1') {
			c.set('selfHostedOverride', true);
		}
		await next();
		if (c.get('selfHostedOverride')) {
			const location = c.res.headers.get('Location');
			if (location?.startsWith('/') && !location.includes('sh=1')) {
				const separator = location.includes('?') ? '&' : '?';
				const newHeaders = new Headers(c.res.headers);
				newHeaders.set('Location', `${location}${separator}sh=1`);
				c.res = new Response(c.res.body, {
					status: c.res.status,
					headers: newHeaders,
				});
			}
		}
	});

	app.onError(createAdminErrorHandler(logger, config.env === 'development', config.basePath));

	app.get('/_health', (c) => c.json({status: 'ok'}));

	app.get('/robots.txt', (c) => {
		return c.text('User-agent: *\nDisallow: /\n');
	});

	const requireAuth = createRequireAuth(config, assetVersion);
	const deps = {config, assetVersion, requireAuth};

	const routeFactories: Array<RouteFactory> = [
		createAuthRoutes,
		createSystemRoutes,
		createUsersRoutes,
		createGuildsRoutes,
		createBansRoutes,
		createReportsRoutes,
		createMessagesRoutes,
		createVoiceRoutes,
		createCodesRoutes,
		createDiscoveryRoutes,
		createVisionarySlotsRoutes,
		createAdminRoutes,
	];

	for (const factory of routeFactories) {
		app.route('/', factory(deps));
	}

	const shutdown = (): void => {
		logger.info('Admin app shutting down');
	};

	return {app, shutdown};
}

function createHttpKVProvider(config: AdminConfig, _logger: LoggerInterface): IKVProvider | null {
	if (!config.kvUrl) {
		return null;
	}

	return new KVClient({url: config.kvUrl});
}

function normalizeAdminSecurityConfig(config: AdminConfig): AdminConfig {
	const isProduction = config.env === 'production';
	const apiEndpoint = validateOutboundEndpointUrl(config.apiEndpoint, {
		name: 'admin.apiEndpoint',
		allowHttp: !isProduction,
		allowLocalhost: !isProduction,
		allowPrivateIpLiterals: !isProduction,
	});

	return {
		...config,
		apiEndpoint: normalizeEndpointOrigin(apiEndpoint),
		kvUrl: normalizeKVUrl(config.kvUrl),
	};
}

function normalizeKVUrl(rawKvUrl: string): string {
	const kvUrl = new URL(rawKvUrl);
	if (kvUrl.protocol !== 'redis:' && kvUrl.protocol !== 'rediss:') {
		throw new Error('admin.kvUrl must use redis or rediss');
	}
	if (kvUrl.search || kvUrl.hash) {
		throw new Error('admin.kvUrl must not include query string or fragment');
	}
	return rawKvUrl.trim();
}

function stripLeadingBasePath(path: string, basePath: string): string {
	if (!basePath) return path;
	if (path === basePath) return '';
	if (path.startsWith(`${basePath}/`)) return path.slice(basePath.length);
	return path;
}

function toRelativeStaticPath(path: string): string {
	if (!path) return path;
	return path.startsWith('/') ? path.slice(1) : path;
}
