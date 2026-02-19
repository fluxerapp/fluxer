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

import type {Config} from '@app/Config';
import {requireValue} from '@app/utils/ConfigUtils';
import {type AdminAppResult, createAdminApp} from '@fluxer/admin/src/App';
import type {AdminConfig} from '@fluxer/admin/src/types/Config';
import {type APIAppResult, createAPIApp} from '@fluxer/api/src/App';
import {buildAPIConfigFromMaster, initializeConfig} from '@fluxer/api/src/Config';
import {DirectMediaService} from '@fluxer/api/src/infrastructure/DirectMediaService';
import {initializeLogger} from '@fluxer/api/src/Logger';
import {
	setInjectedKVProvider,
	setInjectedMediaService,
	setInjectedS3Service,
	setInjectedWorkerService,
} from '@fluxer/api/src/middleware/ServiceRegistry';
import {createTestHarnessResetHandler, registerTestHarnessReset} from '@fluxer/api/src/test/TestHarnessReset';
import {JetStreamWorkerQueue} from '@fluxer/api/src/worker/JetStreamWorkerQueue';
import {WorkerService} from '@fluxer/api/src/worker/WorkerService';
import {createAppServer} from '@fluxer/app_proxy/src/AppServer';
import type {AppServerResult} from '@fluxer/app_proxy/src/AppServerTypes';
import {getBuildMetadata} from '@fluxer/config/src/BuildMetadata';
import {ADMIN_OAUTH2_APPLICATION_ID} from '@fluxer/constants/src/Core';
import {createServiceTelemetry} from '@fluxer/hono/src/middleware/TelemetryAdapters';
import type {IKVProvider} from '@fluxer/kv_client/src/IKVProvider';
import {KVClient} from '@fluxer/kv_client/src/KVClient';
import type {Logger} from '@fluxer/logger/src/Logger';
import type {MediaProxyAppResult} from '@fluxer/media_proxy/src/App';
import {createMediaProxyApp} from '@fluxer/media_proxy/src/App';
import {JetStreamConnectionManager} from '@fluxer/nats/src/JetStreamConnectionManager';
import type {S3AppResult} from '@fluxer/s3/src/App';
import {createS3App} from '@fluxer/s3/src/App';
import {setUser} from '@fluxer/sentry/src/Sentry';

export interface ServiceInitializationContext {
	config: Config;
	logger: Logger;
	staticDir?: string;
}

export interface InitializedServices {
	kv?: IKVProvider;
	s3?: S3AppResult;
	jsConnectionManager?: JetStreamConnectionManager;
	mediaProxy?: MediaProxyAppResult;
	admin?: AdminAppResult;
	api?: APIAppResult;
	appServer?: AppServerResult;
}

export interface ServiceInitializer {
	name: string;
	initialize?: () => Promise<void> | void;
	start?: () => Promise<void>;
	shutdown: () => Promise<void>;
	service: unknown;
}

function createKVProvider(config: Config): IKVProvider {
	return new KVClient({
		url: requireValue(config.internal.kv, 'internal.kv'),
	});
}

function createS3Initializer(context: ServiceInitializationContext): ServiceInitializer {
	const {config, logger} = context;
	const componentLogger = logger.child({component: 's3'});
	const telemetry = createServiceTelemetry({serviceName: 'fluxer-s3', skipPaths: ['/_health']});

	const globalS3Config = requireValue(config.s3, 's3');
	const bucketConfig = requireValue(globalS3Config.buckets, 's3.buckets');
	const buckets = Object.values(bucketConfig) as Array<string>;

	const s3App = createS3App({
		logger: componentLogger,
		s3Config: {
			root: requireValue(config.services.s3?.data_dir, 'services.s3.data_dir'),
			buckets,
		},
		authConfig: {
			accessKey: requireValue(globalS3Config.access_key_id, 's3.access_key_id'),
			secretKey: requireValue(globalS3Config.secret_access_key, 's3.secret_access_key'),
		},
		metricsCollector: telemetry.metricsCollector,
		tracing: telemetry.tracing,
	});

	return {
		name: 'S3',
		initialize: async () => {
			componentLogger.info('Initializing S3 storage buckets');
			await s3App.initialize();
		},
		shutdown: async () => {
			componentLogger.info('Shutting down S3 service');
			s3App.shutdown();
		},
		service: s3App,
	};
}

function createJetStreamInitializer(context: ServiceInitializationContext): ServiceInitializer {
	const {config, logger} = context;
	const baseLogger = logger.child({component: 'jetstream'});

	const natsConfig = config.services.nats;
	const connectionManager = new JetStreamConnectionManager({
		url: natsConfig?.jetstream_url ?? 'nats://127.0.0.1:4223',
		token: natsConfig?.auth_token || undefined,
		name: 'fluxer-server-worker',
	});

	return {
		name: 'JetStream',
		initialize: async () => {
			await connectionManager.connect();
			baseLogger.info('JetStream connection established');

			const workerQueue = new JetStreamWorkerQueue(connectionManager);
			await workerQueue.ensureInfrastructure();
			baseLogger.info('JetStream stream and consumer verified');

			const workerService = new WorkerService(workerQueue);
			setInjectedWorkerService(workerService);
			baseLogger.info('JetStream worker service injected');
		},
		shutdown: async () => {
			baseLogger.info('Draining JetStream connection');
			await connectionManager.drain();
		},
		service: connectionManager,
	};
}

interface MediaProxyInitializerOptions {
	publicOnly?: boolean;
}

async function createMediaProxyInitializer(
	context: ServiceInitializationContext,
	options: MediaProxyInitializerOptions = {},
): Promise<ServiceInitializer> {
	const {config, logger} = context;
	const {publicOnly = false} = options;
	const componentLogger = logger.child({component: 'media-proxy'});
	const telemetry = createServiceTelemetry({
		serviceName: 'fluxer-media-proxy',
		skipPaths: ['/_health', '/internal/telemetry'],
	});

	const globalS3Config = requireValue(config.s3, 's3');
	const bucketCdn = requireValue(globalS3Config.buckets?.cdn, 's3.buckets.cdn');
	const bucketUploads = requireValue(globalS3Config.buckets?.uploads, 's3.buckets.uploads');
	const s3Host = requireValue(config.services.s3?.host, 'services.s3.host');
	const s3Port = requireValue(config.services.s3?.port, 'services.s3.port');
	const s3Endpoint = globalS3Config.endpoint ?? `http://${s3Host}:${s3Port}`;

	const mediaProxySecretKey = requireValue(config.services.media_proxy?.secret_key, 'services.media_proxy.secret_key');

	const mediaProxyApp = await createMediaProxyApp({
		logger: componentLogger,
		config: {
			nodeEnv: config.env === 'development' ? 'development' : 'production',
			secretKey: mediaProxySecretKey,
			requireCloudflareEdge: false,
			staticMode: false,
			s3: {
				endpoint: s3Endpoint,
				region: requireValue(globalS3Config.region, 's3.region'),
				accessKeyId: requireValue(globalS3Config.access_key_id, 's3.access_key_id'),
				secretAccessKey: requireValue(globalS3Config.secret_access_key, 's3.secret_access_key'),
				bucketCdn,
				bucketUploads,
			},
		},
		requestMetricsCollector: telemetry.metricsCollector,
		requestTracing: telemetry.tracing,
		publicOnly,
	});

	return {
		name: 'Media Proxy',
		initialize: () => {
			componentLogger.info('Media Proxy service initialized');
		},
		shutdown: async () => {
			componentLogger.info('Shutting down Media Proxy service');
			await mediaProxyApp.shutdown();
		},
		service: mediaProxyApp,
	};
}

function createAdminInitializer(
	context: ServiceInitializationContext,
	kvProvider?: IKVProvider | null,
): ServiceInitializer {
	const {config, logger} = context;
	const componentLogger = logger.child({component: 'admin'});

	const adminConfigSrc = requireValue(config.services.admin, 'services.admin');
	const adminBasePath = requireValue(adminConfigSrc.base_path, 'services.admin.base_path');
	const adminEndpoint = requireValue(config.endpoints.admin, 'endpoints.admin');
	const adminRateLimit =
		adminConfigSrc.rate_limit && adminConfigSrc.rate_limit.limit != null && adminConfigSrc.rate_limit.window_ms != null
			? {
					limit: adminConfigSrc.rate_limit.limit,
					windowMs: adminConfigSrc.rate_limit.window_ms,
				}
			: undefined;
	const buildMetadata = getBuildMetadata();
	const adminOAuthRedirectUri = `${adminEndpoint}/oauth2_callback`;

	const adminConfig: AdminConfig = {
		env: config.env,
		secretKeyBase: requireValue(adminConfigSrc.secret_key_base, 'services.admin.secret_key_base'),
		apiEndpoint: requireValue(config.endpoints.api, 'endpoints.api'),
		mediaEndpoint: requireValue(config.endpoints.media, 'endpoints.media'),
		staticCdnEndpoint: requireValue(config.endpoints.static_cdn, 'endpoints.static_cdn'),
		adminEndpoint,
		webAppEndpoint: requireValue(config.endpoints.app, 'endpoints.app'),
		kvUrl: requireValue(config.internal.kv, 'internal.kv'),
		oauthClientId: ADMIN_OAUTH2_APPLICATION_ID.toString(),
		oauthClientSecret: requireValue(adminConfigSrc.oauth_client_secret, 'services.admin.oauth_client_secret'),
		oauthRedirectUri: adminOAuthRedirectUri,
		basePath: adminBasePath,
		selfHosted: config.instance.self_hosted,
		buildTimestamp: buildMetadata.buildTimestamp,
		releaseChannel: buildMetadata.releaseChannel,
		rateLimit: adminRateLimit,
	};

	const adminApp = createAdminApp({
		config: adminConfig,
		logger: componentLogger,
		kvProvider,
	});

	return {
		name: 'Admin',
		initialize: () => {
			componentLogger.info('Admin service initialized');
		},
		shutdown: async () => {
			componentLogger.info('Shutting down Admin service');
			adminApp.shutdown();
		},
		service: adminApp,
	};
}

function createAppServerInitializer(context: ServiceInitializationContext): ServiceInitializer {
	const {config, logger, staticDir} = context;
	const componentLogger = logger.child({component: 'app'});
	const telemetry = createServiceTelemetry({serviceName: 'fluxer-app', skipPaths: ['/_health']});

	if (staticDir === undefined) {
		throw new Error('Static directory is required for App Server');
	}

	const publicUrlHost = new URL(requireValue(config.endpoints.app, 'endpoints.app')).origin;
	const mediaUrlHost = new URL(requireValue(config.endpoints.media, 'endpoints.media')).origin;

	const appServer = createAppServer({
		staticDir,
		logger: componentLogger,
		env: config.env,
		telemetry: {
			metricsCollector: telemetry.metricsCollector,
			tracing: telemetry.tracing,
		},
		cspDirectives: {
			defaultSrc: ["'self'"],
			scriptSrc: ["'self'", "'unsafe-inline'"],
			styleSrc: ["'self'", "'unsafe-inline'"],
			imgSrc: ["'self'", 'data:', 'blob:', publicUrlHost, mediaUrlHost],
			connectSrc: ["'self'", 'wss:', 'ws:', publicUrlHost],
			fontSrc: ["'self'"],
			mediaSrc: ["'self'", 'blob:', mediaUrlHost],
			frameSrc: ["'none'"],
		},
	});

	return {
		name: 'App Server',
		initialize: () => {
			componentLogger.info({staticDir}, 'SPA App server initialized');
		},
		shutdown: async () => {
			componentLogger.info('Shutting down App server');
			appServer.shutdown();
		},
		service: appServer,
	};
}

async function createAPIInitializer(context: ServiceInitializationContext): Promise<ServiceInitializer> {
	const {config, logger} = context;
	const componentLogger = logger.child({component: 'api'});

	const apiConfig = buildAPIConfigFromMaster(config);

	initializeConfig(apiConfig);
	initializeLogger(componentLogger);

	const apiApp = await createAPIApp({
		config: apiConfig,
		logger: componentLogger,
		setSentryUser: setUser,
		isTelemetryActive: () => config.telemetry.enabled,
	});

	return {
		name: 'API',
		initialize: async () => {
			componentLogger.info('Running API service initialization');
			await apiApp.initialize();
		},
		shutdown: async () => {
			componentLogger.info('Shutting down API service');
			await apiApp.shutdown();
		},
		service: apiApp,
	};
}

export async function initializeAllServices(context: ServiceInitializationContext): Promise<{
	services: InitializedServices;
	initializers: Array<ServiceInitializer>;
}> {
	const {staticDir} = context;
	const rootLogger = context.logger.child({component: 'service-initializer'});

	const initializers: Array<ServiceInitializer> = [];
	const services: InitializedServices = {};
	let kvProvider: IKVProvider | null = null;

	rootLogger.info('Starting service initialization');

	try {
		rootLogger.info('Initializing KV provider');
		kvProvider = createKVProvider(context.config);
		services.kv = kvProvider;
		setInjectedKVProvider(kvProvider);

		rootLogger.info('Initializing S3 service');
		const s3Init = createS3Initializer(context);
		initializers.push(s3Init);
		services.s3 = s3Init.service as S3AppResult;

		if (services.s3) {
			rootLogger.info('Wiring DirectS3StorageService for in-process communication');
			setInjectedS3Service(services.s3.getS3Service());
		}

		rootLogger.info('Initializing JetStream worker queue');
		const jetStreamInit = createJetStreamInitializer(context);
		initializers.push(jetStreamInit);
		services.jsConnectionManager = jetStreamInit.service as JetStreamConnectionManager;

		rootLogger.info('Initializing Media Proxy service');
		const mediaProxyInit = await createMediaProxyInitializer(context, {publicOnly: context.config.isMonolith});
		initializers.push(mediaProxyInit);
		services.mediaProxy = mediaProxyInit.service as MediaProxyAppResult;

		if (services.mediaProxy.services) {
			rootLogger.info('Wiring DirectMediaService for in-process communication');
			const directMediaService = new DirectMediaService({
				metadataService: services.mediaProxy.services.metadataService,
				frameService: services.mediaProxy.services.frameService,
				mediaProxyEndpoint: requireValue(context.config.endpoints.media, 'endpoints.media'),
				mediaProxySecretKey: requireValue(
					context.config.services.media_proxy?.secret_key,
					'services.media_proxy.secret_key',
				),
				logger: context.logger.child({component: 'direct-media-service'}),
			});
			setInjectedMediaService(directMediaService);
		}

		rootLogger.info('Initializing Admin service');
		const adminInit = createAdminInitializer(context, kvProvider);
		initializers.push(adminInit);
		services.admin = adminInit.service as AdminAppResult;

		rootLogger.info('Initializing API service');
		const apiInit = await createAPIInitializer(context);
		initializers.push(apiInit);
		services.api = apiInit.service as APIAppResult;

		if (staticDir !== undefined) {
			rootLogger.info('Initializing App Server');
			const appServerInit = createAppServerInitializer(context);
			initializers.push(appServerInit);
			services.appServer = appServerInit.service as AppServerResult;
		} else {
			rootLogger.info('No static directory configured, SPA App server disabled');
		}

		rootLogger.info({serviceCount: initializers.length}, 'All services created successfully');

		if (context.config.dev.test_mode_enabled) {
			registerTestHarnessReset(
				createTestHarnessResetHandler({
					kvProvider: services.kv,
					s3Service: services.s3?.getS3Service(),
				}),
			);
		}

		return {services, initializers};
	} catch (error) {
		rootLogger.error(
			{error: error instanceof Error ? error.message : 'Unknown error'},
			'Failed to initialize services',
		);
		throw error;
	}
}

export async function runServiceInitialization(initializers: Array<ServiceInitializer>, logger: Logger): Promise<void> {
	const rootLogger = logger.child({component: 'service-initializer'});

	rootLogger.info('Running service initialization tasks');

	for (const initializer of initializers) {
		try {
			if (initializer.initialize !== undefined) {
				rootLogger.debug({service: initializer.name}, 'Running initialization');
				await initializer.initialize();
			}
		} catch (error) {
			rootLogger.error(
				{service: initializer.name, error: error instanceof Error ? error.message : 'Unknown error'},
				'Service initialization failed',
			);
			throw error;
		}
	}

	rootLogger.info('All service initialization tasks completed');
}

export async function startBackgroundServices(initializers: Array<ServiceInitializer>, logger: Logger): Promise<void> {
	const rootLogger = logger.child({component: 'service-initializer'});

	rootLogger.info('Starting background services');

	for (const initializer of initializers) {
		try {
			if (initializer.start !== undefined) {
				rootLogger.debug({service: initializer.name}, 'Starting background tasks');
				await initializer.start();
			}
		} catch (error) {
			rootLogger.error(
				{service: initializer.name, error: error instanceof Error ? error.message : 'Unknown error'},
				'Failed to start background service',
			);
			throw error;
		}
	}

	rootLogger.info('All background services started');
}

export async function shutdownAllServices(initializers: Array<ServiceInitializer>, logger: Logger): Promise<void> {
	const rootLogger = logger.child({component: 'service-initializer'});

	rootLogger.info('Beginning graceful shutdown of all services');

	for (const initializer of initializers.reverse()) {
		try {
			rootLogger.debug({service: initializer.name}, 'Shutting down service');
			await initializer.shutdown();
		} catch (error) {
			rootLogger.error(
				{service: initializer.name, error: error instanceof Error ? error.message : 'Unknown error'},
				'Error during service shutdown',
			);
		}
	}

	rootLogger.info('All services shut down');
}
