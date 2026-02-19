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
import {createHealthCheckHandler, createLivenessCheckHandler, createReadinessCheckHandler} from '@app/HealthCheck';
import {createComponentLogger} from '@app/Logger';
import {
	type InitializedServices,
	initializeAllServices,
	runServiceInitialization,
	type ServiceInitializer,
	shutdownAllServices,
	startBackgroundServices,
} from '@app/ServiceInitializer';
import {getBuildMetadata} from '@fluxer/config/src/BuildMetadata';
import {AppErrorHandler, AppNotFoundHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {applyMiddlewareStack} from '@fluxer/hono/src/middleware/MiddlewareStack';
import {createServiceTelemetry} from '@fluxer/hono/src/middleware/TelemetryAdapters';
import type {BaseHonoEnv} from '@fluxer/hono_types/src/HonoTypes';
import {Hono} from 'hono';
import {trimTrailingSlash} from 'hono/trailing-slash';

export interface MountedRoutes {
	app: Hono<BaseHonoEnv>;
	services: InitializedServices;
	initialize: () => Promise<void>;
	start: () => Promise<void>;
	shutdown: () => Promise<void>;
}

export interface MountRoutesOptions {
	config: Config;
	staticDir?: string | undefined;
}

const startTime = Date.now();
const BUILD_METADATA = getBuildMetadata();

export async function mountRoutes(options: MountRoutesOptions): Promise<MountedRoutes> {
	const {config, staticDir} = options;
	const logger = createComponentLogger('routes');
	const VERSION = BUILD_METADATA.buildNumber ?? '0.0.0';

	logger.info('Starting route mounting and service initialization');

	const app = new Hono<BaseHonoEnv>();

	app.use(trimTrailingSlash());

	const telemetry = createServiceTelemetry({
		serviceName: 'fluxer-server',
		skipPaths: ['/_health', '/_ready', '/_live'],
	});

	applyMiddlewareStack(app, {
		requestId: {},
		tracing: telemetry.tracing,
		metrics: {
			enabled: true,
			collector: telemetry.metricsCollector,
			skipPaths: ['/_health', '/_ready', '/_live'],
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
			skip: ['/_health', '/_ready', '/_live'],
		},
		skipErrorHandler: true,
	});
	let initializers: Array<ServiceInitializer> = [];
	let services: InitializedServices = {};

	try {
		const result = await initializeAllServices({
			config,
			logger,
			staticDir,
		});

		initializers = result.initializers;
		services = result.services;

		if (services.s3 !== undefined) {
			app.route('/s3', services.s3.app);
			logger.info(config.isMonolith ? 'S3 service mounted at /s3 (restricted mode)' : 'S3 service mounted at /s3');
		}

		if (services.mediaProxy !== undefined) {
			app.route('/media', services.mediaProxy.app);
			logger.info(
				config.isMonolith
					? 'Media Proxy service mounted at /media (public-only mode)'
					: 'Media Proxy service mounted at /media',
			);
		}

		if (services.admin !== undefined) {
			app.route('/admin', services.admin.app);
			logger.info('Admin service mounted at /admin');
		}

		if (services.api !== undefined) {
			const apiService = services.api;
			app.route('/api', apiService.app);
			app.get('/.well-known/fluxer', (ctx) => apiService.app.fetch(ctx.req.raw));
			logger.info('API service mounted at /api');
		}

		const healthHandler = createHealthCheckHandler({
			services,
			staticDir,
			version: VERSION,
			startTime,
			latencyThresholdMs: config.healthCheck.latencyThresholdMs,
		});
		app.get('/_health', healthHandler);
		logger.info('Health check endpoint mounted at /_health');

		const readinessHandler = createReadinessCheckHandler({
			services,
			staticDir,
			version: VERSION,
			startTime,
			latencyThresholdMs: config.healthCheck.latencyThresholdMs,
		});
		app.get('/_ready', readinessHandler);
		logger.info('Readiness check endpoint mounted at /_ready');

		const livenessHandler = createLivenessCheckHandler();
		app.get('/_live', livenessHandler);
		logger.info('Liveness check endpoint mounted at /_live');

		if (services.appServer !== undefined) {
			app.route('/', services.appServer.app);
			logger.info('SPA App server mounted at /');
		}

		app.onError(AppErrorHandler);
		app.notFound(AppNotFoundHandler);

		logger.info({serviceCount: initializers.length}, 'All services mounted successfully');
	} catch (error) {
		logger.error({error: error instanceof Error ? error.message : 'Unknown error'}, 'Failed to mount routes');
		throw error;
	}

	const initialize = async (): Promise<void> => {
		await runServiceInitialization(initializers, logger);
	};

	const start = async (): Promise<void> => {
		await startBackgroundServices(initializers, logger);
	};

	const shutdown = async (): Promise<void> => {
		await shutdownAllServices(initializers, logger);
	};

	return {
		app,
		services,
		initialize,
		start,
		shutdown,
	};
}
