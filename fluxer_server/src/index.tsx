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

import type {Server} from 'node:http';
import {Config, type Config as FluxerServerConfig} from '@app/Config';
import {shutdownInstrumentation} from '@app/Instrument';
import {createComponentLogger, Logger} from '@app/Logger';
import {mountRoutes} from '@app/Routes';
import {createGatewayProcessManager, type GatewayProcessManager} from '@app/utils/GatewayProcessManager';
import {createGatewayProxy} from '@app/utils/GatewayProxy';
import {getSnowflakeService} from '@fluxer/api/src/middleware/ServiceRegistry';
import {CronScheduler} from '@fluxer/api/src/worker/CronScheduler';
import {JetStreamWorkerQueue} from '@fluxer/api/src/worker/JetStreamWorkerQueue';
import {setWorkerDependencies} from '@fluxer/api/src/worker/WorkerContext';
import {initializeWorkerDependencies} from '@fluxer/api/src/worker/WorkerDependencies';
import {WorkerRunner} from '@fluxer/api/src/worker/WorkerRunner';
import {workerTasks} from '@fluxer/api/src/worker/WorkerTaskRegistry';
import {createServerWithUpgrade} from '@fluxer/hono/src/Server';
import type {BaseHonoEnv} from '@fluxer/hono_types/src/HonoTypes';
import type {Hono} from 'hono';

export interface FluxerServerOptions {
	config?: FluxerServerConfig;
	staticDir?: string;
}

export interface FluxerServerResult {
	app: Hono<BaseHonoEnv>;
	initialize: () => Promise<void>;
	start: () => Promise<void>;
	shutdown: () => Promise<void>;
}

export async function createFluxerServer(options: FluxerServerOptions = {}): Promise<FluxerServerResult> {
	const config = options.config ?? Config;
	const staticDir = options.staticDir;

	const mounted = await mountRoutes({
		config,
		staticDir,
	});

	let server: Server | null = null;
	let workerRunner: WorkerRunner | null = null;
	let cronScheduler: CronScheduler | null = null;
	let gatewayManager: GatewayProcessManager | null = null;
	let isShuttingDown = false;

	const start = async (): Promise<void> => {
		Logger.info(
			{
				host: config.host,
				port: config.port,
				env: config.env,
				database: config.database.backend,
			},
			'Starting Fluxer Server',
		);

		Logger.info('Starting background services');
		await mounted.start();

		const shouldStartGatewayProcess =
			config.services.gateway && (config.env === 'production' || config.dev.test_mode_enabled);
		if (shouldStartGatewayProcess) {
			Logger.info('Initializing Gateway Process Manager');
			gatewayManager = createGatewayProcessManager();
			await gatewayManager.start();
		}

		if (mounted.services.jsConnectionManager) {
			const workerLogger = createComponentLogger('worker');

			workerLogger.info('Initializing worker dependencies');
			const snowflakeService = getSnowflakeService();
			await snowflakeService.initialize();
			const workerDeps = await initializeWorkerDependencies(snowflakeService);
			setWorkerDependencies(workerDeps);

			const workerQueue = new JetStreamWorkerQueue(mounted.services.jsConnectionManager);
			const concurrency = 5;

			cronScheduler = new CronScheduler(workerQueue, workerLogger);
			cronScheduler.upsert('processAssetDeletionQueue', 'processAssetDeletionQueue', {}, '0 */5 * * * *');
			cronScheduler.upsert('processCloudflarePurgeQueue', 'processCloudflarePurgeQueue', {}, '0 */2 * * * *');
			cronScheduler.upsert(
				'processPendingBulkMessageDeletions',
				'processPendingBulkMessageDeletions',
				{},
				'0 */10 * * * *',
			);
			cronScheduler.upsert('processInactivityDeletions', 'processInactivityDeletions', {}, '0 0 */6 * * *');
			cronScheduler.upsert('expireAttachments', 'expireAttachments', {}, '0 0 */12 * * *');
			cronScheduler.upsert('syncDiscoveryIndex', 'syncDiscoveryIndex', {}, '0 */15 * * * *');
			cronScheduler.start();
			workerLogger.info('Cron scheduler started');

			workerRunner = new WorkerRunner({
				tasks: workerTasks,
				queue: workerQueue,
				concurrency,
			});

			workerLogger.info({taskCount: Object.keys(workerTasks).length, concurrency}, 'Starting embedded worker');
			await workerRunner.start();
		}

		const gatewayProxy = createGatewayProxy();

		const onUpgrade = gatewayProxy.onUpgrade;

		return await new Promise((resolve) => {
			server = createServerWithUpgrade(mounted.app, {
				hostname: config.host,
				port: config.port,
				onUpgrade,
				onListen: (info) => {
					Logger.info(
						{
							address: info.address,
							port: info.port,
						},
						'Fluxer Server listening',
					);
					resolve();
				},
			});
		});
	};

	const shutdown = async (): Promise<void> => {
		if (isShuttingDown) {
			Logger.warn('Shutdown already in progress, ignoring duplicate signal');
			return;
		}
		isShuttingDown = true;

		Logger.info('Beginning graceful shutdown of Fluxer Server');

		const shutdownSteps = [
			{
				name: 'Worker',
				fn: async () => {
					if (cronScheduler !== null) {
						Logger.info('Stopping cron scheduler');
						cronScheduler.stop();
						cronScheduler = null;
					}
					if (workerRunner !== null) {
						Logger.info('Stopping embedded worker');
						await workerRunner.stop();
						workerRunner = null;
					}
				},
			},
			{
				name: 'HTTP Server',
				fn: async () => {
					if (server !== null) {
						Logger.info('Stopping HTTP server');
						server.closeAllConnections();
						await new Promise<void>((resolve) => {
							const timeout = setTimeout(() => {
								Logger.warn('HTTP server close timeout, forcing shutdown');
								resolve();
							}, 3000);
							server!.close((err) => {
								clearTimeout(timeout);
								if (err !== undefined) {
									Logger.error({error: err.message}, 'Error closing HTTP server');
								} else {
									Logger.info('HTTP server closed');
								}
								resolve();
							});
						});
					}
				},
			},
			{
				name: 'Application Services',
				fn: async () => {
					Logger.info('Shutting down application services');
					await mounted.shutdown();
				},
			},
			{
				name: 'Gateway Process',
				fn: async () => {
					if (gatewayManager) {
						Logger.info('Stopping Gateway process');
						await gatewayManager.stop();
					}
				},
			},
			{
				name: 'Instrumentation',
				fn: async () => {
					Logger.info('Shutting down telemetry and instrumentation');
					await shutdownInstrumentation();
				},
			},
		];

		for (const step of shutdownSteps) {
			try {
				await step.fn();
			} catch (error) {
				Logger.error(
					{
						step: step.name,
						error: error instanceof Error ? error.message : 'Unknown error',
					},
					'Error during shutdown step',
				);
			}
		}

		Logger.info('Fluxer Server shutdown complete');
	};

	const initialize = async (): Promise<void> => {
		Logger.info('Initializing services');
		await mounted.initialize();
	};

	return {
		app: mounted.app,
		initialize,
		start,
		shutdown,
	};
}
