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

import {Config} from '@fluxer/api/src/Config';
import {getMetricsService, initializeMetricsService} from '@fluxer/api/src/infrastructure/MetricsService';
import {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import {Logger} from '@fluxer/api/src/Logger';
import {getKVClient, setInjectedWorkerService} from '@fluxer/api/src/middleware/ServiceRegistry';
import {initializeSearch} from '@fluxer/api/src/SearchFactory';
import {CronScheduler} from '@fluxer/api/src/worker/CronScheduler';
import {JetStreamWorkerQueue} from '@fluxer/api/src/worker/JetStreamWorkerQueue';
import {setWorkerDependencies} from '@fluxer/api/src/worker/WorkerContext';
import {initializeWorkerDependencies, shutdownWorkerDependencies} from '@fluxer/api/src/worker/WorkerDependencies';
import {WorkerMetricsCollector} from '@fluxer/api/src/worker/WorkerMetricsCollector';
import {WorkerRunner} from '@fluxer/api/src/worker/WorkerRunner';
import {WorkerService} from '@fluxer/api/src/worker/WorkerService';
import {workerTasks} from '@fluxer/api/src/worker/WorkerTaskRegistry';
import {setupGracefulShutdown} from '@fluxer/hono/src/Server';
import {JetStreamConnectionManager} from '@fluxer/nats/src/JetStreamConnectionManager';
import {captureException, flushSentry as flush} from '@fluxer/sentry/src/Sentry';
import {ms} from 'itty-time';

const WORKER_CONCURRENCY = 20;

function registerCronJobs(cron: CronScheduler): void {
	cron.upsert('processAssetDeletionQueue', 'processAssetDeletionQueue', {}, '0 */5 * * * *');
	cron.upsert('processCloudflarePurgeQueue', 'processCloudflarePurgeQueue', {}, '0 */2 * * * *');
	cron.upsert('processPendingBulkMessageDeletions', 'processPendingBulkMessageDeletions', {}, '0 */10 * * * *');
	cron.upsert('processInactivityDeletions', 'processInactivityDeletions', {}, '0 0 */6 * * *');
	cron.upsert('expireAttachments', 'expireAttachments', {}, '0 0 */12 * * *');
	// cron.upsert('cleanupCsamEvidence', 'cleanupCsamEvidence', {}, '0 0 3 * * *');
	// cron.upsert('csamScanConsumer', 'csamScanConsumer', {}, '* * * * * *');
	cron.upsert('syncDiscoveryIndex', 'syncDiscoveryIndex', {}, '0 */15 * * * *');

	Logger.info('Cron jobs registered successfully');
}

export async function startWorkerMain(): Promise<void> {
	Logger.info('Starting worker backend...');

	initializeMetricsService();
	Logger.info('MetricsService initialised');

	const kvClient = getKVClient();
	const snowflakeService = new SnowflakeService(kvClient);
	await snowflakeService.initialize();
	Logger.info('Shared SnowflakeService initialised');

	const jsConnectionManager = new JetStreamConnectionManager({
		url: Config.nats.jetStreamUrl,
		token: Config.nats.authToken || undefined,
		name: 'fluxer-worker',
	});
	await jsConnectionManager.connect();
	Logger.info('JetStream connection established');

	const queue = new JetStreamWorkerQueue(jsConnectionManager);
	await queue.ensureInfrastructure();
	Logger.info('JetStream stream and consumer verified');

	const workerService = new WorkerService(queue);
	setInjectedWorkerService(workerService);

	const dependencies = await initializeWorkerDependencies(snowflakeService);
	setWorkerDependencies(dependencies);

	const cron = new CronScheduler(queue, Logger);
	registerCronJobs(cron);

	const metricsCollector = new WorkerMetricsCollector({
		kvClient: dependencies.kvClient,
		metricsService: getMetricsService(),
		assetDeletionQueue: dependencies.assetDeletionQueue,
		purgeQueue: dependencies.purgeQueue,
		bulkMessageDeletionQueue: dependencies.bulkMessageDeletionQueueService,
		accountDeletionQueue: dependencies.deletionQueueService,
	});

	const runner = new WorkerRunner({
		tasks: workerTasks,
		queue,
		concurrency: WORKER_CONCURRENCY,
	});

	try {
		try {
			await initializeSearch();
			Logger.info('Search initialised for worker backend');
		} catch (error) {
			Logger.warn({err: error}, 'Search initialisation failed; continuing without search');
		}

		metricsCollector.start();
		Logger.info('WorkerMetricsCollector started');

		cron.start();
		Logger.info('Cron scheduler started');

		await runner.start();
		Logger.info(`Worker runner started with ${WORKER_CONCURRENCY} workers`);

		const shutdown = async (): Promise<void> => {
			Logger.info('Shutting down worker backend...');
			cron.stop();
			metricsCollector.stop();
			await runner.stop();
			await jsConnectionManager.drain();
			await shutdownWorkerDependencies(dependencies);
			await snowflakeService.shutdown();
		};

		setupGracefulShutdown(shutdown, {logger: Logger, timeoutMs: 30000});

		process.on('uncaughtException', async (error) => {
			Logger.error({err: error}, 'Uncaught Exception');
			captureException(error);
			await flush(ms('2 seconds'));
			await shutdown();
			process.exit(0);
		});

		process.on('unhandledRejection', async (reason: unknown) => {
			Logger.error({err: reason}, 'Unhandled Rejection at Promise');
			captureException(reason instanceof Error ? reason : new Error(String(reason)));
			await flush(ms('2 seconds'));
			setTimeout(() => process.exit(1), ms('5 seconds')).unref();
			await shutdown();
		});
	} catch (error: unknown) {
		Logger.error({err: error}, 'Failed to start worker backend');
		captureException(error instanceof Error ? error : new Error(String(error)));
		await flush(ms('2 seconds'));
		process.exit(1);
	}
}
