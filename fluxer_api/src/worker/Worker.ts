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

import '../instrument';
import 'module-alias/register';
import * as Sentry from '@sentry/node';
import {run} from 'graphile-worker';
import {Redis} from 'ioredis';
import {Pool} from 'pg';
import {Config} from '~/Config';
import {getMetricsService, initializeMetricsService} from '~/infrastructure/MetricsService';
import {SnowflakeService} from '~/infrastructure/SnowflakeService';
import {Logger} from '~/Logger';
import {initializeMeilisearch} from '~/Meilisearch';
import applicationProcessDeletion from '~/worker/tasks/applicationProcessDeletion';
import batchGuildAuditLogMessageDeletes from '~/worker/tasks/batchGuildAuditLogMessageDeletes';
import bulkDeleteUserMessages from '~/worker/tasks/bulkDeleteUserMessages';
import deleteUserMessagesInGuildByTime from '~/worker/tasks/deleteUserMessagesInGuildByTime';
import expireAttachments from '~/worker/tasks/expireAttachments';
import extractEmbeds from '~/worker/tasks/extractEmbeds';
import handleMentions from '~/worker/tasks/handleMentions';
import harvestGuildData from '~/worker/tasks/harvestGuildData';
import harvestUserData from '~/worker/tasks/harvestUserData';
import indexChannelMessages from '~/worker/tasks/indexChannelMessages';
import messageShred from '~/worker/tasks/messageShred';
import processAssetDeletionQueue from '~/worker/tasks/processAssetDeletionQueue';
import processCloudfarePurgeQueue from '~/worker/tasks/processCloudfarePurgeQueue';
import processInactivityDeletions from '~/worker/tasks/processInactivityDeletions';
import processPendingBulkMessageDeletions from '~/worker/tasks/processPendingBulkMessageDeletions';
import refreshSearchIndex from '~/worker/tasks/refreshSearchIndex';
import {sendScheduledMessage} from '~/worker/tasks/sendScheduledMessage';
import userProcessPendingDeletion from '~/worker/tasks/userProcessPendingDeletion';
import userProcessPendingDeletions from '~/worker/tasks/userProcessPendingDeletions';
import {setWorkerDependencies} from './WorkerContext';
import {initializeWorkerDependencies, shutdownWorkerDependencies} from './WorkerDependencies';
import {WorkerMetricsCollector} from './WorkerMetricsCollector';

async function main() {
	Logger.info('Starting Graphile Worker...');

	initializeMetricsService(Config.metrics.host ?? null);
	Logger.info('MetricsService initialized');

	const redis = new Redis(Config.redis.url);
	const snowflakeService = new SnowflakeService(redis);
	await snowflakeService.initialize();
	Logger.info('Shared SnowflakeService initialized');

	const dependencies = await initializeWorkerDependencies(snowflakeService);

	setWorkerDependencies(dependencies);

	const workerConcurrency = 5;
	const runnerOptions = {
		connectionString: Config.postgres.url,
		concurrency: workerConcurrency,
		noHandleSignals: false,
		pollInterval: 1000,
		taskList: {
			applicationProcessDeletion,
			batchGuildAuditLogMessageDeletes,
			bulkDeleteUserMessages,
			deleteUserMessagesInGuildByTime,
			extractEmbeds,
			handleMentions,
			harvestUserData,
			harvestGuildData,
			indexChannelMessages,
			expireAttachments,
			processAssetDeletionQueue,
			processCloudfarePurgeQueue,
			processInactivityDeletions,
			messageShred,
			refreshSearchIndex,
			sendScheduledMessage,
			userProcessPendingDeletion,
			userProcessPendingDeletions,
			processPendingBulkMessageDeletions,
		},
		crontabFile: './src/worker/.crontab',
	};

	try {
		await initializeMeilisearch();

		const runner = await run(runnerOptions);
		const jobTimings = new Map<string, number>();

		runner.events.on('job:start', ({job}) => {
			jobTimings.set(job.id, performance.now());

			const waitTimeMs = Date.now() - job.run_at.getTime();
			if (waitTimeMs > 0) {
				getMetricsService().histogram({
					name: 'worker.job.wait_time',
					dimensions: {task: job.task_identifier},
					valueMs: waitTimeMs,
				});
			}
		});

		runner.events.on('job:success', ({job}) => {
			const startTime = jobTimings.get(job.id) ?? performance.now();
			const durationMs = performance.now() - startTime;
			jobTimings.delete(job.id);

			getMetricsService().counter({
				name: 'worker.job.success',
				dimensions: {task: job.task_identifier},
			});
			getMetricsService().histogram({
				name: 'worker.job.latency',
				dimensions: {task: job.task_identifier, status: 'success'},
				valueMs: durationMs,
			});
		});

		runner.events.on('job:error', ({job, error}) => {
			const startTime = jobTimings.get(job.id) ?? performance.now();
			const durationMs = performance.now() - startTime;
			jobTimings.delete(job.id);

			getMetricsService().counter({
				name: 'worker.job.error',
				dimensions: {task: job.task_identifier},
			});
			getMetricsService().histogram({
				name: 'worker.job.latency',
				dimensions: {task: job.task_identifier, status: 'error'},
				valueMs: durationMs,
			});

			if (job.attempts > 1) {
				getMetricsService().counter({
					name: 'worker.job.retry',
					dimensions: {task: job.task_identifier},
				});
			}

			const willRetry = job.attempts < job.max_attempts;
			getMetricsService().counter({
				name: willRetry ? 'worker.job.retry_scheduled' : 'worker.job.permanently_failed',
				dimensions: {task: job.task_identifier},
			});

			Sentry.withScope((scope) => {
				scope.setTag('task', job.task_identifier);
				scope.setExtra('job_id', job.id);
				scope.setExtra('attempts', job.attempts);
				scope.setExtra('max_attempts', job.max_attempts);
				scope.setExtra('payload', job.payload);
				scope.setExtra('job_queue_id', job.job_queue_id);
				Sentry.captureException(error);
			});
		});

		Logger.info('Graphile Worker started successfully');

		const pgPool = new Pool({connectionString: Config.postgres.url});
		const metricsCollector = new WorkerMetricsCollector({
			pgPool,
			redis: dependencies.redis,
			metricsService: getMetricsService(),
			assetDeletionQueue: dependencies.assetDeletionQueue,
			cloudflarePurgeQueue: dependencies.cloudflarePurgeQueue,
			bulkMessageDeletionQueue: dependencies.bulkMessageDeletionQueueService,
			accountDeletionQueue: dependencies.deletionQueueService,
			workerConcurrency,
		});
		metricsCollector.start();
		Logger.info('WorkerMetricsCollector started');

		const shutdown = async () => {
			Logger.info('Shutting down Graphile Worker...');
			metricsCollector.stop();
			await pgPool.end();
			await snowflakeService.shutdown();
			await shutdownWorkerDependencies(dependencies);
			await redis.quit();
			await runner.stop();
			process.exit(0);
		};

		process.on('SIGINT', shutdown);
		process.on('SIGTERM', shutdown);

		process.on('uncaughtException', async (error) => {
			Logger.error({err: error}, 'Uncaught Exception');
			Sentry.captureException(error);
			await Sentry.flush(2000);
			shutdown();
		});

		process.on('unhandledRejection', async (reason) => {
			Logger.error({err: reason}, 'Unhandled Rejection at Promise');
			Sentry.captureException(reason);
			await Sentry.flush(2000);
			shutdown();
		});

		await runner.promise;
	} catch (error) {
		Logger.error({err: error}, 'Failed to start Graphile Worker');
		Sentry.captureException(error);
		await Sentry.flush(2000);
		process.exit(1);
	}
}

main();
