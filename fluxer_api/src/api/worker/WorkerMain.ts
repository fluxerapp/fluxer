// SPDX-License-Identifier: AGPL-3.0-or-later

import {setupGracefulShutdown} from '@fluxer/hono/src/Server';
import {initCassandra, shutdownCassandra} from '@pkgs/cassandra/src/Client';
import {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import {getDefaultPostgresClient, initPostgres, shutdownPostgres} from '@pkgs/postgres/src/Client';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import {Config} from '../Config';
import {setDatabaseQueryExecutor} from '../database/CassandraQueryExecution';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from '../database/PostgresKvQueryExecutor';
import type {ISnowflakeService} from '../infrastructure/ISnowflakeService';
import {JobLedgerRepository} from '../jobs/JobLedgerRepository';
import {Logger} from '../Logger';
import {
	createSnowflakeService,
	setInjectedSnowflakeService,
	setInjectedWorkerService,
} from '../middleware/ServiceRegistry';
import {getCacheService} from '../middleware/ServiceSingletons';
import {initializeSearch, shutdownSearch} from '../SearchFactory';
import {CronScheduler} from './CronScheduler';
import {JetStreamWorkerQueue} from './JetStreamWorkerQueue';
import {clearWorkerDependencies, setWorkerDependencies} from './WorkerContext';
import {initializeWorkerDependencies, shutdownWorkerDependencies, type WorkerDependencies} from './WorkerDependencies';
import {
	resolveCronSchedulerEnabled,
	resolveWorkerLanes,
	validateLaneCompleteness,
	type WorkerLaneDefinition,
} from './WorkerLaneConfig';
import {WorkerRunner} from './WorkerRunner';
import {WorkerService} from './WorkerService';
import {workerTasks} from './WorkerTaskRegistry';

const SEARCH_REQUIRED_TASKS = new Set<string>([
	'indexChannelMessages',
	'indexGuildMembers',
	'refreshSearchIndex',
	'syncDiscoveryIndex',
]);

function registerCronJobs(cron: CronScheduler): void {
	cron.upsert('processAssetDeletionQueue', 'processAssetDeletionQueue', {}, '0 */5 * * * *');
	cron.upsert('processBunnyPurgeQueue', 'processBunnyPurgeQueue', {}, '*/10 * * * * *');
	cron.upsert('processPendingBulkMessageDeletions', 'processPendingBulkMessageDeletions', {}, '0 */10 * * * *');
	cron.upsert('userProcessPendingDeletions', 'userProcessPendingDeletions', {}, '0 * * * * *');
	cron.upsert('processPremiumStateReconciliationQueue', 'processPremiumStateReconciliationQueue', {}, '0 * * * * *');
	cron.upsert('processExpiredPremiumSweep', 'processExpiredPremiumSweep', {}, '0 0 * * * *');
	cron.upsert('processInactivityDeletions', 'processInactivityDeletions', {}, '0 0 */6 * * *');
	cron.upsert('expireAttachments', 'expireAttachments', {}, '0 0 */12 * * *');
	cron.upsert('prunePostgresKvTtl', 'prunePostgresKvTtl', {}, '0 */5 * * * *');
	cron.upsert('syncDiscoveryIndex', 'syncDiscoveryIndex', {}, '0 */15 * * * *');
	cron.upsert('syncDisposableEmailDomains', 'syncDisposableEmailDomains', {}, '0 */30 * * * *');
	cron.upsert('syncUrlBlocklists', 'syncUrlBlocklists', {}, '0 0 */6 * * *');
	cron.upsert('syncFileShaBlocklists', 'syncFileShaBlocklists', {}, '0 0 */12 * * *');
	cron.upsert('flushUserActivityBuffer', 'flushUserActivityBuffer', {}, '*/10 * * * * *');
	Logger.info('Cron jobs registered successfully');
}

function workerLanesRequireSearch(activeWorkerLanes: ReadonlyArray<WorkerLaneDefinition>): boolean {
	return activeWorkerLanes.some((lane) => lane.taskTypes.some((taskType) => SEARCH_REQUIRED_TASKS.has(taskType)));
}

export async function startWorkerMain(): Promise<void> {
	Logger.info('Starting worker backend...');
	let cassandraInitialized = false;
	let postgresInitialized = false;
	let jsConnectionManager: JetStreamConnectionManager | null = null;
	let snowflakeService: ISnowflakeService | null = null;
	let dependencies: WorkerDependencies | null = null;
	let cron: CronScheduler | null = null;
	const runners: Array<WorkerRunner> = [];
	let searchInitialized = false;
	let shuttingDown = false;

	const cleanupStep = async (label: string, fn: () => Promise<void> | void): Promise<void> => {
		try {
			await fn();
		} catch (error) {
			Logger.error({err: error}, `Error during worker shutdown step: ${label}`);
		}
	};

	const shutdown = async (): Promise<void> => {
		if (shuttingDown) {
			return;
		}
		shuttingDown = true;
		Logger.info('Shutting down worker backend...');
		await cleanupStep('cron', () => cron?.stop());
		await cleanupStep('runners', async () => {
			await Promise.all(runners.map((runner) => runner.stop()));
		});
		await cleanupStep('jetstream', async () => {
			await jsConnectionManager?.drain();
			jsConnectionManager = null;
		});
		await cleanupStep('worker dependencies', async () => {
			if (dependencies) {
				await shutdownWorkerDependencies(dependencies);
				dependencies = null;
			}
			clearWorkerDependencies();
			setInjectedWorkerService(undefined);
		});
		await cleanupStep('search', async () => {
			if (searchInitialized) {
				await shutdownSearch();
				searchInitialized = false;
			}
		});
		await cleanupStep('cassandra', async () => {
			if (cassandraInitialized) {
				await shutdownCassandra();
				cassandraInitialized = false;
			}
		});
		await cleanupStep('postgres', async () => {
			if (postgresInitialized) {
				setDatabaseQueryExecutor(null);
				await shutdownPostgres();
				postgresInitialized = false;
			}
		});
		await cleanupStep('snowflake', async () => {
			if (snowflakeService) {
				await snowflakeService.shutdown();
				snowflakeService = null;
			}
			setInjectedSnowflakeService(undefined);
		});
	};

	try {
		if (Config.database.backend === 'postgres') {
			await initPostgres(Config.postgres);
			const postgres = getDefaultPostgresClient();
			await ensurePostgresKvSchema(postgres);
			setDatabaseQueryExecutor(new PostgresKvQueryExecutor(postgres));
			postgresInitialized = true;
			Logger.info('Postgres KV client initialised for worker backend');
		}
		if (Config.database.backend === 'cassandra') {
			await initCassandra({
				hosts: Config.cassandra.hosts.split(',').filter(Boolean),
				port: Config.cassandra.port,
				keyspace: Config.cassandra.keyspace,
				localDc: Config.cassandra.localDc,
				username: Config.cassandra.username || undefined,
				password: Config.cassandra.password || undefined,
			});
			cassandraInitialized = true;
			Logger.info('Cassandra client initialised for worker backend');
		}
		validateLaneCompleteness(workerTasks);
		Logger.info('Worker lane configuration validated');
		const activeWorkerLanes = resolveWorkerLanes({
			mode: Config.worker.mode,
			laneName: Config.worker.laneName,
			taskName: Config.worker.taskName,
			laneConcurrencyOverrides: Config.worker.laneConcurrencyOverrides,
		});
		const cronSchedulerEnabled = resolveCronSchedulerEnabled(Config.worker.mode, Config.worker.enableCronScheduler);
		Logger.info(
			{
				workerMode: Config.worker.mode,
				workerLane: Config.worker.laneName,
				workerTask: Config.worker.taskName,
				lanes: activeWorkerLanes.map((lane) => `${lane.name}(${lane.concurrency})`).join(', '),
				cronSchedulerEnabled,
			},
			'Worker runtime configuration resolved',
		);
		snowflakeService = createSnowflakeService();
		await snowflakeService.initialize();
		setInjectedSnowflakeService(snowflakeService);
		Logger.info('Shared SnowflakeService initialised');
		jsConnectionManager = new JetStreamConnectionManager({
			url: Config.nats.jetStreamUrl,
			token: Config.nats.authToken || undefined,
			name: 'fluxer-worker',
		});
		await jsConnectionManager.connect();
		Logger.info('JetStream connection established');
		const queue = new JetStreamWorkerQueue(jsConnectionManager);
		await queue.ensureInfrastructure(activeWorkerLanes);
		Logger.info('JetStream stream and lane consumers verified');
		const jobLedger = new JobLedgerRepository();
		const workerService = new WorkerService(queue, snowflakeService, jobLedger);
		setInjectedWorkerService(workerService);
		dependencies = await initializeWorkerDependencies(snowflakeService);
		setWorkerDependencies(dependencies);
		const didClaimEmailSync = await dependencies.kvClient.setnx('sync:email_domains:initialized', '1');
		if (didClaimEmailSync) {
			Logger.info('Triggering initial disposable email domain sync');
			await workerService.addJob('syncDisposableEmailDomains', {});
		}
		cron = new CronScheduler(workerService, Logger, dependencies.kvClient);
		registerCronJobs(cron);
		for (const lane of activeWorkerLanes) {
			const laneTasks: Record<string, WorkerTaskHandler> = {};
			for (const taskType of lane.taskTypes) {
				const handler = workerTasks[taskType];
				if (handler) {
					laneTasks[taskType] = handler;
				}
			}
			const runner = new WorkerRunner({
				tasks: laneTasks,
				queue,
				consumerName: lane.consumerName,
				laneName: lane.name,
				ledger: jobLedger,
				concurrency: lane.concurrency,
				maxDeliver: lane.maxDeliver,
				ackWaitMs: lane.ackWaitMs,
			});
			runners.push(runner);
		}
		if (workerLanesRequireSearch(activeWorkerLanes)) {
			try {
				await initializeSearch(getCacheService());
				searchInitialized = true;
				Logger.info('Search initialised for worker backend');
			} catch (error) {
				Logger.error({err: error}, 'Search initialisation failed for worker backend');
				throw error;
			}
		} else {
			Logger.info('Search initialisation skipped for worker lanes without search tasks');
		}
		if (dependencies.voiceReconciliationWorker !== null) {
			dependencies.voiceReconciliationWorker.start();
			Logger.info('VoiceReconciliationWorker started');
		}
		if (cronSchedulerEnabled) {
			cron.start();
			Logger.info('Cron scheduler started');
		} else {
			Logger.info('Cron scheduler disabled for this worker process');
		}
		await Promise.all(runners.map((runner) => runner.start()));
		Logger.info(
			{lanes: activeWorkerLanes.map((l) => `${l.name}(${l.concurrency})`).join(', ')},
			'Worker runners started',
		);
		setupGracefulShutdown(shutdown, {logger: Logger, timeoutMs: 30000});
		process.on('uncaughtException', async (error) => {
			Logger.error({err: error}, 'Uncaught Exception');
			setTimeout(() => process.exit(1), ms('5 seconds')).unref();
			await shutdown();
			process.exit(1);
		});
		process.on('unhandledRejection', async (reason: unknown) => {
			Logger.error({err: reason}, 'Unhandled Rejection at Promise');
			setTimeout(() => process.exit(1), ms('5 seconds')).unref();
			await shutdown();
			process.exit(1);
		});
	} catch (error: unknown) {
		Logger.error({err: error}, 'Failed to start worker backend');
		await shutdown();
		process.exit(1);
	}
}
