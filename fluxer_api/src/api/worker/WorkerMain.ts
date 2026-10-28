// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {createApiContext} from '@app/api/CreateApiContext';
import {setDatabaseQueryExecutor} from '@app/api/database/CassandraQueryExecution';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import {
	activityEventsPublishing,
	jetStreamActivityPublisher,
	shutdownActivityEvents,
	startActivityEvents,
} from '@app/api/infrastructure/activity/ActivityEvents';
import {setActivityProcessChannel} from '@app/api/infrastructure/activity/ActivityMeta';
import {startSharedListWatch, stopSharedListWatch} from '@app/api/infrastructure/activity/SharedLists';
import {sizeProcessForHost} from '@app/api/infrastructure/HostResources';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {shutdownStorageChangeFeed} from '@app/api/infrastructure/StorageServiceFactory';
import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {JobLedgerRepository} from '@app/api/jobs/JobLedgerRepository';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {startContentBlocklistCaches, stopContentBlocklistCaches} from '@app/api/middleware/ContentBlocklistCaches';
import {
	closeOwnedKVClient,
	createSnowflakeService,
	getKVClient,
	setInjectedSnowflakeService,
	setInjectedWorkerService,
	shutdownVoiceResources,
} from '@app/api/middleware/ServiceRegistry';
import {
	getAdminArchiveService,
	getAdminRepository,
	getCacheService,
	getInstanceConfigRepository,
	getLimitConfigService,
} from '@app/api/middleware/ServiceSingletons';
import {initializeSearch, shutdownSearch} from '@app/api/SearchFactory';
import {getStripeClient} from '@app/api/stripe/StripeClient';
import {accountStateDepsFromContext} from '@app/api/user/services/AccountStateApplier';
import {awaitAll} from '@app/api/utils/ConcurrencyUtils';
import {readOptionalEnv} from '@app/api/utils/IntegerOptions';
import {startAccountActionConsumer, stopAccountActionConsumer} from '@app/api/worker/AccountActionConsumer';
import {queueBlocklistFeedStartupJobs} from '@app/api/worker/BlocklistFeedStartup';
import {CronScheduler} from '@app/api/worker/CronScheduler';
import {JetStreamWorkerQueue, JOBS_STREAM_MAX_AGE_MS} from '@app/api/worker/JetStreamWorkerQueue';
import {queueDeletedUsersBackfill} from '@app/api/worker/tasks/BackfillDeletedUsers';
import {queueHiddenProfileBadgesBackfill} from '@app/api/worker/tasks/BackfillHiddenProfileBadges';
import {queuePasskeyTraitBackfill} from '@app/api/worker/tasks/BackfillPasskeyTrait';
import {queueUserAclIndexBackfill} from '@app/api/worker/tasks/BackfillUserAclIndex';
import {singleCommunityJoinPending} from '@app/api/worker/tasks/JoinSingleCommunityMembers';
import {clearWorkerDependencies, setWorkerDependencies} from '@app/api/worker/WorkerContext';
import {
	guildInvitePauseDeps,
	initializeWorkerDependencies,
	type WorkerDependencies,
} from '@app/api/worker/WorkerDependencies';
import {WorkerHeartbeat} from '@app/api/worker/WorkerHeartbeat';
import {
	resolveCronSchedulerEnabled,
	resolveWorkerLanes,
	validateLaneCompleteness,
} from '@app/api/worker/WorkerLaneConfig';
import {createWorkerProcessErrorHandler} from '@app/api/worker/WorkerProcessErrorHandler';
import {WorkerRunner} from '@app/api/worker/WorkerRunner';
import {WorkerService} from '@app/api/worker/WorkerService';
import {workerTasks} from '@app/api/worker/WorkerTaskRegistry';
import {createRegisteredMetricsHandler} from '@fluxer/hono/src/middleware/Metrics';
import {createServer, setupGracefulShutdown} from '@fluxer/hono/src/Server';
import type {ServerType} from '@hono/node-server';
import {BACKGROUND_READ_TIMEOUT_MS, getClient, initCassandra, shutdownCassandra} from '@pkgs/cassandra/src/Client';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import {getDefaultPostgresClient, initPostgres, shutdownPostgres} from '@pkgs/postgres/src/Client';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {Hono} from 'hono';
import {ms} from 'itty-time';

function startWorkerMetricsServer(port: number): ServerType {
	const app = new Hono();
	app.get('/_metrics', createRegisteredMetricsHandler());
	return createServer(app, {port, onListen: (info) => Logger.info({port: info.port}, 'Worker metrics listening')});
}

function registerCronJobs(cron: CronScheduler, jobsStreamMaxAgeMs: number, kvClient: IKVProvider): void {
	cron.upsert('processAssetDeletionQueue', 'processAssetDeletionQueue', {}, '0 */5 * * * *', {ledger: false});
	if (Config.cachePurge.adapter !== 'none') {
		cron.upsert('processCachePurgeQueue', 'processCachePurgeQueue', {}, '*/10 * * * * *', {ledger: false});
	}
	cron.upsert('processPendingBulkMessageDeletions', 'processPendingBulkMessageDeletions', {}, '0 */10 * * * *', {
		ledger: false,
	});
	cron.upsert('userProcessPendingDeletions', 'userProcessPendingDeletions', {}, '0 * * * * *', {ledger: false});
	cron.upsert('processPremiumStateReconciliationQueue', 'processPremiumStateReconciliationQueue', {}, '0 * * * * *', {
		ledger: false,
		enabled: () => getStripeClient() !== null,
	});
	cron.upsert('scanBillingNotices', 'scanBillingNotices', {}, '0 15 9 * * *', {
		ledger: false,
		enabled: () => getStripeClient() !== null,
	});
	cron.upsert('processExpiredPremiumSweep', 'processExpiredPremiumSweep', {}, '0 0 * * * *', {ledger: false});
	cron.upsert('closePaymentDisputeAccounts', 'closePaymentDisputeAccounts', {}, '0 40 * * * *', {ledger: false});
	if (!Config.instance.selfHosted) {
		cron.upsert('processStorePurchaseRefreshQueue', 'processStorePurchaseRefreshQueue', {}, '0 */5 * * * *', {
			ledger: false,
		});
		cron.upsert('pollGooglePlayVoidedPurchases', 'pollGooglePlayVoidedPurchases', {}, '0 30 4 * * *', {ledger: false});
		cron.upsert('pollAppStoreNotificationHistory', 'pollAppStoreNotificationHistory', {}, '0 45 4 * * *', {
			ledger: false,
		});
		cron.upsert('operatorPassSaleBroadcast', 'operatorPassSaleBroadcast', {}, '0 * * * * *', {ledger: false});
		cron.upsert('operatorPassWaitlistCleanup', 'operatorPassWaitlistCleanup', {}, '0 20 * * * *', {ledger: false});
		cron.upsert('operatorPassRemovalExpiry', 'operatorPassRemovalExpiry', {}, '0 */5 * * * *', {ledger: false});
		cron.upsert('operatorPassWallRetention', 'operatorPassWallRetention', {}, '0 40 3 * * *', {ledger: false});
		cron.upsert('operatorPassOfferCompletion', 'operatorPassOfferCompletion', {}, '30 */5 * * * *', {ledger: false});
	}
	cron.upsert('processInactivityDeletions', 'processInactivityDeletions', {}, '0 0 */6 * * *', {ledger: false});
	cron.upsert('expireAttachments', 'expireAttachments', {}, '0 0 */12 * * *', {ledger: false});
	cron.upsert('expireReportSnapshots', 'expireReportSnapshots', {}, '0 15 5 * * *', {ledger: false});
	if (jobsStreamMaxAgeMs > 0 && jobsStreamMaxAgeMs <= JOBS_STREAM_MAX_AGE_MS) {
		cron.upsert('expireStaleJobs', 'expireStaleJobs', {}, '0 45 3 * * *', {ledger: false});
	} else {
		Logger.warn(
			{jobsStreamMaxAgeMs},
			'Jobs stream keeps jobs past 7 days, stale jobs stay active until their ledger rows expire',
		);
	}
	cron.upsert('prunePostgresKvTtl', 'prunePostgresKvTtl', {}, '0 */5 * * * *', {ledger: false});
	cron.upsert('syncDiscoveryIndex', 'syncDiscoveryIndex', {}, '0 */15 * * * *', {ledger: false});
	if (Config.blocklistFeeds.enabled) {
		cron.upsert('syncUrlBlocklists', 'syncUrlBlocklists', {}, '0 0 */6 * * *', {ledger: true});
		cron.upsert('syncFileShaBlocklists', 'syncFileShaBlocklists', {}, '0 0 */12 * * *', {ledger: true});
	}
	cron.upsert(
		'flushUserActivityBuffer',
		'flushUserActivityBuffer',
		{},
		Config.instance.selfHosted ? '0 * * * * *' : '*/10 * * * * *',
		{ledger: false},
	);
	cron.upsert('drainActivitySpool', 'drainActivitySpool', {}, '*/5 * * * * *', {
		ledger: false,
		enabled: activityEventsPublishing,
	});
	cron.upsert('joinSingleCommunityMembers', 'joinSingleCommunityMembers', {}, '0 * * * * *', {
		ledger: false,
		enabled: () => singleCommunityJoinPending(kvClient),
	});
	cron.upsert('liftExpiredGuildInvitePauses', 'liftExpiredGuildInvitePauses', {}, '0 * * * * *', {ledger: false});
	cron.upsert('archiveInactiveThreads', 'archiveInactiveThreads', {}, '0 * * * * *', {
		ledger: false,
		enabled: () => true,
	});
	Logger.info(
		{
			blocklistFeeds: Config.blocklistFeeds.enabled,
			cachePurgeAdapter: Config.cachePurge.adapter,
			selfHosted: Config.instance.selfHosted,
		},
		'Cron jobs registered successfully',
	);
}

export interface WorkerRuntime {
	stop(): Promise<void>;
	stalledComponents(): Array<string>;
}

interface WorkerRuntimeOptions {
	jsConnectionManager: JetStreamConnectionManager;
	queue: JetStreamWorkerQueue;
	workerService: WorkerService;
	jobLedger: JobLedgerRepository;
	dependencies: WorkerDependencies;
	cores: number;
}

export async function startWorkerRuntime(options: WorkerRuntimeOptions): Promise<WorkerRuntime> {
	const {jsConnectionManager, queue, workerService, jobLedger, dependencies} = options;
	const heartbeat = new WorkerHeartbeat({logger: Logger});
	const runners: Array<WorkerRunner> = [];
	let cron: CronScheduler | null = null;
	let stopPromise: Promise<void> | null = null;

	const cleanupStep = async (label: string, fn: () => Promise<void> | void): Promise<void> => {
		try {
			await fn();
		} catch (error) {
			Logger.error({err: error}, `Error during worker shutdown step: ${label}`);
		}
	};

	const performStop = async (): Promise<void> => {
		await cleanupStep('heartbeat', () => heartbeat.stop());
		await cleanupStep('cron', () => cron?.stop());
		await cleanupStep('account actions', stopAccountActionConsumer);
		await cleanupStep('runners', async () => {
			await awaitAll(
				runners.map((runner) => runner.stop()),
				'Failed to stop worker runners',
			);
		});
	};
	const runtime: WorkerRuntime = {
		stop: () => {
			stopPromise ??= performStop();
			return stopPromise;
		},
		stalledComponents: () => heartbeat.stalledComponents(),
	};

	try {
		validateLaneCompleteness(workerTasks);
		Logger.info('Worker lane configuration validated');
		const activeWorkerLanes = resolveWorkerLanes({
			mode: Config.worker.mode,
			laneName: Config.worker.laneName,
			taskName: Config.worker.taskName,
			laneConcurrencyOverrides: Config.worker.laneConcurrencyOverrides,
			availableParallelism: options.cores,
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
		await queue.ensureInfrastructure(activeWorkerLanes);
		Logger.info('JetStream stream and lane consumers verified');
		await queueBlocklistFeedStartupJobs(dependencies.kvClient, workerService, Config.blocklistFeeds.enabled);
		await queuePasskeyTraitBackfill(dependencies.kvClient, workerService);
		await queueUserAclIndexBackfill(dependencies.kvClient, workerService);
		await queueDeletedUsersBackfill(dependencies.kvClient, workerService);
		await queueHiddenProfileBadgesBackfill(dependencies.kvClient, workerService);
		try {
			const normalized = await getInstanceConfigRepository().normalizeStoredBrandingAssets(dependencies.storageService);
			if (normalized > 0) {
				Logger.info({normalized}, 'Normalised stored instance branding assets to references');
			}
		} catch (error) {
			Logger.warn({err: error}, 'Failed to normalise stored instance branding assets');
		}
		if (activeWorkerLanes.some((lane) => lane.name === 'lifecycle')) {
			const apiContext = createApiContext();
			startAccountActionConsumer({
				js: jsConnectionManager.getJetStreamClient(),
				state: accountStateDepsFromContext(
					apiContext,
					getAdminRepository(),
					{
						userCacheService: dependencies.userCacheService,
						guildRepository: dependencies.guildRepository,
					},
					{
						archives: getAdminArchiveService(),
						messageDeletionQueue: dependencies.bulkMessageDeletionQueueService,
						messageDeletionDelayMs: Config.automatedMessageDeletionDelayDays * ms('1 day'),
					},
					dependencies.channelRepository,
				),
				guildInvites: guildInvitePauseDeps(dependencies),
			});
			Logger.info('Account action consumer started');
		}
		cron = new CronScheduler(workerService, Logger, dependencies.kvClient, heartbeat);
		registerCronJobs(cron, queue.getJobsStreamMaxAgeMs(), dependencies.kvClient);
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
				retiredTaskTypes: lane.retiredTaskTypes,
				queue,
				consumerName: lane.consumerName,
				laneName: lane.name,
				ledger: jobLedger,
				concurrency: lane.concurrency,
				maxDeliver: lane.maxDeliver,
				ackWaitMs: lane.ackWaitMs,
				heartbeat,
			});
			runners.push(runner);
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
		heartbeat.start();
		return runtime;
	} catch (error) {
		await runtime.stop();
		throw error;
	}
}

export async function startWorkerMain(): Promise<void> {
	Logger.info('Starting worker backend...');
	const host = sizeProcessForHost(Logger);
	let cassandraInitialized = false;
	let postgresInitialized = false;
	let jsConnectionManager: JetStreamConnectionManager | null = null;
	let snowflakeService: ISnowflakeService | null = null;
	let limitConfigService: LimitConfigService | null = null;
	let instanceConfigRepository: InstanceConfigRepository | null = null;
	let dependencies: WorkerDependencies | null = null;
	let runtime: WorkerRuntime | null = null;
	let metricsServer: ServerType | null = null;
	let searchInitialized = false;
	let shutdownPromise: Promise<void> | null = null;

	const cleanupStep = async (label: string, fn: () => Promise<void> | void): Promise<void> => {
		try {
			await fn();
		} catch (error) {
			Logger.error({err: error}, `Error during worker shutdown step: ${label}`);
		}
	};

	const performShutdown = async (): Promise<void> => {
		Logger.info('Shutting down worker backend...');
		const voiceShutdown = cleanupStep('voice resources', shutdownVoiceResources);
		await cleanupStep('metrics server', () => {
			metricsServer?.close();
			metricsServer = null;
		});
		await cleanupStep('worker runtime', () => runtime?.stop());
		await voiceShutdown;
		await cleanupStep('storage change feed', shutdownStorageChangeFeed);
		await cleanupStep('shared lists', stopSharedListWatch);
		await cleanupStep('activity events', () => shutdownActivityEvents());
		await cleanupStep('jetstream', async () => {
			await jsConnectionManager?.drain();
			jsConnectionManager = null;
		});
		await cleanupStep('content blocklist caches', stopContentBlocklistCaches);
		await cleanupStep('worker dependencies', () => {
			dependencies = null;
			clearWorkerDependencies();
			setInjectedWorkerService(undefined);
		});
		await cleanupStep('limit config', async () => {
			if (limitConfigService) {
				await limitConfigService.shutdown();
				limitConfigService = null;
			}
		});
		await cleanupStep('instance config repository', async () => {
			if (instanceConfigRepository) {
				await instanceConfigRepository.shutdown();
				instanceConfigRepository = null;
			}
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
		await cleanupStep('key-value client', closeOwnedKVClient);
	};
	const shutdown = (): Promise<void> => {
		shutdownPromise ??= Promise.resolve().then(performShutdown);
		return shutdownPromise;
	};

	try {
		if (Config.database.backend === 'postgres') {
			await initPostgres(
				{
					...Config.postgres,
					synchronousCommit: readOptionalEnv('FLUXER_WORKER_POSTGRES_SYNCHRONOUS_COMMIT')?.trim() ?? 'off',
				},
				(diagnostic) => Logger.error(diagnostic, 'Postgres connection error'),
			);
			postgresInitialized = true;
			const postgres = getDefaultPostgresClient();
			await ensurePostgresKvSchema(postgres);
			setDatabaseQueryExecutor(new PostgresKvQueryExecutor(postgres));
			Logger.info('Postgres KV client initialised for worker backend');
		}
		if (Config.database.backend === 'cassandra') {
			const {ensureCassandraSchema, logMissingCassandraKeyspace} = await import(
				'@app/api/database/CassandraSchemaStartup'
			);
			await initCassandra({
				hosts: Config.cassandra.hosts.split(',').filter(Boolean),
				port: Config.cassandra.port,
				keyspace: Config.cassandra.keyspace,
				localDc: Config.cassandra.localDc,
				username: Config.cassandra.username || undefined,
				password: Config.cassandra.password || undefined,
				readTimeoutMs: BACKGROUND_READ_TIMEOUT_MS,
			}).catch((error: unknown) => {
				if (Config.instance.selfHosted) {
					logMissingCassandraKeyspace(error, Config.cassandra.keyspace, Config.cassandra.localDc, Logger);
				}
				throw error;
			});
			cassandraInitialized = true;
			Logger.info('Cassandra client initialised for worker backend');
			if (Config.instance.selfHosted) {
				await ensureCassandraSchema({
					client: getClient(),
					keyspace: Config.cassandra.keyspace,
					kv: getKVClient(),
					logger: Logger,
				});
			}
		}
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
		const jobLedger = new JobLedgerRepository();
		const workerService = new WorkerService(queue, snowflakeService, jobLedger);
		setInjectedWorkerService(workerService);
		instanceConfigRepository = getInstanceConfigRepository();
		limitConfigService = getLimitConfigService();
		try {
			await initializeSearch(getCacheService());
			searchInitialized = true;
			Logger.info('Search initialised for worker backend');
		} catch (error) {
			Logger.error({err: error}, 'Search initialisation failed for worker backend');
			throw error;
		}
		dependencies = await initializeWorkerDependencies(snowflakeService);
		setWorkerDependencies(dependencies);
		await startContentBlocklistCaches({kvClient: dependencies.kvClient, storageService: dependencies.storageService});
		Logger.info('Content blocklist caches initialised for worker backend');
		setActivityProcessChannel('worker');
		await startActivityEvents({
			publisher: jetStreamActivityPublisher(jsConnectionManager.getJetStreamClient()),
			kv: dependencies.kvClient,
			jsm: await jsConnectionManager.getJetStreamManager(),
			spoolWhileMissing: !Config.instance.selfHosted,
		});
		startSharedListWatch(jsConnectionManager.getJetStreamClient(), {selfHosted: Config.instance.selfHosted});
		runtime = await startWorkerRuntime({
			jsConnectionManager,
			queue,
			workerService,
			jobLedger,
			dependencies,
			cores: host.cores,
		});
		if (Config.worker.metricsPort !== undefined) {
			metricsServer = startWorkerMetricsServer(Config.worker.metricsPort);
		}
		setupGracefulShutdown(shutdown, {logger: Logger, timeoutMs: 30000});
		const handleProcessError = createWorkerProcessErrorHandler({
			logger: Logger,
			shutdown,
			exit: (code) => {
				process.exit(code);
			},
		});
		process.on('uncaughtException', (error) => {
			void handleProcessError('uncaughtException', error);
		});
		process.on('unhandledRejection', (reason: unknown) => {
			void handleProcessError('unhandledRejection', reason);
		});
	} catch (error: unknown) {
		Logger.error({err: error}, 'Failed to start worker backend');
		await shutdown();
		process.exit(1);
	}
}
