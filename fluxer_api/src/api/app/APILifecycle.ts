// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {ensureDeletionQueueState} from '@app/api/app/DeletionQueueStartup';
import type {APIConfig} from '@app/api/config/APIConfig';
import {hasDatabaseQueryExecutor, setDatabaseQueryExecutor} from '@app/api/database/CassandraQueryExecution';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {
	jetStreamActivityPublisher,
	renderActivityMetrics,
	shutdownActivityEvents,
	startActivityEvents,
} from '@app/api/infrastructure/activity/ActivityEvents';
import {
	renderSharedListMetrics,
	startSharedListWatch,
	stopSharedListWatch,
} from '@app/api/infrastructure/activity/SharedLists';
import {shutdownStorageChangeFeed} from '@app/api/infrastructure/StorageServiceFactory';
import {JobLedgerRepository} from '@app/api/jobs/JobLedgerRepository';
import {ipBanCache} from '@app/api/middleware/IpBanMiddleware';
import {startRequestErrorTelemetry, stopRequestErrorTelemetry} from '@app/api/middleware/RequestErrorTelemetry';
import {initializeServiceSingletons, shutdownReportService} from '@app/api/middleware/ServiceMiddleware';
import {
	closeOwnedKVClient,
	ensureVoiceResourcesInitialized,
	getKVClient,
	getSnowflakeService,
	setInjectedWorkerService,
	shutdownVoiceResources,
} from '@app/api/middleware/ServiceRegistry';
import {
	getCacheService,
	getCustomBadgeCatalog,
	getInstanceConfigRepository,
	getKVAccountDeletionQueue,
	getReportRepository,
	getUserRepository,
	type ServiceSingletonDependencies,
	shutdownInstanceConfigRepository,
	shutdownServiceSingletons,
} from '@app/api/middleware/ServiceSingletons';
import {initializeSearch, shutdownSearch} from '@app/api/SearchFactory';
import {warmupAdminSearchIndexes} from '@app/api/search/SearchWarmup';
import {VisionarySlotInitializer} from '@app/api/stripe/VisionarySlotInitializer';
import {VoiceDataInitializer} from '@app/api/voice/VoiceDataInitializer';
import {JetStreamWorkerQueue} from '@app/api/worker/JetStreamWorkerQueue';
import {WorkerService} from '@app/api/worker/WorkerService';
import {registerMetricsSection} from '@fluxer/hono/src/middleware/Metrics';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {getClient, initCassandra, shutdownCassandra} from '@pkgs/cassandra/src/Client';
import {ensureGeoipDatabaseOnStartup} from '@pkgs/geoip/src/GeoipStartup';
import {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import {getDefaultPostgresClient, initPostgres, shutdownPostgres} from '@pkgs/postgres/src/Client';

let jsConnectionManager: JetStreamConnectionManager | null = null;
let jobQueue: ApiJobQueue | null = null;
let searchInitRetryTimer: ReturnType<typeof setTimeout> | null = null;
let searchInitStopped = false;
let searchInitInFlight: Promise<void> | null = null;
let markSearchReady: () => void = () => {};
const searchReady = new Promise<void>((resolve) => {
	markSearchReady = resolve;
});
const SEARCH_INIT_RETRY_MIN_MS = 5_000;
const SEARCH_INIT_RETRY_MAX_MS = 60_000;
const unregisterMetricsSections: Array<() => void> = [];

interface RefreshCacheLifecycle {
	initialize(): Promise<void>;
	shutdown(): Promise<void>;
}

const refreshCaches = new Map<RefreshCacheLifecycle, string>();
let refreshCacheShutdownPromise: Promise<void> | null = null;

async function initializeRefreshCache(
	cache: RefreshCacheLifecycle,
	name: string,
	logger: LoggerInterface,
): Promise<void> {
	refreshCaches.set(cache, name);
	await cache.initialize();
	logger.info(`${name} initialized`);
}

async function shutdownRefreshCaches(logger: LoggerInterface): Promise<void> {
	if (refreshCacheShutdownPromise) return await refreshCacheShutdownPromise;
	const caches = [...refreshCaches];
	refreshCaches.clear();
	const shutdown = Promise.all(
		caches.map(async ([cache, name]) => {
			try {
				await cache.shutdown();
				logger.info(`${name} shut down`);
			} catch (error) {
				logger.error({error}, `Error shutting down ${name}`);
			}
		}),
	).then(() => undefined);
	refreshCacheShutdownPromise = shutdown;
	try {
		await shutdown;
	} finally {
		if (refreshCacheShutdownPromise === shutdown) refreshCacheShutdownPromise = null;
	}
}

interface ApiJobQueue {
	jsConnectionManager: JetStreamConnectionManager;
	queue: JetStreamWorkerQueue;
	workerService: WorkerService;
	jobLedger: JobLedgerRepository;
}

export function getApiJobQueue(): ApiJobQueue | null {
	return jobQueue;
}

export function whenSearchReady(): Promise<void> {
	return searchReady;
}

function unsupportedDatabaseBackend(backend: never): never {
	throw new Error(`Unsupported database backend during shutdown: ${String(backend)}`);
}

async function initializeSearchAndWarmup(
	kvClient: ReturnType<typeof getKVClient>,
	logger: LoggerInterface,
): Promise<void> {
	await initializeSearch(getCacheService());
	markSearchReady();
	if (searchInitStopped) return;
	logger.info('Search initialized');
	const warmupLockKey = 'fluxer:search:warmup:admin';
	const warmupLockToken = randomUUID();
	const warmupLockTtlSeconds = 60 * 60;
	const acquiredWarmupLock = await kvClient.acquireLock(warmupLockKey, warmupLockToken, warmupLockTtlSeconds);
	if (!acquiredWarmupLock) {
		logger.info('Another API instance is warming search indexes, skipping warmup');
		return;
	}
	try {
		await warmupAdminSearchIndexes({
			userRepository: getUserRepository(),
			guildRepository: new GuildRepository(),
			reportRepository: getReportRepository(),
			logger,
		});
	} catch (error) {
		logger.error({error}, 'Admin search warmup failed (continuing startup)');
	} finally {
		try {
			await kvClient.releaseLock(warmupLockKey, warmupLockToken);
		} catch (error) {
			logger.warn({error}, 'Failed to release admin search warmup lock');
		}
	}
}

function scheduleSearchInitRetry(
	kvClient: ReturnType<typeof getKVClient>,
	logger: LoggerInterface,
	attempt: number,
): void {
	if (searchInitStopped) return;
	const delay = Math.min(SEARCH_INIT_RETRY_MIN_MS * 2 ** attempt, SEARCH_INIT_RETRY_MAX_MS);
	searchInitRetryTimer = setTimeout(() => {
		searchInitRetryTimer = null;
		searchInitInFlight = initializeSearchAndWarmup(kvClient, logger)
			.catch((error) => {
				logger.error({error, attempt: attempt + 1}, 'Search initialisation failed, retrying in the background');
				scheduleSearchInitRetry(kvClient, logger, attempt + 1);
			})
			.finally(() => {
				searchInitInFlight = null;
			});
	}, delay);
}

export function createInitializer(
	config: APIConfig,
	logger: LoggerInterface,
	dependencies: ServiceSingletonDependencies = {},
): () => Promise<void> {
	return async (): Promise<void> => {
		try {
			logger.info('Initializing API service...');
			const geoipStartupResult = await ensureGeoipDatabaseOnStartup({
				geoip: config.geoip,
				s3Config: {
					endpoint: config.s3.endpoint,
					region: config.s3.region,
					accessKeyId: config.s3.accessKeyId,
					secretAccessKey: config.s3.secretAccessKey,
				},
			});
			if (geoipStartupResult.mode === 's3') {
				logger.info(
					{
						maxmind_db_path: geoipStartupResult.maxmindDbPath,
						city: geoipStartupResult.city,
						s3_bucket: geoipStartupResult.bucket,
						s3_key: geoipStartupResult.key,
					},
					'GeoIP database downloaded from S3',
				);
			}
			if (config.database.backend === 'postgres' && !hasDatabaseQueryExecutor()) {
				await initPostgres(config.postgres, (diagnostic) => logger.error(diagnostic, 'Postgres connection error'));
				const postgres = getDefaultPostgresClient();
				await ensurePostgresKvSchema(postgres);
				setDatabaseQueryExecutor(new PostgresKvQueryExecutor(postgres));
				logger.info('Postgres KV client initialized');
			} else if (config.database.backend === 'postgres') {
				logger.info('Using injected database query executor for tests');
			}
			if (config.database.backend === 'cassandra' && !hasDatabaseQueryExecutor()) {
				const {ensureCassandraSchema, logMissingCassandraKeyspace} = await import(
					'@app/api/database/CassandraSchemaStartup'
				);
				await initCassandra({
					hosts: config.cassandra.hosts.split(',').filter(Boolean),
					port: config.cassandra.port,
					keyspace: config.cassandra.keyspace,
					localDc: config.cassandra.localDc,
					username: config.cassandra.username || undefined,
					password: config.cassandra.password || undefined,
				}).catch((error: unknown) => {
					if (config.instance.selfHosted) {
						logMissingCassandraKeyspace(error, config.cassandra.keyspace, config.cassandra.localDc, logger);
					}
					throw error;
				});
				logger.info('Cassandra client initialized');
				if (config.instance.selfHosted) {
					await ensureCassandraSchema({
						client: getClient(),
						keyspace: config.cassandra.keyspace,
						kv: getKVClient(),
						logger,
					});
				}
			} else if (config.database.backend === 'cassandra') {
				logger.info('Using injected database query executor for tests');
			}
			const kvClient = getKVClient();
			ipBanCache.setRefreshSubscriber(kvClient);
			await initializeRefreshCache(ipBanCache, 'IP ban cache', logger);
			const {urlBlocklistCache} = await import('@app/api/middleware/UrlBlocklistCache');
			urlBlocklistCache.setRefreshSubscriber(kvClient);
			const {getStorageService} = await import('@app/api/middleware/ServiceSingletons');
			urlBlocklistCache.setStorageService(getStorageService());
			await initializeRefreshCache(urlBlocklistCache, 'URL blocklist cache', logger);
			const {fileShaCache} = await import('@app/api/middleware/FileShaCache');
			fileShaCache.setRefreshSubscriber(kvClient);
			await initializeRefreshCache(fileShaCache, 'File SHA blocklist cache', logger);
			const {phraseBlocklistCache} = await import('@app/api/middleware/PhraseBlocklistCache');
			phraseBlocklistCache.setRefreshSubscriber(kvClient);
			await initializeRefreshCache(phraseBlocklistCache, 'Phrase blocklist cache', logger);
			const {bannedAvatarHashCache} = await import('@app/api/middleware/BannedAvatarHashCache');
			bannedAvatarHashCache.setRefreshSubscriber(kvClient);
			await initializeRefreshCache(bannedAvatarHashCache, 'Banned avatar hash cache', logger);
			const {profileSubstringBlocklistCache} = await import('@app/api/middleware/ProfileSubstringBlocklistCache');
			profileSubstringBlocklistCache.setRefreshSubscriber(kvClient);
			await initializeRefreshCache(profileSubstringBlocklistCache, 'Profile substring blocklist cache', logger);
			await initializeServiceSingletons(dependencies);
			logger.info('Service singletons initialized');
			await getCustomBadgeCatalog().initialize();
			logger.info('Custom badge catalog initialized');
			if (!config.dev.testModeEnabled) {
				jsConnectionManager = new JetStreamConnectionManager({
					url: config.nats.jetStreamUrl,
					token: config.nats.authToken || undefined,
					name: 'api-worker',
				});
				await jsConnectionManager.connect();
				const queue = new JetStreamWorkerQueue(jsConnectionManager);
				await queue.ensureStream();
				const jobLedger = new JobLedgerRepository();
				const workerService = new WorkerService(queue, getSnowflakeService(), jobLedger);
				setInjectedWorkerService(workerService);
				jobQueue = {jsConnectionManager, queue, workerService, jobLedger};
				logger.info('JetStream worker service initialized');
				const connection = jsConnectionManager;
				await startActivityEvents({
					publisher: jetStreamActivityPublisher(connection.getJetStreamClient()),
					kv: kvClient,
					jsm: await connection.getJetStreamManager(),
					spoolWhileMissing: !config.instance.selfHosted,
				});
				startRequestErrorTelemetry();
				startSharedListWatch(connection.getJetStreamClient(), {selfHosted: config.instance.selfHosted});
				unregisterMetricsSections.push(
					registerMetricsSection(renderActivityMetrics),
					registerMetricsSection(renderSharedListMetrics),
				);
				logger.info('Activity events initialized');
			}
			await ensureDeletionQueueState(getKVAccountDeletionQueue(), logger);
			logger.info('Initializing search indexes...');
			searchInitStopped = false;
			try {
				await initializeSearchAndWarmup(kvClient, logger);
			} catch (error) {
				logger.error({error}, 'Search initialisation failed, retrying in the background');
				scheduleSearchInitRetry(kvClient, logger, 0);
			}
			if (config.voice.enabled && config.voice.defaultRegion) {
				const voiceDataInitializer = new VoiceDataInitializer();
				await voiceDataInitializer.initialize();
				await ensureVoiceResourcesInitialized();
				logger.info('Voice data initialized');
			}
			if (config.dev.testModeEnabled && config.stripe.enabled) {
				const visionarySlotInitializer = new VisionarySlotInitializer();
				await visionarySlotInitializer.initialize();
				logger.info('Stripe visionary slots initialized');
			}
			if (config.dev.testModeEnabled) {
				const instanceConfigRepository = getInstanceConfigRepository();
				try {
					await instanceConfigRepository.setSsoConfig({
						enabled: false,
						authorizationUrl: null,
						tokenUrl: null,
						clientId: null,
					});
					logger.info('Reset SSO config to disabled for test mode');
				} catch (error) {
					logger.warn({error}, 'Failed to reset SSO config for test mode');
				}
			}
			logger.info('API service initialization complete');
		} catch (error) {
			logger.error({error}, 'API service initialization failed');
			await createShutdown(config, logger)();
			throw error;
		}
	};
}

export function createShutdown(config: APIConfig, logger: LoggerInterface): () => Promise<void> {
	return async (): Promise<void> => {
		logger.info('Shutting down API service...');
		try {
			await shutdownVoiceResources();
			logger.info('Voice resources shut down');
		} catch (error) {
			logger.error({error}, 'Error shutting down voice resources');
		}
		stopRequestErrorTelemetry();
		stopSharedListWatch();
		for (const unregister of unregisterMetricsSections.splice(0)) unregister();
		await shutdownActivityEvents();
		jobQueue = null;
		if (jsConnectionManager) {
			try {
				await jsConnectionManager.drain();
				jsConnectionManager = null;
			} catch (error) {
				logger.error({error}, 'Error draining JetStream worker connection');
			}
		}
		await shutdownStorageChangeFeed();
		setInjectedWorkerService(undefined);
		searchInitStopped = true;
		if (searchInitRetryTimer) {
			clearTimeout(searchInitRetryTimer);
			searchInitRetryTimer = null;
		}
		await searchInitInFlight;
		try {
			await shutdownSearch();
			logger.info('Search service shut down');
		} catch (error) {
			logger.error({error}, 'Error shutting down search service');
		}
		await Promise.all([
			shutdownRefreshCaches(logger),
			shutdownServiceSingletons()
				.then(() => {
					logger.info('Service singletons shut down');
				})
				.catch((error) => {
					logger.error({error}, 'Error shutting down service singletons');
				}),
		]);
		try {
			await shutdownInstanceConfigRepository();
			logger.info('Instance config repository shut down');
		} catch (error) {
			logger.error({error}, 'Error shutting down instance config repository');
		}
		try {
			shutdownReportService();
			logger.info('Report service shut down');
		} catch (error) {
			logger.error({error}, 'Error shutting down report service');
		}
		setDatabaseQueryExecutor(null);
		switch (config.database.backend) {
			case 'postgres':
				try {
					await shutdownPostgres();
					logger.info('Postgres client shut down');
				} catch (error) {
					logger.error({error}, 'Error shutting down Postgres client');
				}
				break;
			case 'cassandra':
				try {
					await shutdownCassandra();
					logger.info('Cassandra client shut down');
				} catch (error) {
					logger.error({error}, 'Error shutting down Cassandra client');
				}
				break;
			default:
				unsupportedDatabaseBackend(config.database.backend);
		}
		try {
			closeOwnedKVClient();
			logger.info('Key-value client closed');
		} catch (error) {
			logger.error({error}, 'Error closing key-value client');
		}
		logger.info('API service shutdown complete');
	};
}
