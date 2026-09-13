// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {ensureDeletionQueueState} from '@app/api/app/DeletionQueueStartup';
import type {APIConfig} from '@app/api/config/APIConfig';
import {hasDatabaseQueryExecutor, setDatabaseQueryExecutor} from '@app/api/database/CassandraQueryExecution';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import {GuildDataRepository} from '@app/api/guild/repositories/GuildDataRepository';
import type {ILogger} from '@app/api/ILogger';
import {JobLedgerRepository} from '@app/api/jobs/JobLedgerRepository';
import {startAbuseReplicationSubscriber, stopAbuseReplicationSubscriber} from '@app/api/middleware/AbusiveIpAutoBanner';
import {ipBanCache} from '@app/api/middleware/IpBanMiddleware';
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
	getInstanceConfigRepository,
	getKVAccountDeletionQueue,
	getReportRepository,
	getUserRepository,
	shutdownInstanceConfigRepository,
	shutdownServiceSingletons,
} from '@app/api/middleware/ServiceSingletons';
import {torExitListCache} from '@app/api/middleware/TorExitListCache';
import {ensureApnsSigningKey} from '@app/api/push/ApnsPushService';
import {initializeSearch, shutdownSearch} from '@app/api/SearchFactory';
import {warmupAdminSearchIndexes} from '@app/api/search/SearchWarmup';
import {VisionarySlotInitializer} from '@app/api/stripe/VisionarySlotInitializer';
import {VoiceDataInitializer} from '@app/api/voice/VoiceDataInitializer';
import {JetStreamWorkerQueue} from '@app/api/worker/JetStreamWorkerQueue';
import {WorkerService} from '@app/api/worker/WorkerService';
import {initCassandra, shutdownCassandra} from '@pkgs/cassandra/src/Client';
import {ensureGeoipDatabaseOnStartup} from '@pkgs/geoip/src/GeoipStartup';
import {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import {getDefaultPostgresClient, initPostgres, shutdownPostgres} from '@pkgs/postgres/src/Client';

let jsConnectionManager: JetStreamConnectionManager | null = null;

interface RefreshCacheLifecycle {
	initialize(): Promise<void>;
	shutdown(): Promise<void>;
}

const refreshCaches = new Map<RefreshCacheLifecycle, string>();
let refreshCacheShutdownPromise: Promise<void> | null = null;

async function initializeRefreshCache(cache: RefreshCacheLifecycle, name: string, logger: ILogger): Promise<void> {
	refreshCaches.set(cache, name);
	await cache.initialize();
	logger.info(`${name} initialized`);
}

async function shutdownRefreshCaches(logger: ILogger): Promise<void> {
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

function unsupportedDatabaseBackend(backend: never): never {
	throw new Error(`Unsupported database backend during shutdown: ${String(backend)}`);
}

export function createInitializer(config: APIConfig, logger: ILogger): () => Promise<void> {
	return async (): Promise<void> => {
		try {
			logger.info('Initializing API service...');
			await ensureApnsSigningKey();
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
						asn: geoipStartupResult.asn,
						s3_bucket: geoipStartupResult.bucket,
						s3_key: geoipStartupResult.key,
					},
					'GeoIP databases downloaded from S3',
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
				await initCassandra({
					hosts: config.cassandra.hosts.split(',').filter(Boolean),
					port: config.cassandra.port,
					keyspace: config.cassandra.keyspace,
					localDc: config.cassandra.localDc,
					username: config.cassandra.username || undefined,
					password: config.cassandra.password || undefined,
				});
				logger.info('Cassandra client initialized');
			} else if (config.database.backend === 'cassandra') {
				logger.info('Using injected database query executor for tests');
			}
			const kvClient = getKVClient();
			ipBanCache.setRefreshSubscriber(kvClient);
			await initializeRefreshCache(ipBanCache, 'IP ban cache', logger);
			await startAbuseReplicationSubscriber(kvClient);
			logger.info('Abusive-IP auto-banner replication started');
			torExitListCache.setKvClient(kvClient);
			await torExitListCache.initialize();
			logger.info('Tor exit list cache initialized');
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
			await initializeServiceSingletons();
			logger.info('Service singletons initialized');
			if (!config.dev.testModeEnabled) {
				jsConnectionManager = new JetStreamConnectionManager({
					url: config.nats.jetStreamUrl,
					token: config.nats.authToken || undefined,
					name: 'api-worker',
				});
				await jsConnectionManager.connect();
				const workerQueue = new JetStreamWorkerQueue(jsConnectionManager);
				await workerQueue.ensureStream();
				setInjectedWorkerService(new WorkerService(workerQueue, getSnowflakeService(), new JobLedgerRepository()));
				logger.info('JetStream worker service initialized');
			}
			await ensureDeletionQueueState(getKVAccountDeletionQueue(), logger);
			logger.info('Initializing search indexes...');
			let searchInitialized = false;
			try {
				await initializeSearch(getCacheService());
				searchInitialized = true;
				logger.info('Search initialized');
			} catch (error) {
				logger.error({error}, 'Search initialisation failed');
				throw error;
			}
			if (searchInitialized) {
				const warmupLockKey = 'fluxer:search:warmup:admin';
				const warmupLockToken = randomUUID();
				const warmupLockTtlSeconds = 60 * 60;
				const acquiredWarmupLock = await kvClient.acquireLock(warmupLockKey, warmupLockToken, warmupLockTtlSeconds);
				if (!acquiredWarmupLock) {
					logger.info('Another API instance is warming search indexes, skipping warmup');
				} else {
					try {
						await warmupAdminSearchIndexes({
							userRepository: getUserRepository(),
							guildRepository: new GuildDataRepository(),
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

export function createShutdown(config: APIConfig, logger: ILogger): () => Promise<void> {
	return async (): Promise<void> => {
		logger.info('Shutting down API service...');
		try {
			await shutdownVoiceResources();
			logger.info('Voice resources shut down');
		} catch (error) {
			logger.error({error}, 'Error shutting down voice resources');
		}
		if (jsConnectionManager) {
			try {
				await jsConnectionManager.drain();
				jsConnectionManager = null;
			} catch (error) {
				logger.error({error}, 'Error draining JetStream worker connection');
			}
		}
		setInjectedWorkerService(undefined);
		try {
			await shutdownSearch();
			logger.info('Search service shut down');
		} catch (error) {
			logger.error({error}, 'Error shutting down search service');
		}
		try {
			await stopAbuseReplicationSubscriber();
			logger.info('Abusive-IP auto-banner replication stopped');
		} catch (error) {
			logger.error({error}, 'Error stopping abusive-IP auto-banner replication');
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
			await torExitListCache.shutdown();
			logger.info('Tor exit list cache shut down');
		} catch (error) {
			logger.error({error}, 'Error shutting down Tor exit list cache');
		}
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
