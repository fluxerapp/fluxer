// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {initCassandra, shutdownCassandra} from '@pkgs/cassandra/src/Client';
import {ensureGeoipDatabaseOnStartup} from '@pkgs/geoip/src/GeoipStartup';
import {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import {getDefaultPostgresClient, initPostgres, shutdownPostgres} from '@pkgs/postgres/src/Client';
import type {APIConfig} from '../config/APIConfig';
import {hasDatabaseQueryExecutor, setDatabaseQueryExecutor} from '../database/CassandraQueryExecution';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from '../database/PostgresKvQueryExecutor';
import {GuildDataRepository} from '../guild/repositories/GuildDataRepository';
import type {ILogger} from '../ILogger';
import {JobLedgerRepository} from '../jobs/JobLedgerRepository';
import {startAbuseReplicationSubscriber, stopAbuseReplicationSubscriber} from '../middleware/AbusiveIpAutoBanner';
import {ipBanCache} from '../middleware/IpBanMiddleware';
import {
	getRiskCacheManagerInstance,
	initializeServiceSingletons,
	shutdownReportService,
} from '../middleware/ServiceMiddleware';
import {
	ensureVoiceResourcesInitialized,
	getKVClient,
	getSnowflakeService,
	setInjectedWorkerService,
} from '../middleware/ServiceRegistry';
import {
	getCacheService,
	getInstanceConfigRepository,
	getKVAccountDeletionQueue,
	getReportRepository,
	getUserRepository,
} from '../middleware/ServiceSingletons';
import {torExitListCache} from '../middleware/TorExitListCache';
import {initializeSearch, shutdownSearch} from '../SearchFactory';
import {warmupAdminSearchIndexes} from '../search/SearchWarmup';
import {VisionarySlotInitializer} from '../stripe/VisionarySlotInitializer';
import {VoiceDataInitializer} from '../voice/VoiceDataInitializer';
import {JetStreamWorkerQueue} from '../worker/JetStreamWorkerQueue';
import {WorkerService} from '../worker/WorkerService';

let jsConnectionManager: JetStreamConnectionManager | null = null;
let riskCacheRefreshInterval: NodeJS.Timeout | null = null;
let riskCacheRefreshInFlight = false;

const RISK_CACHE_REFRESH_INTERVAL_MS = 5 * 60 * 1000;

async function refreshRiskCache(logger: ILogger, source: 'startup' | 'interval'): Promise<void> {
	if (riskCacheRefreshInFlight) {
		return;
	}
	riskCacheRefreshInFlight = true;
	try {
		const result = await getRiskCacheManagerInstance().refresh();
		if (result.subtaskErrors.length > 0) {
			logger.warn({source, errors: result.subtaskErrors}, 'Risk cache refresh completed with errors');
			return;
		}
		logger.info(
			{
				source,
				disposableDomainCount: result.disposableDomainCount,
			},
			source === 'startup' ? 'Risk cache initialized on API startup' : 'Risk cache refresh complete on API',
		);
	} catch (error) {
		if (source === 'startup') {
			logger.warn({error}, 'Risk cache initialisation failed on API startup');
			return;
		}
		logger.warn({error}, 'Periodic risk cache refresh failed on API');
	} finally {
		riskCacheRefreshInFlight = false;
	}
}

function startRiskCacheRefreshLoop(logger: ILogger): void {
	if (riskCacheRefreshInterval) {
		return;
	}
	riskCacheRefreshInterval = setInterval(() => {
		void refreshRiskCache(logger, 'interval');
	}, RISK_CACHE_REFRESH_INTERVAL_MS);
}

function stopRiskCacheRefreshLoop(): void {
	if (!riskCacheRefreshInterval) {
		return;
	}
	clearInterval(riskCacheRefreshInterval);
	riskCacheRefreshInterval = null;
}

export function createInitializer(config: APIConfig, logger: ILogger): () => Promise<void> {
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
						asn: geoipStartupResult.asn,
						s3_bucket: geoipStartupResult.bucket,
						s3_key: geoipStartupResult.key,
					},
					'GeoIP databases downloaded from S3',
				);
			}
			if (config.database.backend === 'postgres' && !hasDatabaseQueryExecutor()) {
				await initPostgres(config.postgres);
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
			await ipBanCache.initialize();
			logger.info('IP ban cache initialized');
			await startAbuseReplicationSubscriber(kvClient);
			logger.info('Abusive-IP auto-banner replication started');
			torExitListCache.setKvClient(kvClient);
			await torExitListCache.initialize();
			logger.info('Tor exit list cache initialized');
			const {urlBlocklistCache} = await import('../middleware/UrlBlocklistCache');
			urlBlocklistCache.setRefreshSubscriber(kvClient);
			const {getStorageService} = await import('../middleware/ServiceSingletons');
			urlBlocklistCache.setStorageService(getStorageService());
			await urlBlocklistCache.initialize();
			logger.info('URL blocklist cache initialized');
			const {fileShaCache} = await import('../middleware/FileShaCache');
			fileShaCache.setRefreshSubscriber(kvClient);
			await fileShaCache.initialize();
			logger.info('File SHA blocklist cache initialized');
			const {phraseBlocklistCache} = await import('../middleware/PhraseBlocklistCache');
			phraseBlocklistCache.setRefreshSubscriber(kvClient);
			await phraseBlocklistCache.initialize();
			logger.info('Phrase blocklist cache initialized');
			const {bannedAvatarHashCache} = await import('../middleware/BannedAvatarHashCache');
			bannedAvatarHashCache.setRefreshSubscriber(kvClient);
			await bannedAvatarHashCache.initialize();
			logger.info('Banned avatar hash cache initialized');
			const {profileSubstringBlocklistCache} = await import('../middleware/ProfileSubstringBlocklistCache');
			profileSubstringBlocklistCache.setRefreshSubscriber(kvClient);
			await profileSubstringBlocklistCache.initialize();
			logger.info('Profile substring blocklist cache initialized');
			await initializeServiceSingletons();
			logger.info('Service singletons initialized');
			await refreshRiskCache(logger, 'startup');
			startRiskCacheRefreshLoop(logger);
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
			try {
				const kvDeletionQueue = getKVAccountDeletionQueue();
				if (await kvDeletionQueue.needsRebuild()) {
					logger.warn('KV deletion queue needs rebuild, rebuilding...');
					await kvDeletionQueue.rebuildState();
				} else {
					logger.info('KV deletion queue state is healthy');
				}
			} catch (error) {
				logger.error({error}, 'Failed to verify KV deletion queue state');
				throw error;
			}
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
			await createShutdown(logger)();
			throw error;
		}
	};
}

export function createShutdown(logger: ILogger): () => Promise<void> {
	return async (): Promise<void> => {
		logger.info('Shutting down API service...');
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
			stopRiskCacheRefreshLoop();
			logger.info('Risk cache refresh loop shut down');
		} catch (error) {
			logger.error({error}, 'Error shutting down risk cache refresh loop');
		}
		try {
			ipBanCache.shutdown();
			logger.info('IP ban cache shut down');
		} catch (error) {
			logger.error({error}, 'Error shutting down IP ban cache');
		}
		try {
			await stopAbuseReplicationSubscriber();
			logger.info('Abusive-IP auto-banner replication stopped');
		} catch (error) {
			logger.error({error}, 'Error stopping abusive-IP auto-banner replication');
		}
		try {
			torExitListCache.shutdown();
			logger.info('Tor exit list cache shut down');
		} catch (error) {
			logger.error({error}, 'Error shutting down Tor exit list cache');
		}
		try {
			getInstanceConfigRepository().shutdown();
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
		try {
			setDatabaseQueryExecutor(null);
			await shutdownPostgres();
			logger.info('Postgres client shut down');
		} catch (error) {
			logger.error({error}, 'Error shutting down Postgres client');
		}
		try {
			await shutdownCassandra();
			logger.info('Cassandra client shut down');
		} catch (error) {
			logger.error({error}, 'Error shutting down Cassandra client');
		}
		logger.info('API service shutdown complete');
	};
}
