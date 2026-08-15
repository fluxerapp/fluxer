// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {KVClient} from '@pkgs/kv_client/src/KVClient';
import {NatsConnectionManager} from '@pkgs/nats/src/NatsConnectionManager';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {BillingRepository} from '../billing/repositories/BillingRepository';
import {BlueskyOAuthService} from '../bluesky/BlueskyOAuthService';
import {DisabledBlueskyOAuthService} from '../bluesky/DisabledBlueskyOAuthService';
import type {IBlueskyOAuthService} from '../bluesky/IBlueskyOAuthService';
import {Config} from '../Config';
import type {BlueskyOAuthConfig} from '../config/APIConfig';
import {DisabledLiveKitService} from '../infrastructure/DisabledLiveKitService';
import {GatewayService as ProdGatewayService} from '../infrastructure/GatewayService';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import type {ILiveKitService} from '../infrastructure/ILiveKitService';
import type {IMediaService} from '../infrastructure/IMediaService';
import {InMemoryVoiceRoomStore} from '../infrastructure/InMemoryVoiceRoomStore';
import type {ISnowflakeService} from '../infrastructure/ISnowflakeService';
import type {IVoiceRoomStore} from '../infrastructure/IVoiceRoomStore';
import {LiveKitService} from '../infrastructure/LiveKitService';
import {MediaService as ProdMediaService} from '../infrastructure/MediaService';
import {SnowflakeService} from '../infrastructure/SnowflakeService';
import {VoiceRoomStore} from '../infrastructure/VoiceRoomStore';
import type {InstanceConfigRepository} from '../instance/InstanceConfigRepository';
import {Logger} from '../Logger';
import {setInjectedSearchProvider} from '../SearchFactory';
import type {ISearchProvider} from '../search/ISearchProvider';
import {VoiceAvailabilityService} from '../voice/VoiceAvailabilityService';
import {VoiceRepository} from '../voice/VoiceRepository';
import {VoiceTopology} from '../voice/VoiceTopology';
import type {WorkerTaskName} from '../worker/WorkerLaneConfig';

const DEFAULT_SNOWFLAKE_SERVICE_BATCH_SIZE = 128;
const DEFAULT_SNOWFLAKE_SERVICE_LOW_WATERMARK = 32;
const DEFAULT_SNOWFLAKE_SERVICE_MAX_BUFFER_AGE_MS = 5000;
const DEFAULT_SNOWFLAKE_SERVICE_REQUEST_TIMEOUT_MS = 5000;

function readPositiveIntegerEnv(names: string | Array<string>, fallback: number): number {
	const envNames = Array.isArray(names) ? names : [names];
	const value = envNames.map((name) => process.env[name]).find((candidate) => candidate && candidate.length > 0);
	if (!value) {
		return fallback;
	}
	const parsed = Number(value);
	if (!Number.isInteger(parsed) || parsed <= 0) {
		return fallback;
	}
	return parsed;
}

export function createSnowflakeService(): SnowflakeService {
	const connectionManager = new NatsConnectionManager({
		url: Config.nats.coreUrl,
		token: Config.nats.authToken || undefined,
		name: process.env.FLUXER_SNOWFLAKE_SERVICE_NATS_CLIENT_NAME || 'fluxer-api-snowflakes',
	});
	return new SnowflakeService({
		connectionManager,
		subject: process.env.FLUXER_SNOWFLAKE_SERVICE_SUBJECT || 'svc.snowflakes',
		batchSize: readPositiveIntegerEnv('FLUXER_SNOWFLAKE_SERVICE_BATCH_SIZE', DEFAULT_SNOWFLAKE_SERVICE_BATCH_SIZE),
		lowWatermark: readPositiveIntegerEnv(
			'FLUXER_SNOWFLAKE_SERVICE_LOW_WATERMARK',
			DEFAULT_SNOWFLAKE_SERVICE_LOW_WATERMARK,
		),
		maxBufferAgeMs: readPositiveIntegerEnv(
			'FLUXER_SNOWFLAKE_SERVICE_MAX_BUFFER_AGE_MS',
			DEFAULT_SNOWFLAKE_SERVICE_MAX_BUFFER_AGE_MS,
		),
		requestTimeoutMs: readPositiveIntegerEnv(
			'FLUXER_SNOWFLAKE_SERVICE_REQUEST_TIMEOUT_MS',
			DEFAULT_SNOWFLAKE_SERVICE_REQUEST_TIMEOUT_MS,
		),
	});
}

let _kvClient: IKVProvider | null = null;
let _injectedKVProvider: IKVProvider | undefined;

export function setInjectedKVProvider(provider: IKVProvider | undefined): void {
	_injectedKVProvider = provider;
}

export function getKVClient(): IKVProvider {
	if (_injectedKVProvider) {
		return _injectedKVProvider;
	}
	if (!_kvClient) {
		_kvClient = new KVClient({
			url: Config.kv.url,
			mode: Config.kv.mode,
			clusterNodes: Config.kv.clusterNodes,
			clusterNatMap: Config.kv.clusterNatMap,
		});
	}
	return _kvClient;
}

let _injectedWorkerService: IWorkerService<WorkerTaskName> | undefined;

export function setInjectedWorkerService(service: IWorkerService<WorkerTaskName> | undefined): void {
	_injectedWorkerService = service;
}

export function getWorkerService(): IWorkerService<WorkerTaskName> {
	if (_injectedWorkerService) {
		return _injectedWorkerService;
	}
	throw new Error('WorkerService has not been initialized. Call setInjectedWorkerService() during startup.');
}

let _injectedGatewayService: IGatewayService | undefined;

export function setInjectedGatewayService(service: IGatewayService | undefined): void {
	_injectedGatewayService = service;
}

export function getGatewayService(): IGatewayService {
	if (_injectedGatewayService) {
		return _injectedGatewayService;
	}
	return new ProdGatewayService();
}

let _snowflakeService: SnowflakeService | null = null;
let _injectedSnowflakeService: ISnowflakeService | undefined;

export function setInjectedSnowflakeService(service: ISnowflakeService | undefined): void {
	_injectedSnowflakeService = service;
}

export function getSnowflakeService(): ISnowflakeService {
	if (_injectedSnowflakeService) {
		return _injectedSnowflakeService;
	}
	if (!_snowflakeService) {
		_snowflakeService = createSnowflakeService();
	}
	return _snowflakeService;
}

let _billingRepository: BillingRepository | null = null;
let _injectedBillingRepository: BillingRepository | undefined;

export function getBillingRepository(): BillingRepository {
	if (_injectedBillingRepository) {
		return _injectedBillingRepository;
	}
	if (!_billingRepository) {
		_billingRepository = new BillingRepository(getSnowflakeService(), getKVClient());
	}
	return _billingRepository;
}

let _injectedMediaService: IMediaService | undefined;

export function setInjectedMediaService(mediaService: IMediaService | undefined): void {
	_injectedMediaService = mediaService;
}

export function getMediaService(): IMediaService {
	if (_injectedMediaService) {
		return _injectedMediaService;
	}
	return new ProdMediaService();
}

let _injectedSearchProvider: ISearchProvider | undefined;

export function setInjectedSearchProviderService(provider: ISearchProvider | undefined): void {
	_injectedSearchProvider = provider;
	setInjectedSearchProvider(provider);
}

export function getInjectedSearchProvider(): ISearchProvider | undefined {
	return _injectedSearchProvider;
}

let _injectedBlueskyOAuthService: IBlueskyOAuthService | undefined;
let _blueskyOAuthService: IBlueskyOAuthService | undefined;
let _blueskyOAuthInitializationPromise: Promise<IBlueskyOAuthService> | null = null;
let _blueskyOAuthConfigSignature: string | null = null;
let _blueskyOAuthInitializationSignature: string | null = null;
let _disabledBlueskyOAuthService: DisabledBlueskyOAuthService | undefined;

export function setInjectedBlueskyOAuthService(service: IBlueskyOAuthService | undefined): void {
	_injectedBlueskyOAuthService = service;
}

function getDisabledBlueskyOAuthService(): DisabledBlueskyOAuthService {
	if (!_disabledBlueskyOAuthService) {
		_disabledBlueskyOAuthService = new DisabledBlueskyOAuthService();
	}
	return _disabledBlueskyOAuthService;
}

function getBlueskyOAuthConfigSignature(config: BlueskyOAuthConfig): string {
	return JSON.stringify(config);
}

export async function resolveBlueskyOAuthService(
	instanceConfigRepository?: Pick<InstanceConfigRepository, 'getEffectiveBlueskyConfig'>,
): Promise<IBlueskyOAuthService> {
	if (_injectedBlueskyOAuthService) {
		return _injectedBlueskyOAuthService;
	}
	const blueskyConfig = instanceConfigRepository
		? await instanceConfigRepository.getEffectiveBlueskyConfig()
		: Config.auth.bluesky;
	const signature = getBlueskyOAuthConfigSignature(blueskyConfig);
	if (_blueskyOAuthService !== undefined && _blueskyOAuthConfigSignature === signature) {
		return _blueskyOAuthService;
	}
	if (!blueskyConfig.enabled) {
		_blueskyOAuthService = getDisabledBlueskyOAuthService();
		_blueskyOAuthConfigSignature = signature;
		return _blueskyOAuthService;
	}
	if (blueskyConfig.keys.length === 0) {
		Logger.warn(
			'Bluesky OAuth is enabled but no signing keys are configured – disabling. Run scripts/bootstrap/generate_bluesky_oauth_keys.sh or configure keys manually.',
		);
		_blueskyOAuthService = getDisabledBlueskyOAuthService();
		_blueskyOAuthConfigSignature = signature;
		return _blueskyOAuthService;
	}
	if (!_blueskyOAuthInitializationPromise || _blueskyOAuthInitializationSignature !== signature) {
		_blueskyOAuthInitializationSignature = signature;
		_blueskyOAuthInitializationPromise = BlueskyOAuthService.create(
			blueskyConfig,
			getKVClient(),
			Config.endpoints.apiPublic,
		)
			.then((service) => {
				_blueskyOAuthService = service;
				_blueskyOAuthConfigSignature = signature;
				return service;
			})
			.catch((error) => {
				Logger.error({error}, 'Bluesky OAuth signing keys were rejected – disabling Bluesky OAuth.');
				_blueskyOAuthService = getDisabledBlueskyOAuthService();
				_blueskyOAuthConfigSignature = signature;
				return _blueskyOAuthService;
			})
			.finally(() => {
				_blueskyOAuthInitializationPromise = null;
				_blueskyOAuthInitializationSignature = null;
			});
	}
	return _blueskyOAuthInitializationPromise;
}

let voiceTopology: VoiceTopology | null = null;
let voiceAvailabilityService: VoiceAvailabilityService | null = null;
let liveKitServiceInstance: ILiveKitService | null = null;
let voiceRoomStoreInstance: IVoiceRoomStore | null = null;
let voiceConfigSubscriber: IKVProvider | null = null;
let voiceInitializationPromise: Promise<void> | null = null;

export async function ensureVoiceResourcesInitialized(): Promise<void> {
	if (!Config.voice.enabled) {
		if (!liveKitServiceInstance) {
			liveKitServiceInstance = new DisabledLiveKitService();
		}
		if (!voiceRoomStoreInstance) {
			voiceRoomStoreInstance = new InMemoryVoiceRoomStore();
		}
		voiceTopology = null;
		voiceAvailabilityService = null;
		return;
	}
	if (voiceTopology && voiceAvailabilityService && liveKitServiceInstance && voiceRoomStoreInstance) {
		return;
	}
	if (!voiceInitializationPromise) {
		voiceInitializationPromise = (async () => {
			const voiceRepository = new VoiceRepository();
			if (!voiceConfigSubscriber) {
				voiceConfigSubscriber = getKVClient();
			}
			const topology = new VoiceTopology(voiceRepository, voiceConfigSubscriber);
			await topology.initialize();
			voiceTopology = topology;
			voiceAvailabilityService = new VoiceAvailabilityService(topology);
			liveKitServiceInstance = new LiveKitService(topology);
			voiceRoomStoreInstance = new VoiceRoomStore(getKVClient());
		})().finally(() => {
			voiceInitializationPromise = null;
		});
	}
	await voiceInitializationPromise;
}

export function getVoiceTopology(): VoiceTopology | null {
	return voiceTopology;
}

export function getVoiceAvailabilityService(): VoiceAvailabilityService | null {
	return voiceAvailabilityService;
}

export function getLiveKitServiceInstance(): ILiveKitService | null {
	return liveKitServiceInstance;
}

export function getVoiceRoomStoreInstance(): IVoiceRoomStore | null {
	return voiceRoomStoreInstance;
}
