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
import {readOptionalIntegerEnv} from '../utils/IntegerOptions';
import {VoiceAvailabilityService} from '../voice/VoiceAvailabilityService';
import {VoiceRepository} from '../voice/VoiceRepository';
import {VoiceServerLoadTracker} from '../voice/VoiceServerLoad';
import {VoiceTopology} from '../voice/VoiceTopology';
import type {WorkerTaskName} from '../worker/WorkerLaneConfig';

export function createSnowflakeService(): SnowflakeService {
	const connectionManager = new NatsConnectionManager({
		url: Config.nats.coreUrl,
		token: Config.nats.authToken || undefined,
		name: process.env.FLUXER_SNOWFLAKE_SERVICE_NATS_CLIENT_NAME || 'fluxer-api-snowflakes',
	});
	return new SnowflakeService({
		connectionManager,
		subject: process.env.FLUXER_SNOWFLAKE_SERVICE_SUBJECT || undefined,
		batchSize: readOptionalIntegerEnv('FLUXER_SNOWFLAKE_SERVICE_BATCH_SIZE'),
		lowWatermark: readOptionalIntegerEnv('FLUXER_SNOWFLAKE_SERVICE_LOW_WATERMARK'),
		maxBufferAgeMs: readOptionalIntegerEnv('FLUXER_SNOWFLAKE_SERVICE_MAX_BUFFER_AGE_MS'),
		requestTimeoutMs: readOptionalIntegerEnv('FLUXER_SNOWFLAKE_SERVICE_REQUEST_TIMEOUT_MS'),
	});
}

let _kvClient: KVClient | null = null;
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

export function closeOwnedKVClient(): void {
	const client = _kvClient;
	if (client === null) {
		return;
	}
	client.close();
	_kvClient = null;
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

export interface SnowflakeServiceHandle {
	readonly service: ISnowflakeService;
	readonly owned: boolean;
}

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

export function acquireSnowflakeService(): SnowflakeServiceHandle {
	return {service: getSnowflakeService(), owned: _injectedSnowflakeService === undefined};
}

export async function releaseSnowflakeService({service, owned}: SnowflakeServiceHandle): Promise<void> {
	if (!owned) return;
	try {
		await service.shutdown();
	} finally {
		if (_snowflakeService === service) _snowflakeService = null;
	}
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

interface BlueskyOAuthServiceSelection {
	readonly config: BlueskyOAuthConfig;
	readonly signature: string;
	readonly service: Promise<IBlueskyOAuthService>;
}

let _injectedBlueskyOAuthService: IBlueskyOAuthService | undefined;
let _blueskyOAuthSelection: BlueskyOAuthServiceSelection | undefined;
let _blueskyOAuthGeneration = Symbol();
let _disabledBlueskyOAuthService: DisabledBlueskyOAuthService | undefined;

export function setInjectedBlueskyOAuthService(service: IBlueskyOAuthService | undefined): void {
	_injectedBlueskyOAuthService = service;
	resetBlueskyOAuthServiceCache();
}

function resetBlueskyOAuthServiceCache(): void {
	_blueskyOAuthSelection = undefined;
	_blueskyOAuthGeneration = Symbol();
	_disabledBlueskyOAuthService = undefined;
}

function getDisabledBlueskyOAuthService(): DisabledBlueskyOAuthService {
	if (!_disabledBlueskyOAuthService) {
		_disabledBlueskyOAuthService = new DisabledBlueskyOAuthService();
	}
	return _disabledBlueskyOAuthService;
}

async function createBlueskyOAuthService(config: BlueskyOAuthConfig): Promise<IBlueskyOAuthService> {
	const disabledService = getDisabledBlueskyOAuthService();
	if (!config.enabled) return disabledService;
	if (config.keys.length === 0) {
		Logger.warn('Bluesky OAuth is enabled but no signing keys are configured; disabling Bluesky OAuth.');
		return disabledService;
	}
	try {
		return await BlueskyOAuthService.create(config, getKVClient(), Config.endpoints.apiPublic);
	} catch (error) {
		Logger.error({error}, 'Bluesky OAuth initialization failed; disabling Bluesky OAuth for this configuration.');
		return disabledService;
	}
}

export async function resolveBlueskyOAuthService(
	instanceConfigRepository?: Pick<InstanceConfigRepository, 'getEffectiveBlueskyConfig'>,
): Promise<IBlueskyOAuthService> {
	if (_injectedBlueskyOAuthService) {
		return _injectedBlueskyOAuthService;
	}
	const generation = _blueskyOAuthGeneration;
	const blueskyConfig = instanceConfigRepository
		? await instanceConfigRepository.getEffectiveBlueskyConfig()
		: Config.auth.bluesky;
	if (_injectedBlueskyOAuthService) {
		return _injectedBlueskyOAuthService;
	}
	if (generation !== _blueskyOAuthGeneration) {
		throw new Error('Bluesky OAuth service resolution was invalidated before initialization');
	}
	const selection = _blueskyOAuthSelection;
	if (selection?.config === blueskyConfig) return selection.service;
	const signature = JSON.stringify(blueskyConfig);
	if (selection?.signature === signature) return selection.service;
	const next: BlueskyOAuthServiceSelection = {
		config: blueskyConfig,
		signature,
		service: createBlueskyOAuthService(blueskyConfig),
	};
	_blueskyOAuthSelection = next;
	return next.service;
}

interface VoiceResources {
	readonly topology: VoiceTopology | null;
	readonly availability: VoiceAvailabilityService | null;
	readonly liveKit: ILiveKitService;
	readonly roomStore: IVoiceRoomStore;
}

class VoiceResourceSession {
	readonly ready: Promise<void>;
	private current: VoiceResources | null = null;
	private stopCompletion: Promise<void> | null = null;

	constructor(
		private readonly topology: VoiceTopology | null,
		createResources: () => VoiceResources,
	) {
		this.ready = Promise.resolve().then(() => this.initialize(createResources));
	}

	get resources(): VoiceResources | null {
		return this.current;
	}

	get isStopped(): boolean {
		return this.stopCompletion !== null;
	}

	private async initialize(createResources: () => VoiceResources): Promise<void> {
		try {
			if (this.isStopped) throw new Error('Voice resources stopped before initialization');
			await this.topology?.initialize();
			if (this.isStopped) throw new Error('Voice resources stopped during initialization');
			this.current = createResources();
		} catch (error) {
			try {
				await this.topology?.shutdown();
			} catch (cleanupError) {
				throw new AggregateError([error, cleanupError], 'Failed to initialize and close voice resources');
			}
			throw error;
		}
	}

	shutdown(): Promise<void> {
		this.current = null;
		this.stopCompletion ??= this.close();
		return this.stopCompletion;
	}

	private async close(): Promise<void> {
		const [closed] = await Promise.allSettled([this.topology?.shutdown(), this.ready]);
		if (closed.status === 'rejected') throw closed.reason;
	}
}

let voiceSession: VoiceResourceSession | null = null;
let voiceResourcesStopped = false;

function createVoiceSession(): VoiceResourceSession {
	if (!Config.voice.enabled) {
		return new VoiceResourceSession(null, () => ({
			topology: null,
			availability: null,
			liveKit: new DisabledLiveKitService(),
			roomStore: new InMemoryVoiceRoomStore(),
		}));
	}
	const kvClient = getKVClient();
	const gatewayService = getGatewayService();
	const topology = new VoiceTopology(new VoiceRepository(), kvClient);
	return new VoiceResourceSession(topology, () => ({
		topology,
		availability: new VoiceAvailabilityService(topology, new VoiceServerLoadTracker({gatewayService})),
		liveKit: new LiveKitService(topology),
		roomStore: new VoiceRoomStore(kvClient),
	}));
}

export async function ensureVoiceResourcesInitialized(): Promise<void> {
	if (voiceResourcesStopped) throw new Error('Voice resources are shut down');
	const session = (voiceSession ??= createVoiceSession());
	try {
		await session.ready;
	} catch (error) {
		if (voiceSession === session) voiceSession = null;
		throw error;
	}
	if (voiceSession !== session) throw new Error('Voice resource initialization was superseded');
	if (session.isStopped) throw new Error('Voice resources stopped during initialization');
}

export async function shutdownVoiceResources(): Promise<void> {
	voiceResourcesStopped = true;
	await voiceSession?.shutdown();
}

export function getVoiceTopology(): VoiceTopology | null {
	return voiceSession?.resources?.topology ?? null;
}

export function getVoiceAvailabilityService(): VoiceAvailabilityService | null {
	return voiceSession?.resources?.availability ?? null;
}

export function getLiveKitServiceInstance(): ILiveKitService | null {
	return voiceSession?.resources?.liveKit ?? null;
}

export function getVoiceRoomStoreInstance(): IVoiceRoomStore | null {
	return voiceSession?.resources?.roomStore ?? null;
}

export function resetServiceRegistryForTesting(): void {
	_kvClient = null;
	_snowflakeService = null;
	_billingRepository = null;
	resetBlueskyOAuthServiceCache();
	const previousVoiceSession = voiceSession;
	voiceSession = null;
	voiceResourcesStopped = false;
	void previousVoiceSession?.shutdown().catch((error) => {
		Logger.error({error}, 'Failed to close voice resources during registry reset');
	});
}
