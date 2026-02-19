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

import type {IBlueskyOAuthService} from '@fluxer/api/src/bluesky/IBlueskyOAuthService';
import {Config} from '@fluxer/api/src/Config';
import {DisabledLiveKitService} from '@fluxer/api/src/infrastructure/DisabledLiveKitService';
import {GatewayService as ProdGatewayService} from '@fluxer/api/src/infrastructure/GatewayService';
import type {IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import type {ILiveKitService} from '@fluxer/api/src/infrastructure/ILiveKitService';
import type {IMediaService} from '@fluxer/api/src/infrastructure/IMediaService';
import {InMemoryVoiceRoomStore} from '@fluxer/api/src/infrastructure/InMemoryVoiceRoomStore';
import type {IVoiceRoomStore} from '@fluxer/api/src/infrastructure/IVoiceRoomStore';
import {LiveKitService} from '@fluxer/api/src/infrastructure/LiveKitService';
import {MediaService as ProdMediaService} from '@fluxer/api/src/infrastructure/MediaService';
import {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import {VoiceRoomStore} from '@fluxer/api/src/infrastructure/VoiceRoomStore';
import {setInjectedSearchProvider} from '@fluxer/api/src/SearchFactory';
import type {ISearchProvider} from '@fluxer/api/src/search/ISearchProvider';
import {VoiceAvailabilityService} from '@fluxer/api/src/voice/VoiceAvailabilityService';
import {VoiceRepository} from '@fluxer/api/src/voice/VoiceRepository';
import {VoiceTopology} from '@fluxer/api/src/voice/VoiceTopology';
import type {IKVProvider} from '@fluxer/kv_client/src/IKVProvider';
import {KVClient} from '@fluxer/kv_client/src/KVClient';
import type {S3Service} from '@fluxer/s3/src/s3/S3Service';
import type {IWorkerService} from '@fluxer/worker/src/contracts/IWorkerService';

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
		});
	}
	return _kvClient;
}

let _injectedWorkerService: IWorkerService | undefined;

export function setInjectedWorkerService(service: IWorkerService | undefined): void {
	_injectedWorkerService = service;
}

export function getWorkerService(): IWorkerService {
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
export function getSnowflakeService(): SnowflakeService {
	if (!_snowflakeService) {
		_snowflakeService = Config.dev.testModeEnabled ? new SnowflakeService() : new SnowflakeService(getKVClient());
	}
	return _snowflakeService;
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

let _injectedS3Service: S3Service | undefined;

export function setInjectedS3Service(s3Service: S3Service | undefined): void {
	_injectedS3Service = s3Service;
}

export function getInjectedS3Service(): S3Service | undefined {
	return _injectedS3Service;
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

export function setInjectedBlueskyOAuthService(service: IBlueskyOAuthService | undefined): void {
	_injectedBlueskyOAuthService = service;
}

export function getInjectedBlueskyOAuthService(): IBlueskyOAuthService | undefined {
	return _injectedBlueskyOAuthService;
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
