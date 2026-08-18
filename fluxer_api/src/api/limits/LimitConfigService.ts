// SPDX-License-Identifier: AGPL-3.0-or-later

import {computeWireFormat} from '@fluxer/limits/src/LimitDiffer';
import {computeDefaultsHash} from '@fluxer/limits/src/LimitHashing';
import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IKVProvider, IKVSubscription} from '@pkgs/kv_client/src/IKVProvider';
import {Config} from '../Config';
import type {CachedLimitConfig} from '../constants/LimitConfig';
import {
	createDefaultLimitConfig,
	getLimitConfigKvKey,
	LIMIT_CONFIG_REFRESH_CHANNEL,
	LIMIT_CONFIG_REFRESH_LOCK_KEY,
	mergeWithCurrentDefaults,
	sanitizeLimitConfigForInstance,
} from '../constants/LimitConfig';
import type {InstanceConfigRepository, InstancePremiumMode} from '../instance/InstanceConfigRepository';
import {Logger} from '../Logger';
import {setCachedInstancePremiumMode} from './InstancePremiumModeCache';

let globalLimitConfigService: LimitConfigService | null = null;

export class LimitConfigService {
	private premiumMode: InstancePremiumMode = 'everyone';
	private config: LimitConfigSnapshot = createDefaultLimitConfig({
		selfHosted: Config.instance.selfHosted,
		premiumMode: 'everyone',
	});
	private repository: InstanceConfigRepository;
	private cacheService: ICacheService;
	private kvClient: IKVProvider | null;
	private kvSubscription: IKVSubscription | null = null;
	private subscriberInitialized = false;
	private readonly cacheKey: string;
	private messageHandler: ((channel: string) => void) | null = null;

	constructor(repository: InstanceConfigRepository, cacheService: ICacheService, kvClient: IKVProvider | null = null) {
		this.repository = repository;
		this.cacheService = cacheService;
		this.kvClient = kvClient;
		this.cacheKey = getLimitConfigKvKey(Config.instance.selfHosted);
	}

	private get limitBuildOptions(): {selfHosted: boolean; premiumMode: InstancePremiumMode} {
		return {selfHosted: Config.instance.selfHosted, premiumMode: this.premiumMode};
	}

	getPremiumMode(): InstancePremiumMode {
		return this.premiumMode;
	}

	setAsGlobalInstance(): void {
		globalLimitConfigService = this;
	}

	async initialize(): Promise<void> {
		await this.refreshCache();
		this.initializeSubscriber();
		Logger.info('LimitConfigService initialized');
	}

	getConfigSnapshot(): LimitConfigSnapshot {
		return this.config;
	}

	getConfigWireFormat(): LimitConfigWireFormat {
		return computeWireFormat(this.config);
	}

	async refreshCache(): Promise<void> {
		const policyConfig = await this.repository.getInstancePolicyConfig();
		this.premiumMode = policyConfig.premium_mode;
		setCachedInstancePremiumMode(this.premiumMode);
		const currentHash = computeDefaultsHash();
		const lockToken = await this.cacheService.acquireLock(LIMIT_CONFIG_REFRESH_LOCK_KEY, 10);
		if (!lockToken) {
			Logger.debug('Limit config refresh already in progress, waiting for cache update');
			await this.sleep(50);
			const cached = await this.cacheService.get<CachedLimitConfig>(this.cacheKey);
			if (cached && cached.defaultsHash === currentHash) {
				this.config = sanitizeLimitConfigForInstance(cached.config, this.limitBuildOptions);
				return;
			}
			this.config = createDefaultLimitConfig(this.limitBuildOptions);
			return;
		}
		try {
			const cached = await this.cacheService.get<CachedLimitConfig>(this.cacheKey);
			if (cached && cached.defaultsHash === currentHash) {
				this.config = sanitizeLimitConfigForInstance(cached.config, this.limitBuildOptions);
				return;
			}
			const dbConfig = await this.repository.getLimitConfig();
			if (dbConfig === null) {
				this.config = createDefaultLimitConfig(this.limitBuildOptions);
				await this.cacheService.delete(this.cacheKey);
				Logger.debug('No database limit config, using fresh defaults');
				return;
			}
			Logger.info(
				{hashMismatch: cached?.defaultsHash !== currentHash},
				'Merging database config with current defaults',
			);
			const merged = mergeWithCurrentDefaults(dbConfig, this.limitBuildOptions);
			await this.repository.setLimitConfig(merged);
			await this.cacheService.set(this.cacheKey, {
				config: merged,
				defaultsHash: currentHash,
			});
			this.config = sanitizeLimitConfigForInstance(merged, this.limitBuildOptions);
		} finally {
			await this.cacheService.releaseLock(LIMIT_CONFIG_REFRESH_LOCK_KEY, lockToken);
		}
	}

	private sleep(ms: number): Promise<void> {
		return new Promise((resolve) => setTimeout(resolve, ms));
	}

	async updateConfig(config: LimitConfigSnapshot): Promise<void> {
		const normalized = sanitizeLimitConfigForInstance(config, this.limitBuildOptions);
		await this.repository.setLimitConfig(normalized);
		await this.cacheService.delete(this.cacheKey);
		await this.refreshCache();
		await this.cacheService.publish(LIMIT_CONFIG_REFRESH_CHANNEL, 'refresh');
		Logger.info({ruleCount: normalized.rules.length}, 'Limit config updated');
	}

	async reloadForPolicyChange(): Promise<void> {
		await this.cacheService.delete(this.cacheKey);
		await this.refreshCache();
		await this.cacheService.publish(LIMIT_CONFIG_REFRESH_CHANNEL, 'refresh');
	}

	private initializeSubscriber(): void {
		if (this.subscriberInitialized || !this.kvClient) {
			return;
		}
		const subscription = this.kvClient.duplicate();
		this.kvSubscription = subscription;
		this.messageHandler = (channel: string) => {
			if (channel === LIMIT_CONFIG_REFRESH_CHANNEL) {
				this.refreshCache().catch((err) => {
					Logger.error({err}, 'Failed to refresh limit config from pubsub');
				});
			}
		};
		subscription
			.connect()
			.then(() => subscription.subscribe(LIMIT_CONFIG_REFRESH_CHANNEL))
			.then(() => {
				if (this.messageHandler) {
					subscription.on('message', this.messageHandler);
				}
			})
			.catch((error) => {
				Logger.error({error}, 'Failed to subscribe to limit config refresh channel');
			});
		this.subscriberInitialized = true;
	}

	shutdown(): void {
		if (this.kvSubscription && this.messageHandler) {
			this.kvSubscription.off('message', this.messageHandler);
		}
		if (this.kvSubscription) {
			this.kvSubscription.quit().catch((err) => {
				Logger.error({err}, 'Failed to close KV subscription');
			});
			this.kvSubscription = null;
		}
		this.messageHandler = null;
	}
}

export function getGlobalLimitConfigSnapshot(): LimitConfigSnapshot {
	if (!globalLimitConfigService) {
		throw new Error('LimitConfigService global instance has not been initialized');
	}
	return globalLimitConfigService.getConfigSnapshot();
}
