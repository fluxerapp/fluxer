// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {
	createDefaultLimitConfig,
	getLegacyLimitConfigKvKey,
	LIMIT_CONFIG_REFRESH_CHANNEL,
	mergeWithCurrentDefaults,
	sanitizeLimitConfigForInstance,
} from '@app/api/constants/LimitConfig';
import {
	INSTANCE_CONFIG_REFRESH_CHANNEL,
	type InstanceConfigRepository,
	type InstancePolicyConfig,
	type InstancePremiumMode,
} from '@app/api/instance/InstanceConfigRepository';
import {Logger} from '@app/api/Logger';
import {setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {awaitAll} from '@app/api/utils/ConcurrencyUtils';
import {RefreshSubscription} from '@app/api/utils/RefreshSubscription';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {computeWireFormat} from '@fluxer/limits/src/LimitDiffer';
import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

const MAX_PENDING_OPERATIONS = 32;

let globalLimitConfigService: LimitConfigService | null = null;

interface LimitBuildOptions {
	selfHosted: boolean;
	premiumMode: InstancePremiumMode;
}

export class LimitConfigService {
	private premiumMode: InstancePremiumMode = 'everyone';
	private config: LimitConfigSnapshot = createDefaultLimitConfig({
		selfHosted: Config.instance.selfHosted,
		premiumMode: 'everyone',
	});
	private hasLifecycleStarted = false;
	private lifecycleGeneration = 0;
	private initialization: Promise<void> | null = null;
	private stopping: Promise<void> | null = null;
	private operationTail: Promise<void> = Promise.resolve();
	private pendingOperations = 0;
	private readonly refreshSubscription = new RefreshSubscription({
		name: 'LimitConfigService',
		channels: [LIMIT_CONFIG_REFRESH_CHANNEL, INSTANCE_CONFIG_REFRESH_CHANNEL],
		refresh: () => this.refreshCache(),
		onRefreshError: (err) => {
			Logger.error({err}, 'Failed to refresh limit config from pubsub');
		},
		closeMode: 'quit',
	});

	constructor(
		private readonly repository: InstanceConfigRepository,
		private readonly cacheService: ICacheService,
		private readonly kvClient: IKVProvider | null = null,
	) {}

	setAsGlobalInstance(): void {
		const defaultOnly = this.kvClient === null && !this.hasLifecycleStarted;
		if (!defaultOnly && !this.refreshSubscription.isReady) {
			throw new Error('Cannot publish LimitConfigService before initialization or after shutdown');
		}
		globalLimitConfigService = this;
	}

	async initialize(): Promise<void> {
		if (this.stopping) throw new Error('Cannot initialize LimitConfigService while it is shutting down');
		this.initialization ??= this.start().finally(() => {
			this.initialization = null;
		});
		await this.initialization;
	}

	private async start(): Promise<void> {
		this.hasLifecycleStarted = true;
		const generation = this.lifecycleGeneration;
		try {
			await this.refreshSubscription.start(this.kvClient);
			this.assertRefreshActive(generation);
			Logger.info('LimitConfigService initialized');
		} catch (error) {
			try {
				await this.shutdown();
			} catch (cleanupError) {
				throw new AggregateError([error, cleanupError], 'Limit config initialization and cleanup failed');
			}
			throw error;
		}
	}

	getConfigSnapshot(): LimitConfigSnapshot {
		return this.config;
	}

	getDefaultConfigSnapshot(): LimitConfigSnapshot {
		return createDefaultLimitConfig({selfHosted: Config.instance.selfHosted, premiumMode: this.premiumMode});
	}

	getConfigWireFormat(): LimitConfigWireFormat {
		return computeWireFormat(this.config);
	}

	refreshCache(): Promise<void> {
		return this.runOperation((generation) => this.refreshConfig(generation));
	}

	private assertRefreshActive(generation: number): void {
		if (generation !== this.lifecycleGeneration) {
			throw new Error('Limit config operation was interrupted by shutdown');
		}
		if (this.stopping || (this.hasLifecycleStarted && !this.refreshSubscription.isActive)) {
			throw new Error('LimitConfigService is not active');
		}
	}

	private async runOperation(operation: (generation: number) => Promise<void>): Promise<void> {
		const generation = this.lifecycleGeneration;
		this.assertRefreshActive(generation);
		if (this.pendingOperations >= MAX_PENDING_OPERATIONS) {
			Logger.warn({pendingOperations: this.pendingOperations}, 'Limit config operation capacity exceeded');
			throw new ServiceUnavailableError();
		}
		this.pendingOperations++;
		const completion = this.operationTail
			.then(() => {
				this.assertRefreshActive(generation);
				return operation(generation);
			})
			.finally(() => {
				this.pendingOperations--;
			});
		this.operationTail = completion.then(
			() => undefined,
			() => undefined,
		);
		await completion;
	}

	private async refreshConfig(generation: number): Promise<void> {
		this.assertRefreshActive(generation);
		const {config: stored, premiumMode} = await this.repository.readLimitConfigInputs();
		this.assertRefreshActive(generation);
		const buildOptions: LimitBuildOptions = {
			selfHosted: Config.instance.selfHosted,
			premiumMode,
		};
		this.config =
			stored === null
				? createDefaultLimitConfig(buildOptions)
				: mergeWithCurrentDefaults(sanitizeLimitConfigForInstance(stored, buildOptions), buildOptions);
		this.premiumMode = premiumMode;
		setCachedInstancePremiumMode(premiumMode);
	}

	updateConfig(config: LimitConfigSnapshot): Promise<void> {
		return this.runOperation(async (generation) => {
			const policy = await this.repository.readStoredInstancePolicyConfig();
			this.assertRefreshActive(generation);
			const buildOptions: LimitBuildOptions = {
				selfHosted: Config.instance.selfHosted,
				premiumMode: policy.premium_mode,
			};
			const normalized = mergeWithCurrentDefaults(sanitizeLimitConfigForInstance(config, buildOptions), buildOptions);
			await this.repository.setLimitConfig(normalized);
			await this.reloadConfig(generation);
			Logger.info({ruleCount: normalized.rules.length}, 'Limit config updated');
		});
	}

	updatePolicyConfig(patch: Partial<InstancePolicyConfig>): Promise<void> {
		return this.runOperation(async (generation) => {
			await this.repository.setInstancePolicyConfig(patch);
			await this.reloadConfig(generation);
		});
	}

	private async reloadConfig(generation: number): Promise<void> {
		this.assertRefreshActive(generation);
		await this.cacheService.delete(getLegacyLimitConfigKvKey(Config.instance.selfHosted));
		await this.refreshConfig(generation);
		this.assertRefreshActive(generation);
		await this.cacheService.publish(LIMIT_CONFIG_REFRESH_CHANNEL, 'refresh');
	}

	shutdown(): Promise<void> {
		if (this.stopping) return this.stopping;
		this.hasLifecycleStarted = true;
		this.lifecycleGeneration++;
		if (globalLimitConfigService === this) globalLimitConfigService = null;
		this.stopping = awaitAll(
			[this.refreshSubscription.stop(), this.operationTail],
			'Limit config shutdown failed',
		).finally(() => {
			this.stopping = null;
		});
		return this.stopping;
	}
}

export function resetGlobalLimitConfigServiceForTesting(): void {
	globalLimitConfigService = null;
}

export function getGlobalLimitConfigSnapshot(): LimitConfigSnapshot {
	if (!globalLimitConfigService) {
		throw new Error('LimitConfigService global instance has not been initialized');
	}
	return globalLimitConfigService.getConfigSnapshot();
}
