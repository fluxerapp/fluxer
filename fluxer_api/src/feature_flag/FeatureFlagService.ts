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

import type {Redis} from 'ioredis';
import type {GuildID, UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {
	ALL_FEATURE_FLAGS,
	FEATURE_FLAG_REDIS_KEY,
	FEATURE_FLAG_REFRESH_CHANNEL,
	FEATURE_FLAG_USER_CACHE_PREFIX,
	FEATURE_FLAG_USER_CACHE_TTL_SECONDS,
	type FeatureFlag,
} from '~/constants/FeatureFlags';
import type {ICacheService} from '~/infrastructure/ICacheService';
import {Logger} from '~/Logger';
import type {FeatureFlagRepository} from './FeatureFlagRepository';

interface SerializedFeatureFlagConfig {
	[key: string]: Array<string>;
}

export class FeatureFlagService {
	private inMemoryCache: Map<FeatureFlag, Set<string>> = new Map();
	private repository: FeatureFlagRepository;
	private cacheService: ICacheService;
	private redisSubscriber: Redis | null;
	private subscriberInitialized = false;

	constructor(repository: FeatureFlagRepository, cacheService: ICacheService, redisSubscriber: Redis | null = null) {
		this.repository = repository;
		this.cacheService = cacheService;
		this.redisSubscriber = redisSubscriber;

		for (const flag of ALL_FEATURE_FLAGS) {
			this.inMemoryCache.set(flag, new Set());
		}
	}

	async initialize(): Promise<void> {
		await this.refreshCache();
		this.initializeSubscriber();
		Logger.info('FeatureFlagService initialized');
	}

	shutdown(): void {
		// Subscriber connections are managed externally.
	}

	private initializeSubscriber(): void {
		if (this.subscriberInitialized || !this.redisSubscriber) {
			return;
		}

		const subscriber = this.redisSubscriber;
		subscriber
			.subscribe(FEATURE_FLAG_REFRESH_CHANNEL)
			.then(() => {
				subscriber.on('message', (channel) => {
					if (channel === FEATURE_FLAG_REFRESH_CHANNEL) {
						this.refreshCache().catch((err) => {
							Logger.error({err}, 'Failed to refresh feature flag cache from pubsub');
						});
					}
				});
			})
			.catch((error) => {
				Logger.error({error}, 'Failed to subscribe to feature flag refresh channel');
			});

		this.subscriberInitialized = true;
	}

	private async refreshCache(): Promise<void> {
		let config = await this.cacheService.get<SerializedFeatureFlagConfig>(FEATURE_FLAG_REDIS_KEY);

		if (!config) {
			const dbConfig = await this.repository.getAllFeatureFlags();
			config = this.serializeConfig(dbConfig);
			await this.cacheService.set(FEATURE_FLAG_REDIS_KEY, config);
			Logger.debug('Feature flag config loaded from database and cached in Redis');
		}

		this.deserializeIntoMemory(config);
	}

	private serializeConfig(config: Map<FeatureFlag, Set<string>>): SerializedFeatureFlagConfig {
		const result: SerializedFeatureFlagConfig = {};
		for (const [flag, guildIds] of config) {
			result[flag] = Array.from(guildIds);
		}
		return result;
	}

	private deserializeIntoMemory(config: SerializedFeatureFlagConfig): void {
		for (const flag of ALL_FEATURE_FLAGS) {
			const guildIds = config[flag];
			if (guildIds) {
				this.inMemoryCache.set(flag, new Set(guildIds));
			} else {
				this.inMemoryCache.set(flag, new Set());
			}
		}
	}

	isFeatureEnabled(flag: FeatureFlag, guildId: string): boolean {
		if (Config.nodeEnv === 'development') {
			return true;
		}

		const guildIds = this.inMemoryCache.get(flag);
		return guildIds?.has(guildId) ?? false;
	}

	async isFeatureEnabledForUser(
		flag: FeatureFlag,
		userId: UserID,
		guildFetcher: () => Promise<Array<GuildID>>,
	): Promise<boolean> {
		const cacheKey = this.getUserCacheKey(flag, userId);
		const cached = await this.cacheService.get<string>(cacheKey);
		if (cached !== null) {
			return cached === '1';
		}

		const guildIds = await guildFetcher();
		const allowed = guildIds.some((guildId) => this.isFeatureEnabled(flag, guildId.toString()));
		await this.cacheService.set(cacheKey, allowed ? '1' : '0', FEATURE_FLAG_USER_CACHE_TTL_SECONDS);
		return allowed;
	}

	private getUserCacheKey(flag: FeatureFlag, userId: UserID): string {
		return `${FEATURE_FLAG_USER_CACHE_PREFIX}:${flag}:${userId.toString()}`;
	}

	async setFeatureGuildIds(flag: FeatureFlag, guildIds: Set<string>): Promise<void> {
		await this.repository.setFeatureFlag(flag, guildIds);
		await this.invalidateUserFeatureCache(flag);
		await this.cacheService.delete(FEATURE_FLAG_REDIS_KEY);
		await this.refreshCache();
		await this.cacheService.publish(FEATURE_FLAG_REFRESH_CHANNEL, 'refresh');
		Logger.info({flag, guildCount: guildIds.size}, 'Feature flag guild IDs updated');
	}

	getConfigForSession(): Record<string, Array<string>> {
		const result: Record<string, Array<string>> = {};
		for (const [flag, guildIds] of this.inMemoryCache) {
			result[flag] = Array.from(guildIds);
		}
		return result;
	}

	getGuildIdsForFlag(flag: FeatureFlag): Set<string> {
		return new Set(this.inMemoryCache.get(flag) ?? []);
	}

	private async invalidateUserFeatureCache(flag: FeatureFlag): Promise<void> {
		const pattern = `${FEATURE_FLAG_USER_CACHE_PREFIX}:${flag}:*`;
		await this.cacheService.deletePattern(pattern);
	}
}
