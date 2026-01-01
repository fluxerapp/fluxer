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

import type {ICacheService} from './ICacheService';
import type {BucketConfig, IRateLimitService, RateLimitConfig, RateLimitResult} from './IRateLimitService';

interface RateLimitData {
	attempts: number;
	resetTime: Date;
}

export class RateLimitService implements IRateLimitService {
	private static readonly GLOBAL_WINDOW_MS = 1000;

	constructor(private cacheService: ICacheService) {}

	private async getRateLimitData(key: string): Promise<RateLimitData | null> {
		const rawData = await this.cacheService.get<RateLimitData>(key);
		if (!rawData) return null;

		return {
			...rawData,
			resetTime: rawData.resetTime instanceof Date ? rawData.resetTime : new Date(rawData.resetTime),
		};
	}

	private async checkLimitInternal(
		key: string,
		limit: number,
		windowMs: number,
		global?: boolean,
	): Promise<RateLimitResult> {
		const now = new Date();
		const data = await this.getRateLimitData(key);

		if (!data || now >= data.resetTime) {
			const resetTime = new Date(now.getTime() + windowMs);
			const newData: RateLimitData = {
				attempts: 1,
				resetTime,
			};

			await this.cacheService.set(key, newData, Math.ceil(windowMs / 1000));

			return {
				allowed: true,
				limit,
				remaining: limit - 1,
				resetTime,
				global,
			};
		}

		if (data.attempts >= limit) {
			const retryAfterDecimal = (data.resetTime.getTime() - now.getTime()) / 1000;
			const retryAfter = Math.ceil(retryAfterDecimal);
			return {
				allowed: false,
				limit,
				remaining: 0,
				resetTime: data.resetTime,
				retryAfter,
				retryAfterDecimal,
				global,
			};
		}

		const updatedData: RateLimitData = {
			...data,
			attempts: data.attempts + 1,
		};

		const ttl = Math.ceil((data.resetTime.getTime() - now.getTime()) / 1000);
		await this.cacheService.set(key, updatedData, ttl);

		return {
			allowed: true,
			limit,
			remaining: limit - updatedData.attempts,
			resetTime: data.resetTime,
			global,
		};
	}

	async checkLimit(config: RateLimitConfig): Promise<RateLimitResult> {
		const key = `ratelimit:${config.identifier}`;
		return this.checkLimitInternal(key, config.maxAttempts, config.windowMs);
	}

	async checkBucketLimit(bucket: string, config: BucketConfig): Promise<RateLimitResult> {
		const key = `ratelimit:bucket:${bucket}`;
		return this.checkLimitInternal(key, config.limit, config.windowMs);
	}

	async checkGlobalLimit(identifier: string, limit: number): Promise<RateLimitResult> {
		const key = `ratelimit:global:${identifier}`;
		return this.checkLimitInternal(key, limit, RateLimitService.GLOBAL_WINDOW_MS, true);
	}

	async resetLimit(identifier: string): Promise<void> {
		const key = `ratelimit:${identifier}`;
		await this.cacheService.delete(key);
	}

	async getRemainingAttempts(identifier: string, _windowMs: number): Promise<number> {
		const key = `ratelimit:${identifier}`;
		const data = await this.getRateLimitData(key);
		const now = new Date();

		if (!data || now >= data.resetTime) {
			return 0;
		}

		return Math.max(0, data.attempts);
	}

	async getResetTime(identifier: string, _windowMs: number): Promise<Date> {
		const key = `ratelimit:${identifier}`;
		const data = await this.getRateLimitData(key);
		const now = new Date();

		if (!data || now >= data.resetTime) {
			return now;
		}

		return data.resetTime;
	}
}
