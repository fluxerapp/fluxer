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

export interface RateLimitResult {
	allowed: boolean;
	limit: number;
	remaining: number;
	resetTime: Date;
	retryAfter?: number;
	retryAfterDecimal?: number;
	global?: boolean;
}

export interface RateLimitConfig {
	maxAttempts: number;
	windowMs: number;
	identifier: string;
}

export interface BucketConfig {
	limit: number;
	windowMs: number;
	exemptFromGlobal?: boolean;
}

export interface IRateLimitService {
	checkLimit(config: RateLimitConfig): Promise<RateLimitResult>;
	checkBucketLimit(bucket: string, config: BucketConfig): Promise<RateLimitResult>;
	checkGlobalLimit(identifier: string, limit: number): Promise<RateLimitResult>;
	resetLimit(identifier: string): Promise<void>;
	getRemainingAttempts(identifier: string, windowMs: number): Promise<number>;
	getResetTime(identifier: string, windowMs: number): Promise<Date>;
}
