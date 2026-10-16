// SPDX-License-Identifier: AGPL-3.0-or-later

export type RateLimitScope = 'global' | 'shared' | 'user';
export interface RateLimitResult {
	allowed: boolean;
	limit: number;
	remaining: number;
	resetTime: Date;
	resetAfterDecimal: number;
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
	peekLimit(config: RateLimitConfig): Promise<RateLimitResult>;
	checkBucketLimit(bucket: string, config: BucketConfig): Promise<RateLimitResult>;
	checkGlobalLimit(identifier: string, limit: number): Promise<RateLimitResult>;
	resetLimit(identifier: string): Promise<void>;
	clearLimitsByIdentifierPrefix(identifierPrefix: string): Promise<number>;
}
