// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {
	sanitizeRetryAfterDecimalSeconds,
	sanitizeRetryAfterSeconds,
} from '@fluxer/errors/src/domains/core/RetryAfterSeconds';
import {ThrottledError} from '@fluxer/errors/src/domains/core/ThrottledError';
import type {FluxerErrorData} from '@fluxer/errors/src/FluxerError';

type RateLimitScope = 'global' | 'shared' | 'user';

function sanitizeResetTime(resetTime: Date): number {
	const timestamp = resetTime.getTime();
	if (!Number.isFinite(timestamp)) {
		return Math.floor(Date.now() / 1000) + 60;
	}
	const resetSeconds = Math.floor(timestamp / 1000);
	const nowSeconds = Math.floor(Date.now() / 1000);
	if (resetSeconds <= nowSeconds) {
		return nowSeconds + 1;
	}
	return resetSeconds;
}

function sanitizeRateLimitScope(scope: RateLimitScope | undefined, global: boolean): RateLimitScope {
	if (scope === 'shared' || scope === 'user' || scope === 'global') {
		return scope;
	}
	return global ? 'global' : 'user';
}

export class RateLimitError extends ThrottledError {
	constructor({
		code = APIErrorCodes.RATE_LIMITED,
		message,
		global = false,
		retryAfter,
		retryAfterDecimal,
		limit,
		resetTime,
		resetAfterDecimal,
		bucketHash,
		scope,
	}: {
		code?: string;
		message?: string;
		global?: boolean;
		retryAfter: number | undefined;
		retryAfterDecimal?: number;
		limit: number;
		resetTime: Date;
		resetAfterDecimal?: number;
		bucketHash?: string;
		scope?: RateLimitScope;
	}) {
		const safeRetryAfter = sanitizeRetryAfterSeconds(retryAfter);
		const safeRetryAfterDecimal = sanitizeRetryAfterDecimalSeconds(retryAfterDecimal, safeRetryAfter);
		const safeResetTimestamp = sanitizeResetTime(resetTime);
		const safeLimit = Number.isFinite(limit) && limit > 0 ? limit : 1;
		const safeResetAfterDecimal =
			resetAfterDecimal == null || !Number.isFinite(resetAfterDecimal) || resetAfterDecimal < 0
				? safeRetryAfterDecimal
				: resetAfterDecimal;
		const safeScope = sanitizeRateLimitScope(scope, global);
		const data: FluxerErrorData = {
			global,
			retry_after: safeRetryAfterDecimal,
		};
		const headers: Record<string, string> = {
			'Retry-After': safeRetryAfter.toString(),
			'X-RateLimit-Scope': safeScope,
		};
		if (global) {
			headers['X-RateLimit-Global'] = 'true';
		} else {
			headers['X-RateLimit-Limit'] = safeLimit.toString();
			headers['X-RateLimit-Remaining'] = '0';
			headers['X-RateLimit-Reset'] = safeResetTimestamp.toString();
			headers['X-RateLimit-Reset-After'] = safeResetAfterDecimal.toString();
			if (bucketHash) {
				headers['X-RateLimit-Bucket'] = bucketHash;
			}
		}
		super({code, message, data, headers});
	}
}
