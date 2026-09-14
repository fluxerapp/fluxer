import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import type {RateLimitResult} from '@pkgs/rate_limit/src/IRateLimitService';

export function getRetryAfterSeconds(result: RateLimitResult): number {
	return result.retryAfter ?? Math.max(0, Math.ceil((result.resetTime.getTime() - Date.now()) / 1000));
}

export function createRateLimitError(result: RateLimitResult): RateLimitError {
	return new RateLimitError({
		retryAfter: getRetryAfterSeconds(result),
		limit: result.limit,
		resetTime: result.resetTime,
	});
}
