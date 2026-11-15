// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ThrottledError} from '@fluxer/errors/src/domains/core/ThrottledError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

const LOCK_TTL_SECONDS = 5;
const LOCK_ACQUIRE_ATTEMPTS = 6;
const LOCK_RETRY_DELAY_MS = 50;

export async function withRetryingLock<T>(
	cacheService: ICacheService,
	lockKey: string,
	fn: () => Promise<T>,
	keepAliveTtlSeconds?: number,
): Promise<T> {
	const ttlSeconds = keepAliveTtlSeconds ?? LOCK_TTL_SECONDS;
	let lockToken: string | null = null;
	for (let attempt = 0; attempt < LOCK_ACQUIRE_ATTEMPTS; attempt++) {
		lockToken = await cacheService.acquireLock(lockKey, ttlSeconds);
		if (lockToken) break;
		await new Promise((resolve) => setTimeout(resolve, LOCK_RETRY_DELAY_MS * (attempt + 1)));
	}
	if (!lockToken) {
		throw new ThrottledError({
			code: APIErrorCodes.RESOURCE_LOCKED,
			retryAfterSeconds: 1,
			data: {retry_after: 1},
		});
	}
	const token = lockToken;
	const keepAlive =
		keepAliveTtlSeconds === undefined
			? null
			: setInterval(
					() => {
						cacheService.extendLock(lockKey, token, keepAliveTtlSeconds).catch(() => {});
					},
					(keepAliveTtlSeconds * 1000) / 3,
				);
	try {
		return await fn();
	} finally {
		if (keepAlive) clearInterval(keepAlive);
		await cacheService.releaseLock(lockKey, token).catch(() => {});
	}
}
