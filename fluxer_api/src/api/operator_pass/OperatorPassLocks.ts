// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {ms, seconds} from 'itty-time';

const LOCK_TTL_SECONDS = seconds('2 minutes');
const LOCK_RENEW_MS = ms('30 seconds');

export interface OperatorPassLockLease {
	assertHeld(): Promise<void>;
}

export function operatorPassPurchaseLockKey(checkoutSessionId: string): string {
	return `operator_pass:purchase_lock:${checkoutSessionId}`;
}

export function operatorPassHolderLockKey(userId: bigint): string {
	return `operator_pass:holder_lock:${userId}`;
}

export function operatorPassReleaseLockKey(year: number): string {
	return `operator_pass:release_lock:${year}`;
}

export async function withOperatorPassLock<T>(
	cacheService: ICacheService,
	lockKey: string,
	action: (lease: OperatorPassLockLease) => Promise<T>,
	busyError: () => Error,
): Promise<T> {
	const token = await cacheService.acquireLock(lockKey, LOCK_TTL_SECONDS);
	if (!token) {
		throw busyError();
	}
	let lost = false;
	const renew = async (): Promise<boolean> => {
		if (lost) return false;
		try {
			lost = !(await cacheService.extendLock(lockKey, token, LOCK_TTL_SECONDS));
		} catch {
			lost = true;
		}
		return !lost;
	};
	const timer = setInterval(() => void renew(), LOCK_RENEW_MS);
	timer.unref?.();
	const lease: OperatorPassLockLease = {
		assertHeld: async () => {
			if (!(await renew())) {
				throw busyError();
			}
		},
	};
	try {
		return await action(lease);
	} finally {
		clearInterval(timer);
		await cacheService.releaseLock(lockKey, token);
	}
}
