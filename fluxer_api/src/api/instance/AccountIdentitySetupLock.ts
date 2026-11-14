// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResourceLockedError} from '@fluxer/errors/src/domains/core/ResourceLockedError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

const SETUP_LOCK_KEY = 'account-identity-setup';
const SETUP_LOCK_TTL_SECONDS = 30;
const SETUP_LOCK_WAIT_MS = 15_000;
const SETUP_LOCK_RETRY_MS = 50;

export async function withAccountIdentitySetupLock<T>(cache: ICacheService, run: () => Promise<T>): Promise<T> {
	const deadline = Date.now() + SETUP_LOCK_WAIT_MS;
	let token: string | null = null;
	while (token === null) {
		token = await cache.acquireLock(SETUP_LOCK_KEY, SETUP_LOCK_TTL_SECONDS);
		if (token !== null) break;
		if (Date.now() >= deadline) throw new ResourceLockedError();
		await new Promise((resolve) => setTimeout(resolve, SETUP_LOCK_RETRY_MS));
	}
	try {
		return await run();
	} finally {
		await cache.releaseLock(SETUP_LOCK_KEY, token);
	}
}
