// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID} from '@app/api/BrandedTypes';
import {ResourceLockedError} from '@fluxer/errors/src/domains/core/ResourceLockedError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

const GUILD_UPDATE_LOCK_TTL_SECONDS = 10;
const GUILD_UPDATE_LOCK_RETRY_DELAY_MS = 50;
const GUILD_UPDATE_LOCK_MAX_WAIT_MS = 5000;

export type GuildUpdateLocks = Pick<ICacheService, 'acquireLock' | 'releaseLock'>;

async function acquireGuildUpdateLock(locks: GuildUpdateLocks, lockKey: string): Promise<string | null> {
	const startTime = Date.now();
	while (Date.now() - startTime < GUILD_UPDATE_LOCK_MAX_WAIT_MS) {
		const token = await locks.acquireLock(lockKey, GUILD_UPDATE_LOCK_TTL_SECONDS);
		if (token) {
			return token;
		}
		await new Promise((resolve) => setTimeout(resolve, GUILD_UPDATE_LOCK_RETRY_DELAY_MS));
	}
	return null;
}

export async function withGuildUpdateLock<T>(
	locks: GuildUpdateLocks,
	guildId: GuildID,
	fn: () => Promise<T>,
): Promise<T> {
	const lockKey = `guild:${guildId}:update`;
	const lockToken = await acquireGuildUpdateLock(locks, lockKey);
	if (!lockToken) {
		throw new ResourceLockedError();
	}
	try {
		return await fn();
	} finally {
		await locks.releaseLock(lockKey, lockToken);
	}
}
