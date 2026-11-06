// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {ResourceLockedError} from '@fluxer/errors/src/domains/core/ResourceLockedError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

const MEMBER_ROLE_WRITE_LOCK_TTL_SECONDS = 10;
const MEMBER_ROLE_WRITE_LOCK_RETRY_DELAY_MS = 25;
const MEMBER_ROLE_WRITE_LOCK_MAX_WAIT_MS = 5000;

export type GuildMemberRoleWriteLocks = Pick<ICacheService, 'acquireLock' | 'releaseLock'>;

export async function withGuildMemberRoleWriteLock<T>(
	locks: GuildMemberRoleWriteLocks,
	guildId: GuildID,
	userId: UserID,
	fn: () => Promise<T>,
): Promise<T> {
	const lockKey = `guild:${guildId}:member:${userId}:roles`;
	const startTime = Date.now();
	let lockToken: string | null = null;
	while (lockToken === null && Date.now() - startTime < MEMBER_ROLE_WRITE_LOCK_MAX_WAIT_MS) {
		lockToken = await locks.acquireLock(lockKey, MEMBER_ROLE_WRITE_LOCK_TTL_SECONDS);
		if (lockToken === null) {
			await new Promise((resolve) => setTimeout(resolve, MEMBER_ROLE_WRITE_LOCK_RETRY_DELAY_MS));
		}
	}
	if (lockToken === null) {
		throw new ResourceLockedError();
	}
	try {
		return await fn();
	} finally {
		await locks.releaseLock(lockKey, lockToken).catch(() => {});
	}
}
