// SPDX-License-Identifier: AGPL-3.0-or-later

import {ms} from 'itty-time';
import {afterEach, describe, expect, it, vi} from 'vitest';
import {createUserID} from '../../BrandedTypes';
import {EMPTY_USER_ROW} from '../../database/types/UserTypes';
import {User} from '../../models/User';
import {MockKVProvider} from '../../test/mocks/MockKVProvider';
import type {UserRepository} from '../../user/repositories/UserRepository';
import {KVAccountDeletionQueueService} from '../KVAccountDeletionQueueService';

const PAGE_DURATION_MS = ms('3 minutes');
const PAGE_COUNT = 3;

function createPendingUser(index: number): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: createUserID(BigInt(7000 + index)),
		pending_deletion_at: new Date('2026-06-01T00:00:00.000Z'),
		deletion_reason_code: 0,
		bot: false,
		flags: 0n,
	});
}

function createSlowUserRepository(): UserRepository {
	let page = 0;
	return {
		async scanAllUsersPage() {
			vi.advanceTimersByTime(PAGE_DURATION_MS);
			page += 1;
			return {
				users: [createPendingUser(page)],
				pageState: page < PAGE_COUNT ? `page-${page}` : null,
			};
		},
	} as unknown as UserRepository;
}

describe('KVAccountDeletionQueueService rebuild lock', () => {
	afterEach(() => {
		vi.useRealTimers();
	});

	it('keeps holding the rebuild lock across a scan longer than the lock ttl', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-06-01T00:00:00.000Z'));
		const kvClient = new MockKVProvider();
		const service = new KVAccountDeletionQueueService(kvClient, createSlowUserRepository());
		const token = await service.acquireRebuildLock();
		expect(token).not.toBeNull();

		await service.rebuildState(token);

		expect(await service.acquireRebuildLock()).toBeNull();
		expect(await service.releaseRebuildLock(token!)).toBe(true);
	});
});
