// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {KVActivityTracker} from '@app/api/infrastructure/KVActivityTracker';
import type {User} from '@app/api/models/User';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {FanoutRecordingKVProvider} from '@pkgs/kv_client/src/testing/FanoutRecordingKVProvider';
import {afterEach, describe, expect, it, vi} from 'vitest';

const REBUILD_KV_BATCH_SIZE = 1000;
const MAX_CONCURRENT_ROUND_TRIPS = 16;

function mockUserPage(count: number): Array<User> {
	const lastActiveAt = new Date('2026-06-01T00:00:00.000Z');
	const users = Array.from(
		{length: count},
		(_unused, index) => ({id: createUserID(BigInt(index + 1)), lastActiveAt}) as unknown as User,
	);
	vi.spyOn(UserRepository.prototype, 'scanAllUsersPage').mockResolvedValue({users, pageState: null});
	return users;
}

describe('KVActivityTracker rebuild fan out', () => {
	afterEach(() => {
		vi.restoreAllMocks();
	});

	it('writes a full rebuild batch in one round trip outside cluster mode', async () => {
		const kvClient = new FanoutRecordingKVProvider();
		kvClient.clustered = false;
		mockUserPage(REBUILD_KV_BATCH_SIZE);

		await new KVActivityTracker(kvClient).rebuildActivities();

		expect(kvClient.batches.map((batch) => batch.keys.length)).toEqual([REBUILD_KV_BATCH_SIZE, 1]);
		expect(kvClient.batches[1]?.keys).toEqual(['activity_tracker:state_version']);
		expect(kvClient.peakInFlight).toBe(1);
	});

	it('bounds concurrent round trips when a rebuild batch spans hash slots', async () => {
		const kvClient = new FanoutRecordingKVProvider();
		const users = mockUserPage(REBUILD_KV_BATCH_SIZE);

		await new KVActivityTracker(kvClient).rebuildActivities();

		expect(kvClient.peakInFlight).toBeLessThanOrEqual(MAX_CONCURRENT_ROUND_TRIPS);
		expect(kvClient.crossSlotBatches()).toEqual([]);
		expect(
			kvClient.batches.flatMap((batch) => batch.keys).filter((key) => key.startsWith('user_activity:')).length,
		).toBe(REBUILD_KV_BATCH_SIZE);
		expect(kvClient.batches.at(-1)?.keys).toEqual(['activity_tracker:state_version']);
		expect(await kvClient.get(`user_activity:${users[0].id}`)).toBe(users[0].lastActiveAt?.getTime().toString());
	});

	it('fails the rebuild when a batched write fails', async () => {
		const kvClient = new FanoutRecordingKVProvider();
		const users = mockUserPage(REBUILD_KV_BATCH_SIZE);
		kvClient.failingKey = `user_activity:${users[0].id}`;

		await expect(new KVActivityTracker(kvClient).rebuildActivities()).rejects.toThrow('write rejected');
		expect(await kvClient.get('activity_tracker:state_version')).toBeNull();
	});
});
