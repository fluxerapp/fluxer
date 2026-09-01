// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVPipeline} from '@pkgs/kv_client/src/IKVProvider';
import {afterEach, describe, expect, it, vi} from 'vitest';
import {createUserID} from '../../BrandedTypes';
import type {User} from '../../models/User';
import {BatchRecordingKVProvider} from '../../test/mocks/BatchRecordingKVProvider';
import {UserRepository} from '../../user/repositories/UserRepository';
import {KVActivityTracker} from '../KVActivityTracker';

const REBUILD_KV_BATCH_SIZE = 1000;
const MAX_CONCURRENT_ROUND_TRIPS = 16;

class FanoutRecordingKVProvider extends BatchRecordingKVProvider {
	inFlight = 0;
	peakInFlight = 0;
	failingKey: string | null = null;

	private insidePipeline = 0;

	override pipeline(): IKVPipeline {
		const inner = super.pipeline();
		return {
			...inner,
			exec: async () =>
				await this.trackRoundTrip(async () => {
					this.insidePipeline += 1;
					try {
						return await inner.exec();
					} finally {
						this.insidePipeline -= 1;
					}
				}),
		};
	}

	override async setex(key: string, ttlSeconds: number, value: string): Promise<void> {
		if (key === this.failingKey) {
			throw new Error('write rejected');
		}
		if (this.insidePipeline > 0) {
			await super.setex(key, ttlSeconds, value);
			return;
		}
		await this.trackRoundTrip(async () => {
			await super.setex(key, ttlSeconds, value);
		});
	}

	private async trackRoundTrip<T>(run: () => Promise<T>): Promise<T> {
		this.inFlight += 1;
		this.peakInFlight = Math.max(this.peakInFlight, this.inFlight);
		try {
			await new Promise((resolve) => setTimeout(resolve, 0));
			return await run();
		} finally {
			this.inFlight -= 1;
		}
	}
}

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

		expect(kvClient.batches.map((batch) => batch.keys.length)).toEqual([REBUILD_KV_BATCH_SIZE]);
		expect(kvClient.peakInFlight).toBe(1);
	});

	it('bounds concurrent round trips when a rebuild batch spans hash slots', async () => {
		const kvClient = new FanoutRecordingKVProvider();
		const users = mockUserPage(REBUILD_KV_BATCH_SIZE);

		await new KVActivityTracker(kvClient).rebuildActivities();

		expect(kvClient.peakInFlight).toBeLessThanOrEqual(MAX_CONCURRENT_ROUND_TRIPS);
		expect(kvClient.crossSlotBatches()).toEqual([]);
		expect(kvClient.batches.flatMap((batch) => batch.keys).length).toBe(REBUILD_KV_BATCH_SIZE);
		expect(await kvClient.get(`user_activity:${users[0].id}`)).toBe(users[0].lastActiveAt?.getTime().toString());
	});

	it('fails the rebuild when a batched write fails', async () => {
		const kvClient = new FanoutRecordingKVProvider();
		const users = mockUserPage(REBUILD_KV_BATCH_SIZE);
		kvClient.failingKey = `user_activity:${users[0].id}`;

		await expect(new KVActivityTracker(kvClient).rebuildActivities()).rejects.toThrow('write rejected');
	});
});
