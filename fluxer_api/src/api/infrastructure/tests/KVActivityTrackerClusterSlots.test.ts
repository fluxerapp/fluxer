// SPDX-License-Identifier: AGPL-3.0-or-later

import {computeHashSlot} from '@pkgs/kv_client/src/KVHashSlots';
import {afterEach, describe, expect, it, vi} from 'vitest';
import {createUserID} from '../../BrandedTypes';
import type {User} from '../../models/User';
import {BatchRecordingKVProvider} from '../../test/mocks/BatchRecordingKVProvider';
import {UserRepository} from '../../user/repositories/UserRepository';
import {KVActivityTracker} from '../KVActivityTracker';

function createUser(id: bigint, lastActiveAt: Date): User {
	return {id: createUserID(id), lastActiveAt} as unknown as User;
}

describe('KVActivityTracker cluster hash slots', () => {
	afterEach(() => {
		vi.restoreAllMocks();
	});

	it('keeps the rebuild writes off batched commands that span hash slots', async () => {
		const kvClient = new BatchRecordingKVProvider();
		const lastActiveAt = new Date('2026-06-01T00:00:00.000Z');
		const users = [createUser(1234n, lastActiveAt), createUser(5678n, lastActiveAt)];
		vi.spyOn(UserRepository.prototype, 'scanAllUsersPage').mockResolvedValue({users, pageState: null});

		expect(computeHashSlot('user_activity:1234')).not.toBe(computeHashSlot('user_activity:5678'));

		await new KVActivityTracker(kvClient).rebuildActivities();

		expect(await kvClient.get('user_activity:1234')).toBe(lastActiveAt.getTime().toString());
		expect(await kvClient.get('user_activity:5678')).toBe(lastActiveAt.getTime().toString());
		expect(kvClient.crossSlotBatches()).toEqual([]);
	});
});
