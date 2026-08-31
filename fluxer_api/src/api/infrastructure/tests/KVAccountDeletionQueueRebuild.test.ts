// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {describe, expect, it} from 'vitest';
import {createUserID} from '../../BrandedTypes';
import {EMPTY_USER_ROW, type UserRow} from '../../database/types/UserTypes';
import {User} from '../../models/User';
import {MockKVProvider} from '../../test/mocks/MockKVProvider';
import type {UserRepository} from '../../user/repositories/UserRepository';
import {KVAccountDeletionQueueService} from '../KVAccountDeletionQueueService';

function createUser(id: bigint, overrides: Partial<Pick<UserRow, 'bot' | 'flags'>> = {}): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: createUserID(id),
		pending_deletion_at: new Date('2026-06-01T00:00:00.000Z'),
		deletion_reason_code: 0,
		bot: overrides.bot ?? false,
		flags: overrides.flags ?? 0n,
	});
}

function createUserRepository(users: Array<User>): UserRepository {
	let served = false;
	return {
		async scanAllUsersPage() {
			if (served) {
				return {users: [], pageState: null};
			}
			served = true;
			return {users, pageState: null};
		},
	} as unknown as UserRepository;
}

describe('KVAccountDeletionQueueService rebuild', () => {
	it('does not requeue accounts the deletion worker refuses to process', async () => {
		const kvClient = new MockKVProvider();
		const users = [createUser(1n, {bot: true}), createUser(2n, {flags: UserFlags.APP_STORE_REVIEWER}), createUser(3n)];
		const service = new KVAccountDeletionQueueService(kvClient, createUserRepository(users));

		await service.rebuildState();

		expect(await service.getQueueSize()).toBe(1);
		expect(await service.getReadyDeletions(Date.now(), 100)).toEqual([{userId: 3n, deletionReasonCode: 0}]);
	});
});
