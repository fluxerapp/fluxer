// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {EMPTY_USER_ROW, type UserRow} from '@app/api/database/types/UserTypes';
import {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {describe, expect, it} from 'vitest';

function userRow(id: bigint, overrides: Partial<Pick<UserRow, 'bot' | 'flags'>> = {}): UserRow {
	return {
		...EMPTY_USER_ROW,
		user_id: createUserID(id),
		bot: overrides.bot ?? false,
		flags: overrides.flags ?? 0n,
	};
}

describe('KVAccountDeletionQueueService rebuild', () => {
	it('does not requeue accounts the deletion worker refuses to process', async () => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
		const kvClient = new MockKVProvider();
		const userRepository = new UserRepository(kvClient);
		for (const row of [
			userRow(1400000000000000001n, {bot: true}),
			userRow(1400000000000000002n, {flags: UserFlags.APP_STORE_REVIEWER}),
			userRow(1400000000000000003n),
		]) {
			const user = await userRepository.create(row);
			await userRepository.updateDeletionSchedule(user, {
				pending_deletion_at: new Date('2026-06-01T00:00:00.000Z'),
				deletion_reason_code: 0,
			});
		}
		const service = new KVAccountDeletionQueueService(kvClient, userRepository);

		await service.rebuildState();

		expect(await service.getQueueSize()).toBe(1);
		expect(await service.getReadyDeletions(Date.now(), 100)).toEqual([
			{userId: 1400000000000000003n, deletionReasonCode: 0},
		]);
	});
});
