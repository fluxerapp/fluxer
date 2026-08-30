// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserProfileFullResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createFriendship} from '../../channel/tests/ChannelTestUtils';
import {UserCacheService} from '../../infrastructure/UserCacheService';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';

describe('User Profile Cache Invalidation', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	it('resolves the profile subject and mutual friends without invalidating the users service cache', async () => {
		const viewerAccount = await createTestAccount(harness);
		const targetAccount = await createTestAccount(harness);
		const mutualAccount = await createTestAccount(harness);
		await createFriendship(harness, viewerAccount, targetAccount);
		await createFriendship(harness, viewerAccount, mutualAccount);
		await createFriendship(harness, targetAccount, mutualAccount);
		const invalidateUserCache = vi.spyOn(UserCacheService.prototype, 'invalidateUserCache');

		try {
			const profile = await createBuilder<UserProfileFullResponse>(harness, viewerAccount.token)
				.get(`/users/${targetAccount.userId}/profile?with_mutual_friends=true`)
				.execute();

			expect(profile.user.id).toBe(targetAccount.userId);
			expect(profile.mutual_friends?.map((friend) => friend.id)).toEqual([mutualAccount.userId]);
			expect(invalidateUserCache).not.toHaveBeenCalled();
		} finally {
			invalidateUserCache.mockRestore();
		}
	});
});
