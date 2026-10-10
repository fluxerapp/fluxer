// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createFriendship, createGroupDmChannel} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {profileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, afterEach, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Group DM nickname update', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterEach(() => {
		profileSubstringBlocklistCache.remove('nickname', 'blockedslug');
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('non-owner cannot update another users nickname', async () => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		const user3 = await createTestAccount(harness);
		await ensureSessionStarted(harness, user1.token);
		await ensureSessionStarted(harness, user2.token);
		await ensureSessionStarted(harness, user3.token);
		await createFriendship(harness, user1, user2);
		await createFriendship(harness, user1, user3);
		const groupDm = await createGroupDmChannel(harness, user1.token, [user2.userId, user3.userId]);
		await createBuilder(harness, user2.token)
			.patch(`/channels/${groupDm.id}`)
			.body({
				nicks: {
					[user3.userId]: 'User 3 Nick by User 2',
				},
			})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
