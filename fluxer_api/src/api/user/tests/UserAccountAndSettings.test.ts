// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {sendFriendRequest} from '@app/api/user/tests/RelationshipTestUtils';
import {fetchUserProfile} from '@app/api/user/tests/UserTestUtils';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('User Account And Settings', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('pending outgoing friend request allows viewing target profile', async () => {
		const requester = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		await createBuilder(harness, requester.token)
			.get(`/users/${target.userId}/profile`)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACCESS')
			.execute();
		await sendFriendRequest(harness, requester.token, target.userId);
		const profile = await fetchUserProfile(harness, target.userId, requester.token);
		expect(profile.json.user.id).toBe(target.userId);
	});
});
