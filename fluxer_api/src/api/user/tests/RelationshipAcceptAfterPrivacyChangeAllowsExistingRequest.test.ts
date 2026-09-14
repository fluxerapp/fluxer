// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	acceptFriendRequest,
	assertRelationshipId,
	assertRelationshipType,
	listRelationships,
	sendFriendRequest,
} from '@app/api/user/tests/RelationshipTestUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {beforeEach, describe, expect, test} from 'vitest';

describe('RelationshipAcceptAfterPrivacyChangeAllowsExistingRequest', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('accepting friend request after privacy change allows existing request', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await sendFriendRequest(harness, alice.token, bob.userId);
		await createBuilder(harness, bob.token)
			.patch('/users/@me/settings')
			.body({friend_source_flags: 3})
			.expect(HTTP_STATUS.OK)
			.execute();
		const {json: accepted} = await acceptFriendRequest(harness, bob.token, alice.userId);
		assertRelationshipId(accepted, alice.userId);
		assertRelationshipType(accepted, RelationshipTypes.FRIEND);
		const {json: aliceRels} = await listRelationships(harness, alice.token);
		expect(aliceRels).toHaveLength(1);
		assertRelationshipId(aliceRels[0]!, bob.userId);
		assertRelationshipType(aliceRels[0]!, RelationshipTypes.FRIEND);
	});
});
