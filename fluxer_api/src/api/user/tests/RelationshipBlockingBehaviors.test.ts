// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	assertRelationshipId,
	assertRelationshipType,
	blockUser,
	listRelationships,
	removeRelationship,
	sendFriendRequest,
} from '@app/api/user/tests/RelationshipTestUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {beforeEach, describe, expect, test} from 'vitest';

describe('RelationshipBlockingBehaviors', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	describe('Friend Request Blocking', () => {
		test('friend request to blocker is generic blocked error', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await blockUser(harness, bob.token, alice.userId);
			await createBuilder(harness, alice.token)
				.post(`/users/@me/relationships/${bob.userId}`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'FRIEND_REQUEST_BLOCKED')
				.execute();
		});
	});
	describe('Notification Behavior', () => {
		test('blocking ignores incoming request without notifying sender', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			const {json: outgoing} = await sendFriendRequest(harness, alice.token, bob.userId);
			assertRelationshipId(outgoing, bob.userId);
			assertRelationshipType(outgoing, RelationshipTypes.OUTGOING_REQUEST);
			const {json: blocked} = await blockUser(harness, bob.token, alice.userId);
			assertRelationshipId(blocked, alice.userId);
			assertRelationshipType(blocked, RelationshipTypes.BLOCKED);
			const {json: aliceRels} = await listRelationships(harness, alice.token);
			expect(aliceRels).toHaveLength(1);
			expect(aliceRels[0]!.type).toBe(RelationshipTypes.OUTGOING_REQUEST);
			assertRelationshipId(aliceRels[0]!, bob.userId);
		});
		test('ignoring incoming request does not notify sender', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			const {json: outgoing} = await sendFriendRequest(harness, alice.token, bob.userId);
			assertRelationshipId(outgoing, bob.userId);
			assertRelationshipType(outgoing, RelationshipTypes.OUTGOING_REQUEST);
			await removeRelationship(harness, bob.token, alice.userId);
			const {json: aliceRels} = await listRelationships(harness, alice.token);
			expect(aliceRels).toHaveLength(1);
			expect(aliceRels[0]!.type).toBe(RelationshipTypes.OUTGOING_REQUEST);
			assertRelationshipId(aliceRels[0]!, bob.userId);
		});
	});
	describe('Outgoing Request Handling', () => {
		test('blocking with outgoing request withdraws the request and notifies target', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			const {json: outgoing} = await sendFriendRequest(harness, alice.token, bob.userId);
			assertRelationshipId(outgoing, bob.userId);
			assertRelationshipType(outgoing, RelationshipTypes.OUTGOING_REQUEST);
			const {json: blocked} = await blockUser(harness, alice.token, bob.userId);
			assertRelationshipId(blocked, bob.userId);
			assertRelationshipType(blocked, RelationshipTypes.BLOCKED);
			const {json: bobRels} = await listRelationships(harness, bob.token);
			expect(bobRels).toHaveLength(0);
		});
	});
});
