// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, unclaimAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	acceptFriendRequest,
	assertRelationshipId,
	assertRelationshipType,
	blockUser,
	findRelationship,
	listRelationships,
	removeRelationship,
	sendFriendRequest,
	sendFriendRequestByTag,
} from '@app/api/user/tests/RelationshipTestUtils';
import {fetchUserMe} from '@app/api/user/tests/UserTestUtils';
import {RelationshipTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

async function markUserScheduledForDeletion(harness: ApiTestHarness, userId: string): Promise<void> {
	const pendingDeletionAt = new Date(Date.now() + 14 * 24 * 60 * 60 * 1000).toISOString();
	await createBuilder(harness, '')
		.post(`/test/users/${userId}/set-pending-deletion`)
		.body({pending_deletion_at: pendingDeletionAt, set_self_deleted_flag: false})
		.expect(HTTP_STATUS.OK)
		.execute();
	await markUserDeleted(harness, userId);
}

async function markUserDeleted(harness: ApiTestHarness, userId: string): Promise<void> {
	await createBuilder(harness, '')
		.patch(`/test/users/${userId}/flags`)
		.body({flags: UserFlags.DELETED.toString()})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('UserRelationshipStateTransitions', () => {
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
	describe('friend request flow', () => {
		test('complete friend request lifecycle', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			const {json: outgoing} = await sendFriendRequest(harness, alice.token, bob.userId);
			assertRelationshipType(outgoing, RelationshipTypes.OUTGOING_REQUEST);
			assertRelationshipId(outgoing, bob.userId);
			const {json: aliceRels} = await listRelationships(harness, alice.token);
			expect(aliceRels).toHaveLength(1);
			assertRelationshipType(aliceRels[0]!, RelationshipTypes.OUTGOING_REQUEST);
			const {json: bobRels} = await listRelationships(harness, bob.token);
			expect(bobRels).toHaveLength(1);
			assertRelationshipType(bobRels[0]!, RelationshipTypes.INCOMING_REQUEST);
			const {json: friendship} = await acceptFriendRequest(harness, bob.token, alice.userId);
			assertRelationshipType(friendship, RelationshipTypes.FRIEND);
			const {json: aliceAfter} = await listRelationships(harness, alice.token);
			expect(aliceAfter).toHaveLength(1);
			assertRelationshipType(aliceAfter[0]!, RelationshipTypes.FRIEND);
			const {json: bobAfter} = await listRelationships(harness, bob.token);
			expect(bobAfter).toHaveLength(1);
			assertRelationshipType(bobAfter[0]!, RelationshipTypes.FRIEND);
		});
		test('withdrawing outgoing friend request', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await sendFriendRequest(harness, alice.token, bob.userId);
			await removeRelationship(harness, alice.token, bob.userId);
			const {json: aliceRels} = await listRelationships(harness, alice.token);
			expect(aliceRels).toHaveLength(0);
			const {json: bobRels} = await listRelationships(harness, bob.token);
			expect(bobRels).toHaveLength(0);
		});
		test('rejecting incoming friend request silently ignores without notifying sender', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await sendFriendRequest(harness, alice.token, bob.userId);
			await removeRelationship(harness, bob.token, alice.userId);
			const {json: bobRels} = await listRelationships(harness, bob.token);
			expect(bobRels).toHaveLength(0);
			const {json: aliceRels} = await listRelationships(harness, alice.token);
			expect(aliceRels).toHaveLength(1);
			assertRelationshipType(aliceRels[0]!, RelationshipTypes.OUTGOING_REQUEST);
		});
		test('mutual friend request auto-accepts', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await sendFriendRequest(harness, alice.token, bob.userId);
			const {json: friendship} = await sendFriendRequest(harness, bob.token, alice.userId);
			assertRelationshipType(friendship, RelationshipTypes.FRIEND);
			const {json: aliceRels} = await listRelationships(harness, alice.token);
			expect(aliceRels).toHaveLength(1);
			assertRelationshipType(aliceRels[0]!, RelationshipTypes.FRIEND);
		});
		test('friend request by tag creates outgoing request', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			const {json: bobProfile} = await fetchUserMe(harness, bob.token);
			const {json: rel} = await sendFriendRequestByTag(
				harness,
				alice.token,
				bobProfile.username,
				bobProfile.discriminator,
			);
			assertRelationshipType(rel, RelationshipTypes.OUTGOING_REQUEST);
			expect(rel.user.id).toBe(bob.userId);
		});
	});
	describe('blocking transitions', () => {
		test('blocking with outgoing request withdraws and blocks', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await sendFriendRequest(harness, alice.token, bob.userId);
			const {json: blocked} = await blockUser(harness, alice.token, bob.userId);
			assertRelationshipType(blocked, RelationshipTypes.BLOCKED);
			const {json: bobRels} = await listRelationships(harness, bob.token);
			expect(bobRels).toHaveLength(0);
		});
	});
	describe('removing relationships', () => {
		test('removing friendship removes from both sides', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await sendFriendRequest(harness, alice.token, bob.userId);
			await acceptFriendRequest(harness, bob.token, alice.userId);
			await removeRelationship(harness, alice.token, bob.userId);
			const {json: aliceRels} = await listRelationships(harness, alice.token);
			expect(aliceRels).toHaveLength(0);
			const {json: bobRels} = await listRelationships(harness, bob.token);
			expect(bobRels).toHaveLength(0);
		});
	});
	describe('error paths', () => {
		test('cannot send friend request to yourself', async () => {
			const alice = await createTestAccount(harness);
			await createBuilder(harness, alice.token)
				.post(`/users/@me/relationships/${alice.userId}`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_FRIEND_REQUEST_TO_SELF')
				.execute();
		});
		test('cannot send friend request to deleted user', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await markUserDeleted(harness, bob.userId);
			await createBuilder(harness, alice.token)
				.post(`/users/@me/relationships/${bob.userId}`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'FRIEND_REQUEST_BLOCKED')
				.execute();
		});
		test('deleted user cannot send friend request', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await markUserDeleted(harness, alice.userId);
			await createBuilder(harness, alice.token)
				.post(`/users/@me/relationships/${bob.userId}`)
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
		test('can accept a friend request from a user scheduled for deletion', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await sendFriendRequest(harness, bob.token, alice.userId);
			await markUserScheduledForDeletion(harness, bob.userId);
			const {json: friendship} = await acceptFriendRequest(harness, alice.token, bob.userId);
			assertRelationshipType(friendship, RelationshipTypes.FRIEND);
			const {json: aliceAfter} = await listRelationships(harness, alice.token);
			assertRelationshipType(findRelationship(aliceAfter, bob.userId)!, RelationshipTypes.FRIEND);
		});
		test('cannot send friend request to user who blocked you', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await blockUser(harness, bob.token, alice.userId);
			await createBuilder(harness, alice.token)
				.post(`/users/@me/relationships/${bob.userId}`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'FRIEND_REQUEST_BLOCKED')
				.execute();
		});
		test('cannot send friend request to user you blocked', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await blockUser(harness, alice.token, bob.userId);
			await createBuilder(harness, alice.token)
				.post(`/users/@me/relationships/${bob.userId}`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_FRIEND_REQUEST_TO_BLOCKED_USER')
				.execute();
		});
		test('unclaimed account cannot send friend request', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await unclaimAccount(harness, alice.userId);
			await createBuilder(harness, alice.token)
				.post(`/users/@me/relationships/${bob.userId}`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'UNCLAIMED_ACCOUNT_CANNOT_SEND_FRIEND_REQUESTS')
				.execute();
		});
		test('unclaimed account cannot accept friend request', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await sendFriendRequest(harness, alice.token, bob.userId);
			await unclaimAccount(harness, bob.userId);
			await createBuilder(harness, bob.token)
				.put(`/users/@me/relationships/${alice.userId}`)
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST, 'UNCLAIMED_ACCOUNT_CANNOT_ACCEPT_FRIEND_REQUESTS')
				.execute();
		});
		test('cannot accept non-existent incoming request', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await createBuilder(harness, alice.token)
				.put(`/users/@me/relationships/${bob.userId}`)
				.body({})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_USER')
				.execute();
		});
		test('friend request by tag blocks deleted users', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			const {json: bobProfile} = await fetchUserMe(harness, bob.token);
			await markUserDeleted(harness, bob.userId);
			await createBuilder(harness, alice.token)
				.post('/users/@me/relationships')
				.body({
					username: bobProfile.username,
					discriminator: Number.parseInt(bobProfile.discriminator, 10),
				})
				.expect(HTTP_STATUS.BAD_REQUEST, 'FRIEND_REQUEST_BLOCKED')
				.execute();
		});
		test('cannot accept request when requester is deleted', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await sendFriendRequest(harness, alice.token, bob.userId);
			await markUserDeleted(harness, alice.userId);
			await createBuilder(harness, bob.token)
				.put(`/users/@me/relationships/${alice.userId}`)
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST, 'FRIEND_REQUEST_BLOCKED')
				.execute();
		});
		test('deleted user cannot accept incoming request', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await sendFriendRequest(harness, alice.token, bob.userId);
			await markUserDeleted(harness, bob.userId);
			await createBuilder(harness, bob.token)
				.put(`/users/@me/relationships/${alice.userId}`)
				.body({})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
	describe('already friends', () => {
		test('already friends error when using tag endpoint', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await sendFriendRequest(harness, alice.token, bob.userId);
			await acceptFriendRequest(harness, bob.token, alice.userId);
			const {json: bobProfile} = await fetchUserMe(harness, bob.token);
			await createBuilder(harness, alice.token)
				.post('/users/@me/relationships')
				.body({username: bobProfile.username, discriminator: parseInt(bobProfile.discriminator, 10)})
				.expect(HTTP_STATUS.BAD_REQUEST, 'ALREADY_FRIENDS')
				.execute();
		});
	});
});
