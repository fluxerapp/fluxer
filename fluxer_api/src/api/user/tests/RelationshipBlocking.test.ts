// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createDmChannel, createFriendship} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	acceptFriendRequest,
	assertRelationshipId,
	assertRelationshipType,
	blockUser,
	listRelationships,
	removeRelationship,
	sendFriendRequest,
} from '@app/api/user/tests/RelationshipTestUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('RelationshipBlocking', () => {
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
	test('blocking a user removes existing friendship', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await createFriendship(harness, alice, bob);
		const {json: aliceRelsBeforeBlock} = await listRelationships(harness, alice.token);
		expect(aliceRelsBeforeBlock).toHaveLength(1);
		assertRelationshipType(aliceRelsBeforeBlock[0]!, RelationshipTypes.FRIEND);
		const {json: blocked} = await blockUser(harness, alice.token, bob.userId);
		assertRelationshipId(blocked, bob.userId);
		assertRelationshipType(blocked, RelationshipTypes.BLOCKED);
		const {json: aliceRelsAfterBlock} = await listRelationships(harness, alice.token);
		expect(aliceRelsAfterBlock).toHaveLength(1);
		assertRelationshipType(aliceRelsAfterBlock[0]!, RelationshipTypes.BLOCKED);
		const {json: bobRelsAfterBlock} = await listRelationships(harness, bob.token);
		expect(bobRelsAfterBlock).toHaveLength(0);
	});
	test('blocked user cannot send friend request', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await blockUser(harness, alice.token, bob.userId);
		await createBuilder(harness, bob.token)
			.post(`/users/@me/relationships/${alice.userId}`)
			.expect(HTTP_STATUS.BAD_REQUEST, 'FRIEND_REQUEST_BLOCKED')
			.execute();
	});
	test('blocking ignores incoming friend requests without notifying sender', async () => {
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
		assertRelationshipType(aliceRels[0]!, RelationshipTypes.OUTGOING_REQUEST);
		assertRelationshipId(aliceRels[0]!, bob.userId);
	});
	test('accept friend request after privacy setting change allows existing request', async () => {
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
	test('unblock user works correctly', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		const {json: blocked} = await blockUser(harness, alice.token, bob.userId);
		assertRelationshipType(blocked, RelationshipTypes.BLOCKED);
		const {json: aliceRelsBeforeUnblock} = await listRelationships(harness, alice.token);
		expect(aliceRelsBeforeUnblock).toHaveLength(1);
		assertRelationshipType(aliceRelsBeforeUnblock[0]!, RelationshipTypes.BLOCKED);
		await removeRelationship(harness, alice.token, bob.userId);
		const {json: aliceRelsAfterUnblock} = await listRelationships(harness, alice.token);
		expect(aliceRelsAfterUnblock).toHaveLength(0);
		const {json: outgoing} = await sendFriendRequest(harness, bob.token, alice.userId);
		assertRelationshipType(outgoing, RelationshipTypes.OUTGOING_REQUEST);
	});
	test('cannot send DM to blocked user', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await ensureSessionStarted(harness, alice.token);
		await createFriendship(harness, alice, bob);
		const channel = await createDmChannel(harness, alice.token, bob.userId);
		await blockUser(harness, alice.token, bob.userId);
		await createBuilder(harness, alice.token)
			.post(`/channels/${channel.id}/messages`)
			.body({content: 'hello'})
			.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_MESSAGES_TO_USER')
			.execute();
	});
});
