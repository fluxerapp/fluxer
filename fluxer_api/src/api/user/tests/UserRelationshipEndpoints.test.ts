// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
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
import {updateUserSettings} from '@app/api/user/tests/UserTestUtils';
import {RelationshipTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {beforeEach, describe, expect, test} from 'vitest';

async function setUserFlags(harness: ApiTestHarness, userId: string, flags: bigint): Promise<void> {
	await createBuilder(harness, '')
		.patch(`/test/users/${userId}/flags`)
		.body({flags: flags.toString()})
		.expect(200)
		.execute();
}

describe('UserRelationshipEndpoints', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('friend request lifecycle', async () => {
		const requester = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const {json: outgoing} = await sendFriendRequest(harness, requester.token, target.userId);
		assertRelationshipId(outgoing, target.userId);
		assertRelationshipType(outgoing, RelationshipTypes.OUTGOING_REQUEST);
		const {json: targetRels} = await listRelationships(harness, target.token);
		expect(targetRels).toHaveLength(1);
		expect(targetRels[0]!.type).toBe(RelationshipTypes.INCOMING_REQUEST);
		assertRelationshipId(targetRels[0]!, requester.userId);
		const {json: accepted} = await acceptFriendRequest(harness, target.token, requester.userId);
		assertRelationshipId(accepted, requester.userId);
		assertRelationshipType(accepted, RelationshipTypes.FRIEND);
		const {json: requesterRels} = await listRelationships(harness, requester.token);
		expect(requesterRels).toHaveLength(1);
		assertRelationshipType(requesterRels[0]!, RelationshipTypes.FRIEND);
		assertRelationshipId(requesterRels[0]!, target.userId);
		await removeRelationship(harness, requester.token, target.userId);
		const {json: afterDelete} = await listRelationships(harness, target.token);
		expect(afterDelete).toHaveLength(0);
		const {json: blocked} = await blockUser(harness, target.token, requester.userId);
		assertRelationshipType(blocked, RelationshipTypes.BLOCKED);
		assertRelationshipId(blocked, requester.userId);
	});
	test('staff force accept immediately creates friendship despite target privacy settings', async () => {
		const requester = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		await setUserFlags(harness, requester.userId, UserFlags.STAFF);
		await updateUserSettings(harness, target.token, {friend_source_flags: 0});
		const {json: friendship} = await sendFriendRequest(harness, requester.token, target.userId, {
			staffForceAccept: true,
		});
		assertRelationshipType(friendship, RelationshipTypes.FRIEND);
		assertRelationshipId(friendship, target.userId);
		const {json: requesterRelationships} = await listRelationships(harness, requester.token);
		const {json: targetRelationships} = await listRelationships(harness, target.token);
		expect(requesterRelationships).toHaveLength(1);
		expect(targetRelationships).toHaveLength(1);
		assertRelationshipType(requesterRelationships[0]!, RelationshipTypes.FRIEND);
		assertRelationshipType(targetRelationships[0]!, RelationshipTypes.FRIEND);
		assertRelationshipId(requesterRelationships[0]!, target.userId);
		assertRelationshipId(targetRelationships[0]!, requester.userId);
	});
});
