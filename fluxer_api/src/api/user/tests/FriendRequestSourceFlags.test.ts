// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	assertRelationshipId,
	assertRelationshipType,
	createFriendship,
	sendFriendRequest,
} from '@app/api/user/tests/RelationshipTestUtils';
import {FriendSourceFlags, RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('FriendRequestSourceFlags', () => {
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
	describe('NO_RELATION allows friend requests from strangers', () => {
		test('user with NO_RELATION flag can receive friend requests from users with no mutual friends or guilds', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await createBuilder(harness, bob.token)
				.patch('/users/@me/settings')
				.body({friend_source_flags: FriendSourceFlags.NO_RELATION})
				.expect(HTTP_STATUS.OK)
				.execute();
			const {json: outgoing} = await sendFriendRequest(harness, alice.token, bob.userId);
			assertRelationshipId(outgoing, bob.userId);
			assertRelationshipType(outgoing, RelationshipTypes.OUTGOING_REQUEST);
		});
		test('NO_RELATION with all flags set allows friend requests from strangers', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await createBuilder(harness, bob.token)
				.patch('/users/@me/settings')
				.body({
					friend_source_flags:
						FriendSourceFlags.NO_RELATION | FriendSourceFlags.MUTUAL_FRIENDS | FriendSourceFlags.MUTUAL_GUILDS,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const {json: outgoing} = await sendFriendRequest(harness, alice.token, bob.userId);
			assertRelationshipId(outgoing, bob.userId);
			assertRelationshipType(outgoing, RelationshipTypes.OUTGOING_REQUEST);
		});
	});
	describe('without NO_RELATION blocks strangers', () => {
		test('user with only MUTUAL_FRIENDS rejects requests from strangers with no mutual friends', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await createBuilder(harness, bob.token)
				.patch('/users/@me/settings')
				.body({friend_source_flags: FriendSourceFlags.MUTUAL_FRIENDS})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, alice.token)
				.post(`/users/@me/relationships/${bob.userId}`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'FRIEND_REQUEST_BLOCKED')
				.execute();
		});
		test('user with only MUTUAL_GUILDS rejects requests from strangers with no mutual guilds', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await createBuilder(harness, bob.token)
				.patch('/users/@me/settings')
				.body({friend_source_flags: FriendSourceFlags.MUTUAL_GUILDS})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, alice.token)
				.post(`/users/@me/relationships/${bob.userId}`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'FRIEND_REQUEST_BLOCKED')
				.execute();
		});
		test('user with zero flags rejects all friend requests', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			await createBuilder(harness, bob.token)
				.patch('/users/@me/settings')
				.body({friend_source_flags: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, alice.token)
				.post(`/users/@me/relationships/${bob.userId}`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'FRIEND_REQUEST_BLOCKED')
				.execute();
		});
	});
	describe('MUTUAL_FRIENDS allows requests from users sharing mutual friends', () => {
		test('user with MUTUAL_FRIENDS flag accepts request when they share a mutual friend', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			const charlie = await createTestAccount(harness);
			await createFriendship(harness, alice, charlie);
			await createFriendship(harness, bob, charlie);
			await createBuilder(harness, bob.token)
				.patch('/users/@me/settings')
				.body({friend_source_flags: FriendSourceFlags.MUTUAL_FRIENDS})
				.expect(HTTP_STATUS.OK)
				.execute();
			const {json: outgoing} = await sendFriendRequest(harness, alice.token, bob.userId);
			assertRelationshipId(outgoing, bob.userId);
			assertRelationshipType(outgoing, RelationshipTypes.OUTGOING_REQUEST);
		});
	});
	describe('default flags', () => {
		test('adult users default to allowing everyone (NO_RELATION set)', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness);
			const {json: outgoing} = await sendFriendRequest(harness, alice.token, bob.userId);
			assertRelationshipId(outgoing, bob.userId);
			assertRelationshipType(outgoing, RelationshipTypes.OUTGOING_REQUEST);
		});
		test('teen users default to blocking strangers', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness, {dateOfBirth: '2012-01-01'});
			await createBuilder(harness, alice.token)
				.post(`/users/@me/relationships/${bob.userId}`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'FRIEND_REQUEST_BLOCKED')
				.execute();
		});
		test('teen users can receive requests from mutual friends', async () => {
			const alice = await createTestAccount(harness);
			const bob = await createTestAccount(harness, {dateOfBirth: '2012-01-01'});
			const charlie = await createTestAccount(harness);
			await createFriendship(harness, alice, charlie);
			await createFriendship(harness, bob, charlie);
			const {json: outgoing} = await sendFriendRequest(harness, alice.token, bob.userId);
			assertRelationshipId(outgoing, bob.userId);
			assertRelationshipType(outgoing, RelationshipTypes.OUTGOING_REQUEST);
		});
	});
	describe('settings persistence', () => {
		test('NO_RELATION flag is preserved after settings update', async () => {
			const bob = await createTestAccount(harness);
			await createBuilder(harness, bob.token)
				.patch('/users/@me/settings')
				.body({friend_source_flags: FriendSourceFlags.NO_RELATION})
				.expect(HTTP_STATUS.OK)
				.execute();
			const {json: settings} = await createBuilder<{
				friend_source_flags: number;
			}>(harness, bob.token)
				.get('/users/@me/settings')
				.executeWithResponse();
			expect(settings.friend_source_flags & FriendSourceFlags.NO_RELATION).toBe(FriendSourceFlags.NO_RELATION);
		});
	});
});
