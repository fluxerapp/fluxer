// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {profileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	acceptFriendRequest,
	listRelationships,
	sendFriendRequest,
	updateFriendNickname,
} from '@app/api/user/tests/RelationshipTestUtils';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('RelationshipNicknameUpdate', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		profileSubstringBlocklistCache.remove('nickname', 'blockedslug');
		await harness?.shutdown();
	});
	test('update friend nickname', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);
		const {json: updated} = await updateFriendNickname(harness, alice.token, bob.userId, 'Bestie Bob');
		expect(updated.nickname).toBe('Bestie Bob');
		const {json: aliceRels} = await listRelationships(harness, alice.token);
		const bobRel = aliceRels.find((r) => r.id === bob.userId);
		expect(bobRel?.nickname).toBe('Bestie Bob');
		const {json: bobRels} = await listRelationships(harness, bob.token);
		const aliceRel = bobRels.find((r) => r.id === alice.userId);
		expect(aliceRel?.nickname).toBeNull();
	});
	test('remove friend nickname', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);
		await updateFriendNickname(harness, alice.token, bob.userId, 'Bobby');
		const {json: updated} = await updateFriendNickname(harness, alice.token, bob.userId, null);
		expect(updated.nickname).toBeNull();
		const {json: aliceRels} = await listRelationships(harness, alice.token);
		const bobRel = aliceRels.find((r) => r.id === bob.userId);
		expect(bobRel?.nickname).toBeNull();
	});
	test('allows banned profile substrings in friend nicknames', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);
		profileSubstringBlocklistCache.add('nickname', 'blockedslug');
		await createBuilder(harness, alice.token)
			.patch(`/users/@me/relationships/${bob.userId}`)
			.body({nickname: 'BlockedSlug Bob'})
			.expect(HTTP_STATUS.OK)
			.execute();
	});
});
