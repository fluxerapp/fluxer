// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createFriendship, createGroupDmChannel} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Group DM Add Recipient Permissions', () => {
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
	it('rejects adding non-friend to group DM', async () => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		const user3 = await createTestAccount(harness);
		const user4 = await createTestAccount(harness);
		await createFriendship(harness, user1, user2);
		await createFriendship(harness, user1, user3);
		const groupDmChannel = await createGroupDmChannel(harness, user1.token, [user2.userId, user3.userId]);
		await createBuilder(harness, user1.token)
			.put(`/channels/${groupDmChannel.id}/recipients/${user4.userId}`)
			.body(null)
			.expect(HTTP_STATUS.BAD_REQUEST, 'NOT_FRIENDS_WITH_USER')
			.execute();
	});
	it('allows adding friend to group DM', async () => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		const user3 = await createTestAccount(harness);
		const user4 = await createTestAccount(harness);
		await createFriendship(harness, user1, user2);
		await createFriendship(harness, user1, user3);
		await createFriendship(harness, user1, user4);
		const groupDmChannel = await createGroupDmChannel(harness, user1.token, [user2.userId, user3.userId]);
		await createBuilder(harness, user1.token)
			.put(`/channels/${groupDmChannel.id}/recipients/${user4.userId}`)
			.body(null)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
	it('allows member to add their own friend to group DM', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const otherMember = await createTestAccount(harness);
		const newUser = await createTestAccount(harness);
		await createFriendship(harness, owner, member);
		await createFriendship(harness, owner, otherMember);
		await createFriendship(harness, member, newUser);
		const groupDmChannel = await createGroupDmChannel(harness, owner.token, [member.userId, otherMember.userId]);
		await createBuilder(harness, member.token)
			.put(`/channels/${groupDmChannel.id}/recipients/${newUser.userId}`)
			.body(null)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
	it('rejects non-participant from adding recipients to group DM', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const third = await createTestAccount(harness);
		const outsider = await createTestAccount(harness);
		await createFriendship(harness, owner, member);
		await createFriendship(harness, owner, third);
		await createFriendship(harness, member, third);
		await createFriendship(harness, outsider, third);
		const groupDmChannel = await createGroupDmChannel(harness, owner.token, [member.userId, third.userId]);
		await createBuilder(harness, outsider.token)
			.put(`/channels/${groupDmChannel.id}/recipients/${third.userId}`)
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACCESS')
			.execute();
	});
	it('rejects member from adding non-friend to group DM', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const otherMember = await createTestAccount(harness);
		const ownerFriend = await createTestAccount(harness);
		const outsider = await createTestAccount(harness);
		await createFriendship(harness, owner, member);
		await createFriendship(harness, owner, otherMember);
		await createFriendship(harness, owner, ownerFriend);
		await createFriendship(harness, outsider, otherMember);
		const groupDmChannel = await createGroupDmChannel(harness, owner.token, [member.userId, otherMember.userId]);
		await createBuilder(harness, owner.token)
			.put(`/channels/${groupDmChannel.id}/recipients/${ownerFriend.userId}`)
			.body(null)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, member.token)
			.put(`/channels/${groupDmChannel.id}/recipients/${outsider.userId}`)
			.body(null)
			.expect(HTTP_STATUS.BAD_REQUEST, 'NOT_FRIENDS_WITH_USER')
			.execute();
	});
});
