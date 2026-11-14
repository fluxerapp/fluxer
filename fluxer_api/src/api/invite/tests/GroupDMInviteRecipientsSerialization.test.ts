// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelInvite, createFriendship, createGroupDmChannel} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface GroupDmInviteChannelResponse {
	id: string;
	type: number;
	recipients?: Array<{
		username: string;
	}>;
}

interface GroupDmInviteWithChannelResponse {
	code: string;
	channel: GroupDmInviteChannelResponse;
}

describe('Group DM Invite Recipients Serialization', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	test('group DM invite recipients contain only username field', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		await createFriendship(harness, owner, member);
		await createFriendship(harness, owner, recipient);
		const groupChannel = await createGroupDmChannel(harness, owner.token, [member.userId, recipient.userId]);
		const invite = await createChannelInvite(harness, owner.token, groupChannel.id);
		const invitePayload = await createBuilder<GroupDmInviteWithChannelResponse>(harness, owner.token)
			.get(`/invites/${invite.code}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(invitePayload.channel).toBeDefined();
		expect(invitePayload.channel.recipients).toBeDefined();
		expect(Array.isArray(invitePayload.channel.recipients)).toBe(true);
		expect(invitePayload.channel.recipients!.length).toBeGreaterThanOrEqual(2);
		for (const recipientEntry of invitePayload.channel.recipients!) {
			const keys = Object.keys(recipientEntry);
			expect(keys).toHaveLength(1);
			expect(keys[0]).toBe('username');
			expect(typeof recipientEntry.username).toBe('string');
		}
	});
});
