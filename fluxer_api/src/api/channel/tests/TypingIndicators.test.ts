// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createFriendship,
	createGuild,
	createPermissionOverwrite,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, test} from 'vitest';

async function createDmChannel(
	harness: ApiTestHarness,
	token: string,
	recipientId: string,
): Promise<{
	id: string;
}> {
	return createBuilder<{
		id: string;
	}>(harness, token)
		.post('/users/@me/channels')
		.body({recipient_id: recipientId})
		.execute();
}

describe('Typing Indicators', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should reject typing indicator from user not in DM channel', async () => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		const user3 = await createTestAccount(harness);
		await createFriendship(harness, user1, user2);
		const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
		await createBuilder(harness, user3.token)
			.post(`/channels/${dmChannel.id}/typing`)
			.body({})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('should reject typing indicator from non-member in guild channel', async () => {
		const account = await createTestAccount(harness);
		const nonMember = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Typing Test Guild');
		const channelId = guild.system_channel_id!;
		await createBuilder(harness, nonMember.token)
			.post(`/channels/${channelId}/typing`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should require SEND_MESSAGES permission for typing indicator', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.SEND_MESSAGES.toString(),
		});
		await createBuilder(harness, member.token)
			.post(`/channels/${systemChannel.id}/typing`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
