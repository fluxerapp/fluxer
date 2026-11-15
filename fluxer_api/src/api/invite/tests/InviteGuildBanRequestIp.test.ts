// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {createChannelInvite, createGuild, getChannel} from '@app/api/channel/tests/ChannelTestUtils';
import {getGuildRepository} from '@app/api/middleware/ServiceSingletons';
import {banUser} from '@app/api/moderation/tests/ModerationTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Invite guild ban request address', () => {
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
	test('guild IP ban applies to the address of the accept request', async () => {
		const owner = await createTestAccount(harness);
		const bannedUser = await createTestAccount(harness, {ipAddress: '203.0.113.10'});
		const otherUser = await createTestAccount(harness, {ipAddress: '198.51.100.99'});
		const guild = await createGuild(harness, owner.token, 'Request Address Ban Test');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const firstInvite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await createBuilder(harness, bannedUser.token)
			.post(`/invites/${firstInvite.code}`)
			.header('x-forwarded-for', bannedUser.ipAddress!)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
		await banUser(harness, owner.token, guild.id, bannedUser.userId, 0);
		const secondInvite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await createBuilder(harness, otherUser.token)
			.post(`/invites/${secondInvite.code}`)
			.header('x-forwarded-for', bannedUser.ipAddress!)
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.USER_IP_BANNED_FROM_GUILD)
			.execute();
		expect(
			await getGuildRepository().getMember(createGuildID(BigInt(guild.id)), createUserID(BigInt(otherUser.userId))),
		).toBeNull();
	});
});
