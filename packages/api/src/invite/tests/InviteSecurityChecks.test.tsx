/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createInviteCode, createUserID} from '@fluxer/api/src/BrandedTypes';
import {
	acceptInvite,
	createChannelInvite,
	createGuild,
	getChannel,
} from '@fluxer/api/src/channel/tests/ChannelTestUtils';
import {InviteRepository} from '@fluxer/api/src/invite/InviteRepository';
import {deleteInvite} from '@fluxer/api/src/invite/tests/InviteTestUtils';
import {banUser} from '@fluxer/api/src/moderation/tests/ModerationTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@fluxer/api/src/test/TestRequestBuilder';
import {InviteTypes} from '@fluxer/constants/src/ChannelConstants';
import type {GuildInviteMetadataResponse} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

async function createGuildInviteWithCodeForTesting(
	code: string,
	guildId: string,
	channelId: string,
	inviterId: string,
): Promise<void> {
	const inviteRepository = new InviteRepository();
	await inviteRepository.create({
		code: createInviteCode(code),
		type: InviteTypes.GUILD,
		guild_id: createGuildID(BigInt(guildId)),
		channel_id: createChannelID(BigInt(channelId)),
		inviter_id: createUserID(BigInt(inviterId)),
		uses: 0,
		max_uses: 0,
		max_age: 0,
		temporary: false,
	});
}

describe('Invite Security Checks', () => {
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

	test('non-member cannot delete invite they did not create', async () => {
		const owner = await createTestAccount(harness);
		const attacker = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Invite Security Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);

		await createBuilder(harness, attacker.token)
			.delete(`/invites/${invite.code}`)
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_GUILD')
			.execute();

		await deleteInvite(harness, owner.token, invite.code);
	});

	test('owner can delete invite', async () => {
		const owner = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Invite Security Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);

		await deleteInvite(harness, owner.token, invite.code);

		await createBuilder(harness, owner.token).get(`/invites/${invite.code}`).expect(HTTP_STATUS.NOT_FOUND).execute();
	});

	test('deleted invite cannot be retrieved', async () => {
		const owner = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Invite Security Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);

		await createBuilder(harness, owner.token).get(`/invites/${invite.code}`).expect(HTTP_STATUS.OK).execute();

		await deleteInvite(harness, owner.token, invite.code);

		await createBuilder(harness, owner.token).get(`/invites/${invite.code}`).expect(HTTP_STATUS.NOT_FOUND).execute();
	});

	test('deleted invite cannot be accepted', async () => {
		const owner = await createTestAccount(harness);
		const joiner = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Invite Security Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);

		await deleteInvite(harness, owner.token, invite.code);

		await createBuilder(harness, joiner.token)
			.post(`/invites/${invite.code}`)
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	test('invite lookup falls back to lowercase when casing differs', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Vanity Lookup Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const inviteCode = 'vanitycase';

		await createGuildInviteWithCodeForTesting(inviteCode, guild.id, systemChannel.id, owner.userId);

		const inviteResponse = await createBuilder<{code: string}>(harness, owner.token)
			.get(`/invites/${inviteCode.toUpperCase()}`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(inviteResponse.code).toBe(inviteCode);

		await deleteInvite(harness, owner.token, inviteCode);
	});

	test('invite accept falls back to lowercase and updates uses', async () => {
		const owner = await createTestAccount(harness);
		const joiner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Invite Accept Case Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const inviteCode = 'acceptcase';

		await createGuildInviteWithCodeForTesting(inviteCode, guild.id, systemChannel.id, owner.userId);

		const accepted = await acceptInvite(harness, joiner.token, inviteCode.toUpperCase());
		expect(accepted.guild.id).toBe(guild.id);

		const invitesList = await createBuilder<Array<GuildInviteMetadataResponse>>(harness, owner.token)
			.get(`/guilds/${guild.id}/invites`)
			.execute();
		const updatedInvite = invitesList.find((invite) => invite.code === inviteCode);
		expect(updatedInvite).toBeDefined();
		expect(updatedInvite!.uses).toBe(1);

		await deleteInvite(harness, owner.token, inviteCode);
	});

	test('invite with max_uses limit becomes invalid after exhaustion', async () => {
		const owner = await createTestAccount(harness);
		const joiner1 = await createTestAccount(harness);
		const joiner2 = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Max Uses Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const invite = await createBuilder<GuildInviteMetadataResponse>(harness, owner.token)
			.post(`/channels/${systemChannel.id}/invites`)
			.body({max_uses: 1})
			.execute();

		expect(invite.max_uses).toBe(1);

		await acceptInvite(harness, joiner1.token, invite.code);

		await createBuilder(harness, joiner2.token)
			.post(`/invites/${invite.code}`)
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	test('banned user cannot use invite to rejoin guild', async () => {
		const owner = await createTestAccount(harness);
		const bannedUser = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Ban Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const joinInvite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, bannedUser.token, joinInvite.code);

		await banUser(harness, owner.token, guild.id, bannedUser.userId, 0);

		const newInvite = await createChannelInvite(harness, owner.token, systemChannel.id);

		await createBuilder(harness, bannedUser.token)
			.post(`/invites/${newInvite.code}`)
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();

		await deleteInvite(harness, owner.token, newInvite.code);
	});

	test('non-member can view public invite', async () => {
		const owner = await createTestAccount(harness);
		const nonMember = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Public Invite Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);

		const inviteData = await createBuilder<{code: string}>(harness, nonMember.token)
			.get(`/invites/${invite.code}`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(inviteData.code).toBe(invite.code);

		await deleteInvite(harness, owner.token, invite.code);
	});

	test('unauthenticated request can view public invite', async () => {
		const owner = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Public Invite Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);

		const inviteData = await createBuilderWithoutAuth<{code: string}>(harness)
			.get(`/invites/${invite.code}`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(inviteData.code).toBe(invite.code);

		await deleteInvite(harness, owner.token, invite.code);
	});

	test('unauthenticated request cannot accept invite', async () => {
		const owner = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Auth Required Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);

		await createBuilderWithoutAuth(harness)
			.post(`/invites/${invite.code}`)
			.body(null)
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();

		await deleteInvite(harness, owner.token, invite.code);
	});

	test('existing member using invite returns success without incrementing uses', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Existing Member Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);

		await createBuilder(harness, member.token)
			.post(`/invites/${invite.code}`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();

		const invitesList = await createBuilder<Array<GuildInviteMetadataResponse>>(harness, owner.token)
			.get(`/guilds/${guild.id}/invites`)
			.execute();

		const updatedInvite = invitesList.find((i) => i.code === invite.code);
		expect(updatedInvite).toBeDefined();
		expect(updatedInvite!.uses).toBe(1);

		await deleteInvite(harness, owner.token, invite.code);
	});

	test('invite with max_uses 0 allows unlimited uses', async () => {
		const owner = await createTestAccount(harness);
		const joiner1 = await createTestAccount(harness);
		const joiner2 = await createTestAccount(harness);
		const joiner3 = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Unlimited Uses Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const invite = await createBuilder<GuildInviteMetadataResponse>(harness, owner.token)
			.post(`/channels/${systemChannel.id}/invites`)
			.body({max_uses: 0})
			.execute();

		expect(invite.max_uses).toBe(0);

		await acceptInvite(harness, joiner1.token, invite.code);
		await acceptInvite(harness, joiner2.token, invite.code);
		await acceptInvite(harness, joiner3.token, invite.code);

		const invitesList = await createBuilder<Array<GuildInviteMetadataResponse>>(harness, owner.token)
			.get(`/guilds/${guild.id}/invites`)
			.execute();

		const updatedInvite = invitesList.find((i) => i.code === invite.code);
		expect(updatedInvite).toBeDefined();
		expect(updatedInvite!.uses).toBe(3);

		await deleteInvite(harness, owner.token, invite.code);
	});
});
