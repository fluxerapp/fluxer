// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	addMemberRole,
	createChannelInvite,
	createGuild,
	createRole,
	deleteInvite,
	getChannel,
	setupTestGuildWithMembers,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Invite Security', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('guild members can view invites', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		const inviteData = await createBuilder<{
			code: string;
		}>(harness, member.token)
			.get(`/invites/${invite.code}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(inviteData.code).toBe(invite.code);
		await deleteInvite(harness, owner.token, invite.code);
	});
	test('non-members can view public invites', async () => {
		const owner = await createTestAccount(harness);
		const nonMember = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await createBuilder(harness, nonMember.token).get(`/invites/${invite.code}`).expect(HTTP_STATUS.OK).execute();
		await deleteInvite(harness, owner.token, invite.code);
	});
	test('only owner can delete invites by default', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await createBuilder(harness, member.token)
			.delete(`/invites/${invite.code}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder(harness, owner.token)
			.delete(`/invites/${invite.code}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
	test('member with MANAGE_GUILD permission can delete invites', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const managerRole = await createRole(harness, owner.token, guild.id, {
			name: 'Manager',
			permissions: Permissions.MANAGE_GUILD.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, managerRole.id);
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await createBuilder(harness, member.token)
			.delete(`/invites/${invite.code}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
	test('deleted invites become inaccessible', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		const inviteCode = invite.code;
		await createBuilder(harness, owner.token).get(`/invites/${inviteCode}`).expect(HTTP_STATUS.OK).execute();
		await deleteInvite(harness, owner.token, inviteCode);
		await createBuilder(harness, owner.token).get(`/invites/${inviteCode}`).expect(HTTP_STATUS.NOT_FOUND).execute();
		await createBuilder(harness, owner.token)
			.post(`/invites/${inviteCode}`)
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('unauthenticated requests can view public invites', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		const inviteData = await createBuilderWithoutAuth<{
			code: string;
			guild: {
				name: string;
			};
		}>(harness)
			.get(`/invites/${invite.code}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(inviteData.code).toBe(invite.code);
		await deleteInvite(harness, owner.token, invite.code);
	});
	test('unauthenticated requests cannot accept invites', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await createBuilderWithoutAuth(harness)
			.post(`/invites/${invite.code}`)
			.body(null)
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
		await deleteInvite(harness, owner.token, invite.code);
	});
	test('invite creator can delete their own invite', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const createInvitesRole = await createRole(harness, owner.token, guild.id, {
			name: 'Inviter',
			permissions: Permissions.CREATE_INSTANT_INVITE.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, createInvitesRole.id);
		const memberInvite = await createChannelInvite(harness, member.token, systemChannel.id);
		await createBuilder(harness, member.token)
			.delete(`/invites/${memberInvite.code}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
	test('member cannot delete invites created by others without permission', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		const [member1, member2] = members;
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const inviterRole = await createRole(harness, owner.token, guild.id, {
			name: 'Inviter',
			permissions: Permissions.CREATE_INSTANT_INVITE.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member1.userId, inviterRole.id);
		await addMemberRole(harness, owner.token, guild.id, member2.userId, inviterRole.id);
		const member1Invite = await createChannelInvite(harness, member1.token, systemChannel.id);
		await createBuilder(harness, member2.token)
			.delete(`/invites/${member1Invite.code}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await deleteInvite(harness, owner.token, member1Invite.code);
	});
	test('double deletion returns not found', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await deleteInvite(harness, owner.token, invite.code);
		await createBuilder(harness, owner.token).delete(`/invites/${invite.code}`).expect(HTTP_STATUS.NOT_FOUND).execute();
	});
});
