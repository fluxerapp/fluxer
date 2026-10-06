// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createGuild,
	getChannel,
	updateRole,
} from '@app/api/channel/tests/ChannelTestUtils';
import {deleteInvite} from '@app/api/invite/tests/InviteTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, beforeAll, beforeEach, describe, test} from 'vitest';

const BASIC_PERMISSIONS =
	Permissions.VIEW_CHANNEL |
	Permissions.SEND_MESSAGES |
	Permissions.READ_MESSAGE_HISTORY |
	Permissions.CONNECT |
	Permissions.SPEAK;

describe('Invite Permissions', () => {
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
	test('member without CREATE_INSTANT_INVITE cannot create invite', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Permission Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		await updateRole(harness, owner.token, guild.id, guild.id, {
			permissions: BASIC_PERMISSIONS.toString(),
		});
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		await createBuilder(harness, member.token)
			.post(`/channels/${systemChannel.id}/invites`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('member without MANAGE_CHANNELS cannot delete invite', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Permission Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const ownerInvite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await updateRole(harness, owner.token, guild.id, guild.id, {
			permissions: BASIC_PERMISSIONS.toString(),
		});
		const joinInvite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, joinInvite.code);
		await createBuilder(harness, member.token)
			.delete(`/invites/${ownerInvite.code}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('member without MANAGE_CHANNELS cannot list channel invites', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Permission Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		await createChannelInvite(harness, owner.token, systemChannel.id);
		await updateRole(harness, owner.token, guild.id, guild.id, {
			permissions: BASIC_PERMISSIONS.toString(),
		});
		const joinInvite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, joinInvite.code);
		await createBuilder(harness, member.token)
			.get(`/channels/${systemChannel.id}/invites`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('member without MANAGE_GUILD cannot list guild invites', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Permission Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		await createChannelInvite(harness, owner.token, systemChannel.id);
		await updateRole(harness, owner.token, guild.id, guild.id, {
			permissions: BASIC_PERMISSIONS.toString(),
		});
		const joinInvite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, joinInvite.code);
		await createBuilder(harness, member.token)
			.get(`/guilds/${guild.id}/invites`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('owner can delete invite', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Permission Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await deleteInvite(harness, owner.token, invite.code);
		await createBuilder(harness, owner.token).get(`/invites/${invite.code}`).expect(HTTP_STATUS.NOT_FOUND).execute();
	});
	test('full permission flow: stripped permissions block all invite operations for member', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Full Permission Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const ownerInvite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await updateRole(harness, owner.token, guild.id, guild.id, {
			permissions: BASIC_PERMISSIONS.toString(),
		});
		const joinInvite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, joinInvite.code);
		await createBuilder(harness, member.token)
			.post(`/channels/${systemChannel.id}/invites`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder(harness, member.token)
			.delete(`/invites/${ownerInvite.code}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder(harness, member.token)
			.get(`/channels/${systemChannel.id}/invites`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder(harness, member.token)
			.get(`/guilds/${guild.id}/invites`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await deleteInvite(harness, owner.token, ownerInvite.code);
		await createBuilder(harness, owner.token)
			.get(`/invites/${ownerInvite.code}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
});
