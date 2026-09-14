// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	addMemberRole,
	createChannelInvite,
	createGuild,
	createRole,
	deleteInvite,
	getChannel,
	getRoles,
	setupTestGuildWithMembers,
	updateRole,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildInviteMetadataResponse} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Invite Permissions', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('cannot create invite without CREATE_INSTANT_INVITE permission', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const roles = await getRoles(harness, owner.token, guild.id);
		const everyoneRole = roles.find((r) => r.id === guild.id);
		if (everyoneRole) {
			const permissions = BigInt(everyoneRole.permissions);
			const newPermissions = permissions & ~Permissions.CREATE_INSTANT_INVITE;
			await updateRole(harness, owner.token, guild.id, everyoneRole.id, {
				permissions: newPermissions.toString(),
			});
		}
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		await createBuilder(harness, member.token)
			.post(`/channels/${systemChannel.id}/invites`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('can create invite with CREATE_INSTANT_INVITE permission', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const inviterRole = await createRole(harness, owner.token, guild.id, {
			name: 'Inviter',
			permissions: Permissions.CREATE_INSTANT_INVITE.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, inviterRole.id);
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createBuilder<GuildInviteMetadataResponse>(harness, member.token)
			.post(`/channels/${systemChannel.id}/invites`)
			.body({})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(invite.code).toBeTruthy();
		await deleteInvite(harness, owner.token, invite.code);
	});
	test('owner can always create invites', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		expect(invite.code).toBeTruthy();
		expect(invite.inviter?.id).toBe(owner.userId);
		await deleteInvite(harness, owner.token, invite.code);
	});
	test('non-member cannot create invite', async () => {
		const owner = await createTestAccount(harness);
		const nonMember = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		await createBuilder(harness, nonMember.token)
			.post(`/channels/${systemChannel.id}/invites`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('can create unlimited invite with max_age 0', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createBuilder<GuildInviteMetadataResponse>(harness, owner.token)
			.post(`/channels/${systemChannel.id}/invites`)
			.body({max_age: 0})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(invite.max_age).toBe(0);
		await deleteInvite(harness, owner.token, invite.code);
	});
	test('can create unlimited invite with max_uses 0', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createBuilder<GuildInviteMetadataResponse>(harness, owner.token)
			.post(`/channels/${systemChannel.id}/invites`)
			.body({max_uses: 0})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(invite.max_uses).toBe(0);
		await deleteInvite(harness, owner.token, invite.code);
	});
	test('invite can be retrieved after use', async () => {
		const owner = await createTestAccount(harness);
		const joiner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, joiner.token, invite.code);
		const updatedInvite = await createBuilder<GuildInviteMetadataResponse>(harness, owner.token)
			.get(`/invites/${invite.code}?with_counts=true`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updatedInvite.code).toBe(invite.code);
		await deleteInvite(harness, owner.token, invite.code);
	});
	test('invite includes inviter information', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		expect(invite.inviter?.id).toBe(owner.userId);
		await deleteInvite(harness, owner.token, invite.code);
	});
	test('list channel invites requires MANAGE_CHANNELS permission', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		await createChannelInvite(harness, owner.token, systemChannel.id);
		await createBuilder(harness, member.token)
			.get(`/channels/${systemChannel.id}/invites`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('owner can list channel invites', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		const invites = await createBuilder<Array<GuildInviteMetadataResponse>>(harness, owner.token)
			.get(`/channels/${systemChannel.id}/invites`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(invites.length).toBeGreaterThanOrEqual(1);
		expect(invites.some((i) => i.code === invite.code)).toBe(true);
		await deleteInvite(harness, owner.token, invite.code);
	});
	test('cannot create invite for channel in another guild', async () => {
		const owner1 = await createTestAccount(harness);
		const owner2 = await createTestAccount(harness);
		const guild1 = await createGuild(harness, owner1.token, 'Guild 1');
		await createGuild(harness, owner2.token, 'Guild 2');
		const channel1 = await getChannel(harness, owner1.token, guild1.system_channel_id!);
		await createBuilder(harness, owner2.token)
			.post(`/channels/${channel1.id}/invites`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
