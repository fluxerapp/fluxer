// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	addMemberRole,
	createChannelInvite,
	createGuild,
	createRole,
	getChannel,
	updateRolePositions,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, test} from 'vitest';

describe('Guild Role Management', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('Role Hierarchy Delete Restrictions', () => {
		test('should prevent deleting role higher in hierarchy than your highest role', async () => {
			const owner = await createTestAccount(harness);
			const moderator = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const highRole = await createRole(harness, owner.token, guild.id, {
				name: 'High Role',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			const lowRole = await createRole(harness, owner.token, guild.id, {
				name: 'Low Role',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: highRole.id, position: 3},
				{id: lowRole.id, position: 2},
			]);
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, moderator.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, moderator.userId, lowRole.id);
			await createBuilder(harness, moderator.token)
				.delete(`/guilds/${guild.id}/roles/${highRole.id}`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('Role Permissions Validation', () => {
		test('should prevent user from granting permissions they do not have', async () => {
			const owner = await createTestAccount(harness);
			const manager = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Manager',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, manager.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			await createBuilder(harness, manager.token)
				.post(`/guilds/${guild.id}/roles`)
				.body({name: 'New Role', permissions: Permissions.ADMINISTRATOR.toString()})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should prevent updating role with permissions user does not have', async () => {
			const owner = await createTestAccount(harness);
			const manager = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Manager',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			const targetRole = await createRole(harness, owner.token, guild.id, {
				name: 'Target Role',
				permissions: '0',
			});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: managerRole.id, position: 3},
				{id: targetRole.id, position: 2},
			]);
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, manager.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			await createBuilder(harness, manager.token)
				.patch(`/guilds/${guild.id}/roles/${targetRole.id}`)
				.body({permissions: Permissions.BAN_MEMBERS.toString()})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
});
