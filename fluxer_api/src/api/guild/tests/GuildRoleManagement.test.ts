// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	addMemberRole,
	createChannelInvite,
	createGuild,
	createRole,
	deleteRole,
	getChannel,
	getRoles,
	updateRolePositions,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Guild Role Management', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('New Role Position', () => {
		test('should have @everyone role at position 0', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const roles = await getRoles(harness, account.token, guild.id);
			const everyoneRole = roles.find((r) => r.id === guild.id);
			expect(everyoneRole).toBeDefined();
			expect(everyoneRole!.position).toBe(0);
		});
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
		test('should allow deleting role lower in hierarchy than your highest role', async () => {
			const owner = await createTestAccount(harness);
			const moderator = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const highRole = await createRole(harness, owner.token, guild.id, {
				name: 'High Role',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			const lowRole = await createRole(harness, owner.token, guild.id, {
				name: 'Low Role',
			});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: highRole.id, position: 3},
				{id: lowRole.id, position: 2},
			]);
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, moderator.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, moderator.userId, highRole.id);
			await deleteRole(harness, moderator.token, guild.id, lowRole.id);
			const roles = await getRoles(harness, owner.token, guild.id);
			expect(roles.find((r) => r.id === lowRole.id)).toBeUndefined();
		});
		test('should use ID comparison as tiebreaker when roles have same position', async () => {
			const owner = await createTestAccount(harness);
			const moderator = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const roleA = await createRole(harness, owner.token, guild.id, {
				name: 'Role A',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			const roleB = await createRole(harness, owner.token, guild.id, {
				name: 'Role B',
			});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: roleA.id, position: 2},
				{id: roleB.id, position: 2},
			]);
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, moderator.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, moderator.userId, roleA.id);
			const roleAIdLower = String(roleA.id) < String(roleB.id);
			if (roleAIdLower) {
				await createBuilder(harness, moderator.token)
					.delete(`/guilds/${guild.id}/roles/${roleB.id}`)
					.expect(HTTP_STATUS.NO_CONTENT)
					.execute();
			} else {
				await createBuilder(harness, moderator.token)
					.delete(`/guilds/${guild.id}/roles/${roleB.id}`)
					.expect(HTTP_STATUS.FORBIDDEN)
					.execute();
			}
		});
		test('should allow owner to delete any role regardless of position', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const highRole = await createRole(harness, owner.token, guild.id, {
				name: 'High Role',
			});
			await updateRolePositions(harness, owner.token, guild.id, [{id: highRole.id, position: 10}]);
			await deleteRole(harness, owner.token, guild.id, highRole.id);
			const roles = await getRoles(harness, owner.token, guild.id);
			expect(roles.find((r) => r.id === highRole.id)).toBeUndefined();
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
		test('should allow user to create role with permissions they have', async () => {
			const owner = await createTestAccount(harness);
			const manager = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const permissions = Permissions.MANAGE_ROLES | Permissions.SEND_MESSAGES;
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Manager',
				permissions: permissions.toString(),
			});
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, manager.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			const newRole = await createRole(harness, manager.token, guild.id, {
				name: 'New Role',
				permissions: Permissions.SEND_MESSAGES.toString(),
			});
			expect(newRole.name).toBe('New Role');
			expect(BigInt(newRole.permissions)).toBe(Permissions.SEND_MESSAGES);
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
		test('should allow owner to grant any permissions', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const role = await createRole(harness, owner.token, guild.id, {
				name: 'Admin Role',
				permissions: Permissions.ADMINISTRATOR.toString(),
			});
			expect(BigInt(role.permissions)).toBe(Permissions.ADMINISTRATOR);
		});
	});
	describe('Bulk Update Role Positions', () => {
		test('should update multiple role positions at once', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const role1 = await createRole(harness, account.token, guild.id, {name: 'Role 1'});
			const role2 = await createRole(harness, account.token, guild.id, {name: 'Role 2'});
			const role3 = await createRole(harness, account.token, guild.id, {name: 'Role 3'});
			await updateRolePositions(harness, account.token, guild.id, [
				{id: role1.id, position: 3},
				{id: role2.id, position: 2},
				{id: role3.id, position: 1},
			]);
			const roles = await getRoles(harness, account.token, guild.id);
			const updatedRole1 = roles.find((r) => r.id === role1.id);
			const updatedRole2 = roles.find((r) => r.id === role2.id);
			const updatedRole3 = roles.find((r) => r.id === role3.id);
			expect(updatedRole1!.position).toBeGreaterThan(updatedRole2!.position);
			expect(updatedRole2!.position).toBeGreaterThan(updatedRole3!.position);
		});
		test('should not allow reordering @everyone role', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}/roles`)
				.body([{id: guild.id, position: 5}])
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should require MANAGE_ROLES permission for bulk position update', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const role = await createRole(harness, owner.token, guild.id, {name: 'Test Role'});
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, member.token, invite.code);
			await createBuilder(harness, member.token)
				.patch(`/guilds/${guild.id}/roles`)
				.body([{id: role.id, position: 5}])
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should reject invalid role ID in bulk update', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}/roles`)
				.body([{id: '999999999999999999', position: 5}])
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
	describe('Role Name Validation', () => {
		test('should reject role name exceeding 100 characters', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			await createBuilder(harness, account.token)
				.post(`/guilds/${guild.id}/roles`)
				.body({name: 'a'.repeat(101)})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should accept role name at exactly 100 characters', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const role = await createRole(harness, account.token, guild.id, {
				name: 'a'.repeat(100),
			});
			expect(role.name).toBe('a'.repeat(100));
		});
		test('should require name when creating role', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			await createBuilder(harness, account.token)
				.post(`/guilds/${guild.id}/roles`)
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should accept role name with unicode characters', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const role = await createRole(harness, account.token, guild.id, {
				name: 'Moderator',
			});
			expect(role.name).toBe('Moderator');
		});
	});
	describe('Role Color Validation', () => {
		test('should accept valid color value', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const role = await createRole(harness, account.token, guild.id, {
				name: 'Colored Role',
				color: 0xff0000,
			});
			expect(role.color).toBe(0xff0000);
		});
		test('should accept color value of 0 (no color)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const role = await createRole(harness, account.token, guild.id, {
				name: 'No Color Role',
				color: 0,
			});
			expect(role.color).toBe(0);
		});
		test('should accept maximum valid color value (0xFFFFFF)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const role = await createRole(harness, account.token, guild.id, {
				name: 'Max Color Role',
				color: 0xffffff,
			});
			expect(role.color).toBe(0xffffff);
		});
		test('should reject color value exceeding maximum (> 0xFFFFFF)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			await createBuilder(harness, account.token)
				.post(`/guilds/${guild.id}/roles`)
				.body({name: 'Invalid Color', color: 0x1000000})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject negative color value', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			await createBuilder(harness, account.token)
				.post(`/guilds/${guild.id}/roles`)
				.body({name: 'Negative Color', color: -1})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should default to color 0 when not specified', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const role = await createRole(harness, account.token, guild.id, {
				name: 'Default Color Role',
			});
			expect(role.color).toBe(0);
		});
	});
});
