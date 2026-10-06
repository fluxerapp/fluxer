// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	addMemberRole,
	createChannelInvite,
	createGuild,
	createRole,
	getChannel,
	getRoles,
	updateRolePositions,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Guild Role Reorder', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('Owner Reordering', () => {
		test('should allow owner to reorder any roles', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const roleA = await createRole(harness, owner.token, guild.id, {name: 'Role A'});
			const roleB = await createRole(harness, owner.token, guild.id, {name: 'Role B'});
			const roleC = await createRole(harness, owner.token, guild.id, {name: 'Role C'});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: roleC.id, position: 3},
				{id: roleB.id, position: 2},
				{id: roleA.id, position: 1},
			]);
			const roles = await getRoles(harness, owner.token, guild.id);
			const updatedA = roles.find((r) => r.id === roleA.id)!;
			const updatedB = roles.find((r) => r.id === roleB.id)!;
			const updatedC = roles.find((r) => r.id === roleC.id)!;
			expect(updatedC.position).toBeGreaterThan(updatedB.position);
			expect(updatedB.position).toBeGreaterThan(updatedA.position);
		});
		test('should allow owner to reverse all role positions', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const roleA = await createRole(harness, owner.token, guild.id, {name: 'Role A'});
			const roleB = await createRole(harness, owner.token, guild.id, {name: 'Role B'});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: roleA.id, position: 2},
				{id: roleB.id, position: 1},
			]);
			const rolesBefore = await getRoles(harness, owner.token, guild.id);
			const beforeA = rolesBefore.find((r) => r.id === roleA.id)!;
			const beforeB = rolesBefore.find((r) => r.id === roleB.id)!;
			expect(beforeA.position).toBeGreaterThan(beforeB.position);
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: roleB.id, position: 2},
				{id: roleA.id, position: 1},
			]);
			const rolesAfter = await getRoles(harness, owner.token, guild.id);
			const afterA = rolesAfter.find((r) => r.id === roleA.id)!;
			const afterB = rolesAfter.find((r) => r.id === roleB.id)!;
			expect(afterB.position).toBeGreaterThan(afterA.position);
		});
	});
	describe('Non-Owner with MANAGE_ROLES', () => {
		test('should allow member with MANAGE_ROLES to reorder roles below their highest role', async () => {
			const owner = await createTestAccount(harness);
			const manager = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Manager',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			const lowRoleA = await createRole(harness, owner.token, guild.id, {name: 'Low A'});
			const lowRoleB = await createRole(harness, owner.token, guild.id, {name: 'Low B'});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: managerRole.id, position: 4},
				{id: lowRoleA.id, position: 3},
				{id: lowRoleB.id, position: 2},
			]);
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, manager.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			await updateRolePositions(harness, manager.token, guild.id, [
				{id: lowRoleB.id, position: 3},
				{id: lowRoleA.id, position: 2},
			]);
			const roles = await getRoles(harness, owner.token, guild.id);
			const updatedA = roles.find((r) => r.id === lowRoleA.id)!;
			const updatedB = roles.find((r) => r.id === lowRoleB.id)!;
			expect(updatedB.position).toBeGreaterThan(updatedA.position);
		});
		test('should allow a full payload when unmanageable roles keep their original positions', async () => {
			const owner = await createTestAccount(harness);
			const manager = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const highRole = await createRole(harness, owner.token, guild.id, {name: 'High Role'});
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Manager',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			const lowRoleA = await createRole(harness, owner.token, guild.id, {name: 'Low A'});
			const lowRoleB = await createRole(harness, owner.token, guild.id, {name: 'Low B'});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: highRole.id, position: 4},
				{id: managerRole.id, position: 3},
				{id: lowRoleA.id, position: 2},
				{id: lowRoleB.id, position: 1},
			]);
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, manager.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			await createBuilder(harness, manager.token)
				.patch(`/guilds/${guild.id}/roles`)
				.body([
					{id: highRole.id, position: 4},
					{id: managerRole.id, position: 3},
					{id: lowRoleB.id, position: 2},
					{id: lowRoleA.id, position: 1},
				])
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const roles = await getRoles(harness, owner.token, guild.id);
			const updatedHighRole = roles.find((role) => role.id === highRole.id)!;
			const updatedManagerRole = roles.find((role) => role.id === managerRole.id)!;
			const updatedLowRoleA = roles.find((role) => role.id === lowRoleA.id)!;
			const updatedLowRoleB = roles.find((role) => role.id === lowRoleB.id)!;
			expect(updatedHighRole.position).toBeGreaterThan(updatedManagerRole.position);
			expect(updatedManagerRole.position).toBeGreaterThan(updatedLowRoleB.position);
			expect(updatedLowRoleB.position).toBeGreaterThan(updatedLowRoleA.position);
		});
		test('should reject reordering a role above the user highest role', async () => {
			const owner = await createTestAccount(harness);
			const manager = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const highRole = await createRole(harness, owner.token, guild.id, {name: 'High Role'});
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Manager',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: highRole.id, position: 4},
				{id: managerRole.id, position: 3},
			]);
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, manager.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			await createBuilder(harness, manager.token)
				.patch(`/guilds/${guild.id}/roles`)
				.body([{id: highRole.id, position: 1}])
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should keep unmanageable roles anchored when a manageable role is moved past them', async () => {
			const owner = await createTestAccount(harness);
			const manager = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const highRole = await createRole(harness, owner.token, guild.id, {name: 'High Role'});
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Manager',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			const lowRole = await createRole(harness, owner.token, guild.id, {name: 'Low Role'});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: highRole.id, position: 4},
				{id: managerRole.id, position: 3},
				{id: lowRole.id, position: 2},
			]);
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, manager.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			await createBuilder(harness, manager.token)
				.patch(`/guilds/${guild.id}/roles`)
				.body([{id: lowRole.id, position: 5}])
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const roles = await getRoles(harness, owner.token, guild.id);
			const updatedHighRole = roles.find((role) => role.id === highRole.id)!;
			const updatedManagerRole = roles.find((role) => role.id === managerRole.id)!;
			const updatedLowRole = roles.find((role) => role.id === lowRole.id)!;
			expect(updatedHighRole.position).toBeGreaterThan(updatedManagerRole.position);
			expect(updatedManagerRole.position).toBeGreaterThan(updatedLowRole.position);
		});
		test('should allow reordering multiple roles below highest when unmanageable roles stay in place', async () => {
			const owner = await createTestAccount(harness);
			const manager = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const highRole = await createRole(harness, owner.token, guild.id, {name: 'High Role'});
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Manager',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			const lowA = await createRole(harness, owner.token, guild.id, {name: 'Low A'});
			const lowB = await createRole(harness, owner.token, guild.id, {name: 'Low B'});
			const lowC = await createRole(harness, owner.token, guild.id, {name: 'Low C'});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: highRole.id, position: 6},
				{id: managerRole.id, position: 5},
				{id: lowA.id, position: 4},
				{id: lowB.id, position: 3},
				{id: lowC.id, position: 2},
			]);
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, manager.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			await updateRolePositions(harness, manager.token, guild.id, [
				{id: lowC.id, position: 4},
				{id: lowA.id, position: 3},
				{id: lowB.id, position: 2},
			]);
			const roles = await getRoles(harness, owner.token, guild.id);
			const updatedA = roles.find((r) => r.id === lowA.id)!;
			const updatedB = roles.find((r) => r.id === lowB.id)!;
			const updatedC = roles.find((r) => r.id === lowC.id)!;
			expect(updatedC.position).toBeGreaterThan(updatedA.position);
			expect(updatedA.position).toBeGreaterThan(updatedB.position);
		});
		test('should allow reordering manageable roles beneath an equal-position locked prefix', async () => {
			const owner = await createTestAccount(harness);
			const manager = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Manager',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			const lowRoleA = await createRole(harness, owner.token, guild.id, {name: 'Low A'});
			const lowRoleB = await createRole(harness, owner.token, guild.id, {name: 'Low B'});
			const lowRoleC = await createRole(harness, owner.token, guild.id, {name: 'Low C'});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: managerRole.id, position: 4},
				{id: lowRoleA.id, position: 4},
				{id: lowRoleB.id, position: 4},
				{id: lowRoleC.id, position: 4},
			]);
			const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
			await acceptInvite(harness, manager.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			await createBuilder(harness, manager.token)
				.patch(`/guilds/${guild.id}/roles`)
				.body([
					{id: lowRoleC.id, position: 4},
					{id: lowRoleA.id, position: 3},
					{id: lowRoleB.id, position: 2},
				])
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const roles = await getRoles(harness, owner.token, guild.id);
			const updatedManagerRole = roles.find((role) => role.id === managerRole.id)!;
			const updatedLowRoleA = roles.find((role) => role.id === lowRoleA.id)!;
			const updatedLowRoleB = roles.find((role) => role.id === lowRoleB.id)!;
			const updatedLowRoleC = roles.find((role) => role.id === lowRoleC.id)!;
			expect(updatedManagerRole.position).toBeGreaterThan(updatedLowRoleC.position);
			expect(updatedLowRoleC.position).toBeGreaterThan(updatedLowRoleA.position);
			expect(updatedLowRoleA.position).toBeGreaterThan(updatedLowRoleB.position);
		});
	});
	describe('Permission Requirements', () => {
		test('should require MANAGE_ROLES permission to reorder roles', async () => {
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
		test('should reject reorder from a non-guild member', async () => {
			const owner = await createTestAccount(harness);
			const outsider = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const role = await createRole(harness, owner.token, guild.id, {name: 'Test Role'});
			await createBuilder(harness, outsider.token)
				.patch(`/guilds/${guild.id}/roles`)
				.body([{id: role.id, position: 5}])
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
	});
	describe('Validation', () => {
		test('should reject reordering @everyone role', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			await createBuilder(harness, owner.token)
				.patch(`/guilds/${guild.id}/roles`)
				.body([{id: guild.id, position: 5}])
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject reorder with invalid role ID', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			await createBuilder(harness, owner.token)
				.patch(`/guilds/${guild.id}/roles`)
				.body([{id: '999999999999999999', position: 1}])
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should accept reorder with no position changes (no-op)', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const roleA = await createRole(harness, owner.token, guild.id, {name: 'Role A'});
			const roleB = await createRole(harness, owner.token, guild.id, {name: 'Role B'});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: roleA.id, position: 2},
				{id: roleB.id, position: 1},
			]);
			const rolesBefore = await getRoles(harness, owner.token, guild.id);
			const beforeA = rolesBefore.find((r) => r.id === roleA.id)!;
			const beforeB = rolesBefore.find((r) => r.id === roleB.id)!;
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: roleA.id, position: 2},
				{id: roleB.id, position: 1},
			]);
			const rolesAfter = await getRoles(harness, owner.token, guild.id);
			const afterA = rolesAfter.find((r) => r.id === roleA.id)!;
			const afterB = rolesAfter.find((r) => r.id === roleB.id)!;
			expect(afterA.position).toBe(beforeA.position);
			expect(afterB.position).toBe(beforeB.position);
		});
	});
	describe('Position Assignment', () => {
		test('should keep @everyone at position 0 after reorder', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const roleA = await createRole(harness, owner.token, guild.id, {name: 'Role A'});
			const roleB = await createRole(harness, owner.token, guild.id, {name: 'Role B'});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: roleB.id, position: 2},
				{id: roleA.id, position: 1},
			]);
			const roles = await getRoles(harness, owner.token, guild.id);
			const everyone = roles.find((r) => r.id === guild.id)!;
			expect(everyone.position).toBe(0);
		});
		test('should assign positions to all roles after reorder', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const roleA = await createRole(harness, owner.token, guild.id, {name: 'Role A'});
			const roleB = await createRole(harness, owner.token, guild.id, {name: 'Role B'});
			const roleC = await createRole(harness, owner.token, guild.id, {name: 'Role C'});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: roleA.id, position: 3},
				{id: roleB.id, position: 2},
				{id: roleC.id, position: 1},
			]);
			const roles = await getRoles(harness, owner.token, guild.id);
			const nonEveryoneRoles = roles.filter((r) => r.id !== guild.id);
			for (const role of nonEveryoneRoles) {
				expect(role.position).toBeGreaterThan(0);
			}
			const positions = nonEveryoneRoles.map((r) => r.position);
			const uniquePositions = new Set(positions);
			expect(uniquePositions.size).toBe(nonEveryoneRoles.length);
		});
		test('should correctly order roles with a partial position update', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const roleA = await createRole(harness, owner.token, guild.id, {name: 'Role A'});
			const roleB = await createRole(harness, owner.token, guild.id, {name: 'Role B'});
			const roleC = await createRole(harness, owner.token, guild.id, {name: 'Role C'});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: roleA.id, position: 3},
				{id: roleB.id, position: 2},
				{id: roleC.id, position: 1},
			]);
			await updateRolePositions(harness, owner.token, guild.id, [{id: roleC.id, position: 4}]);
			const roles = await getRoles(harness, owner.token, guild.id);
			const updatedA = roles.find((r) => r.id === roleA.id)!;
			const updatedB = roles.find((r) => r.id === roleB.id)!;
			const updatedC = roles.find((r) => r.id === roleC.id)!;
			expect(updatedC.position).toBeGreaterThan(updatedA.position);
			expect(updatedA.position).toBeGreaterThan(updatedB.position);
		});
	});
});
