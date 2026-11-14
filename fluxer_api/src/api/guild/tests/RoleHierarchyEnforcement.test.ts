// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createGuild,
	createRole,
	getChannel,
	getMember,
	updateMember,
	updateRole,
	updateRolePositions,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Role Hierarchy Enforcement', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it('should allow moderator to modify lower role but not equal/higher roles', async () => {
		const owner = await createTestAccount(harness);
		const moderator = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Role Hierarchy Guild');
		const modRole = await createRole(harness, owner.token, guild.id, {
			name: 'Moderator',
			color: 65280,
			permissions: '268435456',
			hoist: true,
		});
		const memberRole = await createRole(harness, owner.token, guild.id, {
			name: 'Member',
			color: 16711680,
			permissions: '0',
			hoist: false,
		});
		await updateRolePositions(harness, owner.token, guild.id, [
			{id: modRole.id, position: 2},
			{id: memberRole.id, position: 1},
		]);
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, moderator.token, invite.code);
		await acceptInvite(harness, member.token, invite.code);
		await updateMember(harness, owner.token, guild.id, moderator.userId, {
			roles: [modRole.id],
		});
		await updateMember(harness, owner.token, guild.id, member.userId, {
			roles: [memberRole.id],
		});
		const updatedMemberRole = await updateRole(harness, moderator.token, guild.id, memberRole.id, {
			color: 255,
		});
		expect(updatedMemberRole.color).toBe(255);
		await createBuilder(harness, moderator.token)
			.patch(`/guilds/${guild.id}/roles/${modRole.id}`)
			.body({permissions: '8'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	it('should prevent member from assigning higher role to themselves via @me endpoint', async () => {
		const owner = await createTestAccount(harness);
		const moderator = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Role Hierarchy Guild');
		const modRole = await createRole(harness, owner.token, guild.id, {
			name: 'Moderator',
			color: 65280,
			permissions: '268435456',
			hoist: true,
		});
		const memberRole = await createRole(harness, owner.token, guild.id, {
			name: 'Member',
			color: 16711680,
			permissions: '0',
			hoist: false,
		});
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, moderator.token, invite.code);
		await acceptInvite(harness, member.token, invite.code);
		await updateMember(harness, owner.token, guild.id, moderator.userId, {
			roles: [modRole.id],
		});
		await updateMember(harness, owner.token, guild.id, member.userId, {
			roles: [memberRole.id],
		});
		await createBuilder(harness, member.token)
			.patch(`/guilds/${guild.id}/members/@me`)
			.body({roles: [modRole.id]})
			.expect(HTTP_STATUS.OK)
			.execute();
		const fetchedMember = await getMember(harness, owner.token, guild.id, member.userId);
		expect(fetchedMember.roles).not.toContain(modRole.id);
		expect(fetchedMember.roles).toContain(memberRole.id);
	});
	it('should allow assigning a lower same-position role by role ID tiebreak', async () => {
		const owner = await createTestAccount(harness);
		const moderator = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Role Hierarchy Guild');
		const modRole = await createRole(harness, owner.token, guild.id, {
			name: 'Moderator',
			permissions: '268435456',
		});
		const assignableRole = await createRole(harness, owner.token, guild.id, {
			name: 'Assignable',
			permissions: '0',
		});
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, moderator.token, invite.code);
		await acceptInvite(harness, member.token, invite.code);
		await updateMember(harness, owner.token, guild.id, moderator.userId, {
			roles: [modRole.id],
		});
		await updateRolePositions(harness, owner.token, guild.id, [
			{id: modRole.id, position: 2},
			{id: assignableRole.id, position: 2},
		]);
		await createBuilder(harness, moderator.token)
			.put(`/guilds/${guild.id}/members/${member.userId}/roles/${assignableRole.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const fetchedMember = await getMember(harness, owner.token, guild.id, member.userId);
		expect(fetchedMember.roles).toContain(assignableRole.id);
	});
	it('should prevent assigning roles to a target whose highest role outranks the caller', async () => {
		const owner = await createTestAccount(harness);
		const moderator = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Role Hierarchy Guild');
		const higherTargetRole = await createRole(harness, owner.token, guild.id, {
			name: 'Higher Target',
			permissions: '0',
		});
		const modRole = await createRole(harness, owner.token, guild.id, {
			name: 'Moderator',
			permissions: '268435456',
		});
		const assignableRole = await createRole(harness, owner.token, guild.id, {
			name: 'Assignable',
			permissions: '0',
		});
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, moderator.token, invite.code);
		await acceptInvite(harness, member.token, invite.code);
		await updateMember(harness, owner.token, guild.id, moderator.userId, {
			roles: [modRole.id],
		});
		await updateMember(harness, owner.token, guild.id, member.userId, {
			roles: [higherTargetRole.id],
		});
		await updateRolePositions(harness, owner.token, guild.id, [
			{id: higherTargetRole.id, position: 3},
			{id: modRole.id, position: 2},
			{id: assignableRole.id, position: 1},
		]);
		await createBuilder(harness, moderator.token)
			.put(`/guilds/${guild.id}/members/${member.userId}/roles/${assignableRole.id}`)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
			.execute();
		const fetchedMember = await getMember(harness, owner.token, guild.id, member.userId);
		expect(fetchedMember.roles).not.toContain(assignableRole.id);
		expect(fetchedMember.roles).toContain(higherTargetRole.id);
	});
	it('should still deny assigning a role at/above the caller regardless of target', async () => {
		const owner = await createTestAccount(harness);
		const moderator = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Role Hierarchy Guild');
		const aboveModRole = await createRole(harness, owner.token, guild.id, {
			name: 'Above Mod',
			permissions: '0',
		});
		const modRole = await createRole(harness, owner.token, guild.id, {
			name: 'Moderator',
			permissions: '268435456',
		});
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, moderator.token, invite.code);
		await acceptInvite(harness, member.token, invite.code);
		await updateMember(harness, owner.token, guild.id, moderator.userId, {
			roles: [modRole.id],
		});
		await updateRolePositions(harness, owner.token, guild.id, [
			{id: aboveModRole.id, position: 3},
			{id: modRole.id, position: 2},
		]);
		await createBuilder(harness, moderator.token)
			.put(`/guilds/${guild.id}/members/${member.userId}/roles/${aboveModRole.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	it('should reflect role position changes without requiring gateway restart', async () => {
		const owner = await createTestAccount(harness);
		const moderator = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Role Hierarchy Guild');
		const modRole = await createRole(harness, owner.token, guild.id, {
			name: 'Moderator',
			permissions: '268435456',
		});
		const targetRole = await createRole(harness, owner.token, guild.id, {
			name: 'Target',
			permissions: '0',
		});
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, moderator.token, invite.code);
		await acceptInvite(harness, member.token, invite.code);
		await updateMember(harness, owner.token, guild.id, moderator.userId, {
			roles: [modRole.id],
		});
		await updateRolePositions(harness, owner.token, guild.id, [
			{id: targetRole.id, position: 2},
			{id: modRole.id, position: 1},
		]);
		await createBuilder(harness, moderator.token)
			.put(`/guilds/${guild.id}/members/${member.userId}/roles/${targetRole.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await updateRolePositions(harness, owner.token, guild.id, [
			{id: modRole.id, position: 2},
			{id: targetRole.id, position: 1},
		]);
		await createBuilder(harness, moderator.token)
			.put(`/guilds/${guild.id}/members/${member.userId}/roles/${targetRole.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const fetchedMember = await getMember(harness, owner.token, guild.id, member.userId);
		expect(fetchedMember.roles).toContain(targetRole.id);
	});
});
