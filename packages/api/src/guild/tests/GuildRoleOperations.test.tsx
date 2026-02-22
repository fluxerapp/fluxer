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
import {createGuildID, createRoleID} from '@fluxer/api/src/BrandedTypes';
import {GuildRoleRepository} from '@fluxer/api/src/guild/repositories/GuildRoleRepository';
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
} from '@fluxer/api/src/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Guild Role Operations', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('should delete a role', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const role = await createRole(harness, account.token, guild.id, {
			name: 'Delete Me',
		});

		await deleteRole(harness, account.token, guild.id, role.id);

		const roles = await getRoles(harness, account.token, guild.id);
		expect(roles.find((r) => r.id === role.id)).toBeUndefined();
	});

	test('should not delete @everyone role', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		await createBuilder(harness, account.token)
			.delete(`/guilds/${guild.id}/roles/${guild.id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	test('should update role positions', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const role1 = await createRole(harness, account.token, guild.id, {name: 'Role 1'});
		const role2 = await createRole(harness, account.token, guild.id, {name: 'Role 2'});

		await updateRolePositions(harness, account.token, guild.id, [
			{id: role1.id, position: 2},
			{id: role2.id, position: 1},
		]);
	});

	test('should require MANAGE_ROLES permission to create role', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);

		await createBuilder(harness, member.token)
			.post(`/guilds/${guild.id}/roles`)
			.body({name: 'Unauthorized Role'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	test('should require MANAGE_ROLES permission to delete role', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const role = await createRole(harness, owner.token, guild.id, {name: 'Protected Role'});

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);

		await createBuilder(harness, member.token)
			.delete(`/guilds/${guild.id}/roles/${role.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	test('should create role with unicode_emoji', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const role = await createRole(harness, account.token, guild.id, {
			name: 'Emoji Role',
			unicode_emoji: '',
		});

		expect(role.name).toBe('Emoji Role');
	});

	test('should validate role name length', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		await createBuilder(harness, account.token)
			.post(`/guilds/${guild.id}/roles`)
			.body({name: 'a'.repeat(101)})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	test('should require a name when creating role', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		await createBuilder(harness, account.token)
			.post(`/guilds/${guild.id}/roles`)
			.body({})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	test('should allow MANAGE_ROLES role to create roles', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const managerRole = await createRole(harness, owner.token, guild.id, {
			name: 'Role Manager',
			permissions: Permissions.MANAGE_ROLES.toString(),
		});

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);

		await addMemberRole(harness, owner.token, guild.id, member.userId, managerRole.id);

		const newRole = await createRole(harness, member.token, guild.id, {
			name: 'Member Created Role',
		});

		expect(newRole.name).toBe('Member Created Role');
	});

	test('should preserve concurrent position updates when applying stale role snapshot', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const role = await createRole(harness, account.token, guild.id, {
			name: 'Role',
		});

		const guildId = createGuildID(BigInt(guild.id));
		const roleId = createRoleID(BigInt(role.id));
		const roleRepository = new GuildRoleRepository();
		const staleRole = await roleRepository.getRole(roleId, guildId);
		expect(staleRole).toBeDefined();
		if (!staleRole) {
			return;
		}

		const staleRoleRow = staleRole.toRow();
		const movedRole = await roleRepository.upsertRole(
			{
				...staleRoleRow,
				position: staleRoleRow.position + 5,
			},
			staleRoleRow,
		);

		await roleRepository.upsertRole(
			{
				...staleRoleRow,
				name: 'Renamed Role',
			},
			staleRoleRow,
		);

		const finalRole = await roleRepository.getRole(roleId, guildId);
		expect(finalRole?.name).toBe('Renamed Role');
		expect(finalRole?.position).toBe(movedRole.position);
	});
});
