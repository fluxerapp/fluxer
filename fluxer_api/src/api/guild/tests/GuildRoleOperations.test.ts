// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createRoleID} from '@app/api/BrandedTypes';
import {GuildRoleRepository} from '@app/api/guild/repositories/GuildRoleRepository';
import {
	acceptInvite,
	createChannelInvite,
	createGuild,
	createRole,
	getChannel,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Guild Role Operations', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should not delete @everyone role', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');
		await createBuilder(harness, account.token)
			.delete(`/guilds/${guild.id}/roles/${guild.id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
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
