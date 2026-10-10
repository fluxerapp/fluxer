// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DiscoveryCategories} from '@fluxer/constants/src/DiscoveryConstants';
import type {DiscoveryApplicationResponse} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import {afterEach, beforeEach, describe, test} from 'vitest';

async function setGuildMemberCount(harness: ApiTestHarness, guildId: string, memberCount: number): Promise<void> {
	await createBuilder(harness, '')
		.post(`/test/guilds/${guildId}/member-count`)
		.body({member_count: memberCount})
		.execute();
}

describe('Discovery Application Validation', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('member count requirements', () => {
		test('should reject application with 0 members', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Empty Guild');
			await setGuildMemberCount(harness, guild.id, 0);
			await createBuilder(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'No members yet', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DISCOVERY_INSUFFICIENT_MEMBERS)
				.execute();
		});
	});
	describe('duplicate application', () => {
		test('should reject duplicate application when pending', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Dupe Pending Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'First application attempt', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Second application attempt', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.DISCOVERY_ALREADY_APPLIED)
				.execute();
		});
		test('should reject duplicate application when approved', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Dupe Approved Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Application to be approved', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Trying to reapply while approved', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.DISCOVERY_ALREADY_APPLIED)
				.execute();
		});
	});
	describe('permission requirements', () => {
		test('should require MANAGE_GUILD permission to apply', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder(harness, member.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Should not be allowed', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		});
		test('should require MANAGE_GUILD permission to edit', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Owner applied for discovery', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, member.token)
				.patch(`/guilds/${guild.id}/discovery`)
				.body({description: 'Member tries to edit'})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		});
		test('should require MANAGE_GUILD permission to withdraw', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Owner applied for discovery', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, member.token)
				.delete(`/guilds/${guild.id}/discovery`)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		});
		test('should require MANAGE_GUILD permission to get status', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await createBuilder(harness, member.token)
				.get(`/guilds/${guild.id}/discovery`)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		});
	});
	describe('edit restrictions', () => {
		test('should not allow editing rejected application', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Rejected Edit Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'To be rejected for edit test', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'rejected', reason: 'Not suitable'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, owner.token)
				.patch(`/guilds/${guild.id}/discovery`)
				.body({description: 'Trying to edit rejected'})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.DISCOVERY_APPLICATION_ALREADY_REVIEWED)
				.execute();
		});
	});
});
