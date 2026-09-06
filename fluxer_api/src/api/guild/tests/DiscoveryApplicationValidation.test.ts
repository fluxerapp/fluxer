// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DiscoveryCategories} from '@fluxer/constants/src/DiscoveryConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {
	DiscoveryApplicationResponse,
	DiscoveryStatusResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {createGuild, setupTestGuildWithMembers} from './GuildTestUtils';

type FormBodyError = {
	code: string;
	errors: Array<{
		path: string;
		code: string;
	}>;
};

function expectCategoryTypeFormatError(json: FormBodyError): void {
	expect(json.code).toBe(APIErrorCodes.INVALID_FORM_BODY);
	const categoryError = json.errors.find((entry) => entry.path.includes('category_type'));
	expect(categoryError).toBeDefined();
	expect(categoryError?.code).toBe(ValidationErrorCodes.INVALID_FORMAT);
}

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
		test('should allow application with 1 member in test mode', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Small Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			const application = await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Small but active community', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(application.status).toBe('pending');
		});
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
	describe('category validation', () => {
		test('should reject invalid category ID above range', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Bad Category Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			const {json} = await createBuilder<FormBodyError>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Valid description here', category_type: 99})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.executeWithResponse();
			expectCategoryTypeFormatError(json);
		});
		test('should reject negative category ID', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Negative Cat Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			const {json} = await createBuilder<FormBodyError>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Valid description here', category_type: -1})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.executeWithResponse();
			expectCategoryTypeFormatError(json);
		});
		test('should reject invalid category on edit', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Edit Bad Cat Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Valid description here', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			const {json} = await createBuilder<FormBodyError>(harness, owner.token)
				.patch(`/guilds/${guild.id}/discovery`)
				.body({category_type: 99})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.executeWithResponse();
			expectCategoryTypeFormatError(json);
		});
	});
	describe('description validation', () => {
		test('should reject description shorter than minimum length', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Short Desc Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Too short', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject description longer than maximum length', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Long Desc Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'A'.repeat(301), category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject missing description', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'No Desc Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject missing category_type', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'No Cat Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Valid description here'})
				.expect(HTTP_STATUS.BAD_REQUEST)
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
				.post(`/admin/discovery/applications/${guild.id}/approve`)
				.body({})
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
	describe('authentication requirements', () => {
		test('should require login to apply', async () => {
			await createBuilderWithoutAuth(harness)
				.post(`/guilds/${TEST_IDS.NONEXISTENT_GUILD}/discovery`)
				.body({description: 'No auth attempt', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
		test('should require login to get status', async () => {
			await createBuilderWithoutAuth(harness)
				.get(`/guilds/${TEST_IDS.NONEXISTENT_GUILD}/discovery`)
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
		test('should require login to edit', async () => {
			await createBuilderWithoutAuth(harness)
				.patch(`/guilds/${TEST_IDS.NONEXISTENT_GUILD}/discovery`)
				.body({description: 'No auth edit'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
		test('should require login to withdraw', async () => {
			await createBuilderWithoutAuth(harness)
				.delete(`/guilds/${TEST_IDS.NONEXISTENT_GUILD}/discovery`)
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
	describe('non-existent application', () => {
		test('should return null application when none exists', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'No App Guild');
			const status = await createBuilder<DiscoveryStatusResponse>(harness, owner.token)
				.get(`/guilds/${guild.id}/discovery`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(status.application).toBeNull();
			expect(status.min_member_count).toBeGreaterThan(0);
		});
		test('should return error when editing non-existent application', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'No Edit App Guild');
			await createBuilder(harness, owner.token)
				.patch(`/guilds/${guild.id}/discovery`)
				.body({description: 'Editing nothing'})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.DISCOVERY_APPLICATION_NOT_FOUND)
				.execute();
		});
		test('should return error when withdrawing non-existent application', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'No Withdraw App Guild');
			await createBuilder(harness, owner.token)
				.delete(`/guilds/${guild.id}/discovery`)
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.DISCOVERY_APPLICATION_NOT_FOUND)
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
				.post(`/admin/discovery/applications/${guild.id}/reject`)
				.body({reason: 'Not suitable'})
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
