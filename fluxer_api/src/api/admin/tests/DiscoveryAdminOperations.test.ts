// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID} from '@app/api/BrandedTypes';
import {GuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import {createGuild, getGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {DiscoveryApplicationStatus, DiscoveryCategories} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {
	DiscoveryAdminListedGuildResponse,
	DiscoveryApplicationResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import type {z} from 'zod';

async function setGuildMemberCount(harness: ApiTestHarness, guildId: string, memberCount: number): Promise<void> {
	await createBuilder(harness, '')
		.post(`/test/guilds/${guildId}/member-count`)
		.body({member_count: memberCount})
		.execute();
}

async function createGuildWithApplication(
	harness: ApiTestHarness,
	name: string,
	description = 'Valid discovery description',
	categoryId = DiscoveryCategories.GAMING,
): Promise<{
	owner: TestAccount;
	guild: GuildResponse;
	application: DiscoveryApplicationResponse;
}> {
	const owner = await createTestAccount(harness);
	const guild = await createGuild(harness, owner.token, name);
	await setGuildMemberCount(harness, guild.id, 10);
	const application = await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
		.post(`/guilds/${guild.id}/discovery`)
		.body({description, category_type: categoryId})
		.expect(HTTP_STATUS.OK)
		.execute();
	return {owner, guild, application};
}

const SEEDED_LISTING_BASE_GUILD_ID = 900000000000000000n;

async function seedApprovedListings(count: number): Promise<void> {
	const discoveryRepository = new GuildDiscoveryRepository();
	for (let i = 0; i < count; i++) {
		const appliedAt = new Date(1700000000000 + i);
		await discoveryRepository.upsert({
			guild_id: createGuildID(SEEDED_LISTING_BASE_GUILD_ID + BigInt(i)),
			status: DiscoveryApplicationStatus.APPROVED,
			category_type: DiscoveryCategories.GAMING,
			description: `Seeded discovery listing ${i}`,
			primary_language: null,
			custom_tags: [],
			applied_at: appliedAt,
			reviewed_at: appliedAt,
			reviewed_by: null,
			review_reason: null,
			removed_at: null,
			removed_by: null,
			removal_reason: null,
		});
	}
}

async function createAdminWithACLs(harness: ApiTestHarness, acls: Array<string>): Promise<TestAccount> {
	const admin = await createTestAccount(harness);
	return setUserACLs(harness, admin, ['admin:authenticate', ...acls]);
}

describe('Discovery Admin Operations', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('approve', () => {
		test('should add DISCOVERABLE feature to guild on approval', async () => {
			const {owner, guild} = await createGuildWithApplication(harness, 'Feature Add Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const guildData = await getGuild(harness, owner.token, guild.id);
			expect(guildData.features).toContain(GuildFeatures.DISCOVERABLE);
		});
	});
	describe('remove', () => {
		test('should remove an approved guild from discovery', async () => {
			const {owner, guild} = await createGuildWithApplication(harness, 'Remove Test Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:remove', 'discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<DiscoveryApplicationResponse>(harness, `${admin.token}`)
				.delete(`/admin/discovery/listings/${guild.id}`)
				.body({reason: 'Violated community guidelines'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.status).toBe('removed');
			const guildData = await getGuild(harness, owner.token, guild.id);
			expect(guildData.features).not.toContain(GuildFeatures.DISCOVERABLE);
		});
	});
	describe('list listed guilds', () => {
		test('returns every approved listing when there are more than a thousand', async () => {
			await seedApprovedListings(1001);
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			const results = await createBuilder<Array<z.infer<typeof DiscoveryAdminListedGuildResponse>>>(
				harness,
				`${admin.token}`,
			)
				.get('/admin/discovery/listings')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(results).toHaveLength(1001);
			const workerRows = await new GuildDiscoveryRepository().listByStatus(DiscoveryApplicationStatus.APPROVED);
			expect(workerRows).toHaveLength(1001);
		});
	});
	describe('ACL requirements', () => {
		test('should require DISCOVERY_REVIEW ACL to list applications', async () => {
			const admin = await createAdminWithACLs(harness, ['user:lookup']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/discovery/applications')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/discovery/listings')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should require DISCOVERY_REVIEW ACL to approve', async () => {
			const {guild} = await createGuildWithApplication(harness, 'ACL Approve Guild');
			const admin = await createAdminWithACLs(harness, ['user:lookup']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should require DISCOVERY_REVIEW ACL to reject', async () => {
			const {guild} = await createGuildWithApplication(harness, 'ACL Reject Guild');
			const admin = await createAdminWithACLs(harness, ['user:lookup']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'rejected', reason: 'Not allowed'})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should require DISCOVERY_REMOVE ACL to remove', async () => {
			const {guild} = await createGuildWithApplication(harness, 'ACL Remove Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, `${admin.token}`)
				.delete(`/admin/discovery/listings/${guild.id}`)
				.body({reason: 'Not allowed to remove'})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should require DISCOVERY_REVIEW ACL to read categories and category listings', async () => {
			const admin = await createAdminWithACLs(harness, ['user:lookup']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/discovery/categories')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/discovery/categories/${DiscoveryCategories.GAMING}/listings`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should require DISCOVERY_REVIEW ACL to edit and move listings', async () => {
			const admin = await createAdminWithACLs(harness, ['user:lookup']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/listings/${TEST_IDS.NONEXISTENT_GUILD}`)
				.body({description: 'A corrected discovery description'})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
			await createBuilder(harness, `${admin.token}`)
				.patch('/admin/discovery/listings')
				.body({guild_ids: [TEST_IDS.NONEXISTENT_GUILD], category_type: DiscoveryCategories.MUSIC})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
});
