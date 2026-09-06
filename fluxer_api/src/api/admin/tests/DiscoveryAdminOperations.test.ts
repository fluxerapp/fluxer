// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DiscoveryApplicationStatus, DiscoveryCategories} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {
	DiscoveryAdminListedGuildResponse,
	DiscoveryAdminListingBulkCategoryResponse,
	DiscoveryAdminPendingApplicationResponse,
	DiscoveryApplicationResponse,
	DiscoveryCategoryListResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import type {z} from 'zod';
import {createTestAccount, setUserACLs, type TestAccount} from '../../auth/tests/AuthTestUtils';
import {createGuildID} from '../../BrandedTypes';
import {GuildDiscoveryRepository} from '../../guild/repositories/GuildDiscoveryRepository';
import {createGuild, getGuild} from '../../guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';

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
		test('should approve a pending application', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Approve Test Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review']);
			const result = await createBuilder<DiscoveryApplicationResponse>(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved', reason: 'Meets all requirements'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.status).toBe('approved');
			expect(result.reviewed_at).toBeTruthy();
			expect(result.review_reason).toBe('Meets all requirements');
		});
		test('should approve without a reason', async () => {
			const {guild} = await createGuildWithApplication(harness, 'No Reason Approve Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review']);
			const result = await createBuilder<DiscoveryApplicationResponse>(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.status).toBe('approved');
			expect(result.review_reason).toBeNull();
		});
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
		test('should not allow approving already approved application', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Double Approve Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.DISCOVERY_APPLICATION_ALREADY_REVIEWED)
				.execute();
		});
		test('should not allow approving non-existent application', async () => {
			const admin = await createAdminWithACLs(harness, ['discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${TEST_IDS.NONEXISTENT_GUILD}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.DISCOVERY_APPLICATION_NOT_FOUND)
				.execute();
		});
	});
	describe('reject', () => {
		test('should reject a pending application with reason', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Reject Test Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review']);
			const result = await createBuilder<DiscoveryApplicationResponse>(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'rejected', reason: 'Description is too vague'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.status).toBe('rejected');
			expect(result.reviewed_at).toBeTruthy();
			expect(result.review_reason).toBe('Description is too vague');
		});
		test('should require reason for rejection', async () => {
			const {guild} = await createGuildWithApplication(harness, 'No Reason Reject Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'rejected'})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should not allow rejecting already rejected application', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Double Reject Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'rejected', reason: 'First rejection'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'rejected', reason: 'Second rejection'})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.DISCOVERY_APPLICATION_ALREADY_REVIEWED)
				.execute();
		});
		test('should not allow rejecting approved application', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Approved Then Reject Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'rejected', reason: 'Changed my mind'})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.DISCOVERY_APPLICATION_ALREADY_REVIEWED)
				.execute();
		});
		test('should not allow rejecting non-existent application', async () => {
			const admin = await createAdminWithACLs(harness, ['discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${TEST_IDS.NONEXISTENT_GUILD}`)
				.body({status: 'rejected', reason: 'Does not exist'})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.DISCOVERY_APPLICATION_NOT_FOUND)
				.execute();
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
		test('should require reason for removal', async () => {
			const {guild} = await createGuildWithApplication(harness, 'No Reason Remove Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:remove', 'discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, `${admin.token}`)
				.delete(`/admin/discovery/listings/${guild.id}`)
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should not allow removing a pending application', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Remove Pending Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:remove', 'discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.delete(`/admin/discovery/listings/${guild.id}`)
				.body({reason: 'Not approved yet'})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE)
				.execute();
		});
		test('should not allow removing non-existent application', async () => {
			const admin = await createAdminWithACLs(harness, ['discovery:remove']);
			await createBuilder(harness, `${admin.token}`)
				.delete(`/admin/discovery/listings/${TEST_IDS.NONEXISTENT_GUILD}`)
				.body({reason: 'Does not exist'})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.DISCOVERY_APPLICATION_NOT_FOUND)
				.execute();
		});
	});
	describe('list pending applications', () => {
		test('returns all pending applications enriched with guild metadata', async () => {
			const created = await createGuildWithApplication(harness, 'List Test Guild 1');
			await createGuildWithApplication(harness, 'List Test Guild 2');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			const results = await createBuilder<Array<z.infer<typeof DiscoveryAdminPendingApplicationResponse>>>(
				harness,
				`${admin.token}`,
			)
				.get('/admin/discovery/applications')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(results.length).toBeGreaterThanOrEqual(2);
			const found = results.find((r) => r.guild_id === created.guild.id);
			expect(found).toBeDefined();
			expect(found?.guild_name).toBe('List Test Guild 1');
			expect(found?.guild_owner_id).toBe(created.owner.userId);
			expect(found?.description).toBe('Valid discovery description');
		});
		test('excludes applications that are no longer pending', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Excluded From Pending');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const results = await createBuilder<Array<z.infer<typeof DiscoveryAdminPendingApplicationResponse>>>(
				harness,
				`${admin.token}`,
			)
				.get('/admin/discovery/applications')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(results.find((r) => r.guild_id === guild.id)).toBeUndefined();
		});
		test('returns empty list when no pending applications exist', async () => {
			await harness.reset();
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			const results = await createBuilder<Array<z.infer<typeof DiscoveryAdminPendingApplicationResponse>>>(
				harness,
				`${admin.token}`,
			)
				.get('/admin/discovery/applications')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(results).toHaveLength(0);
		});
	});
	describe('list listed guilds', () => {
		test('returns all approved discovery guilds with approval timestamps', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Listed Guild A');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const results = await createBuilder<Array<z.infer<typeof DiscoveryAdminListedGuildResponse>>>(
				harness,
				`${admin.token}`,
			)
				.get('/admin/discovery/listings')
				.expect(HTTP_STATUS.OK)
				.execute();
			const found = results.find((r) => r.guild_id === guild.id);
			expect(found).toBeDefined();
			expect(found?.guild_name).toBe('Listed Guild A');
			expect(found?.approved_at).not.toBeNull();
		});
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
		test('does not include pending or removed guilds', async () => {
			const {guild: pendingGuild} = await createGuildWithApplication(harness, 'Still Pending');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:remove', 'discovery:review']);
			const {guild: removedGuild} = await createGuildWithApplication(harness, 'Will Be Removed');
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${removedGuild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, `${admin.token}`)
				.delete(`/admin/discovery/listings/${removedGuild.id}`)
				.body({reason: 'cleanup'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const results = await createBuilder<Array<z.infer<typeof DiscoveryAdminListedGuildResponse>>>(
				harness,
				`${admin.token}`,
			)
				.get('/admin/discovery/listings')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(results.find((r) => r.guild_id === pendingGuild.id)).toBeUndefined();
			expect(results.find((r) => r.guild_id === removedGuild.id)).toBeUndefined();
		});
	});
	describe('list categories', () => {
		test('returns every discovery category a listing can be filed under', async () => {
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			const results = await createBuilder<z.infer<typeof DiscoveryCategoryListResponse>>(harness, `${admin.token}`)
				.get('/admin/discovery/categories')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(results.find((category) => category.id === DiscoveryCategories.GAMING)?.name).toBe('Gaming');
			expect(results.find((category) => category.id === DiscoveryCategories.OTHER)?.name).toBe('Other');
		});
	});
	describe('list category listings', () => {
		test('returns the listings filed under one category', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Category Listing Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const results = await createBuilder<Array<z.infer<typeof DiscoveryAdminListedGuildResponse>>>(
				harness,
				`${admin.token}`,
			)
				.get(`/admin/discovery/categories/${DiscoveryCategories.GAMING}/listings`)
				.expect(HTTP_STATUS.OK)
				.execute();
			const found = results.find((r) => r.guild_id === guild.id);
			expect(found).toBeDefined();
			expect(found?.category_type).toBe(DiscoveryCategories.GAMING);
		});
		test('excludes listings filed under another category', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Other Category Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const results = await createBuilder<Array<z.infer<typeof DiscoveryAdminListedGuildResponse>>>(
				harness,
				`${admin.token}`,
			)
				.get(`/admin/discovery/categories/${DiscoveryCategories.MUSIC}/listings`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(results.find((r) => r.guild_id === guild.id)).toBeUndefined();
		});
		test('rejects an unserved category identifier', async () => {
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/discovery/categories/99/listings')
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
	describe('update listing', () => {
		test('edits the listing copy of an approved guild', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Editable Listing Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<DiscoveryApplicationResponse>(harness, `${admin.token}`)
				.patch(`/admin/discovery/listings/${guild.id}`)
				.body({description: 'A corrected discovery description', category_type: DiscoveryCategories.MUSIC})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.description).toBe('A corrected discovery description');
			expect(result.category_type).toBe(DiscoveryCategories.MUSIC);
			expect(result.status).toBe('approved');
		});
		test('rejects an unserved category identifier', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Bad Category Listing Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/listings/${guild.id}`)
				.body({category_type: 99})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should not allow updating a non-existent application', async () => {
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/listings/${TEST_IDS.NONEXISTENT_GUILD}`)
				.body({description: 'A corrected discovery description'})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.DISCOVERY_APPLICATION_NOT_FOUND)
				.execute();
		});
	});
	describe('bulk move listings', () => {
		test('moves every named listing to one category', async () => {
			const first = await createGuildWithApplication(harness, 'Bulk Move Guild A');
			const second = await createGuildWithApplication(harness, 'Bulk Move Guild B');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			for (const guildId of [first.guild.id, second.guild.id]) {
				await createBuilder(harness, `${admin.token}`)
					.patch(`/admin/discovery/applications/${guildId}`)
					.body({status: 'approved'})
					.expect(HTTP_STATUS.OK)
					.execute();
			}
			const result = await createBuilder<z.infer<typeof DiscoveryAdminListingBulkCategoryResponse>>(
				harness,
				`${admin.token}`,
			)
				.patch('/admin/discovery/listings')
				.body({guild_ids: [first.guild.id, second.guild.id], category_type: DiscoveryCategories.EDUCATION})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.updated).toBe(2);
			expect(result.failed_guild_ids).toHaveLength(0);
			const listings = await createBuilder<Array<z.infer<typeof DiscoveryAdminListedGuildResponse>>>(
				harness,
				`${admin.token}`,
			)
				.get(`/admin/discovery/categories/${DiscoveryCategories.EDUCATION}/listings`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(listings.find((r) => r.guild_id === first.guild.id)).toBeDefined();
			expect(listings.find((r) => r.guild_id === second.guild.id)).toBeDefined();
		});
		test('reports the guilds that could not be moved', async () => {
			const {guild} = await createGuildWithApplication(harness, 'Bulk Move Partial Guild');
			const admin = await createAdminWithACLs(harness, ['discovery:review', 'discovery:review']);
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/discovery/applications/${guild.id}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<z.infer<typeof DiscoveryAdminListingBulkCategoryResponse>>(
				harness,
				`${admin.token}`,
			)
				.patch('/admin/discovery/listings')
				.body({
					guild_ids: [guild.id, TEST_IDS.NONEXISTENT_GUILD],
					category_type: DiscoveryCategories.SCIENCE_AND_TECHNOLOGY,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.updated).toBe(1);
			expect(result.failed_guild_ids).toEqual([TEST_IDS.NONEXISTENT_GUILD]);
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
		test('should require authentication for admin endpoints', async () => {
			await createBuilderWithoutAuth(harness)
				.get('/admin/discovery/applications')
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
			await createBuilderWithoutAuth(harness)
				.patch(`/admin/discovery/applications/${TEST_IDS.NONEXISTENT_GUILD}`)
				.body({status: 'approved'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
			await createBuilderWithoutAuth(harness)
				.patch(`/admin/discovery/applications/${TEST_IDS.NONEXISTENT_GUILD}`)
				.body({status: 'rejected', reason: 'test'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
			await createBuilderWithoutAuth(harness)
				.delete(`/admin/discovery/listings/${TEST_IDS.NONEXISTENT_GUILD}`)
				.body({reason: 'test'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
});
