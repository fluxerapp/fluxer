// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DiscoveryCategories, DiscoveryCategoryLabels} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildVerificationLevel} from '@fluxer/constants/src/GuildConstants';
import type {
	DiscoveryApplicationResponse,
	DiscoveryCategoryResponse,
	DiscoveryGuildListResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {createTestBotAccount} from '../../bot/tests/BotTestUtils';
import {banUser} from '../../moderation/tests/ModerationTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {createGuild, getUserGuilds} from './GuildTestUtils';

async function setGuildMemberCount(harness: ApiTestHarness, guildId: string, memberCount: number): Promise<void> {
	await createBuilder(harness, '')
		.post(`/test/guilds/${guildId}/member-count`)
		.body({member_count: memberCount})
		.execute();
}

async function applyAndApprove(
	harness: ApiTestHarness,
	ownerToken: string,
	adminToken: string,
	guildId: string,
	description: string,
	categoryId: number,
): Promise<void> {
	await createBuilder<DiscoveryApplicationResponse>(harness, ownerToken)
		.post(`/guilds/${guildId}/discovery`)
		.body({description, category_type: categoryId})
		.expect(HTTP_STATUS.OK)
		.execute();
	await createBuilder(harness, `${adminToken}`)
		.post(`/admin/discovery/applications/${guildId}/approve`)
		.body({})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('Discovery Search and Join', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('categories', () => {
		test('should list all discovery categories', async () => {
			const user = await createTestAccount(harness);
			const categories = await createBuilder<Array<DiscoveryCategoryResponse>>(harness, user.token)
				.get('/discovery/categories')
				.expect(HTTP_STATUS.OK)
				.execute();
			const expectedCount = Object.keys(DiscoveryCategoryLabels).length;
			expect(categories).toHaveLength(expectedCount);
			for (const category of categories) {
				expect(category.id).toBeTypeOf('number');
				expect(category.name).toBeTypeOf('string');
				expect(category.name.length).toBeGreaterThan(0);
			}
		});
		test('should include known categories', async () => {
			const user = await createTestAccount(harness);
			const categories = await createBuilder<Array<DiscoveryCategoryResponse>>(harness, user.token)
				.get('/discovery/categories')
				.expect(HTTP_STATUS.OK)
				.execute();
			const names = categories.map((c) => c.name);
			expect(names).toContain('Gaming');
			expect(names).toContain('Music');
			expect(names).toContain('Education');
			expect(names).toContain('Science & Technology');
		});
		test('should require login to list categories', async () => {
			await createBuilderWithoutAuth(harness).get('/discovery/categories').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		});
	});
	describe('search', () => {
		test('should return empty results when no guilds are discoverable', async () => {
			const user = await createTestAccount(harness);
			const results = await createBuilder<DiscoveryGuildListResponse>(harness, user.token)
				.get('/discovery/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(results.guilds).toHaveLength(0);
			expect(results.total).toBe(0);
		});
		test('should return approved guilds in search results', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Searchable Guild');
			await setGuildMemberCount(harness, guild.id, 10);
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			await applyAndApprove(
				harness,
				owner.token,
				admin.token,
				guild.id,
				'A searchable community for all',
				DiscoveryCategories.GAMING,
			);
			const searcher = await createTestAccount(harness);
			const results = await createBuilder<DiscoveryGuildListResponse>(harness, searcher.token)
				.get('/discovery/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(results.guilds.length).toBeGreaterThanOrEqual(1);
			const found = results.guilds.find((g) => g.id === guild.id);
			expect(found).toBeDefined();
			expect(found!.name).toBe('Searchable Guild');
			expect(found!.description).toBe('A searchable community for all');
			expect(found!.category_type).toBe(DiscoveryCategories.GAMING);
			expect(found!.verification_level).toBe(GuildVerificationLevel.LOW);
		});
		test('should report category counts that ignore the selected category filter', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			const gamingOwner = await createTestAccount(harness);
			const gamingGuild = await createGuild(harness, gamingOwner.token, 'Counted Gaming Guild');
			await setGuildMemberCount(harness, gamingGuild.id, 10);
			await applyAndApprove(
				harness,
				gamingOwner.token,
				admin.token,
				gamingGuild.id,
				'A gaming community for counting',
				DiscoveryCategories.GAMING,
			);
			const musicOwner = await createTestAccount(harness);
			const musicGuild = await createGuild(harness, musicOwner.token, 'Counted Music Guild');
			await setGuildMemberCount(harness, musicGuild.id, 10);
			await applyAndApprove(
				harness,
				musicOwner.token,
				admin.token,
				musicGuild.id,
				'A music community for counting',
				DiscoveryCategories.MUSIC,
			);

			const searcher = await createTestAccount(harness);
			const filtered = await createBuilder<DiscoveryGuildListResponse>(harness, searcher.token)
				.get(`/discovery/guilds?category=${DiscoveryCategories.GAMING}`)
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(filtered.guilds.every((guild) => guild.category_type === DiscoveryCategories.GAMING)).toBe(true);
			const gamingCount = filtered.category_counts.find((entry) => entry.category_type === DiscoveryCategories.GAMING);
			const musicCount = filtered.category_counts.find((entry) => entry.category_type === DiscoveryCategories.MUSIC);
			expect(gamingCount?.count).toBeGreaterThanOrEqual(1);
			expect(musicCount?.count).toBeGreaterThanOrEqual(1);
		});

		test('should expose the guild banner hash in search results', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Bannered Guild');
			await setGuildMemberCount(harness, guild.id, 10);
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			await applyAndApprove(
				harness,
				owner.token,
				admin.token,
				guild.id,
				'A community with a banner',
				DiscoveryCategories.GAMING,
			);
			const searcher = await createTestAccount(harness);
			const results = await createBuilder<DiscoveryGuildListResponse>(harness, searcher.token)
				.get('/discovery/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			const found = results.guilds.find((entry) => entry.id === guild.id);
			expect(found).toBeDefined();
			expect(found).toHaveProperty('banner');
		});

		test('should not return pending guilds in search results', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Pending Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Pending application guild', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			const searcher = await createTestAccount(harness);
			const results = await createBuilder<DiscoveryGuildListResponse>(harness, searcher.token)
				.get('/discovery/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			const found = results.guilds.find((g) => g.id === guild.id);
			expect(found).toBeUndefined();
		});
		test('should filter by category', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			const owner1 = await createTestAccount(harness);
			const gamingGuild = await createGuild(harness, owner1.token, 'Gaming Community');
			await setGuildMemberCount(harness, gamingGuild.id, 10);
			await applyAndApprove(
				harness,
				owner1.token,
				admin.token,
				gamingGuild.id,
				'All about gaming',
				DiscoveryCategories.GAMING,
			);
			const owner2 = await createTestAccount(harness);
			const musicGuild = await createGuild(harness, owner2.token, 'Music Community');
			await setGuildMemberCount(harness, musicGuild.id, 10);
			await applyAndApprove(
				harness,
				owner2.token,
				admin.token,
				musicGuild.id,
				'All about music',
				DiscoveryCategories.MUSIC,
			);
			const searcher = await createTestAccount(harness);
			const results = await createBuilder<DiscoveryGuildListResponse>(harness, searcher.token)
				.get(`/discovery/guilds?category=${DiscoveryCategories.GAMING}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			for (const guild of results.guilds) {
				expect(guild.category_type).toBe(DiscoveryCategories.GAMING);
			}
		});
		test('should respect limit parameter', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			for (let i = 0; i < 3; i++) {
				const owner = await createTestAccount(harness);
				const guild = await createGuild(harness, owner.token, `Limit Test Guild ${i}`);
				await setGuildMemberCount(harness, guild.id, 10);
				await applyAndApprove(
					harness,
					owner.token,
					admin.token,
					guild.id,
					`Community number ${i} for testing`,
					DiscoveryCategories.GAMING,
				);
			}
			const searcher = await createTestAccount(harness);
			const results = await createBuilder<DiscoveryGuildListResponse>(harness, searcher.token)
				.get('/discovery/guilds?limit=2')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(results.guilds.length).toBeLessThanOrEqual(2);
		});
		test('should require login to search', async () => {
			await createBuilderWithoutAuth(harness).get('/discovery/guilds').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		});
	});
	describe('join', () => {
		test('should join a discoverable guild', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Joinable Guild');
			await setGuildMemberCount(harness, guild.id, 10);
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			await applyAndApprove(
				harness,
				owner.token,
				admin.token,
				guild.id,
				'Join this community',
				DiscoveryCategories.GAMING,
			);
			const joiner = await createTestAccount(harness);
			await createBuilder(harness, joiner.token)
				.post(`/discovery/guilds/${guild.id}/join`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const guilds = await getUserGuilds(harness, joiner.token);
			const joined = guilds.find((g) => g.id === guild.id);
			expect(joined).toBeDefined();
		});
		test('should block discovery join for same /64 IPv6 guild ban', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'IPv6 Discovery Ban');
			await setGuildMemberCount(harness, guild.id, 10);
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			await applyAndApprove(
				harness,
				owner.token,
				admin.token,
				guild.id,
				'Join this community',
				DiscoveryCategories.GAMING,
			);
			const bannedUser = await createTestAccount(harness, {ipAddress: '2a01:e0a:d10:95b0:9231:a3e4:939:e8e3'});
			await createBuilder(harness, bannedUser.token)
				.post(`/discovery/guilds/${guild.id}/join`)
				.header('x-forwarded-for', bannedUser.ipAddress!)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await banUser(harness, owner.token, guild.id, bannedUser.userId, 0);
			const altUser = await createTestAccount(harness, {ipAddress: '2a01:e0a:d10:95b0:2415:acac:7521:2b4b'});
			await createBuilder(harness, altUser.token)
				.post(`/discovery/guilds/${guild.id}/join`)
				.header('x-forwarded-for', altUser.ipAddress!)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.USER_IP_BANNED_FROM_GUILD)
				.execute();
		});
		test('should not allow joining non-discoverable guild', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Not Discoverable');
			const joiner = await createTestAccount(harness);
			await createBuilder(harness, joiner.token)
				.post(`/discovery/guilds/${guild.id}/join`)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE)
				.execute();
		});
		test('should not allow joining guild with only pending application', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Pending Join Guild');
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Pending but not yet approved', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			const joiner = await createTestAccount(harness);
			await createBuilder(harness, joiner.token)
				.post(`/discovery/guilds/${guild.id}/join`)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE)
				.execute();
		});
		test('should not allow joining with nonexistent guild ID', async () => {
			const joiner = await createTestAccount(harness);
			await createBuilder(harness, joiner.token)
				.post(`/discovery/guilds/${TEST_IDS.NONEXISTENT_GUILD}/join`)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE)
				.execute();
		});
		test('should not allow bot accounts to join discoverable guilds', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'No Bots Guild');
			await setGuildMemberCount(harness, guild.id, 10);
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review']);
			await applyAndApprove(
				harness,
				owner.token,
				admin.token,
				guild.id,
				'No bots allowed to join via discovery',
				DiscoveryCategories.GAMING,
			);
			const botAccount = await createTestBotAccount(harness);
			const botToken = `Bot ${botAccount.botToken}`;
			await createBuilder(harness, botToken)
				.post(`/discovery/guilds/${guild.id}/join`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should require login to join', async () => {
			await createBuilderWithoutAuth(harness)
				.post(`/discovery/guilds/${TEST_IDS.NONEXISTENT_GUILD}/join`)
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
});
