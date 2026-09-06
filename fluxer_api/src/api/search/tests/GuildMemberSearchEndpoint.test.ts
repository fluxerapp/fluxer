// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildMFALevel, JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import type {GuildMemberSearchResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSearchSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, type TestAccount, totpCodeNow} from '../../auth/tests/AuthTestUtils';
import {createGuildID, createUserID} from '../../BrandedTypes';
import {GuildRepository} from '../../guild/repositories/GuildRepository';
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createGuild,
	createRole,
	setupTestGuildWithMembers,
	updateMember,
} from '../../guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS, wait} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';

const TOTP_SECRET = 'JBSWY3DPEHPK3PXP';

async function elevateGuildMfaLevel(harness: ApiTestHarness, owner: TestAccount, guildId: string): Promise<void> {
	await createBuilder(harness, owner.token)
		.post('/users/@me/mfa/totp/enable')
		.body({secret: TOTP_SECRET, code: totpCodeNow(TOTP_SECRET), password: owner.password})
		.expect(HTTP_STATUS.OK)
		.execute();
	const loginResp = await createBuilderWithoutAuth<{
		mfa: true;
		ticket: string;
	}>(harness)
		.post('/auth/login')
		.body({email: owner.email, password: owner.password})
		.expect(HTTP_STATUS.OK)
		.execute();
	const mfaResp = await createBuilderWithoutAuth<{
		token: string;
	}>(harness)
		.post('/auth/login/mfa/totp')
		.body({code: totpCodeNow(TOTP_SECRET), ticket: loginResp.ticket})
		.expect(HTTP_STATUS.OK)
		.execute();
	await createBuilder(harness, mfaResp.token)
		.patch(`/guilds/${guildId}`)
		.body({mfa_level: GuildMFALevel.ELEVATED, mfa_method: 'totp', mfa_code: totpCodeNow(TOTP_SECRET)})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function indexGuildMembers(harness: ApiTestHarness, guildId: string): Promise<void> {
	await createBuilder<{
		guild_id: string;
		indexed_at: string;
		members_indexed: number;
	}>(harness, '')
		.post(`/test/guilds/${guildId}/mark-members-indexed`)
		.body({})
		.execute();
	await wait(50);
}

describe('Guild Member Search Endpoint', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	describe('Authentication', () => {
		test('requires authentication (401 without token)', async () => {
			await createBuilder(harness, '')
				.post('/guilds/123456789/members-search')
				.body({query: 'test'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
		test('works with valid auth token (200)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Search Test Guild');
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, account.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({query: 'nonexistent'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			expect(result.guild_id).toBe(guild.id);
		});
	});
	describe('Indexing Status', () => {
		test('returns indexing true for unindexed guild', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Unindexed Guild');
			const result = await createBuilder<GuildMemberSearchResponse>(harness, account.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({query: 'test'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.indexing).toBe(true);
			expect(result.members).toEqual([]);
			expect(result.page_result_count).toBe(0);
			expect(result.total_result_count).toBe(0);
		});
		test('returns indexing false for indexed guild', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Indexed Guild');
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, account.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({query: 'test'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.indexing).toBe(false);
		});
	});
	describe('Basic Search', () => {
		test('returns empty results when no members match query', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Empty Results Guild');
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, account.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({query: 'nonexistent-user-12345'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members).toEqual([]);
			expect(result.page_result_count).toBe(0);
			expect(result.total_result_count).toBe(0);
			expect(result.indexing).toBe(false);
		});
		test('searches by username', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			await indexGuildMembers(harness, guild.id);
			const targetMember = members[0]!;
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({query: targetMember.username})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.indexing).toBe(false);
			expect(result.members.length).toBeGreaterThan(0);
			const found = result.members.find((m) => m.user_id === targetMember.userId);
			expect(found).toBeDefined();
			expect(found?.username).toBe(targetMember.username);
		});
		test('searches by nickname', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			const targetMember = members[0]!;
			const uniqueNickname = `TestNick-${Date.now()}`;
			await updateMember(harness, owner.token, guild.id, targetMember.userId, {
				nick: uniqueNickname,
			});
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({query: uniqueNickname})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.indexing).toBe(false);
			expect(result.members.length).toBeGreaterThan(0);
			const found = result.members.find((m) => m.user_id === targetMember.userId);
			expect(found).toBeDefined();
			expect(found?.nickname).toBe(uniqueNickname);
		});
		test('searches by user ID', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			await indexGuildMembers(harness, guild.id);
			const targetMember = members[0]!;
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({query: targetMember.userId})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.indexing).toBe(false);
			expect(result.members.length).toBeGreaterThan(0);
			const found = result.members.find((m) => m.user_id === targetMember.userId);
			expect(found).toBeDefined();
			expect(found?.user_id).toBe(targetMember.userId);
		});
		test('returns proper response structure', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 2);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toHaveProperty('guild_id');
			expect(result).toHaveProperty('members');
			expect(result).toHaveProperty('page_result_count');
			expect(result).toHaveProperty('total_result_count');
			expect(result).toHaveProperty('indexing');
			expect(Array.isArray(result.members)).toBe(true);
			expect(typeof result.page_result_count).toBe('number');
			expect(typeof result.total_result_count).toBe('number');
			expect(typeof result.indexing).toBe('boolean');
		});
		test('member objects have correct structure', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 1);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members.length).toBeGreaterThan(0);
			const member = result.members[0]!;
			expect(member).toHaveProperty('id');
			expect(member).toHaveProperty('guild_id');
			expect(member).toHaveProperty('user_id');
			expect(member).toHaveProperty('username');
			expect(member).toHaveProperty('global_name');
			expect(member).toHaveProperty('nickname');
			expect(member).toHaveProperty('role_ids');
			expect(member).toHaveProperty('joined_at');
			expect(member).toHaveProperty('supplemental');
			expect(member.supplemental).toHaveProperty('join_source_type');
			expect(member.supplemental).toHaveProperty('source_invite_code');
			expect(member.supplemental).toHaveProperty('inviter_id');
			expect(member).toHaveProperty('is_bot');
			expect(Array.isArray(member.role_ids)).toBe(true);
		});
	});
	describe('Pagination', () => {
		test('respects limit parameter', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 5);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({limit: 3})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.page_result_count).toBeLessThanOrEqual(3);
			expect(result.members.length).toBeLessThanOrEqual(3);
		});
		test('respects offset parameter', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 5);
			await indexGuildMembers(harness, guild.id);
			const firstPage = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({limit: 2, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			const secondPage = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({limit: 2, offset: 2})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(firstPage.members.length).toBeGreaterThan(0);
			expect(secondPage.members.length).toBeGreaterThan(0);
			const firstPageIds = firstPage.members.map((m) => m.user_id);
			const secondPageIds = secondPage.members.map((m) => m.user_id);
			const overlap = firstPageIds.filter((id) => secondPageIds.includes(id));
			expect(overlap.length).toBe(0);
		});
		test('returns consistent total_result_count across pages', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 5);
			await indexGuildMembers(harness, guild.id);
			const firstPage = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({limit: 2, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			const secondPage = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({limit: 2, offset: 2})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(firstPage.total_result_count).toBe(secondPage.total_result_count);
		});
	});
	describe('Sort Options', () => {
		test('accepts sort_by and sort_order parameters', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 3);
			await indexGuildMembers(harness, guild.id);
			const resultAsc = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({sort_by: 'joinedAt', sort_order: 'asc'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const resultDesc = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({sort_by: 'joinedAt', sort_order: 'desc'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(resultAsc.members.length).toBeGreaterThan(0);
			expect(resultDesc.members.length).toBeGreaterThan(0);
			if (resultAsc.members.length > 1) {
				const firstAsc = resultAsc.members[0]!;
				const lastAsc = resultAsc.members[resultAsc.members.length - 1]!;
				expect(firstAsc.joined_at).toBeLessThanOrEqual(lastAsc.joined_at);
			}
		});
	});
	describe('Bot Filtering', () => {
		test('filters by is_bot parameter', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 2);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({is_bot: false})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.indexing).toBe(false);
			for (const member of result.members) {
				expect(member.is_bot).toBe(false);
			}
		});
	});
	describe('Empty Query', () => {
		test('returns all members with empty query', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.indexing).toBe(false);
			expect(result.members.length).toBeGreaterThanOrEqual(members.length + 1);
		});
		test('returns all members with no query field', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.indexing).toBe(false);
			expect(result.members.length).toBeGreaterThanOrEqual(members.length + 1);
		});
	});
	describe('Permissions', () => {
		test('rejects member without member management permissions (403)', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const regularMember = members[0]!;
			await indexGuildMembers(harness, guild.id);
			await createBuilder(harness, regularMember.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
				.execute();
		});
		test('allows guild owner to search', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 1);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members.length).toBeGreaterThan(0);
		});
		test('rejects an elevated-permission holder without MFA in an elevated guild (400)', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const moderator = members[0]!;
			const banRole = await createRole(harness, owner.token, guild.id, {
				name: 'Ban Members',
				permissions: Permissions.BAN_MEMBERS.toString(),
			});
			await updateMember(harness, owner.token, guild.id, moderator.userId, {roles: [banRole.id]});
			await indexGuildMembers(harness, guild.id);
			await elevateGuildMfaLevel(harness, owner, guild.id);
			await createBuilder(harness, moderator.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST, 'TWO_FACTOR_REQUIRED')
				.execute();
		});
		test('allows a MANAGE_NICKNAMES holder without MFA in an elevated guild', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const nicknameManager = members[0]!;
			const nicknameRole = await createRole(harness, owner.token, guild.id, {
				name: 'Manage Nicknames',
				permissions: Permissions.MANAGE_NICKNAMES.toString(),
			});
			await updateMember(harness, owner.token, guild.id, nicknameManager.userId, {roles: [nicknameRole.id]});
			await indexGuildMembers(harness, guild.id);
			await elevateGuildMfaLevel(harness, owner, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, nicknameManager.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members.length).toBeGreaterThan(0);
		});
		test('returns 404 for non-guild-member', async () => {
			const {guild} = await setupTestGuildWithMembers(harness, 1);
			const outsider = await createTestAccount(harness);
			await indexGuildMembers(harness, guild.id);
			await createBuilder(harness, outsider.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_GUILD')
				.execute();
		});
	});
	describe('Role Filtering', () => {
		test('filters by role_ids for assigned roles', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 3);
			const role1 = await createRole(harness, owner.token, guild.id, {name: 'Role 1'});
			const role2 = await createRole(harness, owner.token, guild.id, {name: 'Role 2'});
			await updateMember(harness, owner.token, guild.id, members[0]!.userId, {
				roles: [role1.id],
			});
			await updateMember(harness, owner.token, guild.id, members[1]!.userId, {
				roles: [role2.id],
			});
			await updateMember(harness, owner.token, guild.id, members[2]!.userId, {
				roles: [role1.id, role2.id],
			});
			await indexGuildMembers(harness, guild.id);
			const role1Results = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({role_ids: [role1.id]})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(role1Results.members.length).toBe(2);
			const role1UserIds = role1Results.members.map((m) => m.user_id);
			expect(role1UserIds).toContain(members[0]!.userId);
			expect(role1UserIds).toContain(members[2]!.userId);
			const role2Results = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({role_ids: [role2.id]})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(role2Results.members.length).toBe(2);
			const role2UserIds = role2Results.members.map((m) => m.user_id);
			expect(role2UserIds).toContain(members[1]!.userId);
			expect(role2UserIds).toContain(members[2]!.userId);
			const bothRolesResults = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({role_ids: [role1.id, role2.id]})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(bothRolesResults.members.length).toBe(1);
			expect(bothRolesResults.members[0]!.user_id).toBe(members[2]!.userId);
		});
		test('filters by role_ids returns empty for unassigned role', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 3);
			const role = await createRole(harness, owner.token, guild.id, {name: 'Unassigned Role'});
			await indexGuildMembers(harness, guild.id);
			const filtered = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({role_ids: [role.id]})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(filtered.members.length).toBe(0);
			expect(filtered.total_result_count).toBe(0);
		});
	});
	describe('Timestamp Filtering', () => {
		test('filters by joined_at_gte', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 2);
			await indexGuildMembers(harness, guild.id);
			const allResults = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({sort_by: 'joinedAt', sort_order: 'asc'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(allResults.members.length).toBeGreaterThanOrEqual(2);
			const earliestJoinedAt = allResults.members[0]!.joined_at;
			const filterTimestamp = earliestJoinedAt + 1;
			const filtered = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({joined_at_gte: filterTimestamp})
				.expect(HTTP_STATUS.OK)
				.execute();
			for (const member of filtered.members) {
				expect(member.joined_at).toBeGreaterThanOrEqual(filterTimestamp);
			}
			expect(filtered.total_result_count).toBeLessThan(allResults.total_result_count);
		});
		test('filters by joined_at_lte', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 2);
			await indexGuildMembers(harness, guild.id);
			const allResults = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({sort_by: 'joinedAt', sort_order: 'asc'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(allResults.members.length).toBeGreaterThanOrEqual(2);
			const latestJoinedAt = allResults.members[allResults.members.length - 1]!.joined_at;
			const filterTimestamp = latestJoinedAt - 1;
			const filtered = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({joined_at_lte: filterTimestamp})
				.expect(HTTP_STATUS.OK)
				.execute();
			for (const member of filtered.members) {
				expect(member.joined_at).toBeLessThanOrEqual(filterTimestamp);
			}
			expect(filtered.total_result_count).toBeLessThan(allResults.total_result_count);
		});
	});
	describe('User Creation Timestamp Filtering', () => {
		test('filters by user_created_at_gte', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 3);
			await indexGuildMembers(harness, guild.id);
			const allResults = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(allResults.members.length).toBeGreaterThanOrEqual(3);
			const recentTimestamp = Math.floor(Date.now() / 1000) - 3600;
			const filtered = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({user_created_at_gte: recentTimestamp})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(filtered.members.length).toBeGreaterThan(0);
			expect(filtered.total_result_count).toBeLessThanOrEqual(allResults.total_result_count);
		});
		test('filters by user_created_at_lte', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 3);
			await indexGuildMembers(harness, guild.id);
			const allResults = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(allResults.members.length).toBeGreaterThanOrEqual(3);
			const futureTimestamp = Math.floor(Date.now() / 1000) + 3600;
			const filtered = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({user_created_at_lte: futureTimestamp})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(filtered.total_result_count).toBe(allResults.total_result_count);
		});
		test('combines user_created_at_gte and user_created_at_lte', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 3);
			await indexGuildMembers(harness, guild.id);
			const recentTimestamp = Math.floor(Date.now() / 1000) - 3600;
			const futureTimestamp = Math.floor(Date.now() / 1000) + 3600;
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({
					user_created_at_gte: recentTimestamp,
					user_created_at_lte: futureTimestamp,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members.length).toBeGreaterThan(0);
		});
	});
	describe('Join Source Filtering', () => {
		test('filters by join_source_type', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 2);
			await indexGuildMembers(harness, guild.id);
			const allResults = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
			const filtered = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({join_source_type: [1]})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(filtered.total_result_count).toBeLessThan(allResults.total_result_count);
		});
		test('returns discovery join sources without response validation errors', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const discoveryMember = members[0]!;
			const guildRepository = new GuildRepository();
			const guildId = createGuildID(BigInt(guild.id));
			const userId = createUserID(BigInt(discoveryMember.userId));
			const member = await guildRepository.getMember(guildId, userId);
			expect(member).not.toBeNull();
			await guildRepository.upsertMember({
				...member!.toRow(),
				join_source_type: JoinSourceTypes.DISCOVERY,
			});
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({query: discoveryMember.username})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members).toContainEqual(
				expect.objectContaining({
					user_id: discoveryMember.userId,
					supplemental: expect.objectContaining({join_source_type: JoinSourceTypes.DISCOVERY}),
				}),
			);
		});
		test('filters by source_invite_code', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 2);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({source_invite_code: ['nonexistent-code']})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members.length).toBe(0);
		});
	});
	describe('Combined Filters', () => {
		test('combines multiple filters together', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 3);
			await indexGuildMembers(harness, guild.id);
			const allResults = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({sort_by: 'joinedAt', sort_order: 'asc'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(allResults.members.length).toBeGreaterThanOrEqual(3);
			const earliestJoinedAt = allResults.members[0]!.joined_at;
			const filterTimestamp = earliestJoinedAt + 1;
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({is_bot: false, joined_at_gte: filterTimestamp})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.total_result_count).toBeLessThan(allResults.total_result_count);
			for (const member of result.members) {
				expect(member.is_bot).toBe(false);
				expect(member.joined_at).toBeGreaterThanOrEqual(filterTimestamp);
			}
		});
	});
	describe('Sort Verification', () => {
		test('sort by joinedAt ascending returns chronological order', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 3);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({sort_by: 'joinedAt', sort_order: 'asc'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members.length).toBeGreaterThan(1);
			for (let i = 1; i < result.members.length; i++) {
				expect(result.members[i]!.joined_at).toBeGreaterThanOrEqual(result.members[i - 1]!.joined_at);
			}
		});
		test('sort by joinedAt descending returns reverse chronological order', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 3);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({sort_by: 'joinedAt', sort_order: 'desc'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members.length).toBeGreaterThan(1);
			for (let i = 1; i < result.members.length; i++) {
				expect(result.members[i]!.joined_at).toBeLessThanOrEqual(result.members[i - 1]!.joined_at);
			}
		});
	});
	describe('Search Quality', () => {
		test('searches by global name', async () => {
			const uniqueGlobalName = `SearchTarget-${Date.now()}`;
			const targetAccount = await createTestAccount(harness, {globalName: uniqueGlobalName});
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Global Name Search Guild');
			const channel = await createChannel(harness, owner.token, guild.id, 'general');
			const invite = await createChannelInvite(harness, owner.token, channel.id);
			await acceptInvite(harness, targetAccount.token, invite.code);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({query: uniqueGlobalName})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members.length).toBeGreaterThan(0);
			const found = result.members.find((m) => m.user_id === targetAccount.userId);
			expect(found).toBeDefined();
			expect(found?.global_name).toBe(uniqueGlobalName);
		});
		test('partial username match works', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
			await indexGuildMembers(harness, guild.id);
			const targetMember = members[0]!;
			const username = targetMember.username!;
			const partial = username.slice(0, Math.max(3, Math.floor(username.length / 2)));
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({query: partial})
				.expect(HTTP_STATUS.OK)
				.execute();
			const found = result.members.find((m) => m.user_id === targetMember.userId);
			expect(found).toBeDefined();
		});
		test('infix username match works', async () => {
			const fragment = Date.now().toString(36);
			const targetUsername = `pre${fragment}post`;
			const targetAccount = await createTestAccount(harness, {username: targetUsername});
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Infix Username Search Guild');
			const channel = await createChannel(harness, owner.token, guild.id, 'general');
			const invite = await createChannelInvite(harness, owner.token, channel.id);
			await acceptInvite(harness, targetAccount.token, invite.code);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({query: fragment})
				.expect(HTTP_STATUS.OK)
				.execute();
			const found = result.members.find((m) => m.user_id === targetAccount.userId);
			expect(found).toBeDefined();
			expect(found?.username).toBe(targetUsername);
		});
	});
	describe('Edge Cases', () => {
		test('handles guild with only the owner gracefully', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Solo Owner Guild');
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members.length).toBe(1);
			expect(result.members[0]!.user_id).toBe(owner.userId);
		});
	});
});
