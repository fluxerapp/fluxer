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

import {createTestAccount, setUserACLs} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {createGuild} from '@fluxer/api/src/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@fluxer/api/src/test/TestRequestBuilder';
import type {GuildAdminResponse} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import type {UserAdminResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface UserSearchResponse {
	users: Array<UserAdminResponse>;
	total: number;
}

interface GuildSearchResponse {
	guilds: Array<GuildAdminResponse>;
	total: number;
}

async function setContactInfo(
	harness: ApiTestHarness,
	userId: string,
	data: {phone?: string | null; email?: string | null},
): Promise<void> {
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${userId}/set-contact-info`)
		.body(data)
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('Admin Search Field Coverage', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'meilisearch'});
	});

	afterEach(async () => {
		await harness.shutdown();
	});

	describe('user search by email', () => {
		test('searching by exact email returns only the matching user', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const uniqueEmail = `precise-email-${Date.now()}@searchtest.example`;
			const targetUser = await createTestAccount(harness, {email: uniqueEmail});
			await createTestAccount(harness, {email: `other-user-${Date.now()}@different.example`});

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: uniqueEmail, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.total).toBeGreaterThanOrEqual(1);
			const found = result.users.find((u) => u.id === targetUser.userId);
			expect(found).toBeDefined();
			expect(found!.email).toBe(uniqueEmail);
		});

		test('searching by email does not return users with different emails', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const emailA = `alpha-${Date.now()}@emailsearch.example`;
			const emailB = `bravo-${Date.now()}@emailsearch.example`;
			const userA = await createTestAccount(harness, {email: emailA});
			const userB = await createTestAccount(harness, {email: emailB});

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: emailA, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const foundA = result.users.find((u) => u.id === userA.userId);
			const foundB = result.users.find((u) => u.id === userB.userId);
			expect(foundA).toBeDefined();
			expect(foundB).toBeUndefined();
		});

		test('searching by partial email domain returns matching users', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const domain = `partialdomain${Date.now()}.example`;
			const user = await createTestAccount(harness, {email: `user@${domain}`});

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: domain, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.total).toBeGreaterThanOrEqual(1);
			const found = result.users.find((u) => u.id === user.userId);
			expect(found).toBeDefined();
		});
	});

	describe('user search by username', () => {
		test('searching by exact username returns only the matching user', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const uniqueUsername = `xuniq_${Date.now()}`;
			const targetUser = await createTestAccount(harness, {username: uniqueUsername});
			await createTestAccount(harness, {username: `yother_${Date.now()}`});

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: uniqueUsername, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.total).toBeGreaterThanOrEqual(1);
			const found = result.users.find((u) => u.id === targetUser.userId);
			expect(found).toBeDefined();
			expect(found!.username).toBe(uniqueUsername);
		});

		test('searching by username does not return users with completely different usernames', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const ts = Date.now();
			const usernameA = `zephyrfox_${ts}`;
			const usernameB = `quasarmoon_${ts}`;
			const userA = await createTestAccount(harness, {username: usernameA});
			const userB = await createTestAccount(harness, {username: usernameB});

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: usernameA, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const foundA = result.users.find((u) => u.id === userA.userId);
			const foundB = result.users.find((u) => u.id === userB.userId);
			expect(foundA).toBeDefined();
			expect(foundB).toBeUndefined();
		});
	});

	describe('user search by user ID', () => {
		test('searching by exact user ID returns only the matching user', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const targetUser = await createTestAccount(harness);
			await createTestAccount(harness);

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: targetUser.userId, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.total).toBeGreaterThanOrEqual(1);
			const found = result.users.find((u) => u.id === targetUser.userId);
			expect(found).toBeDefined();
			expect(found!.id).toBe(targetUser.userId);
		});

		test('user ID search returns the user even when meilisearch has no match via direct DB lookup', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const targetUser = await createTestAccount(harness);

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: targetUser.userId, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.total).toBeGreaterThanOrEqual(1);
			const found = result.users.find((u) => u.id === targetUser.userId);
			expect(found).toBeDefined();
		});

		test('user ID search does not duplicate user when found by both meilisearch and direct lookup', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const targetUser = await createTestAccount(harness);

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: targetUser.userId, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const matchingUsers = result.users.filter((u) => u.id === targetUser.userId);
			expect(matchingUsers).toHaveLength(1);
		});
	});

	describe('user search by phone number', () => {
		test('searching by phone number returns only the matching user', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const phone = `+1555${Date.now().toString().slice(-7)}`;
			const targetUser = await createTestAccount(harness);
			await setContactInfo(harness, targetUser.userId, {phone});

			const otherUser = await createTestAccount(harness);
			await setContactInfo(harness, otherUser.userId, {phone: `+4420${Date.now().toString().slice(-7)}`});

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: phone, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.total).toBeGreaterThanOrEqual(1);
			const found = result.users.find((u) => u.id === targetUser.userId);
			expect(found).toBeDefined();
			expect(found!.phone).toBe(phone);

			const otherFound = result.users.find((u) => u.id === otherUser.userId);
			expect(otherFound).toBeUndefined();
		});

		test('searching by phone does not return users without a phone', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const phone = `+1888${Date.now().toString().slice(-7)}`;
			const userWithPhone = await createTestAccount(harness);
			await setContactInfo(harness, userWithPhone.userId, {phone});

			const userWithoutPhone = await createTestAccount(harness);

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: phone, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.total).toBeGreaterThanOrEqual(1);
			const foundWithPhone = result.users.find((u) => u.id === userWithPhone.userId);
			const foundWithoutPhone = result.users.find((u) => u.id === userWithoutPhone.userId);
			expect(foundWithPhone).toBeDefined();
			expect(foundWithoutPhone).toBeUndefined();
		});
	});

	describe('user search response fields', () => {
		test('user search response includes all expected admin fields', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const email = `fields-check-${Date.now()}@fieldtest.example`;
			const username = `fieldcheck_${Date.now()}`;
			const targetUser = await createTestAccount(harness, {email, username});

			const phone = `+1999${Date.now().toString().slice(-7)}`;
			await setContactInfo(harness, targetUser.userId, {phone});

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: email, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const found = result.users.find((u) => u.id === targetUser.userId);
			expect(found).toBeDefined();
			expect(found!.id).toBe(targetUser.userId);
			expect(found!.username).toBe(username);
			expect(found!.email).toBe(email);
			expect(found!.phone).toBe(phone);
			expect(found!).toHaveProperty('discriminator');
			expect(found!).toHaveProperty('global_name');
			expect(found!).toHaveProperty('flags');
			expect(found!).toHaveProperty('email_verified');
			expect(found!).toHaveProperty('email_bounced');
			expect(found!).toHaveProperty('premium_type');
			expect(found!).toHaveProperty('acls');
			expect(found!).toHaveProperty('suspicious_activity_flags');
		});
	});

	describe('user search isolation across fields', () => {
		test('each searchable field returns only the correct user', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const ts = Date.now();
			const emailA = `vortexfind-${ts}@alphadomain.example`;
			const emailB = `nebulaseek-${ts}@betadomain.example`;
			const usernameA = `vortexfox_${ts}`;
			const usernameB = `nebulawolf_${ts}`;
			const phoneA = `+33612345${ts.toString().slice(-4)}`;
			const phoneB = `+81907654${ts.toString().slice(-4)}`;

			const userA = await createTestAccount(harness, {email: emailA, username: usernameA});
			const userB = await createTestAccount(harness, {email: emailB, username: usernameB});

			await setContactInfo(harness, userA.userId, {phone: phoneA});
			await setContactInfo(harness, userB.userId, {phone: phoneB});

			const searchByEmailA = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: emailA, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(searchByEmailA.users.find((u) => u.id === userA.userId)).toBeDefined();
			expect(searchByEmailA.users.find((u) => u.id === userB.userId)).toBeUndefined();

			const searchByUsernameB = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: usernameB, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(searchByUsernameB.users.find((u) => u.id === userB.userId)).toBeDefined();
			expect(searchByUsernameB.users.find((u) => u.id === userA.userId)).toBeUndefined();

			const searchByPhoneA = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: phoneA, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(searchByPhoneA.users.find((u) => u.id === userA.userId)).toBeDefined();
			expect(searchByPhoneA.users.find((u) => u.id === userB.userId)).toBeUndefined();

			const searchByIdB = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: userB.userId, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(searchByIdB.users.find((u) => u.id === userB.userId)).toBeDefined();
			expect(searchByIdB.users.find((u) => u.id === userA.userId)).toBeUndefined();
		});
	});

	describe('guild search by name', () => {
		test('searching by exact guild name returns only the matching guild', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);

			const ts = Date.now();
			const nameA = `Zephyrion Stronghold ${ts}`;
			const nameB = `Quasarwave Citadel ${ts}`;
			const guildA = await createGuild(harness, admin.token, nameA);
			const guildB = await createGuild(harness, admin.token, nameB);

			const result = await createBuilder<GuildSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/guilds/search')
				.body({query: nameA, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.total).toBeGreaterThanOrEqual(1);
			const foundA = result.guilds.find((g) => g.id === guildA.id);
			const foundB = result.guilds.find((g) => g.id === guildB.id);
			expect(foundA).toBeDefined();
			expect(foundA!.name).toBe(nameA);
			expect(foundB).toBeUndefined();
		});

		test('searching by guild name does not return guilds with different names', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);

			const ts = Date.now();
			const uniqueNameA = `Xanthium ${ts}`;
			const uniqueNameB = `Ytterbium ${ts}`;
			const guildA = await createGuild(harness, admin.token, uniqueNameA);
			const guildB = await createGuild(harness, admin.token, uniqueNameB);

			const result = await createBuilder<GuildSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/guilds/search')
				.body({query: uniqueNameA, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const foundA = result.guilds.find((g) => g.id === guildA.id);
			const foundB = result.guilds.find((g) => g.id === guildB.id);
			expect(foundA).toBeDefined();
			expect(foundB).toBeUndefined();
		});
	});

	describe('guild search by ID', () => {
		test('searching by exact guild ID returns only the matching guild', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);

			const guildA = await createGuild(harness, admin.token, `ID Search A ${Date.now()}`);
			const guildB = await createGuild(harness, admin.token, `ID Search B ${Date.now()}`);

			const result = await createBuilder<GuildSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/guilds/search')
				.body({query: guildA.id, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.total).toBeGreaterThanOrEqual(1);
			const foundA = result.guilds.find((g) => g.id === guildA.id);
			const foundB = result.guilds.find((g) => g.id === guildB.id);
			expect(foundA).toBeDefined();
			expect(foundA!.id).toBe(guildA.id);
			expect(foundB).toBeUndefined();
		});

		test('guild ID search at non-zero offset does not perform direct DB lookup', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);

			const guild = await createGuild(harness, admin.token, `Offset Guild ${Date.now()}`);

			const result = await createBuilder<GuildSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/guilds/search')
				.body({query: guild.id, limit: 10, offset: 1})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.guilds).toEqual([]);
			expect(result.total).toBe(0);
		});
	});

	describe('guild search response fields', () => {
		test('guild search response includes all expected admin fields', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);

			const guildName = `Field Check Guild ${Date.now()}`;
			const guild = await createGuild(harness, admin.token, guildName);

			const result = await createBuilder<GuildSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/guilds/search')
				.body({query: guildName, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const found = result.guilds.find((g) => g.id === guild.id);
			expect(found).toBeDefined();
			expect(found!.id).toBe(guild.id);
			expect(found!.name).toBe(guildName);
			expect(found!.owner_id).toBe(admin.userId);
			expect(found!).toHaveProperty('features');
			expect(found!).toHaveProperty('icon');
			expect(found!).toHaveProperty('banner');
			expect(found!).toHaveProperty('member_count');
			expect(found!.member_count).toBeGreaterThanOrEqual(1);
		});

		test('guild search returns correct owner_id for each guild', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);

			const otherOwner = await createTestAccount(harness);
			const ts = Date.now();
			const guildByAdmin = await createGuild(harness, admin.token, `Admin Owned ${ts}`);
			const guildByOther = await createGuild(harness, otherOwner.token, `Other Owned ${ts}`);

			const resultAdmin = await createBuilder<GuildSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/guilds/search')
				.body({query: `Admin Owned ${ts}`, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const foundAdmin = resultAdmin.guilds.find((g) => g.id === guildByAdmin.id);
			expect(foundAdmin).toBeDefined();
			expect(foundAdmin!.owner_id).toBe(admin.userId);

			const resultOther = await createBuilder<GuildSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/guilds/search')
				.body({query: `Other Owned ${ts}`, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const foundOther = resultOther.guilds.find((g) => g.id === guildByOther.id);
			expect(foundOther).toBeDefined();
			expect(foundOther!.owner_id).toBe(otherOwner.userId);
		});
	});

	describe('omitted and no-match queries', () => {
		test('omitted query on user search returns users', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			await createTestAccount(harness);

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.total).toBeGreaterThanOrEqual(1);
			expect(result.users.length).toBeGreaterThanOrEqual(1);
		});

		test('omitted query on guild search returns guilds', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);

			await createGuild(harness, admin.token, `Omitted Query Guild ${Date.now()}`);

			const result = await createBuilder<GuildSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/guilds/search')
				.body({limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.total).toBeGreaterThanOrEqual(1);
			expect(result.guilds.length).toBeGreaterThanOrEqual(1);
		});

		test('completely unrelated query returns no users', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			const result = await createBuilder<UserSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/users/search')
				.body({query: 'zzz-impossible-match-query-xyzzy-99999', limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.users).toEqual([]);
			expect(result.total).toBe(0);
		});

		test('completely unrelated query returns no guilds', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);

			const result = await createBuilder<GuildSearchResponse>(harness, `Bearer ${admin.token}`)
				.post('/admin/guilds/search')
				.body({query: 'zzz-impossible-match-query-xyzzy-99999', limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(result.guilds).toEqual([]);
			expect(result.total).toBe(0);
		});
	});
});
