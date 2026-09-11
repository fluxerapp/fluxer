// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DELETED_USER_ID} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {createUserID} from '../../BrandedTypes';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {UserRepository} from '../../user/repositories/UserRepository';

const SYNTHETIC_USER_IDS = ['0', String(DELETED_USER_ID)];

const MUTATIONS: Array<{verb: 'put' | 'patch' | 'delete'; path: string; acl: string; body?: unknown}> = [
	{verb: 'patch', path: 'username', acl: AdminACLs.USER_UPDATE_USERNAME, body: {username: 'takenover'}},
	{verb: 'patch', path: 'email', acl: AdminACLs.USER_UPDATE_EMAIL, body: {email: 'takenover@example.com'}},
	{verb: 'patch', path: 'flags', acl: AdminACLs.USER_UPDATE_FLAGS, body: {add_flags: ['STAFF'], remove_flags: []}},
	{verb: 'put', path: 'acls', acl: AdminACLs.ACL_SET_USER, body: {acls: [AdminACLs.AUTHENTICATE]}},
	{verb: 'put', path: 'ban', acl: AdminACLs.USER_TEMP_BAN, body: {duration_hours: 1, reason: 'test'}},
	{verb: 'put', path: 'deletion', acl: AdminACLs.USER_DELETE, body: {delay_days: 1}},
	{verb: 'delete', path: 'profile-fields', acl: AdminACLs.USER_UPDATE_PROFILE, body: {fields: ['bio']}},
	{verb: 'put', path: 'bot-status', acl: AdminACLs.USER_UPDATE_BOT_STATUS, body: {bot: true}},
	{verb: 'put', path: 'system-status', acl: AdminACLs.USER_UPDATE_BOT_STATUS, body: {system: true}},
];

const CASES = SYNTHETIC_USER_IDS.flatMap((userId) =>
	MUTATIONS.map((mutation) => ({
		userId,
		...mutation,
		name: `${mutation.verb.toUpperCase()} ${mutation.path} on ${userId}`,
	})),
);

describe('admin mutations against synthetic accounts', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	test.each(CASES)('$name is rejected and writes no row', async (testCase) => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, testCase.acl]);
		const request = createBuilder(harness, `${admin.token}`)[testCase.verb](
			`/admin/users/${testCase.userId}/${testCase.path}`,
		);
		if (testCase.body !== undefined) {
			request.body(testCase.body);
		}
		await request.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_USER').execute();
		expect(await new UserRepository().listUsers([createUserID(BigInt(testCase.userId))])).toEqual([]);
	});
	test.each(SYNTHETIC_USER_IDS)('reading user %s stays a 200 with no results', async (userId) => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
		const result = await createBuilder<{users: Array<{id: string}>}>(harness, `${admin.token}`)
			.get(`/admin/users/${userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.users).toEqual([]);
	});
	test.each(['%200', '0%0A', '%091', '1%20'])('padded user id %s is rejected the same way', async (encodedUserId) => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_UPDATE_USERNAME]);
		await createBuilder(harness, `${admin.token}`)
			.patch(`/admin/users/${encodedUserId}/username`)
			.body({username: 'takenover'})
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_USER')
			.execute();
		expect(await new UserRepository().listUsers([createUserID(0n), createUserID(DELETED_USER_ID)])).toEqual([]);
	});
	test.each(SYNTHETIC_USER_IDS)('force-adding user %s to a guild is rejected', async (userId) => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.GUILD_FORCE_ADD_MEMBER]);
		await createBuilder(harness, `${admin.token}`)
			.put(`/admin/guilds/1/members/${userId}`)
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_USER')
			.execute();
	});
});
