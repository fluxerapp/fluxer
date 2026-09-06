// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {getUserActivityBuffer} from '../../middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface AclListResponse {
	acls: Array<string>;
}

interface UserListResponse {
	users: Array<{
		id: string;
		email: string | null;
		last_active_ip: string | null;
	}>;
	total: number;
}

async function setLastActiveIp(harness: ApiTestHarness, token: string, ip: string): Promise<void> {
	await createBuilder(harness, `${token}`)
		.get('/users/@me')
		.header('x-forwarded-for', ip)
		.expect(HTTP_STATUS.OK)
		.execute();
	await getUserActivityBuffer().drainAndFlush();
}

describe('Admin user directory', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	describe('GET /admin/acls', () => {
		test('lists every recognised admin permission', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE]);
			const result = await createBuilder<AclListResponse>(harness, `${admin.token}`)
				.get('/admin/acls')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.acls).toContain(AdminACLs.USER_LOOKUP);
			expect(result.acls).toContain(AdminACLs.WILDCARD);
			expect(result.acls).toHaveLength(Object.keys(AdminACLs).length);
		});
		test('requires an authenticated admin', async () => {
			const user = await createTestAccount(harness);
			await createBuilder(harness, `${user.token}`).get('/admin/acls').expect(HTTP_STATUS.FORBIDDEN).execute();
		});
	});
	describe('GET /admin/users', () => {
		test('returns the users named by repeated user_id parameters', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
			const first = await createTestAccount(harness);
			const second = await createTestAccount(harness);
			const result = await createBuilder<UserListResponse>(harness, `${admin.token}`)
				.get(`/admin/users?user_id=${first.userId}&user_id=${second.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.users.map((user) => user.id).sort()).toEqual([first.userId, second.userId].sort());
		});
		test('returns the user matching a free-text query', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
			const target = await createTestAccount(harness);
			const result = await createBuilder<UserListResponse>(harness, `${admin.token}`)
				.get(`/admin/users?q=${target.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.users.map((user) => user.id)).toContain(target.userId);
		});
		test('lists users when no selector is named', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
			const result = await createBuilder<UserListResponse>(harness, `${admin.token}`)
				.get('/admin/users')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.users.length).toBeGreaterThan(0);
			expect(result.total).toBeGreaterThan(0);
		});
		test('requires USER_LOOKUP ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE]);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users?user_id=${admin.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('rejects the email selector without USER_VIEW_EMAIL ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP, AdminACLs.USER_VIEW_IP]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users?email=${encodeURIComponent(target.email)}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
				.execute();
		});
		test('returns the account for the email selector with USER_VIEW_EMAIL ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP, AdminACLs.USER_VIEW_EMAIL]);
			const target = await createTestAccount(harness);
			const result = await createBuilder<UserListResponse>(harness, `${admin.token}`)
				.get(`/admin/users?email=${encodeURIComponent(target.email)}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.users.map((user) => user.id)).toEqual([target.userId]);
			expect(result.users[0]?.email).toBe(target.email);
		});
		test('rejects the last_active_ip selector without USER_VIEW_IP ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP, AdminACLs.USER_VIEW_EMAIL]);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users?last_active_ip=${encodeURIComponent('203.0.113.9')}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
				.execute();
		});
		test('returns the accounts for the last_active_ip selector with USER_VIEW_IP ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP, AdminACLs.USER_VIEW_IP]);
			const target = await createTestAccount(harness);
			await setLastActiveIp(harness, target.token, '203.0.113.9');
			const result = await createBuilder<UserListResponse>(harness, `${admin.token}`)
				.get(`/admin/users?last_active_ip=${encodeURIComponent('203.0.113.9')}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			const found = result.users.find((user) => user.id === target.userId);
			expect(found).toBeDefined();
			expect(found?.last_active_ip).toBe('203.0.113.9');
		});
		test('rejects a resolve value containing an at sign without USER_VIEW_EMAIL ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP, AdminACLs.USER_VIEW_IP]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users?resolve=${encodeURIComponent(target.email)}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
				.execute();
		});
		test('resolves an email address with USER_VIEW_EMAIL ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP, AdminACLs.USER_VIEW_EMAIL]);
			const target = await createTestAccount(harness);
			const result = await createBuilder<UserListResponse>(harness, `${admin.token}`)
				.get(`/admin/users?resolve=${encodeURIComponent(target.email)}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.users.map((user) => user.id)).toEqual([target.userId]);
			expect(result.users[0]?.email).toBe(target.email);
		});
		test('resolves a user ID without a PII ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
			const target = await createTestAccount(harness);
			const result = await createBuilder<UserListResponse>(harness, `${admin.token}`)
				.get(`/admin/users?resolve=${target.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.users.map((user) => user.id)).toEqual([target.userId]);
			expect(result.users[0]?.email).toBeNull();
		});
	});
});
