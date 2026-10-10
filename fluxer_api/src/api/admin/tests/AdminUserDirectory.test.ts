// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getUserActivityBuffer} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface UserListResponse {
	users: Array<{
		id: string;
		email: string | null;
		last_active_ip: string | null;
	}>;
	total: number;
}

const SYNTHETIC_USER_IDS = ['0', '1'];

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
		test('requires an authenticated admin', async () => {
			const user = await createTestAccount(harness);
			await createBuilder(harness, `${user.token}`).get('/admin/acls').expect(HTTP_STATUS.FORBIDDEN).execute();
		});
	});
	describe('GET /admin/users', () => {
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
		test.each(SYNTHETIC_USER_IDS)('omits the synthetic account %s from the resolve selector', async (userId) => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
			const result = await createBuilder<UserListResponse>(harness, `${admin.token}`)
				.get(`/admin/users?resolve=${userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.users).toEqual([]);
			expect(result.total).toBe(0);
		});
		test.each(SYNTHETIC_USER_IDS)('omits the synthetic account %s from the q selector', async (userId) => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
			const result = await createBuilder<UserListResponse>(harness, `${admin.token}`)
				.get(`/admin/users?q=${userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.users.map((user) => user.id)).not.toContain(userId);
		});
	});
});
