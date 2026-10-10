// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, test} from 'vitest';

describe('Admin Search Endpoints', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	describe('GET /admin/users', () => {
		test('requires user:lookup ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users?q=${encodeURIComponent('test')}&limit=10&offset=0`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('GET /admin/users/{user_id}/dm-channels', () => {
		test('requires user:list:dm_channels ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users/${admin.userId}/dm-channels?limit=10`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('GET /admin/guilds', () => {
		test('requires guild:lookup ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/guilds?q=test&limit=10&offset=0')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('/admin/reports', () => {
		test('requires report:view ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/reports?q=example&limit=10&offset=0')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('/admin/audit-logs (search)', () => {
		test('requires audit_log:view ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/audit-logs?q=set_acls&limit=10&offset=0')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('/admin/audit-logs/{log_id}', () => {
		test('requires audit_log:view ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/audit-logs/999999999999999999')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('/admin/audit-logs (list)', () => {
		test('requires audit_log:view ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/audit-logs?limit=10&offset=0')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
});

describe('Admin Search Endpoints without a search backend', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'disabled'});
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	test('GET /admin/guilds answers 403 FEATURE_TEMPORARILY_DISABLED', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/guilds?q=test&limit=10&offset=0')
			.expect(HTTP_STATUS.FORBIDDEN, 'FEATURE_TEMPORARILY_DISABLED')
			.execute();
	});
	test('GET /admin/users with q answers 403 FEATURE_TEMPORARILY_DISABLED', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/users?q=test&limit=10&offset=0')
			.expect(HTTP_STATUS.FORBIDDEN, 'FEATURE_TEMPORARILY_DISABLED')
			.execute();
	});
	test('GET /admin/reports answers 403 FEATURE_TEMPORARILY_DISABLED on the status branch', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'report:view']);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/reports?status=pending&limit=10&offset=0')
			.expect(HTTP_STATUS.FORBIDDEN, 'FEATURE_TEMPORARILY_DISABLED')
			.execute();
	});
	test('GET /admin/reports answers 403 FEATURE_TEMPORARILY_DISABLED on the search branch', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'report:view']);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/reports?q=example&limit=10&offset=0')
			.expect(HTTP_STATUS.FORBIDDEN, 'FEATURE_TEMPORARILY_DISABLED')
			.execute();
	});
});
