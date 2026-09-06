// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {getUserActivityBuffer} from '../../middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface AdminUserSearchResponse {
	users: Array<{
		id: string;
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

describe('Admin last active IP search', () => {
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
	test('finds a user by exact IPv4 last active IP', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup', 'user:view:ip']);
		const targetUser = await createTestAccount(harness);
		await setLastActiveIp(harness, targetUser.token, '198.51.100.91');
		const result = await createBuilder<AdminUserSearchResponse>(harness, `${admin.token}`)
			.get(`/admin/users?last_active_ip=${encodeURIComponent('198.51.100.91')}&limit=10&offset=0`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.users.find((user) => user.id === targetUser.userId)).toBeDefined();
	});
	test('matches IPv6 last active addresses by /64 trust key', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup', 'user:view:ip']);
		const firstUser = await createTestAccount(harness);
		const secondUser = await createTestAccount(harness);
		await setLastActiveIp(harness, firstUser.token, '2a01:e0a:d10:95b0:8f54:410e:f290:1c66');
		await setLastActiveIp(harness, secondUser.token, '2a01:e0a:d10:95b0:01e4:53a8:d0dd:7733');
		const result = await createBuilder<AdminUserSearchResponse>(harness, `${admin.token}`)
			.get(
				`/admin/users?last_active_ip=${encodeURIComponent('2a01:e0a:d10:95b0:b53f:16d3:aff2:9b0f')}&limit=10&offset=0`,
			)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.users.find((user) => user.id === firstUser.userId)).toBeDefined();
		expect(result.users.find((user) => user.id === secondUser.userId)).toBeDefined();
	});
	test('keeps IPv4 last active searches exact', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup', 'user:view:ip']);
		const firstUser = await createTestAccount(harness);
		const secondUser = await createTestAccount(harness);
		await setLastActiveIp(harness, firstUser.token, '198.51.100.91');
		await setLastActiveIp(harness, secondUser.token, '198.51.100.92');
		const result = await createBuilder<AdminUserSearchResponse>(harness, `${admin.token}`)
			.get(`/admin/users?last_active_ip=${encodeURIComponent('198.51.100.91')}&limit=10&offset=0`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.users.find((user) => user.id === firstUser.userId)).toBeDefined();
		expect(result.users.find((user) => user.id === secondUser.userId)).toBeUndefined();
	});
});
