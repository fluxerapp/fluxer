// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {deleteOneOrMany} from '@app/api/database/CassandraQueryExecution';
import {getUserActivityBuffer, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {Users} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {triggerDeletionWorker, waitForDeletionCompletion} from '@app/api/user/tests/UserTestUtils';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const REGISTRATION_IP = '198.51.100.7';
const REQUEST_IP = '203.0.113.7';

async function fetchMeFromIp(harness: ApiTestHarness, token: string, ip: string): Promise<void> {
	await createBuilder(harness, token).get('/users/@me').header('x-forwarded-for', ip).expect(HTTP_STATUS.OK).execute();
}

describe('User activity buffering', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	const selfHosted = getConfig().instance.selfHosted;
	beforeEach(async () => {
		await harness.reset();
	});
	afterEach(() => {
		getConfig().instance.selfHosted = selfHosted;
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	test('an authenticated request buffers last active instead of writing it', async () => {
		const account = await createTestAccount(harness, {ipAddress: REGISTRATION_IP});
		const userId = createUserID(BigInt(account.userId));
		await fetchMeFromIp(harness, account.token, REQUEST_IP);
		const pending = await harness.kvProvider.hgetall('user_activity:pending');
		expect(pending[account.userId]).toBeDefined();
		const beforeFlush = await getUserRepository().getActivityTracking(userId);
		expect(beforeFlush?.last_active_ip).toBe(REGISTRATION_IP);
		await getUserActivityBuffer().drainAndFlush();
		const afterFlush = await getUserRepository().getActivityTracking(userId);
		expect(afterFlush?.last_active_ip).toBe(REQUEST_IP);
	});
	test('flushing moves the last active IP index and prunes the previous address', async () => {
		const account = await createTestAccount(harness, {ipAddress: REGISTRATION_IP});
		const userRepository = getUserRepository();
		const seeded = await userRepository.listUserIdsByLastActiveIp(REGISTRATION_IP, 10, 0);
		expect(seeded.userIds.map((id) => id.toString())).toContain(account.userId);
		await fetchMeFromIp(harness, account.token, REQUEST_IP);
		await getUserActivityBuffer().drainAndFlush();
		const previous = await userRepository.listUserIdsByLastActiveIp(REGISTRATION_IP, 10, 0);
		expect(previous.userIds.map((id) => id.toString())).not.toContain(account.userId);
		const current = await userRepository.listUserIdsByLastActiveIp(REQUEST_IP, 10, 0);
		expect(current.userIds.map((id) => id.toString())).toContain(account.userId);
	});
	test('a flush does not recreate a users row that was removed after the request', async () => {
		const account = await createTestAccount(harness, {ipAddress: REGISTRATION_IP});
		const userId = createUserID(BigInt(account.userId));
		await fetchMeFromIp(harness, account.token, REQUEST_IP);
		await deleteOneOrMany(Users.deleteByPk({user_id: userId}));
		await getUserActivityBuffer().drainAndFlush();
		const userRepository = getUserRepository();
		expect(await userRepository.findUnique(userId)).toBeNull();
		const indexed = await userRepository.listUserIdsByLastActiveIp(REQUEST_IP, 10, 0);
		expect(indexed.userIds.map((id) => id.toString())).not.toContain(account.userId);
	});
	test('a flush does not put the last active IP back on a deleted account', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.USER_DELETE,
		]);
		const account = await createTestAccount(harness, {ipAddress: REGISTRATION_IP});
		const userId = createUserID(BigInt(account.userId));
		await fetchMeFromIp(harness, account.token, REQUEST_IP);
		getConfig().instance.selfHosted = true;
		await createBuilder(harness, admin.token)
			.put(`/admin/users/${account.userId}/deletion`)
			.body({days_until_deletion: 14, basis: {type: 'holder_request'}, immediate: true})
			.expect(HTTP_STATUS.OK)
			.execute();
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, account.userId, 20000);
		await getUserActivityBuffer().drainAndFlush();
		const userRepository = getUserRepository();
		expect(await userRepository.getActivityTracking(userId)).toMatchObject({
			last_active_at: null,
			last_active_ip: null,
		});
		const indexed = await userRepository.listUserIdsByLastActiveIp(REQUEST_IP, 10, 0);
		expect(indexed.userIds.map((id) => id.toString())).not.toContain(account.userId);
	});
});
