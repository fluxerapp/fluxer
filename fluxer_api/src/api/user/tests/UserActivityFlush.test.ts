// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {deleteOneOrMany, fetchMany, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {getUserActivityBuffer, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {AuthSessions, AuthSessionsByAppChannel, AuthSessionsByUserId, Users} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserAccountRepository} from '@app/api/user/repositories/account/UserAccountRepository';
import {UserActivityBuffer} from '@app/api/user/services/UserActivityBuffer';
import {
	deleteAccount,
	setPendingDeletionAt,
	triggerDeletionWorker,
	waitForDeletionCompletion,
} from '@app/api/user/tests/UserTestUtils';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const REGISTRATION_IP = '198.51.100.7';
const REQUEST_IP = '203.0.113.7';
const LATER_REQUEST_IP = '203.0.113.8';

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
	test('a flush without a client IP keeps the indexed IP so deletion still removes it', async () => {
		const account = await createTestAccount(harness, {ipAddress: REGISTRATION_IP});
		const userId = createUserID(BigInt(account.userId));
		await new UserAccountRepository(harness.kvProvider).updateLastActiveAt({userId, lastActiveAt: new Date()});
		expect((await getUserRepository().getActivityTracking(userId))?.last_active_ip).toBe(REGISTRATION_IP);
		await deleteAccount(harness, account.token, account.password);
		await setPendingDeletionAt(harness, account.userId, new Date(Date.now() - 60_000));
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, account.userId, 20000);
		const indexed = await getUserRepository().listUserIdsByLastActiveIp(REGISTRATION_IP, 100, 0);
		expect(indexed.userIds.map((id) => id.toString())).not.toContain(account.userId);
	});
	test('a session revoked while its activity flushes leaves no session or app channel rows', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const clientProperties = {
			os: 'android',
			client_platform: 'android',
			client_version: '2026.11.1',
			release_channel: 'beta',
			build_number: 7,
		};
		const debounced = await harness.kvProvider.scan('auth_session_activity:touched:*', 100);
		if (debounced.length > 0) await harness.kvProvider.del(...debounced);
		await createBuilder(harness, account.token)
			.get('/users/@me')
			.header('user-agent', 'Fluxer Android/2026.11.1 (beta)')
			.header('x-fluxer-client-properties', Buffer.from(JSON.stringify(clientProperties)).toString('base64'))
			.expect(HTTP_STATUS.OK)
			.execute();
		const [ref] = await fetchMany<{session_id_hash: Buffer}>(
			AuthSessionsByUserId.selectCql({columns: ['session_id_hash'], where: AuthSessionsByUserId.where.eq('user_id')}),
			{user_id: userId},
		);
		expect(ref).toBeDefined();
		const sessionIdHash = ref!.session_id_hash;
		let revoked = false;
		const buffer = new UserActivityBuffer(harness.kvProvider, async (query, params) => {
			if (!revoked) {
				revoked = true;
				await getUserRepository().deleteAuthSessions(userId, [sessionIdHash]);
			}
			await upsertOne(query, params);
		});
		await buffer.drainAndFlush();
		expect(revoked).toBe(true);
		const channelRows = await fetchMany<{session_id_hash: Buffer}>(
			AuthSessionsByAppChannel.select({
				columns: ['session_id_hash'],
				where: [
					AuthSessionsByAppChannel.where.eq('app_platform'),
					AuthSessionsByAppChannel.where.eq('app_release_channel'),
				],
			}).bind({app_platform: 'android', app_release_channel: 'beta'}),
		);
		expect(channelRows.map((row) => row.session_id_hash.toString('base64url'))).not.toContain(
			sessionIdHash.toString('base64url'),
		);
		const sessionRows = await fetchMany<{session_id_hash: Buffer}>(
			AuthSessions.selectCql({columns: ['session_id_hash'], where: AuthSessions.where.eq('session_id_hash')}),
			{session_id_hash: sessionIdHash},
		);
		expect(sessionRows).toEqual([]);
	});

	test('overlapping flush jobs leave only the current IP indexed', async () => {
		const account = await createTestAccount(harness, {ipAddress: REGISTRATION_IP});
		const userId = createUserID(BigInt(account.userId));
		const accounts = new UserAccountRepository(harness.kvProvider);
		let release!: () => void;
		const gate = new Promise<void>((resolve) => {
			release = resolve;
		});
		const entered: Array<Promise<void>> = [];
		const signals: Array<() => void> = [];
		for (let i = 0; i < 2; i++) {
			entered.push(new Promise<void>((resolve) => signals.push(resolve)));
		}
		let calls = 0;
		const gatedAccounts = {
			async updateLastActiveAt(params: Parameters<UserAccountRepository['updateLastActiveAt']>[0]) {
				signals[calls++]?.();
				await gate;
				return accounts.updateLastActiveAt(params);
			},
		};
		await fetchMeFromIp(harness, account.token, REQUEST_IP);
		const first = new UserActivityBuffer(harness.kvProvider, upsertOne, gatedAccounts).drainAndFlush();
		await entered[0];
		await fetchMeFromIp(harness, account.token, LATER_REQUEST_IP);
		const second = new UserActivityBuffer(harness.kvProvider, upsertOne, gatedAccounts).drainAndFlush();
		await Promise.race([entered[1], second]);
		release();
		await Promise.all([first, second]);
		await getUserActivityBuffer().drainAndFlush();
		const userRepository = getUserRepository();
		expect((await userRepository.getActivityTracking(userId))?.last_active_ip).toBe(LATER_REQUEST_IP);
		const indexed = async (ip: string) =>
			(await userRepository.listUserIdsByLastActiveIp(ip, 10, 0)).userIds.map((id) => id.toString());
		expect(await indexed(LATER_REQUEST_IP)).toContain(account.userId);
		expect(await indexed(REQUEST_IP)).not.toContain(account.userId);
		expect(await indexed(REGISTRATION_IP)).not.toContain(account.userId);
	});
});
