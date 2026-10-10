// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, loginAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import type {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {expectDataExists} from '@app/api/user/tests/UserTestUtils';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface InactivityCheckResult {
	warnings_sent: number;
	deletions_scheduled: number;
	errors: number;
}

async function setUserActivity(harness: ApiTestHarness, userId: string, date: Date): Promise<void> {
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${userId}/set-last-active-at`)
		.body({timestamp: date.toISOString()})
		.execute();
}

async function setBotFlag(harness: ApiTestHarness, userId: string, isBot: boolean): Promise<void> {
	await createBuilderWithoutAuth(harness).post(`/test/users/${userId}/set-bot-flag`).body({is_bot: isBot}).execute();
}

async function setSystemFlag(harness: ApiTestHarness, userId: string, isSystem: boolean): Promise<void> {
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${userId}/set-system-flag`)
		.body({is_system: isSystem})
		.execute();
}

async function processInactivityDeletions(harness: ApiTestHarness): Promise<InactivityCheckResult> {
	return createBuilderWithoutAuth<InactivityCheckResult>(harness)
		.post('/test/worker/process-inactivity-deletions')
		.body({})
		.execute();
}

describe('Inactivity Deletion', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('bot user should never be scheduled for inactivity deletion', async () => {
		const account = await createTestAccount(harness);
		await setBotFlag(harness, account.userId, true);
		const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
		await setUserActivity(harness, account.userId, threeYearsAgo);
		const result = await processInactivityDeletions(harness);
		expect(result.deletions_scheduled).toBe(0);
		expect(result.warnings_sent).toBe(0);
		const dataStatus = await expectDataExists(harness, account.userId);
		expect(dataStatus.userExists).toBe(true);
		expect(dataStatus.hasSelfDeletedFlag).toBe(false);
	});
	test('system user should never be scheduled for inactivity deletion', async () => {
		const account = await createTestAccount(harness);
		await setSystemFlag(harness, account.userId, true);
		const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
		await setUserActivity(harness, account.userId, threeYearsAgo);
		const result = await processInactivityDeletions(harness);
		expect(result.deletions_scheduled).toBe(0);
		expect(result.warnings_sent).toBe(0);
		const dataStatus = await expectDataExists(harness, account.userId);
		expect(dataStatus.userExists).toBe(true);
		expect(dataStatus.hasSelfDeletedFlag).toBe(false);
	});
	test('recently active user should not receive warning', async () => {
		const account = await createTestAccount(harness);
		const oneMonthAgo = new Date(Date.now() - 30 * 24 * 60 * 60 * 1000);
		await setUserActivity(harness, account.userId, oneMonthAgo);
		const result = await processInactivityDeletions(harness);
		expect(result.warnings_sent).toBe(0);
		expect(result.deletions_scheduled).toBe(0);
	});
	test('expired inactivity warning schedules deletion in Cassandra and KV', async () => {
		const account = await createTestAccount(harness);
		const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
		await setUserActivity(harness, account.userId, threeYearsAgo);
		const warningSentAt = Date.now() - 31 * 24 * 60 * 60 * 1000;
		await harness.kvProvider.setex(
			`inactivity_warning_sent:${account.userId}`,
			35 * 24 * 60 * 60,
			String(warningSentAt),
		);
		const result = await processInactivityDeletions(harness);
		expect(result.deletions_scheduled).toBe(1);
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
		const dataStatus = await expectDataExists(harness, account.userId);
		expect(dataStatus.hasSelfDeletedFlag).toBe(true);
		expect(dataStatus.pendingDeletionAt).not.toBeNull();
		const user = await new UserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		expect(user.deletionReasonCode).toBe(DeletionReasons.INACTIVITY);
	});
	test('scheduling an inactivity deletion signs the account out and signing in again cancels it', async () => {
		const account = await createTestAccount(harness);
		const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
		await setUserActivity(harness, account.userId, threeYearsAgo);
		await harness.kvProvider.setex(
			`inactivity_warning_sent:${account.userId}`,
			35 * 24 * 60 * 60,
			String(Date.now() - 31 * 24 * 60 * 60 * 1000),
		);
		const result = await processInactivityDeletions(harness);
		expect(result.deletions_scheduled).toBe(1);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		const login = await loginAccount(harness, account);
		await createBuilder(harness, login.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
		const dataStatus = await expectDataExists(harness, account.userId);
		expect(dataStatus.hasSelfDeletedFlag).toBe(false);
		expect(dataStatus.pendingDeletionAt).toBeNull();
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(0);
	});
	test('user without activity data should not be deleted', async () => {
		const account = await createTestAccount(harness);
		const result = await processInactivityDeletions(harness);
		expect(result.deletions_scheduled).toBe(0);
		const dataStatus = await expectDataExists(harness, account.userId);
		expect(dataStatus.userExists).toBe(true);
	});
	test('user already pending deletion should be skipped', async () => {
		const account = await createTestAccount(harness);
		const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
		await setUserActivity(harness, account.userId, threeYearsAgo);
		const pendingDate = new Date(Date.now() + 7 * 24 * 60 * 60 * 1000);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/set-pending-deletion`)
			.body({pending_deletion_at: pendingDate.toISOString()})
			.execute();
		const result = await processInactivityDeletions(harness);
		expect(result.deletions_scheduled).toBe(0);
		const dataStatus = await expectDataExists(harness, account.userId);
		expect(dataStatus.userExists).toBe(true);
	});
	test('a page issues one batched activity lookup instead of per-user reads', async () => {
		const kvProvider = harness.kvProvider as MockKVProvider;
		const accounts = [
			await createTestAccount(harness),
			await createTestAccount(harness),
			await createTestAccount(harness),
		];
		const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
		for (const account of accounts) {
			await setUserActivity(harness, account.userId, threeYearsAgo);
		}
		kvProvider.mgetSpy.mockClear();
		kvProvider.getSpy.mockClear();
		await processInactivityDeletions(harness);
		const activityBatches = kvProvider.mgetSpy.mock.calls.filter((keys) =>
			keys.some((key) => String(key).startsWith('user_activity:')),
		);
		expect(activityBatches).toHaveLength(1);
		for (const account of accounts) {
			expect(activityBatches[0]).toContain(`user_activity:${account.userId}`);
		}
		const activityGets = kvProvider.getSpy.mock.calls.filter((call) => String(call[0]).startsWith('user_activity:'));
		expect(activityGets).toHaveLength(0);
	});
});
