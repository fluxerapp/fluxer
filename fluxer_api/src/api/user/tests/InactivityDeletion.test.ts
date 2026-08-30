// SPDX-License-Identifier: AGPL-3.0-or-later

import {DeletionReasons} from '@fluxer/constants/src/Core';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createUserID} from '../../BrandedTypes';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import type {MockKVProvider} from '../../test/mocks/MockKVProvider';
import {createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {UserRepository} from '../repositories/UserRepository';
import {expectDataExists} from './UserTestUtils';

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
	test('inactive user should receive warning email', async () => {
		const account = await createTestAccount(harness);
		const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
		await setUserActivity(harness, account.userId, threeYearsAgo);
		const result = await processInactivityDeletions(harness);
		expect(result.warnings_sent).toBeGreaterThanOrEqual(0);
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
	test('warning email should be idempotent', async () => {
		const account = await createTestAccount(harness);
		const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
		await setUserActivity(harness, account.userId, threeYearsAgo);
		const firstResult = await processInactivityDeletions(harness);
		const firstWarnings = firstResult.warnings_sent;
		const secondResult = await processInactivityDeletions(harness);
		expect(secondResult.warnings_sent).toBeLessThanOrEqual(firstWarnings);
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
	test('processing should handle multiple users', async () => {
		const account1 = await createTestAccount(harness);
		const account2 = await createTestAccount(harness);
		const account3 = await createTestAccount(harness);
		await setBotFlag(harness, account1.userId, true);
		await setSystemFlag(harness, account2.userId, true);
		const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
		await setUserActivity(harness, account1.userId, threeYearsAgo);
		await setUserActivity(harness, account2.userId, threeYearsAgo);
		await setUserActivity(harness, account3.userId, threeYearsAgo);
		const result = await processInactivityDeletions(harness);
		expect(result.errors).toBe(0);
		const data1 = await expectDataExists(harness, account1.userId);
		const data2 = await expectDataExists(harness, account2.userId);
		expect(data1.hasSelfDeletedFlag).toBe(false);
		expect(data2.hasSelfDeletedFlag).toBe(false);
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
	test('should return processing statistics', async () => {
		const result = await processInactivityDeletions(harness);
		expect(result).toHaveProperty('warnings_sent');
		expect(result).toHaveProperty('deletions_scheduled');
		expect(result).toHaveProperty('errors');
		expect(typeof result.warnings_sent).toBe('number');
		expect(typeof result.deletions_scheduled).toBe('number');
		expect(typeof result.errors).toBe('number');
	});
});
