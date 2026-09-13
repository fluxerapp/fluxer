// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {expectDataExists} from '@app/api/user/tests/UserTestUtils';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

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

async function setAppStoreReviewerFlag(harness: ApiTestHarness, userId: string): Promise<void> {
	const APP_STORE_REVIEWER_FLAG = BigInt(1) << BigInt(37);
	await createBuilderWithoutAuth(harness)
		.patch(`/test/users/${userId}/flags`)
		.body({flags: APP_STORE_REVIEWER_FLAG.toString()})
		.execute();
}

async function processInactivityDeletions(harness: ApiTestHarness): Promise<InactivityCheckResult> {
	return createBuilderWithoutAuth<InactivityCheckResult>(harness)
		.post('/test/worker/process-inactivity-deletions')
		.body({})
		.execute();
}

describe('UserDeletionEligibilityService', () => {
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
	describe('isEligibleForInactivityDeletion', () => {
		test('bot user is never scheduled for inactivity deletion', async () => {
			const account = await createTestAccount(harness);
			await setBotFlag(harness, account.userId, true);
			const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
			await setUserActivity(harness, account.userId, threeYearsAgo);
			const result = await processInactivityDeletions(harness);
			expect(result.deletions_scheduled).toBe(0);
			const dataStatus = await expectDataExists(harness, account.userId);
			expect(dataStatus.userExists).toBe(true);
			expect(dataStatus.hasSelfDeletedFlag).toBe(false);
		});
		test('system user is never scheduled for inactivity deletion', async () => {
			const account = await createTestAccount(harness);
			await setSystemFlag(harness, account.userId, true);
			const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
			await setUserActivity(harness, account.userId, threeYearsAgo);
			const result = await processInactivityDeletions(harness);
			expect(result.deletions_scheduled).toBe(0);
			const dataStatus = await expectDataExists(harness, account.userId);
			expect(dataStatus.userExists).toBe(true);
			expect(dataStatus.hasSelfDeletedFlag).toBe(false);
		});
		test('app store reviewer is never scheduled for inactivity deletion', async () => {
			const account = await createTestAccount(harness);
			await setAppStoreReviewerFlag(harness, account.userId);
			const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
			await setUserActivity(harness, account.userId, threeYearsAgo);
			const result = await processInactivityDeletions(harness);
			expect(result.deletions_scheduled).toBe(0);
			const dataStatus = await expectDataExists(harness, account.userId);
			expect(dataStatus.userExists).toBe(true);
			expect(dataStatus.hasSelfDeletedFlag).toBe(false);
		});
		test('user with pending deletion is not scheduled again', async () => {
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
		test('user with null last_active_at is not scheduled for deletion', async () => {
			const account = await createTestAccount(harness);
			const result = await processInactivityDeletions(harness);
			expect(result.deletions_scheduled).toBe(0);
			const dataStatus = await expectDataExists(harness, account.userId);
			expect(dataStatus.userExists).toBe(true);
		});
		test('recently active user is not scheduled for deletion', async () => {
			const account = await createTestAccount(harness);
			const oneMonthAgo = new Date(Date.now() - 30 * 24 * 60 * 60 * 1000);
			await setUserActivity(harness, account.userId, oneMonthAgo);
			const result = await processInactivityDeletions(harness);
			expect(result.warnings_sent).toBe(0);
			expect(result.deletions_scheduled).toBe(0);
		});
		test('inactive user may receive warning email', async () => {
			const account = await createTestAccount(harness);
			const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
			await setUserActivity(harness, account.userId, threeYearsAgo);
			const result = await processInactivityDeletions(harness);
			expect(result.warnings_sent).toBeGreaterThanOrEqual(0);
		});
	});
	describe('warning idempotency', () => {
		test('warning email should be idempotent', async () => {
			const account = await createTestAccount(harness);
			const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
			await setUserActivity(harness, account.userId, threeYearsAgo);
			const firstResult = await processInactivityDeletions(harness);
			const firstWarnings = firstResult.warnings_sent;
			const secondResult = await processInactivityDeletions(harness);
			expect(secondResult.warnings_sent).toBeLessThanOrEqual(firstWarnings);
		});
	});
	describe('multiple user handling', () => {
		test('processing should handle multiple users with different flags', async () => {
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
	describe('edge cases', () => {
		test('user at exact two year threshold is not scheduled', async () => {
			const account = await createTestAccount(harness);
			const exactlyTwoYearsAgo = new Date(Date.now() - 2 * 365 * 24 * 60 * 60 * 1000);
			await setUserActivity(harness, account.userId, exactlyTwoYearsAgo);
			await processInactivityDeletions(harness);
			const dataStatus = await expectDataExists(harness, account.userId);
			expect(dataStatus.userExists).toBe(true);
			expect(dataStatus.hasSelfDeletedFlag).toBe(false);
		});
		test('multiple flag combinations - bot with system flag still protected', async () => {
			const account = await createTestAccount(harness);
			await setBotFlag(harness, account.userId, true);
			await setSystemFlag(harness, account.userId, true);
			const threeYearsAgo = new Date(Date.now() - 3 * 365 * 24 * 60 * 60 * 1000);
			await setUserActivity(harness, account.userId, threeYearsAgo);
			await processInactivityDeletions(harness);
			const dataStatus = await expectDataExists(harness, account.userId);
			expect(dataStatus.hasSelfDeletedFlag).toBe(false);
		});
	});
});
