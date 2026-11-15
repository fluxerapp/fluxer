// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getInstanceConfigRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {CleanupModes, LastSessionCleanupModes} from '@fluxer/constants/src/AccountConfigConstants';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const DAY_MS = 24 * 60 * 60 * 1000;

describe('Abandoned account cleanup', () => {
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

	async function registerWithToken(body: Record<string, unknown>): Promise<{user_id: string; token: string}> {
		return await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
			.post('/auth/register')
			.body({username: 'wanderer', consent: true, ...body})
			.execute();
	}

	async function register(body: Record<string, unknown>): Promise<string> {
		return (await registerWithToken(body)).user_id;
	}

	async function signOut(token: string): Promise<void> {
		await createBuilder(harness, token).post('/auth/logout').body(null).expect(204).execute();
	}

	async function idleFor(userId: string, days: number): Promise<void> {
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${userId}/set-last-active-at`)
			.body({timestamp: new Date(Date.now() - days * DAY_MS).toISOString()})
			.execute();
	}

	async function sweep(): Promise<void> {
		await createBuilderWithoutAuth(harness).post('/test/worker/process-inactivity-deletions').body({}).execute();
	}

	async function deletionReason(userId: string): Promise<number | null> {
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(userId)));
		return user.pendingDeletionAt ? user.deletionReasonCode : null;
	}

	it('leaves idle unclaimed accounts alone while the sweep is off', async () => {
		const userId = await register({date_of_birth: '1995-05-05'});
		await idleFor(userId, 120);
		await sweep();
		expect(await deletionReason(userId)).toBeNull();
	});

	it('schedules idle accounts with no way back in once the sweep is on', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({abandoned_account_cleanup: true});
		const unclaimed = await register({date_of_birth: '1995-05-05'});
		const owner = await createTestAccount(harness);
		await idleFor(unclaimed, 120);
		await idleFor(owner.userId, 120);
		await sweep();
		expect(await deletionReason(unclaimed)).toBe(DeletionReasons.UNCLAIMED_ACCOUNT_ABANDONED);
		expect(await deletionReason(owner.userId)).toBeNull();
	});

	it('reads a missing activity entry as the last recorded activity, never as endless inactivity', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({abandoned_account_cleanup: true});
		const fresh = await register({});
		const freshWithDate = await register({date_of_birth: '1995-05-05'});
		const stale = await register({date_of_birth: '1995-05-05'});
		await idleFor(stale, 120);
		await getKVClient().del(`user_activity:${fresh}`, `user_activity:${freshWithDate}`, `user_activity:${stale}`);
		await sweep();
		expect(await deletionReason(fresh)).toBeNull();
		expect(await deletionReason(freshWithDate)).toBeNull();
		expect(await deletionReason(stale)).toBe(DeletionReasons.UNCLAIMED_ACCOUNT_ABANDONED);
	});

	it('keeps an account with no way back in after its last session ends while the cleanup is off', async () => {
		const created = await registerWithToken({date_of_birth: '1995-05-05'});
		await signOut(created.token);
		expect(await deletionReason(created.user_id)).toBeNull();
	});

	it('schedules an account with no way back in a day after its last session ends once the cleanup is on', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({abandoned_account_cleanup: true});
		const created = await registerWithToken({date_of_birth: '1995-05-05'});
		await signOut(created.token);
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(created.user_id)));
		expect(user.deletionReasonCode).toBe(DeletionReasons.UNCLAIMED_ACCOUNT_ABANDONED);
		expect(user.pendingDeletionAt!.getTime() - Date.now()).toBeGreaterThan(23 * 3600 * 1000);
	});

	it('leaves an account with a confirmed email alone when it signs out with the cleanup on', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({abandoned_account_cleanup: true});
		const account = await createTestAccount(harness);
		await signOut(account.token);
		expect(await deletionReason(account.userId)).toBeNull();
	});

	it('schedules accounts that never entered a date of birth after a week', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({abandoned_account_cleanup: true});
		const fresh = await register({});
		const stale = await register({});
		await idleFor(fresh, 2);
		await idleFor(stale, 8);
		await sweep();
		expect(await deletionReason(fresh)).toBeNull();
		expect(await deletionReason(stale)).toBe(DeletionReasons.INACTIVITY);
	});

	it('honours the configured idle days of each cleanup policy', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({
			unclaimed_account_cleanup: {mode: CleanupModes.AFTER_INACTIVITY, inactive_days: 30},
			dob_missing_cleanup: {mode: CleanupModes.AFTER_INACTIVITY, inactive_days: 3},
		});
		const unclaimed = await register({date_of_birth: '1995-05-05'});
		const recent = await register({date_of_birth: '1995-05-05'});
		const noBirthday = await register({});
		await idleFor(unclaimed, 40);
		await idleFor(recent, 20);
		await idleFor(noBirthday, 4);
		await sweep();
		expect(await deletionReason(unclaimed)).toBe(DeletionReasons.UNCLAIMED_ACCOUNT_ABANDONED);
		expect(await deletionReason(noBirthday)).toBe(DeletionReasons.INACTIVITY);
		expect(await deletionReason(recent)).toBeNull();
	});

	it('runs each cleanup policy on its own', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({
			unclaimed_account_cleanup: {mode: CleanupModes.OFF, on_last_session_end: LastSessionCleanupModes.OFF},
			dob_missing_cleanup: {mode: CleanupModes.AFTER_INACTIVITY},
		});
		const unclaimed = await register({date_of_birth: '1995-05-05'});
		const noBirthday = await register({});
		await idleFor(unclaimed, 120);
		await idleFor(noBirthday, 8);
		await sweep();
		expect(await deletionReason(unclaimed)).toBeNull();
		expect(await deletionReason(noBirthday)).toBe(DeletionReasons.INACTIVITY);
		const created = await registerWithToken({date_of_birth: '1995-05-05'});
		await signOut(created.token);
		expect(await deletionReason(created.user_id)).toBeNull();
	});

	it('leaves lifetime members and suspended accounts alone', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({abandoned_account_cleanup: true});
		const lifetime = await register({});
		const suspended = await register({});
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${lifetime}/premium`)
			.body({premium_type: UserPremiumTypes.LIFETIME})
			.execute();
		const users = getUserRepository();
		const banned = await users.findUniqueAssert(createUserID(BigInt(suspended)));
		await users.patchUpsert(
			banned.id,
			{flags: banned.flags | UserFlags.DISABLED, temp_banned_until: new Date(Date.now() + 30 * DAY_MS)},
			banned.toRow(),
		);
		for (const userId of [lifetime, suspended]) {
			await idleFor(userId, 120);
		}
		await sweep();
		expect(await deletionReason(lifetime)).toBeNull();
		expect(await deletionReason(suspended)).toBeNull();
	});
});
