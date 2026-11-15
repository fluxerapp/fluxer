// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {Users} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {backfillHiddenProfileBadges} from '@app/api/worker/tasks/BackfillHiddenProfileBadges';
import {PremiumFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import type {UserPrivateResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const FETCH_USER_ROW = Users.select({where: Users.where.eq('user_id'), limit: 1});

const LEGACY_STAFF_HIDDEN = 1n << 57n;
const LEGACY_PREMIUM_BADGE_HIDDEN_IN_USER_FLAGS = 1n << 40n;
const LEGACY_BADGE_SEQUENCE_HIDDEN_IN_USER_FLAGS = 1n << 43n;
const LEGACY_PREMIUM_BADGE_HIDDEN = 1 << 1;
const LEGACY_BADGE_SEQUENCE_HIDDEN = 1 << 4;
const LEGACY_OPERATOR_PASS_BADGE_HIDDEN = 1 << 9;

describe('hidden profile badges migration', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.resetData();
	});

	async function writeLegacyBits(account: TestAccount, flags: bigint, premiumFlags: number): Promise<void> {
		await upsertOne(
			Users.patchByPk(
				{user_id: createUserID(BigInt(account.userId))},
				{flags: Db.set(flags), premium_flags: Db.set(premiumFlags)},
			),
		);
	}

	async function storedRow(account: TestAccount): Promise<UserRow> {
		const row = await fetchOne<UserRow>(FETCH_USER_ROW.bind({user_id: createUserID(BigInt(account.userId))}));
		return row!;
	}

	async function hiddenList(account: TestAccount): Promise<Array<string>> {
		const me = await createBuilder<UserPrivateResponse>(harness, account.token)
			.get('/users/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		return me.hidden_profile_badges;
	}

	test('each legacy bit loads as its hidden keys and the next write clears the bits', async () => {
		const staff = await createTestAccount(harness);
		await writeLegacyBits(staff, UserFlags.STAFF | LEGACY_STAFF_HIDDEN, LEGACY_OPERATOR_PASS_BADGE_HIDDEN);
		expect(await hiddenList(staff)).toEqual(['operator_pass', 'staff']);

		const premium = await createTestAccount(harness);
		await writeLegacyBits(premium, 0n, LEGACY_PREMIUM_BADGE_HIDDEN);
		expect(await hiddenList(premium)).toEqual(['plutonium', 'premium', 'visionary']);

		const sequence = await createTestAccount(harness);
		await writeLegacyBits(sequence, 0n, LEGACY_BADGE_SEQUENCE_HIDDEN);
		expect(await hiddenList(sequence)).toEqual(['visionary_number']);

		const older = await createTestAccount(harness);
		await writeLegacyBits(
			older,
			LEGACY_PREMIUM_BADGE_HIDDEN_IN_USER_FLAGS | LEGACY_BADGE_SEQUENCE_HIDDEN_IN_USER_FLAGS,
			0,
		);
		expect(await hiddenList(older)).toEqual(['plutonium', 'premium', 'visionary', 'visionary_number']);

		await createBuilder(harness, staff.token)
			.patch('/users/@me')
			.body({bio: 'still hidden'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const row = await storedRow(staff);
		expect(row.flags).toBe(UserFlags.STAFF);
		expect(row.premium_flags ?? 0).toBe(0);
		expect([...(row.hidden_profile_badges ?? [])].sort()).toEqual(['operator_pass', 'staff']);
		expect(await hiddenList(staff)).toEqual(['operator_pass', 'staff']);
	});

	test('moving the bits on a write keeps flags changed by someone else after the read', async () => {
		const account = await createTestAccount(harness);
		await writeLegacyBits(account, LEGACY_STAFF_HIDDEN, LEGACY_PREMIUM_BADGE_HIDDEN);
		const userId = createUserID(BigInt(account.userId));
		const staleRow = (await getUserRepository().findUniqueAssert(userId)).toRow();
		await getUserRepository().updateFlags(userId, (flags) => flags | UserFlags.DISABLED);
		await upsertOne(
			Users.patchByPk(
				{user_id: userId},
				{premium_flags: Db.set(LEGACY_PREMIUM_BADGE_HIDDEN | PremiumFlags.PURCHASE_DISABLED)},
			),
		);

		await getUserRepository().patchUpsert(userId, {bio: 'written from a stale read'}, staleRow);
		const row = await storedRow(account);
		expect(row.flags).toBe(UserFlags.DISABLED);
		expect(row.premium_flags).toBe(PremiumFlags.PURCHASE_DISABLED);
		expect([...(row.hidden_profile_badges ?? [])].sort()).toEqual(['plutonium', 'premium', 'staff', 'visionary']);
	});

	test('the backfill moves every legacy bit once and leaves nothing for a second run', async () => {
		const staff = await createTestAccount(harness);
		const premium = await createTestAccount(harness);
		const untouched = await createTestAccount(harness);
		await writeLegacyBits(staff, UserFlags.STAFF | LEGACY_STAFF_HIDDEN, 0);
		await writeLegacyBits(premium, 0n, LEGACY_PREMIUM_BADGE_HIDDEN | LEGACY_BADGE_SEQUENCE_HIDDEN);
		const untouchedVersion = (await storedRow(untouched)).version;

		expect(await backfillHiddenProfileBadges(getUserRepository(), 2)).toEqual({migrated: 2, failed: 0});
		const staffRow = await storedRow(staff);
		expect(staffRow.flags).toBe(UserFlags.STAFF);
		expect([...(staffRow.hidden_profile_badges ?? [])]).toEqual(['staff']);
		const premiumRow = await storedRow(premium);
		expect(premiumRow.premium_flags ?? 0).toBe(0);
		expect([...(premiumRow.hidden_profile_badges ?? [])].sort()).toEqual([
			'plutonium',
			'premium',
			'visionary',
			'visionary_number',
		]);

		const versions = [(await storedRow(staff)).version, (await storedRow(premium)).version];
		expect(await backfillHiddenProfileBadges(getUserRepository(), 2)).toEqual({migrated: 0, failed: 0});
		expect([(await storedRow(staff)).version, (await storedRow(premium)).version]).toEqual(versions);
		expect((await storedRow(untouched)).version).toBe(untouchedVersion);
	});
});
