// SPDX-License-Identifier: AGPL-3.0-or-later

import {PublicUserFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {fetchUser, fetchUserMe, updateUserProfile} from './UserTestUtils';

async function setUserFlags(harness: ApiTestHarness, userId: string, flags: bigint): Promise<void> {
	await createBuilder(harness, '')
		.patch(`/test/users/${userId}/flags`)
		.body({flags: flags.toString()})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('User flags in responses', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	test('GET /users/@me preserves staff flag when STAFF_HIDDEN is not set', async () => {
		const account = await createTestAccount(harness);
		await setUserFlags(harness, account.userId, UserFlags.STAFF);
		const {json} = await fetchUserMe(harness, account.token);
		expect(json.flags & PublicUserFlags.STAFF).toBe(PublicUserFlags.STAFF);
		expect(json.is_staff).toBe(true);
	});
	test('GET /users/@me hides staff flag when STAFF_HIDDEN is set', async () => {
		const account = await createTestAccount(harness);
		await setUserFlags(harness, account.userId, UserFlags.STAFF | UserFlags.STAFF_HIDDEN);
		const {json} = await fetchUserMe(harness, account.token);
		expect(json.flags & PublicUserFlags.STAFF).toBe(0);
		expect(json.is_staff).toBe(true);
	});
	test('PATCH /users/@me preserves staff flag after profile update', async () => {
		const account = await createTestAccount(harness);
		await setUserFlags(harness, account.userId, UserFlags.STAFF);
		const {json} = await updateUserProfile(harness, account.token, {
			bio: 'updated bio',
		});
		expect(json.flags & PublicUserFlags.STAFF).toBe(PublicUserFlags.STAFF);
		expect(json.is_staff).toBe(true);
	});
	test('PATCH /users/@me preserves staff flag with STAFF_HIDDEN after profile update', async () => {
		const account = await createTestAccount(harness);
		await setUserFlags(harness, account.userId, UserFlags.STAFF | UserFlags.STAFF_HIDDEN);
		const {json} = await updateUserProfile(harness, account.token, {
			bio: 'updated bio',
		});
		expect(json.flags & PublicUserFlags.STAFF).toBe(0);
		expect(json.is_staff).toBe(true);
	});
	test('GET /users/:id returns staff flag in partial response', async () => {
		const account = await createTestAccount(harness);
		const viewer = await createTestAccount(harness);
		await setUserFlags(harness, account.userId, UserFlags.STAFF);
		const {json} = await fetchUser(harness, account.userId, viewer.token);
		expect(json.flags & PublicUserFlags.STAFF).toBe(PublicUserFlags.STAFF);
	});
	test('GET /users/:id hides staff flag when STAFF_HIDDEN is set', async () => {
		const account = await createTestAccount(harness);
		const viewer = await createTestAccount(harness);
		await setUserFlags(harness, account.userId, UserFlags.STAFF | UserFlags.STAFF_HIDDEN);
		const {json} = await fetchUser(harness, account.userId, viewer.token);
		expect(json.flags & PublicUserFlags.STAFF).toBe(0);
	});
	test('non-staff user has flags 0', async () => {
		const account = await createTestAccount(harness);
		const {json} = await fetchUserMe(harness, account.token);
		expect(json.flags).toBe(0);
		expect(json.is_staff).toBe(false);
	});
	test('GET /users/@me includes SPAMMER when set', async () => {
		const account = await createTestAccount(harness);
		await setUserFlags(harness, account.userId, UserFlags.SPAMMER | UserFlags.HAS_SESSION_STARTED);
		const {json} = await fetchUserMe(harness, account.token);
		expect(json.flags & PublicUserFlags.SPAMMER).toBe(PublicUserFlags.SPAMMER);
	});
	test('PATCH /users/@me does not leak internal flags', async () => {
		const account = await createTestAccount(harness);
		await setUserFlags(harness, account.userId, UserFlags.STAFF | UserFlags.SPAMMER | UserFlags.HIGH_GLOBAL_RATE_LIMIT);
		const {json: me} = await fetchUserMe(harness, account.token);
		expect(me.flags & PublicUserFlags.STAFF).toBe(PublicUserFlags.STAFF);
		expect(me.flags & PublicUserFlags.SPAMMER).toBe(PublicUserFlags.SPAMMER);
		expect(me.flags & Number(UserFlags.HIGH_GLOBAL_RATE_LIMIT)).toBe(0);
		const updated = await updateUserProfile(harness, account.token, {
			bio: 'checking internal flags',
		});
		expect(updated.json.flags & PublicUserFlags.STAFF).toBe(PublicUserFlags.STAFF);
		expect(updated.json.flags & PublicUserFlags.SPAMMER).toBe(PublicUserFlags.SPAMMER);
		expect(updated.json.flags & Number(UserFlags.HIGH_GLOBAL_RATE_LIMIT)).toBe(0);
	});
});
