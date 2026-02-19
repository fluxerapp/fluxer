/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import {fetchUser, fetchUserMe, updateUserProfile} from '@fluxer/api/src/user/tests/UserTestUtils';
import {PublicUserFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

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

	test('PATCH /users/@me does not leak internal flags', async () => {
		const account = await createTestAccount(harness);
		await setUserFlags(harness, account.userId, UserFlags.STAFF | UserFlags.HIGH_GLOBAL_RATE_LIMIT);

		const {json: me} = await fetchUserMe(harness, account.token);
		expect(me.flags & PublicUserFlags.STAFF).toBe(PublicUserFlags.STAFF);
		expect(me.flags & Number(UserFlags.HIGH_GLOBAL_RATE_LIMIT)).toBe(0);

		const updated = await updateUserProfile(harness, account.token, {
			bio: 'checking internal flags',
		});
		expect(updated.json.flags & PublicUserFlags.STAFF).toBe(PublicUserFlags.STAFF);
		expect(updated.json.flags & Number(UserFlags.HIGH_GLOBAL_RATE_LIMIT)).toBe(0);
	});
});
