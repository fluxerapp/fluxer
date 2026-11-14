// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {fetchUserMe, updateUserProfile} from '@app/api/user/tests/UserTestUtils';
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
