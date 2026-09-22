// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {fetchUserMe, grantPremium, updateUserProfile} from '@app/api/user/tests/UserTestUtils';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('User FluxerTag rate limit', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('blocks the sixth username change within the cooldown window', async () => {
		const account = await createTestAccount(harness);
		const {json: me} = await fetchUserMe(harness, account.token);
		const usernameBase = me.username.slice(0, Math.min(me.username.length, 24));
		for (const suffix of ['rl_a', 'rl_b', 'rl_c', 'rl_d', 'rl_e']) {
			await updateUserProfile(harness, account.token, {
				username: `${usernameBase}${suffix}`,
				password: account.password,
			});
		}
		const {response, text} = await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({
				username: `${usernameBase}rl_f`,
				password: account.password,
			})
			.executeRaw();
		expect(response.status).toBe(HTTP_STATUS.BAD_REQUEST);
		expect(text).toContain(ValidationErrorCodes.USERNAME_CHANGED_TOO_MANY_TIMES);
	});
	test('blocks the sixth discriminator-only change within the cooldown window', async () => {
		const account = await createTestAccount(harness);
		await grantPremium(harness, account.userId, UserPremiumTypes.SUBSCRIPTION);
		const {json: me} = await fetchUserMe(harness, account.token);
		const targets = Array.from({length: 20}, (_, i) => String(67 + i).padStart(4, '0'))
			.filter((value) => value !== me.discriminator)
			.slice(0, 6);
		for (const discriminator of targets.slice(0, 5)) {
			await updateUserProfile(harness, account.token, {
				discriminator,
				password: account.password,
			});
		}
		const {response, text} = await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({
				discriminator: targets[5],
				password: account.password,
			})
			.executeRaw();
		expect(response.status).toBe(HTTP_STATUS.BAD_REQUEST);
		expect(text).toContain(ValidationErrorCodes.USERNAME_CHANGED_TOO_MANY_TIMES);
	});
});
