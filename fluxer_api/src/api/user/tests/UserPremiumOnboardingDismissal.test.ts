// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {fetchUserMe, grantPremium} from '@app/api/user/tests/UserTestUtils';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {UserPrivateResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('User Premium Onboarding Dismissal', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('non-premium user has has_dismissed_premium_onboarding as false', async () => {
		const account = await createTestAccount(harness);
		const {json} = await fetchUserMe(harness, account.token);
		expect(json.has_dismissed_premium_onboarding).toBe(false);
	});
	test('premium subscription user can dismiss onboarding', async () => {
		const account = await createTestAccount(harness);
		await grantPremium(harness, account.userId, UserPremiumTypes.SUBSCRIPTION);
		const before = await fetchUserMe(harness, account.token);
		expect(before.json.has_dismissed_premium_onboarding).toBe(false);
		await createBuilder<UserPrivateResponse>(harness, account.token)
			.patch('/users/@me')
			.body({has_dismissed_premium_onboarding: true})
			.expect(HTTP_STATUS.OK)
			.execute();
		const after = await fetchUserMe(harness, account.token);
		expect(after.json.has_dismissed_premium_onboarding).toBe(true);
	});
	test('dismissal persists across fetches', async () => {
		const account = await createTestAccount(harness);
		await grantPremium(harness, account.userId, UserPremiumTypes.SUBSCRIPTION);
		await createBuilder<UserPrivateResponse>(harness, account.token)
			.patch('/users/@me')
			.body({has_dismissed_premium_onboarding: true})
			.expect(HTTP_STATUS.OK)
			.execute();
		const fetch1 = await fetchUserMe(harness, account.token);
		expect(fetch1.json.has_dismissed_premium_onboarding).toBe(true);
		const fetch2 = await fetchUserMe(harness, account.token);
		expect(fetch2.json.has_dismissed_premium_onboarding).toBe(true);
	});
	test('PATCH /users/@me returns updated dismissal state in response', async () => {
		const account = await createTestAccount(harness);
		await grantPremium(harness, account.userId, UserPremiumTypes.SUBSCRIPTION);
		const result = await createBuilder<UserPrivateResponse>(harness, account.token)
			.patch('/users/@me')
			.body({has_dismissed_premium_onboarding: true})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.has_dismissed_premium_onboarding).toBe(true);
	});
	test('non-premium user dismissal is not reflected until premium', async () => {
		const account = await createTestAccount(harness);
		await createBuilder<UserPrivateResponse>(harness, account.token)
			.patch('/users/@me')
			.body({has_dismissed_premium_onboarding: true})
			.expect(HTTP_STATUS.OK)
			.execute();
		const stillFalse = await fetchUserMe(harness, account.token);
		expect(stillFalse.json.has_dismissed_premium_onboarding).toBe(false);
		await grantPremium(harness, account.userId, UserPremiumTypes.SUBSCRIPTION);
		const afterPremium = await fetchUserMe(harness, account.token);
		expect(afterPremium.json.has_dismissed_premium_onboarding).toBe(true);
	});
	test('lifetime premium user can dismiss onboarding', async () => {
		const account = await createTestAccount(harness);
		await grantPremium(harness, account.userId, UserPremiumTypes.LIFETIME);
		await createBuilder<UserPrivateResponse>(harness, account.token)
			.patch('/users/@me')
			.body({has_dismissed_premium_onboarding: true})
			.expect(HTTP_STATUS.OK)
			.execute();
		const after = await fetchUserMe(harness, account.token);
		expect(after.json.has_dismissed_premium_onboarding).toBe(true);
	});
});
