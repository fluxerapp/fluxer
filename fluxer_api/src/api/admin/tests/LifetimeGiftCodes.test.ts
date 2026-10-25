// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {setCachedEmailDeliveryState} from '@app/api/instance/AccountsConfigCache';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {
	getInstanceConfigRepository,
	getLimitConfigService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {getStoredBillingConfig, setStoredBillingConfig} from '@app/api/stripe/BillingConfigCache';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {fetchUserMe} from '@app/api/user/tests/UserTestUtils';
import {EmailDeliveryStates} from '@fluxer/constants/src/AccountConfigConstants';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AccountTiers} from '@fluxer/constants/src/AccountTierConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('lifetime gift codes on a self-hosted instance', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	const originalSelfHosted = getConfig().instance.selfHosted;
	const originalPremiumMode = getCachedInstancePremiumMode();
	const originalBilling = getStoredBillingConfig();

	beforeAll(async () => {
		getConfig().instance.selfHosted = true;
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
		setStoredBillingConfig(null);
		await getLimitConfigService().updatePolicyConfig({premium_mode: 'mirror'});
		setCachedInstancePremiumMode('mirror');
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.GIFT_CODES_GENERATE,
		]);
	});

	afterAll(async () => {
		await harness.shutdown();
		getConfig().instance.selfHosted = originalSelfHosted;
		setCachedInstancePremiumMode(originalPremiumMode);
		setStoredBillingConfig(originalBilling);
	});

	async function generate(body: Record<string, unknown>): Promise<string> {
		const {codes} = await createBuilder<{codes: Array<string>}>(harness, admin.token)
			.post('/admin/gift-codes')
			.body({count: 1, ...body})
			.execute();
		const code = codes[0]?.split('/').pop();
		if (!code) throw new Error('no gift code generated');
		return code;
	}

	it('issues a lifetime code that grants lifetime premium', async () => {
		const code = await generate({duration_type: 'lifetime', duration_quantity: 1});
		const gift = await createBuilderWithoutAuth<{duration_quantity: number}>(harness).get(`/gifts/${code}`).execute();
		expect(gift.duration_quantity).toBe(0);
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token).post(`/gifts/${code}/redeem`).expect(HTTP_STATUS.NO_CONTENT).execute();
		const {json: me} = await fetchUserMe(harness, account.token);
		expect(me.premium_type).toBe(UserPremiumTypes.LIFETIME);
		expect(me.premium_until ?? null).toBeNull();
		expect(me.traits).toContain('premium');
	});

	it('lets an account on a username instance redeem a code', async () => {
		await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
		const code = await generate({duration_type: 'lifetime', duration_quantity: 1});
		const account = await createTestAccount(harness);
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		await getUserRepository().patchUpsert(user.id, {email: null, email_verified: false}, user.toRow());
		await createBuilder(harness, account.token).post(`/gifts/${code}/redeem`).expect(HTTP_STATUS.NO_CONTENT).execute();
		const {json: me} = await fetchUserMe(harness, account.token);
		expect(me.premium_type).toBe(UserPremiumTypes.LIFETIME);
	});

	it('lets an account with an unconfirmed email redeem a code', async () => {
		setCachedEmailDeliveryState(EmailDeliveryStates.ON);
		try {
			const code = await generate({duration_type: 'months', duration_quantity: 1});
			const account = await createTestAccount(harness);
			const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
			await getUserRepository().patchUpsert(user.id, {email_verified: false}, user.toRow());
			const {json: before} = await fetchUserMe(harness, account.token);
			expect(before.account_tier).toBe(AccountTiers.UNVERIFIED);
			await createBuilder(harness, account.token)
				.post(`/gifts/${code}/redeem`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const {json: me} = await fetchUserMe(harness, account.token);
			expect(me.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
		} finally {
			setCachedEmailDeliveryState(null);
		}
	});

	it('grants lifetime premium without taking a visionary number', async () => {
		const slotsBefore = await getUserRepository().listVisionarySlots();
		const code = await generate({duration_type: 'lifetime', duration_quantity: 1});
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token).post(`/gifts/${code}/redeem`).expect(HTTP_STATUS.NO_CONTENT).execute();
		const user = await getUserRepository().findUnique(createUserID(BigInt(account.userId)));
		expect(user?.premiumType).toBe(UserPremiumTypes.LIFETIME);
		expect(user?.premiumLifetimeSequence ?? null).toBeNull();
		expect(await getUserRepository().listVisionarySlots()).toEqual(slotsBefore);
	});

	it('refuses lifetime codes on the hosted instance, where lifetime is Visionary', async () => {
		getConfig().instance.selfHosted = false;
		try {
			await createBuilder(harness, admin.token)
				.post('/admin/gift-codes')
				.body({count: 1, duration_type: 'lifetime', duration_quantity: 1})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
		} finally {
			getConfig().instance.selfHosted = true;
		}
	});

	it('still issues codes with a duration', async () => {
		const code = await generate({duration_type: 'months', duration_quantity: 3});
		const gift = await createBuilderWithoutAuth<{duration_type: string; duration_quantity: number}>(harness)
			.get(`/gifts/${code}`)
			.execute();
		expect(gift).toMatchObject({duration_type: 'months', duration_quantity: 3});
	});
});
