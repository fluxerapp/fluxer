// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {
	getInstanceConfigRepository,
	getLimitConfigService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {getStoredBillingConfig, setStoredBillingConfig} from '@app/api/stripe/BillingConfigCache';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('gift code redemption countries', () => {
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

	async function generate(): Promise<string> {
		const {codes} = await createBuilder<{codes: Array<string>}>(harness, admin.token)
			.post('/admin/gift-codes')
			.body({count: 1, duration_type: 'months', duration_quantity: 1})
			.execute();
		const code = codes[0]?.split('/').pop();
		if (!code) throw new Error('no gift code generated');
		return code;
	}

	async function addPayment(userId: UserID, id: string, createdAt: Date, taxCountry: string | null): Promise<void> {
		const payments = new PaymentRepository();
		await payments.createPayment({
			checkout_session_id: id,
			user_id: userId,
			price_id: 'price_test',
			product_type: 'gift_1_month',
			status: 'completed',
			is_gift: true,
			created_at: createdAt,
		});
		if (taxCountry !== null) {
			await payments.updatePayment({checkout_session_id: id, tax_country: taxCountry});
		}
	}

	it('stores the country evidence on the redemption and clears it on unredeem', async () => {
		const code = await generate();
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		await getUserRepository().redeemGiftCode(code, userId, {
			geoipCountryCode: 'DE',
			billingCountryCode: 'FR',
		});
		const redeemed = await getUserRepository().findGiftCode(code);
		expect(redeemed).toMatchObject({
			redeemedByUserId: userId,
			redeemedGeoipCountryCode: 'DE',
			redeemedBillingCountryCode: 'FR',
		});
		await getUserRepository().unredeemGiftCode(code, userId);
		const cleared = await getUserRepository().findGiftCode(code);
		expect(cleared).toMatchObject({
			redeemedByUserId: null,
			redeemedGeoipCountryCode: null,
			redeemedBillingCountryCode: null,
		});
	});

	it('records no country when the request has no geoip result and the redeemer never paid', async () => {
		const code = await generate();
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token).post(`/gifts/${code}/redeem`).expect(HTTP_STATUS.NO_CONTENT).execute();
		const gift = await getUserRepository().findGiftCode(code);
		expect(gift?.redeemedByUserId).toBe(createUserID(BigInt(account.userId)));
		expect(gift).toMatchObject({
			redeemedGeoipCountryCode: null,
			redeemedBillingCountryCode: null,
		});
	});

	it('takes the billing country from the redeemer newest payment that has one', async () => {
		const code = await generate();
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		await addPayment(userId, 'cs_redeem_old', new Date('2026-01-01T00:00:00Z'), 'se');
		await addPayment(userId, 'cs_redeem_mid', new Date('2026-06-01T00:00:00Z'), 'no');
		await addPayment(userId, 'cs_redeem_new', new Date('2026-09-01T00:00:00Z'), null);
		await createBuilder(harness, account.token).post(`/gifts/${code}/redeem`).expect(HTTP_STATUS.NO_CONTENT).execute();
		const gift = await getUserRepository().findGiftCode(code);
		expect(gift?.redeemedBillingCountryCode).toBe('NO');
		expect(gift?.redeemedBillingCountryCode).toMatch(/^[A-Z]{2}$/);
	});
});
