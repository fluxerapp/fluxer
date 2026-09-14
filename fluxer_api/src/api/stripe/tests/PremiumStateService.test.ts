// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers, type StripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface PremiumStatePaymentMethodResponse {
	billing: {
		payment_methods: Array<{
			id: string;
			type: string | null;
			card_brand: string | null;
			card_last4: string | null;
			is_default: boolean;
		}>;
	};
}

describe('PremiumStateService', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	beforeEach(async () => {
		harness = await createApiTestHarness();
		stripeHandlers = createStripeApiHandlers();
		server.use(...stripeHandlers.handlers);
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	test('lazily mirrors subscription default payment method and marks it as the customer default', async () => {
		const account = await createTestAccount(harness);
		const stripeCustomerId = 'cus_pm_repair_1';
		const stripeSubscriptionId = 'sub_pm_repair_1';
		const paymentMethodId = 'pm_pm_repair_1';
		stripeHandlers.reset();
		stripeHandlers = createStripeApiHandlers({
			customers: {
				[stripeCustomerId]: {
					invoice_settings: {default_payment_method: null},
				},
			},
			paymentMethods: {
				[paymentMethodId]: {
					customer: stripeCustomerId,
					card: {
						brand: 'mastercard',
						country: 'US',
						exp_month: 5,
						exp_year: 2032,
						last4: '1555',
					},
				},
			},
			subscriptions: {
				[stripeSubscriptionId]: {
					customer: stripeCustomerId,
					default_payment_method: paymentMethodId,
					status: 'active',
				},
			},
		});
		server.use(...stripeHandlers.handlers);
		await createBuilder(harness, account.token)
			.post(`/test/users/${account.userId}/premium`)
			.body({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString(),
				premium_billing_cycle: 'monthly',
				stripe_customer_id: stripeCustomerId,
				stripe_subscription_id: stripeSubscriptionId,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const state = await createBuilder<PremiumStatePaymentMethodResponse>(harness, account.token)
			.get('/premium/state')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(state.billing.payment_methods).toEqual([
			expect.objectContaining({
				id: paymentMethodId,
				type: 'card',
				card_brand: 'mastercard',
				card_last4: '1555',
				is_default: true,
			}),
		]);
		expect(stripeHandlers.spies.updatedCustomers).toEqual([
			expect.objectContaining({
				id: stripeCustomerId,
				params: expect.objectContaining({
					invoice_settings: expect.objectContaining({
						default_payment_method: paymentMethodId,
					}),
				}),
			}),
		]);
		const mirroredCustomer = await getBillingRepository().customers.findById(stripeCustomerId);
		const mirroredPaymentMethod = await getBillingRepository().paymentMethods.findById(paymentMethodId);
		expect(mirroredCustomer?.default_payment_method).toBe(paymentMethodId);
		expect(mirroredPaymentMethod?.customer_id).toBe(stripeCustomerId);
		expect(mirroredPaymentMethod?.is_default).toBe(true);
	});
	test('returns the stacked gift extension as the actual premium end', async () => {
		const account = await createTestAccount(harness);
		const subscriptionEnd = new Date(Date.now() + 30 * 24 * 60 * 60 * 1000);
		const giftExtensionEnd = new Date(Date.now() + 60 * 24 * 60 * 60 * 1000);
		await createBuilder(harness, account.token)
			.post(`/test/users/${account.userId}/premium`)
			.body({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: subscriptionEnd.toISOString(),
				premium_gift_extension_ends_at: giftExtensionEnd.toISOString(),
				premium_billing_cycle: 'monthly',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const state = await createBuilder<PremiumStateResponse>(harness, account.token)
			.get('/premium/state')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(state.actual.premium_until).toBe(giftExtensionEnd.toISOString());
		expect(state.effective.premium_until).toBe(giftExtensionEnd.toISOString());
	});
});
