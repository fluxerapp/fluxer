// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers, type StripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, expect, test} from 'vitest';

const MOCK_PRICES = {
	monthlyUsd: 'price_waiver_monthly_usd',
	yearlyUsd: 'price_waiver_yearly_usd',
	gift1MonthUsd: 'price_waiver_gift_1_month_usd',
	gift1YearUsd: 'price_waiver_gift_1_year_usd',
};

let harness: ApiTestHarness;
let stripeHandlers: StripeApiHandlers;
let originalPrices: typeof Config.stripe.prices | undefined;

beforeAll(async () => {
	originalPrices = Config.stripe.prices;
	harness = await createApiTestHarness();
});
afterAll(async () => {
	await harness.shutdown();
	Config.stripe.prices = originalPrices;
});
beforeEach(async () => {
	await harness.reset();
	Config.stripe.prices = MOCK_PRICES;
	stripeHandlers = createStripeApiHandlers({
		prices: {[MOCK_PRICES.monthlyUsd]: {unit_amount: 499, currency: 'usd', interval: 'month'}},
		subscriptionsListEmpty: true,
	});
	server.use(...stripeHandlers.handlers);
});

test('a UK consumer asks for an immediate start and is told the days used are paid for', async () => {
	const account = await createTestAccount(harness);
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${account.userId}/security-flags`)
		.body({email_verified: true})
		.expect(HTTP_STATUS.OK)
		.execute();
	await createBuilder(harness, account.token)
		.post('/stripe/checkout/subscription')
		.body({price_id: MOCK_PRICES.monthlyUsd, client_geoip_country_code: 'GB', eu_withdrawal_waiver_accepted: true})
		.expect(HTTP_STATUS.OK)
		.execute();
	const session = stripeHandlers.spies.createdCheckoutSessions[0] as unknown as {
		metadata: Record<string, string>;
		custom_text: {terms_of_service_acceptance: {message: string}};
	};
	expect(session.metadata.eu_withdrawal_waiver_required).toBe('true');
	expect(session.metadata.eu_withdrawal_waiver_accepted).toBe('true');
	expect(session.custom_text.terms_of_service_acceptance.message).toContain('I pay for the days I had it');
});

async function brazilCheckout(body: Record<string, unknown>) {
	const account = await createTestAccount(harness);
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${account.userId}/security-flags`)
		.body({email_verified: true})
		.expect(HTTP_STATUS.OK)
		.execute();
	await createBuilder(harness, account.token)
		.post('/stripe/checkout/subscription')
		.body({price_id: MOCK_PRICES.monthlyUsd, client_geoip_country_code: 'BR', ...body})
		.expect(HTTP_STATUS.OK)
		.execute();
	return stripeHandlers.spies.createdCheckoutSessions[0] as unknown as {
		metadata: Record<string, string>;
		billing_address_collection: string;
		custom_text?: {terms_of_service_acceptance?: {message: string}; submit?: {message: string}};
	};
}

test('a Brazilian buyer sees no waiver, only the 7-day right and that renewals start no new period', async () => {
	const session = await brazilCheckout({is_business: true});
	expect(session.metadata.withdrawal_country).toBe('BR');
	expect(session.metadata.cnpj).toBeUndefined();
	expect(session.billing_address_collection).toBe('required');
	expect(session.custom_text?.terms_of_service_acceptance?.message ?? '').not.toContain('start right away');
	expect(session.custom_text?.submit?.message).toContain("doesn't start a new withdrawal period");
});

test('a Brazilian business buyer with a CNPJ has it stored and gets no withdrawal text', async () => {
	const session = await brazilCheckout({is_business: true, cnpj: '11.222.333/0001-81'});
	expect(session.metadata.cnpj).toBe('11222333000181');
	expect(session.custom_text?.submit).toBeUndefined();
});

test('a CNPJ with wrong check digits is refused', async () => {
	const account = await createTestAccount(harness);
	await createBuilder(harness, account.token)
		.post('/stripe/checkout/subscription')
		.body({
			price_id: MOCK_PRICES.monthlyUsd,
			client_geoip_country_code: 'BR',
			is_business: true,
			cnpj: '11222333000182',
		})
		.expect(HTTP_STATUS.BAD_REQUEST)
		.execute();
});
