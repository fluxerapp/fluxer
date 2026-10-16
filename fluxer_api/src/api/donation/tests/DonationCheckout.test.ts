// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createDonationCheckoutBuilder,
	createValidCheckoutBody,
	DONATION_CURRENCY_VALUES,
	TEST_DONOR_EMAIL,
} from '@app/api/donation/tests/DonationTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers, type StripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {getDonationAmountConstraints} from '@fluxer/schema/src/domains/donation/DonationAmountUtils';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('POST /donations/checkout', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		stripeHandlers = createStripeApiHandlers();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.resetData();
		stripeHandlers.reset();
		server.use(...stripeHandlers.handlers);
	});
	test('creates checkout session with valid params', async () => {
		const response = await createDonationCheckoutBuilder(harness).body(createValidCheckoutBody()).expect(200).execute();
		expect(response.url).toContain('https://');
		expect(response.url).toContain('checkout.stripe.com');
		expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
	});
	test('passes customer email to stripe', async () => {
		await createDonationCheckoutBuilder(harness)
			.body(createValidCheckoutBody({email: TEST_DONOR_EMAIL}))
			.expect(200)
			.execute();
		expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
		const session = stripeHandlers.spies.createdCheckoutSessions[0];
		expect(session?.customer_email).toBe(TEST_DONOR_EMAIL);
	});
	test('sets subscription mode', async () => {
		await createDonationCheckoutBuilder(harness).body(createValidCheckoutBody()).expect(200).execute();
		expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
		const session = stripeHandlers.spies.createdCheckoutSessions[0];
		expect(session?.mode).toBe('subscription');
	});
	test('rejects amount below minimum', async () => {
		const minimumAmountMinor = getDonationAmountConstraints(DONATION_CURRENCY_VALUES.USD).minimumAmountMinor;
		await createDonationCheckoutBuilder(harness)
			.body(
				createValidCheckoutBody({
					amount_cents: minimumAmountMinor - 100,
				}),
			)
			.expect(400, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
	});
	test('rejects amount above maximum', async () => {
		const maximumAmountMinor = getDonationAmountConstraints(DONATION_CURRENCY_VALUES.USD).maximumAmountMinor;
		await createDonationCheckoutBuilder(harness)
			.body(
				createValidCheckoutBody({
					amount_cents: maximumAmountMinor + 100,
				}),
			)
			.expect(400, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
	});
	test.each([
		DONATION_CURRENCY_VALUES.USD,
		DONATION_CURRENCY_VALUES.EUR,
		DONATION_CURRENCY_VALUES.BRL,
		DONATION_CURRENCY_VALUES.INR,
		DONATION_CURRENCY_VALUES.PLN,
		DONATION_CURRENCY_VALUES.TRY,
		DONATION_CURRENCY_VALUES.SEK,
		DONATION_CURRENCY_VALUES.DKK,
		DONATION_CURRENCY_VALUES.NOK,
	])('accepts %s currency', async (currency) => {
		const amount_cents = getDonationAmountConstraints(currency).minimumAmountMinor;
		const response = await createDonationCheckoutBuilder(harness)
			.body(
				createValidCheckoutBody({
					currency,
					amount_cents,
				}),
			)
			.expect(200)
			.execute();
		expect(response.url).toBeDefined();
		expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
		const session = stripeHandlers.spies.createdCheckoutSessions[0];
		const lineItem = session?.line_items?.[0] as
			| {
					price_data?: {
						currency?: string;
					};
			  }
			| undefined;
		expect(lineItem?.price_data?.currency).toBe(currency);
		const nordic = ['sek', 'dkk', 'nok'].includes(currency);
		expect(session?.adaptive_pricing).toEqual(nordic ? {enabled: 'false'} : undefined);
	});
});
