// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DonationRepository} from '@app/api/donation/DonationRepository';
import {DonationCheckoutService} from '@app/api/donation/services/DonationCheckoutService';
import type {EmailDnsValidationService} from '@app/api/infrastructure/EmailDnsValidationService';
import type Stripe from 'stripe';
import {describe, expect, test, vi} from 'vitest';

function setUp() {
	const create = vi.fn(async (_params: Stripe.Checkout.SessionCreateParams) => ({
		id: 'cs_test_donation',
		url: 'https://checkout.stripe.com/c/pay/cs_test_donation',
	}));
	const stripe = {checkout: {sessions: {create}}} as unknown as Stripe;
	const repository = {findDonorByEmail: async () => null} as unknown as DonationRepository;
	const dns = {hasValidDnsRecords: async () => true} as unknown as EmailDnsValidationService;
	const service = new DonationCheckoutService(stripe, repository, dns);
	return {create, service};
}

describe('donation checkout Pix IOF', () => {
	test('a recurring BRL donation absorbs the IOF on the Pix mandate', async () => {
		const {create, service} = setUp();
		await service.createCheckout({email: 'donor@example.com', amountCents: 5000, currency: 'brl', interval: 'year'});
		const params = create.mock.calls[0]![0];
		expect(params.mode).toBe('subscription');
		expect(params.payment_method_options).toEqual({
			pix: {mandate_options: {amount: 5000, amount_includes_iof: 'always', payment_schedule: 'yearly'}},
		});
	});

	test('a recurring donation in another currency sends no Pix options', async () => {
		const {create, service} = setUp();
		await service.createCheckout({email: 'donor@example.com', amountCents: 5000, currency: 'eur', interval: 'month'});
		expect(create.mock.calls[0]![0].payment_method_options).toBeUndefined();
	});
});
