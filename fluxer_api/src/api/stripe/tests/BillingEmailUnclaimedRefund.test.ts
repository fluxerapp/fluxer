// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BillingDocumentService} from '@app/api/billing_documents/BillingDocumentService';
import type {DonationRepository} from '@app/api/donation/DonationRepository';
import type {User} from '@app/api/models/User';
import {getProductRegistry} from '@app/api/stripe/ProductRegistry';
import {BillingEmailService} from '@app/api/stripe/services/BillingEmailService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type Stripe from 'stripe';
import {expect, test, vi} from 'vitest';

test('issues a credit note for a refund of a sale no billing email covers, such as Visionary', async () => {
	const refund = {
		id: 're_visionary',
		object: 'refund',
		status: 'succeeded',
		amount: 29900,
		currency: 'usd',
		created: 1_790_000_000,
		charge: 'ch_visionary',
		metadata: {},
		payment_intent: {id: 'pi_visionary', object: 'payment_intent', customer: 'cus_visionary', metadata: {}},
	};
	const stripe = {
		refunds: {retrieve: vi.fn(async () => refund)},
		invoicePayments: {list: () => (async function* () {})()},
		checkout: {sessions: {list: vi.fn(async () => ({data: [{id: 'cs_visionary', mode: 'payment', metadata: {}}]}))}},
	} as unknown as Stripe;
	const buyer = {id: 42n, locale: 'sv-SE'} as unknown as User;
	const userRepository = {findByStripeCustomerId: vi.fn(async () => buyer)} as unknown as UserRepository;
	const issueForRefund = vi.fn(async () => null);
	const documents = {issueForRefund} as unknown as BillingDocumentService;
	const service = new BillingEmailService(
		stripe,
		userRepository,
		{} as IEmailService,
		{} as ICacheService,
		getProductRegistry(),
		{} as DonationRepository,
		null,
		vi.fn(),
		documents,
	);

	await service.deliver({kind: 'refund', id: 're_visionary'});

	expect(issueForRefund).toHaveBeenCalledWith(refund, {userId: 42n, locale: 'sv-SE'});
});
