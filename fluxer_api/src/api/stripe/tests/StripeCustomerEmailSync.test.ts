// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {getStripeClient} from '@app/api/stripe/StripeClient';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {createStripeApiHandlers, type StripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import reconcileUserPayments from '@app/api/worker/tasks/ReconcileUserPayments';
import syncStripeCustomerEmail from '@app/api/worker/tasks/SyncStripeCustomerEmail';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import type Stripe from 'stripe';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const MOCK_PRICES = {
	monthlyUsd: 'price_email_sync_monthly_usd',
	yearlyUsd: 'price_email_sync_yearly_usd',
	gift1MonthUsd: 'price_email_sync_gift_1_month_usd',
	gift1YearUsd: 'price_email_sync_gift_1_year_usd',
};

const MOCK_PRICE_SEEDS = {
	[MOCK_PRICES.monthlyUsd]: {unit_amount: 499, currency: 'usd', interval: 'month' as const},
	[MOCK_PRICES.yearlyUsd]: {unit_amount: 4999, currency: 'usd', interval: 'year' as const},
};

const STALE_EMAIL = 'previous-address@example.com';

function createHelpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 0n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

async function currentEmail(account: TestAccount): Promise<string> {
	const user = await getUserRepository().findUnique(createUserID(BigInt(account.userId)));
	return user!.email!;
}

async function mirrorCustomer(customerId: string, account: TestAccount, email: string | null): Promise<void> {
	await getBillingRepository().customers.upsertFromStripe(
		{
			id: customerId,
			object: 'customer',
			email,
			created: 1_600_000_000,
			livemode: false,
			metadata: {userId: account.userId},
		} as unknown as Stripe.Customer,
		{knownUserId: BigInt(account.userId)},
	);
}

describe('Stripe customer email sync', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	let originalPrices: typeof Config.stripe.prices | undefined;

	async function createCustomerAccount(customerId: string): Promise<TestAccount> {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: true})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/premium`)
			.body({stripe_customer_id: customerId})
			.expect(HTTP_STATUS.OK)
			.execute();
		return account;
	}

	async function runTask(account: TestAccount): Promise<void> {
		await syncStripeCustomerEmail({userId: account.userId}, createHelpers());
	}

	function emailUpdatesFor(customerId: string): Array<unknown> {
		return stripeHandlers.spies.updatedCustomers
			.filter((update) => update.id === customerId && 'email' in update.params)
			.map((update) => update.params.email);
	}

	beforeAll(async () => {
		originalPrices = Config.stripe.prices;
		Config.stripe.prices = MOCK_PRICES;
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.prices = originalPrices;
	});
	beforeEach(async () => {
		await harness.reset();
		Config.stripe.prices = MOCK_PRICES;
		stripeHandlers = createStripeApiHandlers({prices: MOCK_PRICE_SEEDS, subscriptionsListEmpty: true});
		server.use(...stripeHandlers.handlers);
		const stripe = getStripeClient();
		expect(stripe).not.toBeNull();
		setWorkerDependenciesForTest({userRepository: getUserRepository(), stripe});
	});
	afterEach(() => {
		clearWorkerDependencies();
	});

	describe('worker task', () => {
		test('pushes the account email to a customer that still has the old address', async () => {
			const account = await createCustomerAccount('cus_sync_stale');
			await mirrorCustomer('cus_sync_stale', account, STALE_EMAIL);
			await runTask(account);
			const email = await currentEmail(account);
			expect(emailUpdatesFor('cus_sync_stale')).toEqual([email]);
			expect((await getBillingRepository().customers.findById('cus_sync_stale'))?.email).toBe(email);
		});

		test('pushes the account email when the customer is not mirrored yet', async () => {
			const account = await createCustomerAccount('cus_sync_unmirrored');
			await runTask(account);
			expect(emailUpdatesFor('cus_sync_unmirrored')).toEqual([await currentEmail(account)]);
		});

		test('does nothing when the customer already has the account email', async () => {
			const account = await createCustomerAccount('cus_sync_current');
			await mirrorCustomer('cus_sync_current', account, await currentEmail(account));
			await runTask(account);
			expect(stripeHandlers.spies.updatedCustomers).toHaveLength(0);
		});

		test('does nothing for a customer mirrored as deleted', async () => {
			const account = await createCustomerAccount('cus_sync_deleted');
			await getBillingRepository().customers.upsertFromStripe(
				{id: 'cus_sync_deleted', object: 'customer', deleted: true} as Stripe.DeletedCustomer,
				{knownUserId: BigInt(account.userId)},
			);
			await runTask(account);
			expect(stripeHandlers.spies.updatedCustomers).toHaveLength(0);
		});

		test('completes without retrying when Stripe no longer has the customer', async () => {
			server.use(...createStripeApiHandlers({customerShouldFail: true}).handlers);
			const account = await createCustomerAccount('cus_sync_missing');
			await mirrorCustomer('cus_sync_missing', account, STALE_EMAIL);
			await expect(runTask(account)).resolves.toBeUndefined();
			expect((await getBillingRepository().customers.findById('cus_sync_missing'))?.email).toBe(STALE_EMAIL);
		});

		test('does nothing for users without a Stripe customer', async () => {
			const account = await createTestAccount(harness);
			await runTask(account);
			expect(stripeHandlers.spies.updatedCustomers).toHaveLength(0);
		});
	});

	describe('payment reconciliation', () => {
		test('heals a customer email that drifted before the account email changed', async () => {
			setWorkerDependenciesForTest({
				userRepository: getUserRepository(),
				paymentRepository: new PaymentRepository(),
				stripe: getStripeClient(),
			});
			const account = await createCustomerAccount('cus_sync_reconcile');
			await mirrorCustomer('cus_sync_reconcile', account, STALE_EMAIL);
			await reconcileUserPayments({userId: account.userId}, createHelpers());
			expect(emailUpdatesFor('cus_sync_reconcile')).toEqual([await currentEmail(account)]);
		});

		test('leaves a customer alone when the email already matches', async () => {
			setWorkerDependenciesForTest({
				userRepository: getUserRepository(),
				paymentRepository: new PaymentRepository(),
				stripe: getStripeClient(),
			});
			const account = await createCustomerAccount('cus_sync_reconcile_current');
			await mirrorCustomer('cus_sync_reconcile_current', account, await currentEmail(account));
			await reconcileUserPayments({userId: account.userId}, createHelpers());
			expect(emailUpdatesFor('cus_sync_reconcile_current')).toHaveLength(0);
		});
	});

	describe('checkout', () => {
		test('updates a stale customer email before opening checkout with that customer', async () => {
			const account = await createCustomerAccount('cus_checkout_stale');
			await mirrorCustomer('cus_checkout_stale', account, STALE_EMAIL);
			await createBuilder(harness, account.token)
				.post('/stripe/checkout/subscription')
				.body({price_id: MOCK_PRICES.monthlyUsd})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(emailUpdatesFor('cus_checkout_stale')).toEqual([await currentEmail(account)]);
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
			expect(stripeHandlers.spies.createdCheckoutSessions[0]?.customer).toBe('cus_checkout_stale');
		});

		test('leaves the customer alone when its email already matches', async () => {
			const account = await createCustomerAccount('cus_checkout_current');
			await mirrorCustomer('cus_checkout_current', account, await currentEmail(account));
			await createBuilder(harness, account.token)
				.post('/stripe/checkout/subscription')
				.body({price_id: MOCK_PRICES.monthlyUsd})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(emailUpdatesFor('cus_checkout_current')).toHaveLength(0);
			expect(stripeHandlers.spies.createdCheckoutSessions[0]?.customer).toBe('cus_checkout_current');
		});

		test('still opens checkout when the email update fails', async () => {
			server.use(
				...createStripeApiHandlers({prices: MOCK_PRICE_SEEDS, subscriptionsListEmpty: true, customerShouldFail: true})
					.handlers,
			);
			const account = await createCustomerAccount('cus_checkout_update_fails');
			await mirrorCustomer('cus_checkout_update_fails', account, STALE_EMAIL);
			await createBuilder(harness, account.token)
				.post('/stripe/checkout/subscription')
				.body({price_id: MOCK_PRICES.monthlyUsd})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
	});
});
