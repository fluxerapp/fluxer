// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripeInvoiceToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import {BillingPaymentsByInvoice} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import type Stripe from 'stripe';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const CREATED_AT = 1_779_000_000;
function invoice(
	paymentAmount: number,
	invoicePaidAt = CREATED_AT + 10,
	paymentPaidAt = CREATED_AT + 10,
): Stripe.Invoice {
	return {
		id: 'in_mirror',
		customer: 'cus_mirror',
		status: 'paid',
		currency: 'usd',
		amount_paid: 100,
		created: CREATED_AT,
		status_transitions: {paid_at: invoicePaidAt},
		metadata: {},
		payments: {
			data: [
				{
					id: 'inpay_mirror',
					status: 'paid',
					amount_paid: paymentAmount,
					created: CREATED_AT,
					status_transitions: {paid_at: paymentPaidAt},
					payment: {payment_intent: 'pi_mirror'},
					is_default: true,
				},
			],
		},
	} as Stripe.Invoice;
}

describe('billing mirror persistence', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.reset();
	});

	test('seeds bigint versions and increments updates that fetch their current row', async () => {
		const repository = getBillingRepository().customers;
		const customer = {
			id: 'cus_version',
			created: CREATED_AT,
			email: 'first@example.test',
			metadata: {},
		} as Stripe.Customer;
		expect((await repository.upsertFromStripe(customer)).row.version).toBe(1n);
		expect((await repository.upsertFromStripe({...customer, email: 'next@example.test'})).row.version).toBe(2n);
		expect(await repository.findById(customer.id)).toMatchObject({
			version: 2n,
			email: 'next@example.test',
			name: null,
			metadata: null,
		});
		expect((await repository.upsertFromStripe({...customer, email: 'next@example.test'})).changed).toBe(false);
		await repository.markDeleted(customer.id, new Date((CREATED_AT + 20) * 1000));
		expect(await repository.findById(customer.id)).toMatchObject({version: 3n, deleted: true});
		const methods = getBillingRepository().paymentMethods;
		const method = {
			id: 'pm_version',
			customer: customer.id,
			created: CREATED_AT,
			type: 'card',
			metadata: {},
		} as Stripe.PaymentMethod;
		expect((await methods.upsertFromStripe(method)).row.version).toBe(1n);
		await methods.markDetached(method.id, new Date((CREATED_AT + 20) * 1000));
		expect(await methods.findById(method.id)).toMatchObject({version: 2n, customer_id: null});
		expect(await methods.listByCustomer(customer.id)).toEqual([]);
	});

	test('persists invoice payments when the invoice is equivalent or has an older timestamp', async () => {
		const repository = getBillingRepository();
		expect((await repository.invoices.upsertFromStripe(invoice(100))).changed).toBe(true);
		expect((await repository.invoices.upsertFromStripe(invoice(70))).changed).toBe(false);
		expect(await repository.payments.findById('inpay_mirror')).toMatchObject({amount_paid: 70n});
		expect((await repository.invoices.upsertFromStripe(invoice(80, CREATED_AT + 5, CREATED_AT + 20))).changed).toBe(
			false,
		);
		expect(await repository.payments.listByInvoice('in_mirror')).toMatchObject([{amount_paid: 80n}]);
		expect((await repository.invoices.findById('in_mirror'))?.version).toBe(1n);
		expect((await repository.invoices.listByCustomer('cus_mirror')).rows).toHaveLength(1);
	});

	test('keeps payments without an invoice out of the by-invoice index', async () => {
		const mapped = mapStripeInvoiceToRow(invoice(100)).payments[0]!;
		mapped.primary.invoice_id = '';
		mapped.byInvoice.invoice_id = '';
		expect((await getBillingRepository().payments.upsertFromStripeMapped(mapped)).changed).toBe(true);
		expect(
			await fetchMany(BillingPaymentsByInvoice.selectCql({where: BillingPaymentsByInvoice.where.eq('invoice_id')}), {
				invoice_id: '',
			}),
		).toEqual([]);
	});
});
