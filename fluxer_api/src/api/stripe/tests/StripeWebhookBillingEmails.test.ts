// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearTestEmails, createTestAccount, listTestEmails} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {ProductType} from '@app/api/stripe/ProductRegistry';
import {enqueueBillingEmail} from '@app/api/stripe/services/BillingEmailService';
import {
	BILLING_EMAIL_PRICES,
	type InvoiceOptions,
	invoiceLine,
	objectEvent,
	stripeInvoice,
	stripeRefund,
} from '@app/api/stripe/tests/BillingEmailTestUtils';
import {sendStripeWebhook, setupSyncStripeWebhookWorker} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers, type StripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import sendBillingEmail from '@app/api/worker/tasks/SendBillingEmail';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

const DAY_MS = 24 * 60 * 60 * 1000;

describe('Stripe Webhook - billing emails', () => {
	let harness: ApiTestHarness;
	let stripe: StripeApiHandlers;
	let originalWebhookSecret: string | undefined;
	let originalPrices: typeof Config.stripe.prices | undefined;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		setupSyncStripeWebhookWorker();
		originalWebhookSecret = Config.stripe.webhookSecret;
		originalPrices = Config.stripe.prices;
		Config.stripe.webhookSecret = 'whsec_test_secret';
		Config.stripe.prices = BILLING_EMAIL_PRICES;
		stripe = createStripeApiHandlers();
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.webhookSecret = originalWebhookSecret;
		Config.stripe.prices = originalPrices;
	});
	beforeEach(async () => {
		await harness.resetData();
		stripe.reset();
		server.use(...stripe.handlers);
		await clearTestEmails(harness);
	});
	afterEach(() => {
		server.resetHandlers();
	});

	async function createBuyer(customerId: string, patch: Partial<UserRow> = {}) {
		const account = await createTestAccount(harness);
		const userRepository = new UserRepository();
		const userId = createUserID(BigInt(account.userId));
		await userRepository.patchUpsert(
			userId,
			{stripe_customer_id: customerId, ...patch},
			(await userRepository.findUnique(userId))!.toRow(),
		);
		return account;
	}

	async function createSubscriber(customerId: string, patch: Partial<UserRow> = {}) {
		const subscriptionId = `sub_${customerId}`;
		const account = await createBuyer(customerId, {
			premium_type: UserPremiumTypes.SUBSCRIPTION,
			premium_until: new Date(Date.now() + 2 * DAY_MS),
			stripe_subscription_id: subscriptionId,
			premium_billing_cycle: 'monthly',
			...patch,
		});
		const payments = new PaymentRepository();
		const checkoutSessionId = `cs_${customerId}`;
		await payments.createPayment({
			checkout_session_id: checkoutSessionId,
			user_id: createUserID(BigInt(account.userId)),
			price_id: BILLING_EMAIL_PRICES.monthlyUsd,
			product_type: ProductType.MONTHLY_SUBSCRIPTION,
			status: 'completed',
			is_gift: false,
			created_at: new Date(),
		});
		await payments.updatePayment({checkout_session_id: checkoutSessionId, subscription_id: subscriptionId});
		return account;
	}

	function renewal(customerId: string, options: Partial<InvoiceOptions> = {}): InvoiceOptions {
		return {id: `in_${customerId}`, customerId, subscriptionId: `sub_${customerId}`, ...options};
	}

	async function emailsOfType(recipient: string, type: string) {
		return (await listTestEmails(harness, {recipient})).filter((email) => email.type === type);
	}

	async function invoiceWebhook(type: string, options: InvoiceOptions) {
		const invoice = stripeInvoice(options);
		stripe.put.invoice(invoice);
		await sendStripeWebhook(harness, objectEvent(type, invoice));
		return invoice;
	}

	async function refundWebhook(type: string, refund: ReturnType<typeof stripeRefund>) {
		stripe.put.refund(refund);
		await sendStripeWebhook(harness, objectEvent(type, refund));
	}

	describe('Plutonium receipts', () => {
		test('sends one receipt for a paid invoice across retries and the sibling event', async () => {
			const account = await createSubscriber('cus_paid');
			await invoiceWebhook('invoice.paid', renewal('cus_paid'));
			await invoiceWebhook('invoice.paid', renewal('cus_paid'));
			await invoiceWebhook('invoice.payment_succeeded', renewal('cus_paid'));
			const receipts = await emailsOfType(account.email, 'plutonium_payment_receipt');
			expect(receipts).toHaveLength(1);
			expect(receipts[0]!.metadata).toMatchObject({
				plan: 'monthly',
				invoice_number: 'NUM-in_cus_paid',
				total_minor: '499',
				invoice_url: 'https://invoice.stripe.com/i/in_cus_paid',
				renewal: '',
			});
		});

		test('adds the purchase terms and withdrawal deadline to the first receipt of an EU consumer', async () => {
			const account = await createBuyer('cus_first');
			const acceptedAt = new Date(Date.now() - 60_000).toISOString();
			await invoiceWebhook('invoice.paid', {
				id: 'in_first',
				customerId: 'cus_first',
				billingReason: 'subscription_create',
				subscriptionMetadata: {
					eu_withdrawal_waiver_required: 'true',
					eu_withdrawal_waiver_accepted: 'true',
					eu_withdrawal_waiver_accepted_at: acceptedAt,
				},
			});
			const [receipt] = await emailsOfType(account.email, 'plutonium_payment_receipt');
			expect(receipt!.metadata).toMatchObject({renewal: 'month', consent_at: acceptedAt});
			const withdrawalEndsAt = new Date(receipt!.metadata.withdrawal_ends_at!).getTime();
			expect(withdrawalEndsAt - Date.now()).toBeGreaterThan(13 * DAY_MS);
		});

		test('names the new plan on an immediate switch from monthly to yearly', async () => {
			const account = await createBuyer('cus_switch');
			await invoiceWebhook('invoice.paid', {
				id: 'in_switch',
				customerId: 'cus_switch',
				billingReason: 'subscription_update',
				total: 4500,
				lines: [
					invoiceLine(BILLING_EMAIL_PRICES.monthlyUsd, {amount: -350, days: 18, proration: true}),
					invoiceLine(BILLING_EMAIL_PRICES.yearlyUsd, {amount: 4850, days: 365}),
				],
			});
			const [receipt] = await emailsOfType(account.email, 'plutonium_payment_receipt');
			expect(receipt!.metadata.plan).toBe('yearly');
			const periodDays =
				(Date.parse(receipt!.metadata.period_end!) - Date.parse(receipt!.metadata.period_start!)) / DAY_MS;
			expect(periodDays).toBe(365);
		});

		test('shows the invoice total and the amount paid when credit covers part of it', async () => {
			const account = await createSubscriber('cus_credit');
			await invoiceWebhook(
				'invoice.paid',
				renewal('cus_credit', {total: 999, amountPaid: 499, tax: {amount: 200, behavior: 'inclusive'}}),
			);
			const [receipt] = await emailsOfType(account.email, 'plutonium_payment_receipt');
			expect(receipt!.metadata).toMatchObject({
				total_minor: '999',
				paid_minor: '499',
				tax_minor: '200',
				tax_behavior: 'inclusive',
			});
		});

		test('sends a receipt when account credit pays the whole invoice', async () => {
			const account = await createSubscriber('cus_all_credit');
			await invoiceWebhook('invoice.paid', renewal('cus_all_credit', {amountPaid: 0}));
			expect(await emailsOfType(account.email, 'plutonium_payment_receipt')).toHaveLength(1);
		});

		test('marks a reverse-charged invoice', async () => {
			const account = await createSubscriber('cus_b2b');
			await invoiceWebhook('invoice.paid', renewal('cus_b2b', {reverseCharge: true}));
			const [receipt] = await emailsOfType(account.email, 'plutonium_payment_receipt');
			expect(receipt!.metadata).toMatchObject({reverse_charge: 'true', tax_minor: ''});
		});

		test('attaches the invoice PDF', async () => {
			const account = await createSubscriber('cus_pdf');
			await invoiceWebhook('invoice.paid', renewal('cus_pdf', {invoicePdf: true}));
			const [receipt] = await emailsOfType(account.email, 'plutonium_payment_receipt');
			expect(receipt!.metadata.invoice_pdf).toBe('Invoice-NUM-in_cus_pdf.pdf');
		});

		test('sends a gift receipt without a service period', async () => {
			const account = await createBuyer('cus_gift');
			await invoiceWebhook('invoice.paid', {
				id: 'in_gift',
				customerId: 'cus_gift',
				billingReason: 'manual',
				subscriptionId: null,
				lines: [invoiceLine(BILLING_EMAIL_PRICES.gift1YearUsd, {amount: 4999})],
			});
			const [receipt] = await emailsOfType(account.email, 'plutonium_payment_receipt');
			expect(receipt!.metadata).toMatchObject({plan: 'gift_1_year', period_start: ''});
		});

		test('sends the receipt in the buyer locale', async () => {
			const account = await createSubscriber('cus_swedish', {locale: 'sv-SE'});
			await invoiceWebhook('invoice.paid', renewal('cus_swedish'));
			const receipts = await emailsOfType(account.email, 'plutonium_payment_receipt');
			expect(receipts.map((receipt) => receipt.metadata.locale)).toEqual(['sv-SE']);
		});

		test('sends nothing to a hard-bounced address', async () => {
			const account = await createSubscriber('cus_bounced', {email_bounced: true});
			await invoiceWebhook('invoice.paid', renewal('cus_bounced'));
			expect(await emailsOfType(account.email, 'plutonium_payment_receipt')).toHaveLength(0);
		});

		test('sends nothing for a $0 invoice or an invoice without a Plutonium price', async () => {
			const account = await createSubscriber('cus_zero');
			await invoiceWebhook('invoice.paid', renewal('cus_zero', {total: 0}));
			await invoiceWebhook('invoice.paid', {
				id: 'in_other',
				customerId: 'cus_zero',
				billingReason: 'manual',
				subscriptionId: null,
				lines: [invoiceLine('price_unknown')],
			});
			expect(await emailsOfType(account.email, 'plutonium_payment_receipt')).toHaveLength(0);
		});
	});

	describe('payment problems', () => {
		function openRenewal(customerId: string, attemptCount: number): InvoiceOptions {
			return renewal(customerId, {status: 'open', attemptCount});
		}

		test('sends one notice per failed payment attempt', async () => {
			const account = await createSubscriber('cus_failing');
			await invoiceWebhook('invoice.payment_failed', openRenewal('cus_failing', 1));
			await invoiceWebhook('invoice.payment_failed', openRenewal('cus_failing', 1));
			await invoiceWebhook('invoice.payment_failed', openRenewal('cus_failing', 2));
			const notices = await emailsOfType(account.email, 'plutonium_payment_failed');
			expect(notices.map((notice) => notice.metadata.reason)).toEqual(['declined', 'declined']);
		});

		test('asks the customer to confirm the payment when the bank wants it', async () => {
			const account = await createSubscriber('cus_sca');
			await invoiceWebhook('invoice.payment_action_required', openRenewal('cus_sca', 1));
			const [notice] = await emailsOfType(account.email, 'plutonium_payment_failed');
			expect(notice!.metadata.reason).toBe('action_required');
		});

		test('sends nothing when the invoice is paid by the time the notice goes out', async () => {
			const account = await createSubscriber('cus_paid_later');
			const failed = stripeInvoice(openRenewal('cus_paid_later', 1));
			stripe.put.invoice(stripeInvoice({...openRenewal('cus_paid_later', 1), status: 'paid'}));
			await sendStripeWebhook(harness, objectEvent('invoice.payment_failed', failed));
			expect(await emailsOfType(account.email, 'plutonium_payment_failed')).toHaveLength(0);
		});

		test('gives the access date of a gift extension that outlasts the subscription', async () => {
			const giftEndsAt = new Date(Date.now() + 40 * DAY_MS);
			const account = await createSubscriber('cus_gift_ext', {premium_gift_extension_ends_at: giftEndsAt});
			await invoiceWebhook('invoice.payment_failed', openRenewal('cus_gift_ext', 1));
			const [notice] = await emailsOfType(account.email, 'plutonium_payment_failed');
			expect(Date.parse(notice!.metadata.access_ends_at!)).toBeGreaterThanOrEqual(giftEndsAt.getTime());
		});
	});

	describe('refunds', () => {
		async function refundedPlutoniumBuyer() {
			const account = await createBuyer('cus_refund', {locale: 'de'});
			stripe.put.invoice(stripeInvoice({id: 'in_refunded', customerId: 'cus_refund', total: 4999}));
			return account;
		}

		test('sends one refund receipt per refund across refund and charge events', async () => {
			const account = await refundedPlutoniumBuyer();
			const refund = stripeRefund({id: 're_full', paymentIntentId: 'pi_in_refunded', amount: 4999});
			await refundWebhook('refund.created', refund);
			await refundWebhook('refund.updated', refund);
			await sendStripeWebhook(
				harness,
				objectEvent('charge.refunded', {
					id: 'ch_pi_in_refunded',
					object: 'charge',
					amount: 4999,
					amount_refunded: 4999,
					currency: 'usd',
					customer: 'cus_refund',
					payment_intent: 'pi_in_refunded',
					refunded: true,
					refunds: {object: 'list', data: [refund], has_more: false},
				}),
			);
			const receipts = await emailsOfType(account.email, 'plutonium_refund_receipt');
			expect(receipts).toHaveLength(1);
			expect(receipts[0]!.metadata).toMatchObject({
				invoice_number: 'NUM-in_refunded',
				amount_minor: '4999',
				paid_minor: '4999',
				locale: 'de',
			});
		});

		test('waits until a pending refund succeeds', async () => {
			const account = await refundedPlutoniumBuyer();
			await refundWebhook(
				'refund.created',
				stripeRefund({id: 're_slow', paymentIntentId: 'pi_in_refunded', status: 'pending'}),
			);
			expect(await emailsOfType(account.email, 'plutonium_refund_receipt')).toHaveLength(0);
			await refundWebhook('refund.updated', stripeRefund({id: 're_slow', paymentIntentId: 'pi_in_refunded'}));
			expect(await emailsOfType(account.email, 'plutonium_refund_receipt')).toHaveLength(1);
		});

		test('does not email old refunds listed on a newly refunded charge', async () => {
			const account = await refundedPlutoniumBuyer();
			const old = stripeRefund({id: 're_old', paymentIntentId: 'pi_in_refunded', amount: 100, createdDaysAgo: 30});
			const fresh = stripeRefund({id: 're_new', paymentIntentId: 'pi_in_refunded', amount: 200});
			stripe.put.refund(old);
			stripe.put.refund(fresh);
			await sendStripeWebhook(
				harness,
				objectEvent('charge.refunded', {
					id: 'ch_pi_in_refunded',
					object: 'charge',
					amount: 4999,
					amount_refunded: 300,
					currency: 'usd',
					customer: 'cus_refund',
					payment_intent: 'pi_in_refunded',
					refunded: false,
					refunds: {object: 'list', data: [old, fresh], has_more: false},
				}),
			);
			const receipts = await emailsOfType(account.email, 'plutonium_refund_receipt');
			expect(receipts.map((receipt) => receipt.metadata.amount_minor)).toEqual(['200']);
		});
	});

	describe('donations', () => {
		const DONOR = 'donor@example.com';
		const donationSubscription = {is_donation: 'true', donation_email: DONOR, donation_locale: 'sv-SE'};

		test('sends a receipt for each donation renewal', async () => {
			await invoiceWebhook('invoice.paid', {
				id: 'in_donation',
				customerId: 'cus_donor',
				subscriptionMetadata: donationSubscription,
				lines: [invoiceLine('price_donation_custom', {amount: 1000})],
				total: 1000,
			});
			const [receipt] = await emailsOfType(DONOR, 'donation_receipt');
			expect(receipt!.metadata).toMatchObject({interval: 'month', total_minor: '1000', locale: 'sv-SE'});
		});

		test('tells the donor when a renewal payment fails', async () => {
			await invoiceWebhook('invoice.payment_failed', {
				id: 'in_donation_failed',
				customerId: 'cus_donor',
				status: 'open',
				subscriptionMetadata: donationSubscription,
				lines: [invoiceLine('price_donation_custom', {amount: 1000})],
				total: 1000,
			});
			const [notice] = await emailsOfType(DONOR, 'donation_payment_failed');
			expect(notice!.metadata).toMatchObject({reason: 'declined', amount_minor: '1000'});
		});

		test('sends a refund receipt for a refunded one-time donation', async () => {
			stripe.put.paymentIntent({
				id: 'pi_one_time_donation',
				object: 'payment_intent',
				status: 'succeeded',
				metadata: {is_donation: 'true', donation_email: DONOR},
			});
			await refundWebhook(
				'refund.created',
				stripeRefund({id: 're_donation', paymentIntentId: 'pi_one_time_donation', amount: 2500}),
			);
			const [receipt] = await emailsOfType(DONOR, 'donation_refund_receipt');
			expect(receipt!.metadata.amount_minor).toBe('2500');
		});
	});

	describe('Operator Pass', () => {
		async function operatorPassBuyer() {
			const account = await createTestAccount(harness);
			await new OperatorPassRepository().createPurchase({
				checkout_session_id: 'cs_op',
				purchaser_id: createUserID(BigInt(account.userId)),
				status: 'completed',
				phase: 'early_bird',
				amount_minor: 19900,
				currency: 'usd',
				quote_honored: true,
				payment_intent_id: 'pi_op',
				created_at: new Date(),
				version: 1,
			} as OperatorPassPurchaseRow);
			return account;
		}

		test('emails the buyer once about a refund made in the Stripe dashboard', async () => {
			const account = await operatorPassBuyer();
			const refund = stripeRefund({id: 're_op', paymentIntentId: 'pi_op', amount: 19900});
			await refundWebhook('refund.created', refund);
			await refundWebhook('refund.updated', refund);
			const receipts = await emailsOfType(account.email, 'operator_pass_refund_receipt');
			expect(receipts.map((receipt) => receipt.metadata.amount_minor)).toEqual(['19900']);
		});

		test('leaves refunds from a withdrawal to the withdrawal receipt', async () => {
			const account = await operatorPassBuyer();
			await refundWebhook(
				'refund.created',
				stripeRefund({
					id: 're_op_withdrawal',
					paymentIntentId: 'pi_op',
					amount: 19900,
					metadata: {checkout_session_id: 'cs_op', rejection_reason: 'operator_pass_withdrawal:cs_op'},
				}),
			);
			expect(await emailsOfType(account.email, 'operator_pass_refund_receipt')).toHaveLength(0);
		});

		test('sends the buyer the invoice of a purchase', async () => {
			const account = await createTestAccount(harness);
			await invoiceWebhook('invoice.paid', {
				id: 'in_op',
				billingReason: 'manual',
				subscriptionId: null,
				total: 19900,
				metadata: {operator_pass: 'true', operator_pass_purchaser_id: account.userId},
				lines: [invoiceLine('price_operator_pass_inline', {amount: 19900})],
			});
			const [invoice] = await emailsOfType(account.email, 'operator_pass_invoice');
			expect(invoice!.metadata).toMatchObject({
				invoice_url: 'https://invoice.stripe.com/i/in_op',
				total_minor: '19900',
			});
		});
	});

	describe('delivery', () => {
		test('sends one receipt when the paid and succeeded events arrive together', async () => {
			const account = await createSubscriber('cus_together');
			const invoice = stripeInvoice(renewal('cus_together'));
			stripe.put.invoice(invoice);
			await Promise.all([
				sendStripeWebhook(harness, objectEvent('invoice.paid', invoice)),
				sendStripeWebhook(harness, objectEvent('invoice.payment_succeeded', invoice)),
			]);
			expect(await emailsOfType(account.email, 'plutonium_payment_receipt')).toHaveLength(1);
		});

		test('queues nothing on a self-hosted instance that has not turned billing emails on', async () => {
			const addJob = vi.fn(async () => 1n);
			Config.instance.selfHosted = true;
			try {
				await enqueueBillingEmail({addJob, cancelJob: async () => false}, {kind: 'invoice_paid', id: 'in_any'});
			} finally {
				Config.instance.selfHosted = false;
			}
			expect(addJob).not.toHaveBeenCalled();
		});

		test('schedules a later retry instead of failing when the email provider is down', async () => {
			await createBuyer('cus_outage');
			stripe.put.invoice(stripeInvoice({id: 'in_outage', customerId: 'cus_outage'}));
			const deps = getWorkerDependencies();
			const working = deps.emailService;
			deps.emailService = {...working, send: async () => false, isEmailBounced: async () => false} as IEmailService;
			const addJob = vi.fn(async () => 1n);
			try {
				await sendBillingEmail({kind: 'invoice_paid', id: 'in_outage'}, createTestWorkerHelpers({addJob}));
			} finally {
				deps.emailService = working;
			}
			expect(addJob).toHaveBeenCalledWith(
				'sendBillingEmail',
				{kind: 'invoice_paid', id: 'in_outage', retry: 1},
				expect.objectContaining({jobKey: 'billing-email:invoice_paid:in_outage:retry:1'}),
			);
		});
	});
});
