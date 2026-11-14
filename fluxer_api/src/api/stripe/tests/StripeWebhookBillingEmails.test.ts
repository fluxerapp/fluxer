// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, listTestEmails, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {OperatorPassPurchaseRow, OperatorPassUnitRow} from '@app/api/database/types/OperatorPassTypes';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {
	getCacheService,
	getPremiumStateReconciliationQueueService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {createGiftCodeRow} from '@app/api/models/GiftCode';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {getProductRegistry, ProductType} from '@app/api/stripe/ProductRegistry';
import {type BillingCancelSource, enqueueBillingEmail} from '@app/api/stripe/services/BillingEmailService';
import {StripeGiftReversalHandler} from '@app/api/stripe/services/StripeGiftReversalHandler';
import {StripeSubscriptionService} from '@app/api/stripe/services/StripeSubscriptionService';
import {
	BILLING_EMAIL_PRICES,
	billingEmailTestContext,
	DAY_SECONDS,
	type InvoiceOptions,
	invoiceLine,
	objectEvent,
	plutoniumSubscription,
	type SubscriptionPatch,
	stripeInvoice,
	stripeRefund,
	unixNow,
} from '@app/api/stripe/tests/BillingEmailTestUtils';
import {sendStripeWebhook} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import type {StripeApiHandlers, StripeWebhookEventData} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import reconcileUserPayments from '@app/api/worker/tasks/ReconcileUserPayments';
import sendBillingEmail from '@app/api/worker/tasks/SendBillingEmail';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type Stripe from 'stripe';
import {beforeEach, describe, expect, test, vi} from 'vitest';

const DAY_MS = 24 * 60 * 60 * 1000;

describe('Stripe Webhook - billing emails', () => {
	const context = billingEmailTestContext();
	const {createBuyer, createSubscriber, emailsOfType} = context;
	let harness: ApiTestHarness;
	let stripe: StripeApiHandlers;
	beforeEach(() => {
		harness = context.harness;
		stripe = context.stripe;
	});

	const {webhook} = context;

	async function retrieveSubscription(id: string) {
		return getWorkerDependencies().stripe!.subscriptions.retrieve(id);
	}

	function subscriptionUpdated(
		subscription: Stripe.Subscription,
		previousAttributes: Record<string, unknown>,
		created?: number,
	): StripeWebhookEventData {
		return {
			type: 'customer.subscription.updated',
			created,
			data: {object: {...subscription}, previous_attributes: previousAttributes},
		};
	}

	function renewal(customerId: string, options: Partial<InvoiceOptions> = {}): InvoiceOptions {
		return {id: `in_${customerId}`, customerId, subscriptionId: `sub_${customerId}`, ...options};
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
				invoice_number: expect.stringMatching(/^PL-\d{6}$/),
				total_minor: '499',
				renewal: '',
			});
			expect(receipts[0]!.metadata.document).toBe(receipts[0]!.metadata.invoice_number);
			expect(stripe.spies.retrievedCharges).toContain('ch_pi_in_cus_paid');
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
			const withdrawalEndsOn = new Date(`${receipt!.metadata.withdrawal_ends_on!}T23:59:59Z`).getTime();
			expect(withdrawalEndsOn - Date.now()).toBeGreaterThan(13 * DAY_MS);
		});

		test('gives a Brazilian Pix subscriber the 7-day withdrawal deadline and no EU consent paragraph', async () => {
			const account = await createBuyer('cus_brl');
			await invoiceWebhook('invoice.paid', {
				id: 'in_brl',
				customerId: 'cus_brl',
				billingReason: 'subscription_create',
				subscriptionMetadata: {
					eu_withdrawal_waiver_required: 'false',
					withdrawal_country: 'BR',
					payment_method: 'pix',
				},
			});
			const [receipt] = await emailsOfType(account.email, 'plutonium_payment_receipt');
			expect(receipt!.metadata).toMatchObject({renewal: 'month', consent_at: ''});
			const withdrawalEndsOn = new Date(`${receipt!.metadata.withdrawal_ends_on!}T23:59:59Z`).getTime();
			expect(withdrawalEndsOn - Date.now()).toBeGreaterThan(6 * DAY_MS);
			expect(withdrawalEndsOn - Date.now()).toBeLessThan(9 * DAY_MS);
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

		test('attaches the Fluxer invoice PDF, not the Stripe one', async () => {
			const account = await createSubscriber('cus_pdf');
			await invoiceWebhook('invoice.paid', renewal('cus_pdf', {invoicePdf: true}));
			const [receipt] = await emailsOfType(account.email, 'plutonium_payment_receipt');
			expect(receipt!.metadata.document_pdf).toBe(`Fluxer-${receipt!.metadata.document}.pdf`);
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

		test('sends no gift receipt for a checkout that a promotion code made free', async () => {
			const account = await createBuyer('cus_free_gift');
			stripe.put.checkoutSession({
				id: 'cs_free_gift',
				mode: 'payment',
				subscription: null,
				invoice: null,
				payment_intent: null,
				payment_status: 'paid',
				amount_total: 0,
				metadata: {price_id: BILLING_EMAIL_PRICES.gift1YearUsd, user_id: account.userId},
			});
			await sendBillingEmail({kind: 'gift_paid', id: 'cs_free_gift'}, createTestWorkerHelpers());
			expect(await emailsOfType(account.email, 'plutonium_payment_receipt')).toHaveLength(0);
		});

		test('sends the gift receipt when reconciliation completes a gift whose webhook never came', async () => {
			const account = await createBuyer('cus_stuck_gift');
			stripe.put.checkoutSession({
				id: 'cs_stuck_gift',
				mode: 'payment',
				subscription: null,
				invoice: null,
				payment_intent: 'pi_stuck_gift',
				payment_status: 'paid',
				amount_total: 4999,
				metadata: {price_id: BILLING_EMAIL_PRICES.gift1YearUsd, user_id: account.userId},
			});
			const paymentRepository = new PaymentRepository();
			await paymentRepository.createPayment({
				checkout_session_id: 'cs_stuck_gift',
				user_id: createUserID(BigInt(account.userId)),
				price_id: BILLING_EMAIL_PRICES.gift1YearUsd,
				product_type: ProductType.GIFT_1_YEAR,
				status: 'pending',
				is_gift: true,
				created_at: new Date(Date.now() - 11 * 60 * 1000),
			});
			const dependencies = getWorkerDependencies();
			Object.assign(dependencies, {paymentRepository});
			try {
				await reconcileUserPayments({userId: account.userId}, createTestWorkerHelpers());
			} finally {
				Reflect.deleteProperty(dependencies, 'paymentRepository');
			}
			expect((await paymentRepository.getPaymentByCheckoutSession('cs_stuck_gift'))?.status).toBe('completed');
			const [receipt] = await emailsOfType(account.email, 'plutonium_payment_receipt');
			expect(receipt!.metadata).toMatchObject({plan: 'gift_1_year'});
		});

		test('reuses the gift code already created for a checkout session when reconciliation completes it', async () => {
			const account = await createBuyer('cus_partial_gift');
			const userId = createUserID(BigInt(account.userId));
			stripe.put.checkoutSession({
				id: 'cs_partial_gift',
				mode: 'payment',
				subscription: null,
				invoice: null,
				payment_intent: null,
				payment_status: 'paid',
				amount_total: 0,
				metadata: {price_id: BILLING_EMAIL_PRICES.gift1MonthUsd, user_id: account.userId, is_gift: 'true'},
			});
			const paymentRepository = new PaymentRepository();
			await paymentRepository.createPayment({
				checkout_session_id: 'cs_partial_gift',
				user_id: userId,
				price_id: BILLING_EMAIL_PRICES.gift1MonthUsd,
				product_type: ProductType.GIFT_1_MONTH,
				status: 'pending',
				is_gift: true,
				created_at: new Date(Date.now() - 11 * 60 * 1000),
			});
			await getUserRepository().createGiftCode(
				createGiftCodeRow({
					code: 'partialgiftcode0000000000000000000',
					durationType: 'months',
					durationQuantity: 1,
					createdAt: new Date(),
					createdByUserId: userId,
					checkoutSessionId: 'cs_partial_gift',
				}),
			);
			const dependencies = getWorkerDependencies();
			Object.assign(dependencies, {paymentRepository});
			try {
				await reconcileUserPayments({userId: account.userId}, createTestWorkerHelpers());
			} finally {
				Reflect.deleteProperty(dependencies, 'paymentRepository');
			}
			const payment = await paymentRepository.getPaymentByCheckoutSession('cs_partial_gift');
			expect(payment?.status).toBe('completed');
			expect(payment?.giftCode).toBe('partialgiftcode0000000000000000000');
			const gifts = await getUserRepository().findGiftCodesByCreator(userId);
			expect(gifts.map((gift) => gift.code)).toEqual(['partialgiftcode0000000000000000000']);
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
		function openRenewal(customerId: string, attemptCount: number, options: Partial<InvoiceOptions> = {}) {
			return renewal(customerId, {status: 'open', attemptCount, ...options});
		}

		async function deliverPaymentProblem(invoice: InvoiceOptions, reminder?: 1 | 2) {
			stripe.put.invoice(stripeInvoice(invoice));
			const addJob = vi.fn(async () => 1n);
			await sendBillingEmail(
				{
					kind: 'invoice_payment_problem',
					id: invoice.id,
					paymentAttempt: invoice.attemptCount ?? 1,
					actionRequired: true,
					...(reminder ? {reminder} : {}),
				},
				createTestWorkerHelpers({addJob}),
			);
			return addJob;
		}

		async function failureReasons(email: string) {
			return (await emailsOfType(email, 'plutonium_payment_failed')).map((notice) => notice.metadata.reason);
		}

		test('tells the customer when the first attempt is declined', async () => {
			const account = await createSubscriber('cus_failing');
			await invoiceWebhook('invoice.payment_failed', openRenewal('cus_failing', 1));
			await invoiceWebhook('invoice.payment_failed', openRenewal('cus_failing', 1));
			expect(await failureReasons(account.email)).toEqual(['declined']);
		});

		test('says nothing about a declined retry after telling the customer about the first decline', async () => {
			const account = await createSubscriber('cus_retrying');
			await invoiceWebhook('invoice.payment_failed', openRenewal('cus_retrying', 1));
			await invoiceWebhook('invoice.payment_failed', openRenewal('cus_retrying', 2));
			expect(await failureReasons(account.email)).toEqual(['declined']);
		});

		test('tells the customer about a declined retry after a confirmation they did not finish', async () => {
			const account = await createSubscriber('cus_sca_then_declined');
			await invoiceWebhook('invoice.payment_action_required', openRenewal('cus_sca_then_declined', 1));
			await invoiceWebhook('invoice.payment_failed', openRenewal('cus_sca_then_declined', 2));
			expect(await failureReasons(account.email)).toEqual(['action_required', 'declined']);
		});

		test('leaves the last declined retry to the ended email when the subscription has ended', async () => {
			const account = await createSubscriber('cus_last_retry_ended');
			stripe.put.subscription(
				'sub_cus_last_retry_ended',
				plutoniumSubscription('cus_last_retry_ended', {status: 'canceled'}),
			);
			await invoiceWebhook(
				'invoice.payment_failed',
				openRenewal('cus_last_retry_ended', 4, {nextPaymentAttempt: null}),
			);
			expect(await failureReasons(account.email)).toEqual([]);
		});

		test('tells the customer when the last retry is declined', async () => {
			const account = await createSubscriber('cus_last_retry');
			await invoiceWebhook('invoice.payment_failed', openRenewal('cus_last_retry', 4, {nextPaymentAttempt: null}));
			expect(await failureReasons(account.email)).toEqual(['declined']);
		});

		test('asks the customer to confirm the payment when the bank wants it', async () => {
			const account = await createSubscriber('cus_sca');
			await invoiceWebhook('invoice.payment_action_required', openRenewal('cus_sca', 1));
			expect(await failureReasons(account.email)).toEqual(['action_required']);
		});

		test('asks the customer to confirm the payment for a plan change', async () => {
			const account = await createSubscriber('cus_sca_switch');
			await invoiceWebhook(
				'invoice.payment_action_required',
				openRenewal('cus_sca_switch', 1, {billingReason: 'subscription_update'}),
			);
			expect(await failureReasons(account.email)).toEqual(['action_required']);
		});

		test('marks a plan change payment as a plan change', async () => {
			const account = await createSubscriber('cus_sca_plan_change');
			await invoiceWebhook(
				'invoice.payment_action_required',
				openRenewal('cus_sca_plan_change', 1, {billingReason: 'subscription_update'}),
			);
			const [notice] = await emailsOfType(account.email, 'plutonium_payment_failed');
			expect(notice!.metadata.plan_change).toBe('true');
		});

		test('queues the reminders on a retry when queueing them failed after the email was sent', async () => {
			await createSubscriber('cus_reminder_retry');
			const invoice = openRenewal('cus_reminder_retry', 1);
			stripe.put.invoice(stripeInvoice(invoice));
			const failing = vi.fn(async () => 1n).mockRejectedValueOnce(new Error('queue unavailable'));
			const job = {kind: 'invoice_payment_problem', id: invoice.id, paymentAttempt: 1, actionRequired: true};
			await sendBillingEmail(job, createTestWorkerHelpers({addJob: failing}));
			const addJob = await deliverPaymentProblem(invoice);
			expect(addJob).toHaveBeenCalledTimes(2);
		});

		test('queues two reminders after asking the customer to confirm a payment', async () => {
			await createSubscriber('cus_reminders');
			const addJob = await deliverPaymentProblem(openRenewal('cus_reminders', 1));
			const delays = addJob.mock.calls.map((call) => {
				const [, payload, options] = call as unknown as [string, {reminder: number}, {runAt: Date}];
				return [payload.reminder, Math.round((options.runAt.getTime() - Date.now()) / DAY_MS)];
			});
			expect(delays).toEqual([
				[1, 3],
				[2, 6],
			]);
		});

		test('queues no reminders for a second confirmation request on the same invoice', async () => {
			const account = await createSubscriber('cus_second_sca');
			await deliverPaymentProblem(openRenewal('cus_second_sca', 1));
			const addJob = await deliverPaymentProblem(openRenewal('cus_second_sca', 2));
			expect(addJob).not.toHaveBeenCalled();
			expect(await failureReasons(account.email)).toEqual(['action_required']);
		});

		test('sends no reminder once the payment is confirmed', async () => {
			const account = await createSubscriber('cus_confirmed');
			await deliverPaymentProblem({...openRenewal('cus_confirmed', 1), status: 'paid'}, 1);
			expect(await failureReasons(account.email)).toEqual([]);
		});

		test('sends a reminder while the payment still needs confirming', async () => {
			const account = await createSubscriber('cus_still_sca');
			await deliverPaymentProblem(openRenewal('cus_still_sca', 1, {intentStatus: 'requires_action'}), 1);
			const [notice] = await emailsOfType(account.email, 'plutonium_payment_failed');
			expect(notice!.metadata).toMatchObject({reason: 'action_required', reminder: 'true'});
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

	describe('cancellations', () => {
		async function subscriberWithPlan(customerId: string, patch: SubscriptionPatch = {}) {
			const account = await createSubscriber(customerId, {timezone: 'Europe/Stockholm'});
			stripe.put.subscription(`sub_${customerId}`, plutoniumSubscription(customerId, patch));
			return account;
		}

		async function cancelInApp(account: TestAccount) {
			await createBuilder(harness, account.token).post('/premium/cancel-subscription').expect(204).execute();
		}

		async function resumeInApp(account: TestAccount) {
			await createBuilder(harness, account.token).post('/premium/reactivate-subscription').expect(204).execute();
		}

		async function cancelFor(account: TestAccount, source: BillingCancelSource) {
			const service = new StripeSubscriptionService(
				getWorkerDependencies().stripe,
				getUserRepository(),
				getProductRegistry(),
				getCacheService(),
				getGatewayService(),
			);
			await service.cancelSubscriptionAtPeriodEnd(createUserID(BigInt(account.userId)), source);
		}

		async function confirmations(email: string) {
			return emailsOfType(email, 'plutonium_cancellation_confirmation');
		}

		test('confirms an in-app cancellation with the time it was received and the end date', async () => {
			const periodEnd = unixNow() + 20 * DAY_SECONDS;
			const account = await subscriberWithPlan('cus_cancel', {current_period_end: periodEnd});
			await cancelInApp(account);
			const [confirmation] = await confirmations(account.email);
			expect(Math.abs(Date.parse(confirmation!.metadata.received_at!) - Date.now())).toBeLessThan(60_000);
			expect(confirmation!.metadata).toMatchObject({
				time_zone: 'Europe/Stockholm',
				ends_at: new Date(periodEnd * 1000).toISOString(),
				reference: 'sub_cus_cancel',
			});
		});

		test('sends no second confirmation for the webhook that follows an in-app cancellation', async () => {
			const account = await subscriberWithPlan('cus_cancel_echo');
			await cancelInApp(account);
			await webhook(
				subscriptionUpdated(
					await retrieveSubscription('sub_cus_cancel_echo'),
					{cancel_at_period_end: false},
					unixNow() + 60,
				),
			);
			expect(await confirmations(account.email)).toHaveLength(1);
		});

		test('confirms a cancellation made in the customer portal at the time Stripe received it', async () => {
			const account = await subscriberWithPlan('cus_portal', {cancel_at_period_end: true});
			const created = unixNow() - 120;
			await webhook(
				subscriptionUpdated(await retrieveSubscription('sub_cus_portal'), {cancel_at_period_end: false}, created),
			);
			const [confirmation] = await confirmations(account.email);
			expect(confirmation!.metadata.received_at).toBe(new Date(created * 1000).toISOString());
		});

		test('confirms a cancellation of a plan with a pending change using the end of its last phase', async () => {
			const periodEnd = unixNow() + 12 * DAY_SECONDS;
			const account = await subscriberWithPlan('cus_scheduled', {current_period_end: periodEnd});
			await getWorkerDependencies().stripe!.subscriptionSchedules.create({from_subscription: 'sub_cus_scheduled'});
			await cancelInApp(account);
			const [confirmation] = await confirmations(account.email);
			expect(confirmation!.metadata.ends_at).toBe(new Date(periodEnd * 1000).toISOString());
		});

		test('sends nothing when a gift past the trial limit ends the plan', async () => {
			const account = await subscriberWithPlan('cus_gift_limit');
			await cancelFor(account, 'gift_past_trial_limit');
			expect(await confirmations(account.email)).toHaveLength(0);
		});

		test('sends nothing when a payment dispute ends the plan', async () => {
			const account = await subscriberWithPlan('cus_disputed');
			await cancelFor(account, 'payment_dispute');
			expect(await confirmations(account.email)).toHaveLength(0);
		});

		test('confirms again after the customer resumes and cancels a second time', async () => {
			const account = await subscriberWithPlan('cus_twice');
			await cancelInApp(account);
			await resumeInApp(account);
			const later = Date.now() + 5_000;
			const clock = vi.spyOn(Date, 'now').mockReturnValue(later);
			try {
				await cancelInApp(account);
			} finally {
				clock.mockRestore();
			}
			expect(await confirmations(account.email)).toHaveLength(2);
		});

		test('confirms a portal cancellation after an in-app cancellation was resumed in the portal', async () => {
			const account = await subscriberWithPlan('cus_portal_after_app');
			await cancelInApp(account);
			stripe.put.subscription('sub_cus_portal_after_app', {cancel_at_period_end: false});
			await webhook(
				subscriptionUpdated(await retrieveSubscription('sub_cus_portal_after_app'), {cancel_at_period_end: true}),
			);
			stripe.put.subscription('sub_cus_portal_after_app', {cancel_at_period_end: true});
			await webhook(
				subscriptionUpdated(
					await retrieveSubscription('sub_cus_portal_after_app'),
					{cancel_at_period_end: false},
					unixNow() + 120,
				),
			);
			expect(await confirmations(account.email)).toHaveLength(2);
		});

		test('clears the cancellation source when the customer resumes in the portal', async () => {
			const account = await subscriberWithPlan('cus_portal_resume');
			await cancelInApp(account);
			stripe.put.subscription('sub_cus_portal_resume', {cancel_at_period_end: false});
			await webhook(
				subscriptionUpdated(await retrieveSubscription('sub_cus_portal_resume'), {cancel_at_period_end: true}),
			);
			expect((await retrieveSubscription('sub_cus_portal_resume')).metadata.fluxer_cancel_source).toBeFalsy();
		});

		test('confirms a portal cancellation long after an earlier in-app cancellation', async () => {
			const account = await subscriberWithPlan('cus_portal_stale');
			await cancelInApp(account);
			stripe.put.subscription('sub_cus_portal_stale', {cancel_at_period_end: true});
			await webhook(
				subscriptionUpdated(
					await retrieveSubscription('sub_cus_portal_stale'),
					{cancel_at_period_end: false},
					unixNow() + 3600,
				),
			);
			expect(await confirmations(account.email)).toHaveLength(2);
		});

		test('mentions an unpaid invoice that canceling does not remove', async () => {
			const account = await subscriberWithPlan('cus_cancel_unpaid', {latest_invoice: 'in_cus_cancel_unpaid'});
			stripe.put.invoice(stripeInvoice(renewal('cus_cancel_unpaid', {status: 'open', total: 4999})));
			await cancelInApp(account);
			const [confirmation] = await confirmations(account.email);
			expect(confirmation!.metadata.unpaid_minor).toBe('4999');
		});

		test('sends nothing for an update that does not change the cancellation', async () => {
			const account = await subscriberWithPlan('cus_unrelated', {cancel_at_period_end: true});
			await webhook(subscriptionUpdated(await retrieveSubscription('sub_cus_unrelated'), {metadata: {}}));
			expect(await confirmations(account.email)).toHaveLength(0);
		});
	});

	describe('plans that end unpaid', () => {
		async function endedSubscriber(customerId: string, reason: string, invoiceStatus: string) {
			const account = await createSubscriber(customerId);
			stripe.put.invoice(stripeInvoice(renewal(customerId, {status: invoiceStatus})));
			stripe.put.subscription(
				`sub_${customerId}`,
				plutoniumSubscription(customerId, {
					status: 'canceled',
					latest_invoice: `in_${customerId}`,
					cancellation_details: {reason},
				}),
			);
			await webhook(
				objectEvent('customer.subscription.deleted', {...(await retrieveSubscription(`sub_${customerId}`))}),
			);
			return emailsOfType(account.email, 'plutonium_ended_unpaid');
		}

		test('tells the customer they owe nothing when the invoice was voided', async () => {
			const [email] = await endedSubscriber('cus_ended_void', 'payment_failed', 'void');
			expect(email!.metadata.invoice_status).toBe('void');
		});

		test('tells the customer when the invoice was marked uncollectible', async () => {
			const [email] = await endedSubscriber('cus_ended_uncollectible', 'payment_failed', 'uncollectible');
			expect(email!.metadata.invoice_status).toBe('uncollectible');
		});

		test('tells the customer the invoice is still open', async () => {
			const [email] = await endedSubscriber('cus_ended_open', 'payment_failed', 'open');
			expect(email!.metadata).toMatchObject({invoice_status: 'open', amount_minor: '499'});
		});

		test('sends nothing when the customer asked to cancel', async () => {
			expect(await endedSubscriber('cus_ended_asked', 'cancellation_requested', 'paid')).toHaveLength(0);
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

		function selfServeRefund(id: string) {
			return stripeRefund({
				id,
				paymentIntentId: 'pi_in_refunded',
				amount: 4999,
				metadata: {refund_kind: 'self_serve', subscription_id: 'sub_cus_refund'},
			});
		}

		async function deliverRefund(refund: ReturnType<typeof stripeRefund>, retry?: number) {
			stripe.put.refund(refund);
			const addJob = vi.fn(async () => 1n);
			await sendBillingEmail(
				{kind: 'refund', id: refund.id, ...(retry === undefined ? {} : {retry})},
				createTestWorkerHelpers({addJob}),
			);
			return addJob;
		}

		test('waits for the subscription to be canceled before confirming that a refund ended it', async () => {
			const account = await refundedPlutoniumBuyer();
			stripe.put.subscription('sub_cus_refund', plutoniumSubscription('cus_refund'));
			const addJob = await deliverRefund(selfServeRefund('re_cancel_pending'));
			expect(addJob).toHaveBeenCalledWith('sendBillingEmail', expect.objectContaining({retry: 1}), expect.anything());
			expect(await emailsOfType(account.email, 'plutonium_refund_receipt')).toHaveLength(0);
		});

		test('sends the refund receipt without the ended subscription once waiting runs out', async () => {
			const account = await refundedPlutoniumBuyer();
			stripe.put.subscription('sub_cus_refund', plutoniumSubscription('cus_refund'));
			await deliverRefund(selfServeRefund('re_cancel_failed'), 4);
			const [receipt] = await emailsOfType(account.email, 'plutonium_refund_receipt');
			expect(receipt!.metadata.subscription_reference).toBe('');
		});

		test('confirms that a self-serve refund also ended the subscription', async () => {
			const account = await refundedPlutoniumBuyer();
			stripe.put.subscription('sub_cus_refund', plutoniumSubscription('cus_refund', {status: 'canceled'}));
			await refundWebhook(
				'refund.created',
				stripeRefund({
					id: 're_self_serve',
					paymentIntentId: 'pi_in_refunded',
					amount: 4999,
					metadata: {refund_kind: 'self_serve', subscription_id: 'sub_cus_refund'},
				}),
			);
			const [receipt] = await emailsOfType(account.email, 'plutonium_refund_receipt');
			expect(receipt!.metadata.subscription_reference).toBe('sub_cus_refund');
		});

		test('retries a pending refund until it succeeds and sends one receipt', async () => {
			const account = await refundedPlutoniumBuyer();
			stripe.put.refund(stripeRefund({id: 're_slow', paymentIntentId: 'pi_in_refunded', status: 'pending'}));
			const retries: Array<Record<string, unknown>> = [];
			const addJob = vi.fn(async (_task: string, payload: Record<string, unknown>) => {
				retries.push(payload);
				return 1n;
			});
			await sendBillingEmail({kind: 'refund', id: 're_slow'}, createTestWorkerHelpers({addJob}));
			expect(retries).toEqual([{kind: 'refund', id: 're_slow', retry: 1}]);
			expect(await emailsOfType(account.email, 'plutonium_refund_receipt')).toHaveLength(0);
			stripe.put.refund(stripeRefund({id: 're_slow', paymentIntentId: 'pi_in_refunded'}));
			await sendBillingEmail(retries[0]!, createTestWorkerHelpers({addJob}));
			expect(retries).toHaveLength(1);
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

		test('voids an open invoice when the donor cancels, so nothing more is charged', async () => {
			stripe.put.subscription('sub_cus_donor_cancel', {
				...plutoniumSubscription('cus_donor_cancel', {cancel_at_period_end: true}),
				metadata: donationSubscription,
			});
			const invoice = stripeInvoice({
				id: 'in_donation_unpaid',
				customerId: 'cus_donor_cancel',
				subscriptionId: 'sub_cus_donor_cancel',
				status: 'open',
				subscriptionMetadata: donationSubscription,
				lines: [invoiceLine('price_donation_custom', {amount: 1000})],
				total: 1000,
			});
			stripe.put.invoice(invoice);
			await webhook(
				subscriptionUpdated(await retrieveSubscription('sub_cus_donor_cancel'), {cancel_at_period_end: false}),
			);
			await sendStripeWebhook(harness, objectEvent('invoice.voided', {...invoice, status: 'void'}));
			const voided = await getWorkerDependencies().stripe!.invoices.retrieve('in_donation_unpaid');
			expect(voided.status).toBe('void');
			expect(await emailsOfType(DONOR, 'donation_cancellation_confirmation')).toHaveLength(1);
			expect(await emailsOfType(DONOR, 'donation_receipt')).toHaveLength(0);
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
		async function operatorPassBuyer(patch: Partial<OperatorPassPurchaseRow> = {}) {
			const account = await createTestAccount(harness);
			stripe.put.checkoutSession({
				id: 'cs_op',
				mode: 'payment',
				subscription: null,
				invoice: null,
				payment_intent: 'pi_op',
				metadata: {operator_pass: 'true'},
			});
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
				...patch,
			} as OperatorPassPurchaseRow);
			return account;
		}

		async function deliveredTwoPassBuyer(secondHolder?: string) {
			const deliveredAt = new Date(Date.now() - DAY_MS);
			const account = await operatorPassBuyer({
				quantity: 2,
				amount_minor: 20000,
				invoice_id: 'in_op',
				invoice_number: 'OP-1',
				delivered_at: deliveredAt,
			});
			const repository = new OperatorPassRepository();
			for (const index of [1, 2]) {
				await repository.createUnit({
					pass_id: `cs_op:${index}`,
					purchase_id: 'cs_op',
					buyer_id: createUserID(BigInt(account.userId)),
					holder_id: createUserID(BigInt(index === 2 && secondHolder ? secondHolder : account.userId)),
					source: 'purchase',
					status: 'held',
					transferable: false,
					delivered_at: deliveredAt,
					first_delivered_at: deliveredAt,
					paused_at: null,
					ended_at: null,
					end_reason: null,
					withdrawal_country: null,
					withdrawal_days: null,
					version: 1,
				});
			}
			stripe.put.paymentIntent({id: 'pi_op', object: 'payment_intent', status: 'succeeded', latest_charge: 'ch_pi_op'});
			return {account, repository};
		}

		function refundCharge(amountRefunded: number) {
			stripe.put.charge({
				id: 'ch_pi_op',
				object: 'charge',
				amount: 20000,
				amount_refunded: amountRefunded,
				refunded: amountRefunded >= 20000,
				currency: 'usd',
				payment_intent: 'pi_op',
			});
		}

		async function heldUnits(repository: OperatorPassRepository) {
			return (await repository.listUnitsByPurchase('cs_op')).filter(
				(unit: OperatorPassUnitRow) => unit.status === 'held',
			);
		}

		test('emails the buyer once about a refund made in the Stripe dashboard', async () => {
			const account = await operatorPassBuyer();
			const refund = stripeRefund({id: 're_op', paymentIntentId: 'pi_op', amount: 19900});
			await refundWebhook('refund.created', refund);
			await refundWebhook('refund.updated', refund);
			const receipts = await emailsOfType(account.email, 'operator_pass_refund_receipt');
			expect(receipts.map((receipt) => receipt.metadata.amount_minor)).toEqual(['19900']);
		});

		test('ends the pass a dashboard refund pays back before the refund receipt is written', async () => {
			const {account, repository} = await deliveredTwoPassBuyer();
			refundCharge(10000);
			await refundWebhook('refund.created', stripeRefund({id: 're_op_one', paymentIntentId: 'pi_op', amount: 10000}));
			expect(await heldUnits(repository)).toHaveLength(1);
			const [receipt] = await emailsOfType(account.email, 'operator_pass_refund_receipt');
			expect(receipt!.metadata).toMatchObject({
				delivered: 'true',
				pass_count: '1',
				last_pass: 'false',
			});
		});

		test('counts only the pass the second of two one-pass refunds ended', async () => {
			const {account, repository} = await deliveredTwoPassBuyer();
			refundCharge(10000);
			await refundWebhook('refund.created', stripeRefund({id: 're_op_one', paymentIntentId: 'pi_op', amount: 10000}));
			refundCharge(20000);
			await refundWebhook('refund.created', stripeRefund({id: 're_op_two', paymentIntentId: 'pi_op', amount: 10000}));
			expect(await heldUnits(repository)).toHaveLength(0);
			const receipts = await emailsOfType(account.email, 'operator_pass_refund_receipt');
			expect(receipts.map(({metadata}) => [metadata.pass_count, metadata.last_pass])).toEqual(
				expect.arrayContaining([
					['1', 'false'],
					['1', 'true'],
				]),
			);
		});

		test('tells a buyer whose last own pass a refund ended that it was their last, even when a pass they gave away stays', async () => {
			const transferee = await createTestAccount(harness);
			const {account, repository} = await deliveredTwoPassBuyer(transferee.userId);
			refundCharge(10000);
			await refundWebhook('refund.created', stripeRefund({id: 're_op_one', paymentIntentId: 'pi_op', amount: 10000}));
			expect((await heldUnits(repository)).map((unit) => String(unit.holder_id))).toEqual([transferee.userId]);
			const [receipt] = await emailsOfType(account.email, 'operator_pass_refund_receipt');
			expect(receipt!.metadata).toMatchObject({delivered: 'true', pass_count: '1', last_pass: 'true'});
		});

		test('says a refund smaller than one pass ended no pass', async () => {
			const {account, repository} = await deliveredTwoPassBuyer();
			refundCharge(500);
			await refundWebhook('refund.created', stripeRefund({id: 're_op_small', paymentIntentId: 'pi_op', amount: 500}));
			expect(await heldUnits(repository)).toHaveLength(2);
			const [receipt] = await emailsOfType(account.email, 'operator_pass_refund_receipt');
			expect(receipt!.metadata).toMatchObject({delivered: 'true', pass_count: '0'});
		});

		test('leaves the refund of a recorded withdrawal to the withdrawal receipt', async () => {
			const account = await operatorPassBuyer({withdrawal_refund_id: 're_op_withdrawal'});
			await refundWebhook(
				'refund.created',
				stripeRefund({
					id: 're_op_withdrawal',
					paymentIntentId: 'pi_op',
					amount: 19900,
					metadata: {
						checkout_session_id: 'cs_op',
						rejection_reason: 'operator_pass_withdrawal:all:2026-10-01T00:00:00.000Z',
					},
				}),
			);
			expect(await emailsOfType(account.email, 'operator_pass_refund_receipt')).toHaveLength(0);
		});

		test.each([
			[
				'no refund receipt once a refusal with its own email is recorded',
				'duplicate_refunded',
				'operator_pass_names_missing',
				0,
			],
			[
				'no refund receipt while fulfilment still has to send the refusal email',
				'pending',
				'operator_pass_names_missing',
				0,
			],
			['a refund receipt when the refusal has no email of its own', 'pending', 'operator_pass_removed', 1],
		] as const)('sends %s', async (_case, status, reason, receipts) => {
			const account = await operatorPassBuyer({status});
			await refundWebhook(
				'refund.created',
				stripeRefund({
					id: 're_op_refused',
					paymentIntentId: 'pi_op',
					amount: 19900,
					metadata: {checkout_session_id: 'cs_op', rejection_reason: reason},
				}),
			);
			expect(await emailsOfType(account.email, 'operator_pass_refund_receipt')).toHaveLength(receipts);
		});

		test('leaves the retry of a failed withdrawal refund to the withdrawal receipt', async () => {
			const account = await operatorPassBuyer({withdrawn_at: new Date(), withdrawal_refund_id: 're_failed'});
			await new OperatorPassRepository().createUnit({
				pass_id: 'cs_op:1',
				purchase_id: 'cs_op',
				buyer_id: createUserID(BigInt(account.userId)),
				holder_id: createUserID(BigInt(account.userId)),
				source: 'purchase',
				status: 'withdrawn',
				transferable: false,
				delivered_at: new Date(),
				first_delivered_at: new Date(),
				paused_at: null,
				ended_at: new Date(),
				end_reason: 'withdrawal',
				withdrawal_country: null,
				withdrawal_days: null,
				version: 1,
			});
			for (const scope of ['all:2026-10-01T00:00:00.000Z', 'cs_op:1']) {
				const retry = stripeRefund({
					id: `re_retry_${scope.split(':')[0]}`,
					paymentIntentId: 'pi_op',
					amount: 19900,
					metadata: {
						checkout_session_id: 'cs_op',
						rejection_reason: `operator_pass_withdrawal:${scope}`,
						retry_of: scope === 'all:2026-10-01T00:00:00.000Z' ? 're_failed' : 're_failed_unit',
					},
				});
				await refundWebhook('refund.created', retry);
				await refundWebhook('refund.updated', retry);
			}
			expect(await emailsOfType(account.email, 'operator_pass_refund_receipt')).toHaveLength(0);
		});

		test('sends a refund receipt for a withdrawal refund whose withdrawal was never recorded', async () => {
			const account = await operatorPassBuyer();
			for (const scope of ['all:2026-10-01T00:00:00.000Z', 'cs_op:1']) {
				await refundWebhook(
					'refund.created',
					stripeRefund({
						id: `re_op_unrecorded_${scope.split(':')[0]}`,
						paymentIntentId: 'pi_op',
						amount: 19900,
						metadata: {checkout_session_id: 'cs_op', rejection_reason: `operator_pass_withdrawal:${scope}`},
					}),
				);
			}
			expect(await emailsOfType(account.email, 'operator_pass_refund_receipt')).toHaveLength(2);
		});

		test('sends no email for a paid Operator Pass invoice, since the purchase confirmation has it', async () => {
			const account = await createTestAccount(harness);
			await invoiceWebhook('invoice.paid', {
				id: 'in_op',
				billingReason: 'manual',
				subscriptionId: null,
				total: 19900,
				metadata: {operator_pass: 'true', operator_pass_purchaser_id: account.userId},
				lines: [invoiceLine('price_operator_pass_inline', {amount: 19900})],
			});
			const emails = await listTestEmails(harness, {recipient: account.email});
			expect(emails.filter((email) => email.type.startsWith('operator_pass'))).toEqual([]);
		});
	});

	describe('gifts', () => {
		const MONTHLY: SubscriptionPatch = {price_id: BILLING_EMAIL_PRICES.monthlyUsd, unit_amount: 499, interval: 'month'};
		const YEARLY: SubscriptionPatch = {price_id: BILLING_EMAIL_PRICES.yearlyUsd, unit_amount: 4999, interval: 'year'};

		async function giftTrialSubscriber(
			customerId: string,
			trialDays: number,
			giftDays: number,
			plan: SubscriptionPatch = MONTHLY,
		) {
			const account = await createSubscriber(customerId);
			const gifter = await createTestAccount(harness);
			const code = `gift${customerId.replaceAll('_', '')}`;
			await getUserRepository().createGiftCode({
				code,
				duration_months: null,
				duration_type: 'months',
				duration_quantity: 1,
				created_at: new Date(),
				created_by_user_id: createUserID(BigInt(gifter.userId)),
				redeemed_at: new Date(),
				redeemed_by_user_id: createUserID(BigInt(account.userId)),
				stripe_payment_intent_id: `pi_${code}`,
				visionary_sequence_number: null,
				checkout_session_id: null,
				version: 1,
			});
			const trialEnd = unixNow() + Math.round(trialDays * DAY_SECONDS);
			stripe.put.subscription(
				`sub_${customerId}`,
				plutoniumSubscription(customerId, {
					...plan,
					trial_end: trialEnd,
					current_period_end: trialEnd,
					metadata: {[`gtrial_${code}`]: String(giftDays * DAY_SECONDS)},
				}),
			);
			return {account, code};
		}

		async function reverseGift(code: string) {
			const handler = new StripeGiftReversalHandler(
				getUserRepository(),
				getGatewayService(),
				getPremiumStateReconciliationQueueService(),
			);
			return handler.handleGiftPremiumReversal((await getUserRepository().findGiftCode(code))!, {
				reason: 'gift_chargeback',
			});
		}

		test('confirms a redeemed gift with the date billing starts again', async () => {
			const account = await createSubscriber('cus_redeem');
			stripe.put.subscription('sub_cus_redeem', plutoniumSubscription('cus_redeem', MONTHLY));
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/extend-subscription-trial`)
				.body({duration_type: 'months', duration_quantity: 1, idempotency_key: 'gift_redeem'})
				.expect(204)
				.execute();
			const [confirmation] = await emailsOfType(account.email, 'plutonium_gift_applied');
			expect(confirmation!.metadata).toMatchObject({plan: 'monthly', amount_total_minor: '499'});
		});

		test('reports that billing starts again today when a reversed gift ends the trial', async () => {
			const {code} = await giftTrialSubscriber('cus_gift_now', 10, 10);
			expect(await reverseGift(code)).toEqual({billingResumed: 'monthly'});
		});

		test('reports that billing starts again when a reversed gift leaves less than an hour', async () => {
			const {code} = await giftTrialSubscriber('cus_gift_hour', 10 + 1800 / DAY_SECONDS, 10);
			expect(await reverseGift(code)).toEqual({billingResumed: 'monthly'});
		});

		test('tells the redeemer when a refunded gift restarts billing', async () => {
			const {account, code} = await giftTrialSubscriber('cus_gift_refunded', 10, 10);
			await webhook(
				objectEvent('charge.refunded', {
					id: `ch_${code}`,
					object: 'charge',
					amount: 499,
					amount_refunded: 499,
					currency: 'usd',
					customer: 'cus_gifter',
					payment_intent: `pi_${code}`,
					refunded: true,
					refunds: {object: 'list', data: [], has_more: false},
				}),
			);
			const [notice] = await emailsOfType(account.email, 'gift_chargeback_notification');
			expect(notice!.metadata.billing_resumed).toBe('true');
		});

		test('sends the gift notice when a reversed gift shortens the trial of a yearly plan', async () => {
			const {account, code} = await giftTrialSubscriber('cus_gift_short_yearly', 20, 15, YEARLY);
			await reverseGift(code);
			const [notice] = await emailsOfType(account.email, 'plutonium_renewal_notice');
			expect(notice!.metadata).toMatchObject({variant: 'gift', plan: 'yearly'});
		});

		test('sends the gift notice when a reversed gift shortens the trial', async () => {
			const {account, code} = await giftTrialSubscriber('cus_gift_short', 20, 15);
			await reverseGift(code);
			const [notice] = await emailsOfType(account.email, 'plutonium_renewal_notice');
			expect(notice!.metadata.variant).toBe('gift');
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

		test('sends nothing from a queued job after billing emails are turned off', async () => {
			const account = await createBuyer('cus_turned_off');
			stripe.put.invoice(stripeInvoice({id: 'in_turned_off', customerId: 'cus_turned_off'}));
			Config.instance.selfHosted = true;
			try {
				await sendBillingEmail({kind: 'invoice_paid', id: 'in_turned_off'}, createTestWorkerHelpers());
			} finally {
				Config.instance.selfHosted = false;
			}
			expect(await emailsOfType(account.email, 'plutonium_payment_receipt')).toHaveLength(0);
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
