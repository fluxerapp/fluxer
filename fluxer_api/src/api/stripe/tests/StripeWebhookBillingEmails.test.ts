// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {
	getCacheService,
	getPremiumStateReconciliationQueueService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {getProductRegistry} from '@app/api/stripe/ProductRegistry';
import {type BillingCancelSource, enqueueBillingEmail} from '@app/api/stripe/services/BillingEmailService';
import {StripeGiftReversalHandler} from '@app/api/stripe/services/StripeGiftReversalHandler';
import {StripeSubscriptionService} from '@app/api/stripe/services/StripeSubscriptionService';
import {
	BILLING_EMAIL_PRICES,
	billingEmailTestContext,
	creditNote,
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

	describe('credit notes', () => {
		async function creditNoteWebhook(customerId: string, balanceAmount: number | null, patch: SubscriptionPatch = {}) {
			const account = await createSubscriber(customerId);
			stripe.put.subscription(`sub_${customerId}`, plutoniumSubscription(customerId, patch));
			stripe.put.invoice(stripeInvoice(renewal(customerId)));
			const note = creditNote({id: `cn_${customerId}`, invoiceId: `in_${customerId}`, balanceAmount});
			stripe.put.creditNote(note);
			await webhook(objectEvent('credit_note.created', note));
			return emailsOfType(account.email, 'billing_credit_note');
		}

		test('tells the customer about credit added to their account', async () => {
			const [email] = await creditNoteWebhook('cus_credit_note', 250);
			expect(email!.metadata).toMatchObject({amount_minor: '250', invoice_number: 'NUM-in_cus_credit_note'});
		});

		test('says the credit comes off the next invoice of an active subscription', async () => {
			const [email] = await creditNoteWebhook('cus_credit_active', 250);
			expect(email!.metadata.upcoming_invoice).toBe('true');
		});

		test('offers a refund of the credit when the subscription has ended', async () => {
			const [email] = await creditNoteWebhook('cus_credit_ended', 250, {status: 'canceled'});
			expect(email!.metadata.upcoming_invoice).toBe('false');
		});

		test('sends nothing for a credit note paid out as a refund', async () => {
			expect(await creditNoteWebhook('cus_refund_note', null)).toHaveLength(0);
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
