// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {
	AdminBillingOverviewResponse,
	AdminBillingRefundLatestInvoiceCancelResponse,
	AdminInvoiceListResponse,
} from '@fluxer/schema/src/domains/admin/AdminBillingSchemas';
import type Stripe from 'stripe';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {createUserID} from '../../BrandedTypes';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createStripeApiHandlers} from '../../test/msw/handlers/StripeApiHandlers';
import {server} from '../../test/msw/server';
import {createBuilder} from '../../test/TestRequestBuilder';
import {PaymentRepository} from '../../user/repositories/PaymentRepository';

const DAY_SECONDS = 24 * 60 * 60;

function stripeFixture<T>(value: object): T {
	return value as T;
}

describe('Admin billing overview', () => {
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
	afterEach(() => {
		server.resetHandlers();
	});
	async function setStripeCustomerId(userId: string, stripeCustomerId: string): Promise<void> {
		await createBuilder(harness, '')
			.post(`/test/users/${userId}/premium`)
			.body({stripe_customer_id: stripeCustomerId})
			.execute();
	}
	async function mirrorCustomer(params: {stripeCustomerId: string; userId?: string}): Promise<void> {
		await getBillingRepository().customers.upsertFromStripe(
			{
				id: params.stripeCustomerId,
				object: 'customer',
				created: Math.floor(Date.now() / 1000),
				email: null,
				invoice_settings: {default_payment_method: null},
				livemode: false,
				metadata: params.userId ? {userId: params.userId} : {},
			} as Stripe.Customer,
			params.userId ? {knownUserId: BigInt(params.userId)} : undefined,
		);
	}
	async function mirrorSubscription(params: {
		currentPeriodEnd?: number;
		currentPeriodStart?: number;
		latestInvoiceId?: string;
		status?: Stripe.Subscription.Status;
		stripeCustomerId: string;
		stripeSubscriptionId: string;
		userId?: string;
	}): Promise<void> {
		const now = Math.floor(Date.now() / 1000);
		const currentPeriodStart = params.currentPeriodStart ?? now - DAY_SECONDS;
		const currentPeriodEnd = params.currentPeriodEnd ?? now + 29 * DAY_SECONDS;
		await getBillingRepository().subscriptions.upsertFromStripe(
			{
				id: params.stripeSubscriptionId,
				cancel_at: null,
				cancel_at_period_end: false,
				canceled_at: null,
				collection_method: 'charge_automatically',
				created: currentPeriodStart,
				currency: 'eur',
				customer: params.stripeCustomerId,
				items: {
					data: [
						{
							id: `si_${params.stripeSubscriptionId}`,
							current_period_start: currentPeriodStart,
							current_period_end: currentPeriodEnd,
							price: {
								id: 'price_monthly_eur',
								product: 'prod_monthly',
								unit_amount: 499,
							},
							quantity: 1,
						},
					],
				},
				latest_invoice: params.latestInvoiceId ?? null,
				livemode: false,
				metadata: params.userId ? {userId: params.userId} : {},
				status: params.status ?? 'active',
			},
			params.userId ? {knownUserId: BigInt(params.userId)} : undefined,
		);
	}
	async function mirrorInvoice(params: {
		amountPaidCents: number;
		chargeId?: string;
		created?: number;
		currency?: string;
		invoiceId: string;
		paymentIntentId?: string;
		paymentId?: string;
		stripeCustomerId: string;
		stripeSubscriptionId?: string;
		userId?: string;
	}): Promise<void> {
		const created = params.created ?? Math.floor(Date.now() / 1000);
		await getBillingRepository().invoices.upsertFromStripe(
			stripeFixture<Stripe.Invoice>({
				id: params.invoiceId,
				object: 'invoice',
				amount_due: params.amountPaidCents,
				amount_paid: params.amountPaidCents,
				amount_remaining: 0,
				attempt_count: 1,
				attempted: true,
				billing_reason: 'subscription_cycle',
				collection_method: 'charge_automatically',
				created,
				currency: params.currency ?? 'eur',
				customer: params.stripeCustomerId,
				livemode: false,
				metadata: params.userId ? {userId: params.userId} : {},
				paid: true,
				payments:
					params.paymentIntentId || params.chargeId
						? {
								object: 'list',
								data: [
									{
										id: params.paymentId ?? `inpay_${params.invoiceId}`,
										object: 'invoice_payment',
										amount_paid: params.amountPaidCents,
										amount_requested: params.amountPaidCents,
										created,
										currency: params.currency ?? 'eur',
										invoice: params.invoiceId,
										is_default: true,
										livemode: false,
										payment: {
											type: 'payment_intent',
											payment_intent: params.paymentIntentId ?? null,
											charge: params.chargeId ?? null,
										},
										status: 'paid',
										status_transitions: {canceled_at: null, paid_at: created + 20},
									},
								],
								has_more: false,
								url: `/v1/invoices/${params.invoiceId}/payments`,
							}
						: {object: 'list', data: [], has_more: false, url: `/v1/invoices/${params.invoiceId}/payments`},
				status: 'paid',
				status_transitions: {finalized_at: created, paid_at: created + 20, voided_at: null},
				subscription: params.stripeSubscriptionId ?? null,
				subtotal: params.amountPaidCents,
				total: params.amountPaidCents,
			}),
			params.userId ? {knownUserId: BigInt(params.userId)} : undefined,
		);
	}
	async function mirrorPaymentIntent(params: {
		amountCents?: number;
		chargeId?: string;
		invoiceId?: string;
		paymentIntentId: string;
		stripeCustomerId: string;
	}): Promise<void> {
		await getBillingRepository().paymentIntents.upsertFromStripe(
			stripeFixture<Stripe.PaymentIntent>({
				id: params.paymentIntentId,
				object: 'payment_intent',
				amount: params.amountCents ?? 499,
				amount_capturable: 0,
				amount_received: params.amountCents ?? 499,
				capture_method: 'automatic',
				confirmation_method: 'automatic',
				created: Math.floor(Date.now() / 1000),
				currency: 'eur',
				customer: params.stripeCustomerId,
				invoice: params.invoiceId ?? null,
				latest_charge: params.chargeId ?? null,
				livemode: false,
				metadata: {},
				payment_method_types: ['card'],
				status: 'succeeded',
			}),
		);
	}
	async function mirrorCharge(params: {
		amountCents?: number;
		chargeId: string;
		invoiceId?: string;
		paymentIntentId?: string;
		stripeCustomerId: string;
	}): Promise<void> {
		await getBillingRepository().charges.upsertFromStripe(
			stripeFixture<Stripe.Charge>({
				id: params.chargeId,
				object: 'charge',
				amount: params.amountCents ?? 499,
				amount_captured: params.amountCents ?? 499,
				amount_refunded: 0,
				billing_details: {address: {country: null}},
				captured: true,
				created: Math.floor(Date.now() / 1000),
				currency: 'eur',
				customer: params.stripeCustomerId,
				invoice: params.invoiceId ?? null,
				livemode: false,
				metadata: {},
				paid: true,
				payment_intent: params.paymentIntentId ?? null,
				payment_method_details: {type: 'card', card: {brand: 'visa', last4: '4242', country: null}},
				refunded: false,
				status: 'succeeded',
			}),
		);
	}
	async function mirrorPaymentMethod(params: {paymentMethodId: string; stripeCustomerId: string}): Promise<void> {
		await getBillingRepository().paymentMethods.upsertFromStripe(
			{
				id: params.paymentMethodId,
				object: 'payment_method',
				billing_details: {address: {country: 'US'}, email: null, name: null, phone: null},
				card: {brand: 'visa', country: 'US', exp_month: 12, exp_year: 2031, funding: 'credit', last4: '4242'},
				created: Math.floor(Date.now() / 1000),
				customer: params.stripeCustomerId,
				livemode: false,
				metadata: {},
				type: 'card',
			} as Stripe.PaymentMethod,
			{isDefault: true},
		);
	}
	async function setStripeSubscriptionState(params: {
		userId: string;
		stripeCustomerId: string;
		stripeSubscriptionId: string;
	}): Promise<void> {
		await createBuilder(harness, '')
			.post(`/test/users/${params.userId}/premium`)
			.body({
				stripe_customer_id: params.stripeCustomerId,
				stripe_subscription_id: params.stripeSubscriptionId,
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_billing_cycle: 'monthly',
				premium_will_cancel: false,
			})
			.execute();
	}
	function createRefundPolicyStripeHandlers(params: {
		amountPaidCents: number;
		currency?: string;
		elapsedDays: number;
		invoiceId: string;
		stripeCustomerId: string;
		stripeSubscriptionId: string;
	}) {
		const now = Math.floor(Date.now() / 1000);
		const currentPeriodStart = now - params.elapsedDays * DAY_SECONDS;
		const currentPeriodEnd = currentPeriodStart + 30 * DAY_SECONDS;
		return createStripeApiHandlers({
			subscriptions: {
				[params.stripeSubscriptionId]: {
					customer: params.stripeCustomerId,
					current_period_start: currentPeriodStart,
					current_period_end: currentPeriodEnd,
					latest_invoice: params.invoiceId,
					status: 'active',
				},
			},
			invoices: {
				[params.invoiceId]: {
					customer: params.stripeCustomerId,
					subscriptionId: params.stripeSubscriptionId,
					amount_due: params.amountPaidCents,
					amount_paid: params.amountPaidCents,
					billing_reason: 'subscription_cycle',
					currency: params.currency ?? 'eur',
					created: now - 300,
					status: 'paid',
				},
			},
		});
	}
	async function createPaymentRecord(params: {
		userId: string;
		checkoutSessionId: string;
		completedAt?: Date;
		euWithdrawalWaiverAccepted?: boolean;
		euWithdrawalWaiverAcceptedAt?: Date | null;
		euWithdrawalWaiverRequired?: boolean;
		euWithdrawalWaiverTextVersion?: string | null;
		invoiceId: string;
		purchaseClientCountryCode?: string | null;
		purchaseGeoipCountryCode?: string | null;
		subscriptionId: string;
		stripeCustomerId: string;
	}): Promise<void> {
		const paymentRepository = new PaymentRepository();
		const createdAt = params.completedAt ?? new Date('2026-02-23T14:27:32.409Z');
		await paymentRepository.createPayment({
			checkout_session_id: params.checkoutSessionId,
			user_id: createUserID(BigInt(params.userId)),
			price_id: 'price_monthly_eur',
			product_type: 'monthly_subscription',
			status: 'completed',
			is_gift: false,
			created_at: createdAt,
			purchase_geoip_country_code: params.purchaseGeoipCountryCode ?? null,
			purchase_client_country_code: params.purchaseClientCountryCode ?? null,
			eu_withdrawal_waiver_required: params.euWithdrawalWaiverRequired ?? false,
			eu_withdrawal_waiver_accepted: params.euWithdrawalWaiverAccepted ?? false,
			eu_withdrawal_waiver_accepted_at: params.euWithdrawalWaiverAcceptedAt ?? null,
			eu_withdrawal_waiver_text_version: params.euWithdrawalWaiverTextVersion ?? null,
		});
		await paymentRepository.updatePayment({
			checkout_session_id: params.checkoutSessionId,
			stripe_customer_id: params.stripeCustomerId,
			payment_intent_id: null,
			subscription_id: params.subscriptionId,
			invoice_id: params.invoiceId,
			amount_cents: 499,
			currency: 'eur',
			status: 'completed',
			completed_at: createdAt,
			purchase_geoip_country_code: params.purchaseGeoipCountryCode ?? null,
			purchase_client_country_code: params.purchaseClientCountryCode ?? null,
			eu_withdrawal_waiver_required: params.euWithdrawalWaiverRequired ?? false,
			eu_withdrawal_waiver_accepted: params.euWithdrawalWaiverAccepted ?? false,
			eu_withdrawal_waiver_accepted_at: params.euWithdrawalWaiverAcceptedAt ?? null,
			eu_withdrawal_waiver_text_version: params.euWithdrawalWaiverTextVersion ?? null,
		});
	}
	test('resolves missing payment intents from Stripe invoice payments for overview and invoice listings', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), ['admin:authenticate', 'billing:view']);
		const targetUser = await createTestAccount(harness);
		const stripeCustomerId = 'cus_billing_target';
		await setStripeCustomerId(targetUser.userId, stripeCustomerId);
		await createPaymentRecord({
			userId: targetUser.userId,
			checkoutSessionId: 'cs_billing_overview_1',
			invoiceId: 'in_local_checkout_1',
			subscriptionId: 'sub_billing_target',
			stripeCustomerId,
		});
		const stripeHandlers = createStripeApiHandlers({
			invoices: {
				in_local_checkout_1: {
					customer: stripeCustomerId,
					subscriptionId: 'sub_billing_target',
					amount_due: 499,
					amount_paid: 499,
					billing_reason: 'subscription_create',
					currency: 'eur',
					created: 1771862851,
					payments: {
						object: 'list',
						data: [
							{
								id: 'inpay_local_checkout_1',
								object: 'invoice_payment',
								amount_paid: 499,
								amount_requested: 499,
								created: 1771862851,
								currency: 'eur',
								invoice: 'in_local_checkout_1',
								is_default: true,
								livemode: false,
								payment: {
									type: 'payment_intent',
									payment_intent: 'pi_local_checkout_1',
									charge: 'ch_local_checkout_1',
								},
								status: 'paid',
								status_transitions: {
									canceled_at: null,
									paid_at: 1771862871,
								},
							},
						],
						has_more: false,
						url: '/v1/invoices/in_local_checkout_1/payments',
					},
				},
				in_renewal_1: {
					customer: stripeCustomerId,
					subscriptionId: 'sub_billing_target',
					amount_due: 499,
					amount_paid: 499,
					billing_reason: 'subscription_cycle',
					currency: 'eur',
					created: 1776065330,
					payments: {
						object: 'list',
						data: [
							{
								id: 'inpay_renewal_1',
								object: 'invoice_payment',
								amount_paid: 499,
								amount_requested: 499,
								created: 1776065330,
								currency: 'eur',
								invoice: 'in_renewal_1',
								is_default: true,
								livemode: false,
								payment: {
									type: 'payment_intent',
									payment_intent: 'pi_renewal_1',
									charge: 'ch_renewal_1',
								},
								status: 'paid',
								status_transitions: {
									canceled_at: null,
									paid_at: 1776065360,
								},
							},
						],
						has_more: false,
						url: '/v1/invoices/in_renewal_1/payments',
					},
				},
			},
			paymentIntents: {
				pi_local_checkout_1: {
					customer: stripeCustomerId,
					currency: 'eur',
					latest_charge: 'ch_local_checkout_1',
				},
				pi_renewal_1: {
					customer: stripeCustomerId,
					currency: 'eur',
					latest_charge: 'ch_renewal_1',
				},
			},
			paymentMethods: {
				pm_billing_target_1: {
					customer: stripeCustomerId,
					type: 'card',
					card: {
						brand: 'visa',
						last4: '4242',
						exp_month: 12,
						exp_year: 2031,
						country: 'US',
					},
				},
			},
		});
		server.use(...stripeHandlers.handlers);
		await mirrorInvoice({
			amountPaidCents: 499,
			chargeId: 'ch_local_checkout_1',
			created: 1771862851,
			invoiceId: 'in_local_checkout_1',
			paymentId: 'inpay_local_checkout_1',
			paymentIntentId: 'pi_local_checkout_1',
			stripeCustomerId,
			stripeSubscriptionId: 'sub_billing_target',
			userId: targetUser.userId,
		});
		await mirrorInvoice({
			amountPaidCents: 499,
			chargeId: 'ch_renewal_1',
			created: 1776065330,
			invoiceId: 'in_renewal_1',
			paymentId: 'inpay_renewal_1',
			paymentIntentId: 'pi_renewal_1',
			stripeCustomerId,
			stripeSubscriptionId: 'sub_billing_target',
			userId: targetUser.userId,
		});
		await mirrorPaymentIntent({
			chargeId: 'ch_local_checkout_1',
			invoiceId: 'in_local_checkout_1',
			paymentIntentId: 'pi_local_checkout_1',
			stripeCustomerId,
		});
		await mirrorPaymentIntent({
			chargeId: 'ch_renewal_1',
			invoiceId: 'in_renewal_1',
			paymentIntentId: 'pi_renewal_1',
			stripeCustomerId,
		});
		await mirrorCharge({
			chargeId: 'ch_local_checkout_1',
			invoiceId: 'in_local_checkout_1',
			paymentIntentId: 'pi_local_checkout_1',
			stripeCustomerId,
		});
		await mirrorCharge({
			chargeId: 'ch_renewal_1',
			invoiceId: 'in_renewal_1',
			paymentIntentId: 'pi_renewal_1',
			stripeCustomerId,
		});
		await mirrorPaymentMethod({paymentMethodId: 'pm_billing_target_1', stripeCustomerId});
		const overview = await createBuilder<AdminBillingOverviewResponse>(harness, `${admin.token}`)
			.get(`/admin/billing/users/${targetUser.userId}/overview`)
			.execute();
		expect(overview.payments).toHaveLength(2);
		const localCheckoutPayment = overview.payments.find((payment) => payment.invoice_id === 'in_local_checkout_1');
		expect(localCheckoutPayment?.payment_intent_id).toBe('pi_local_checkout_1');
		expect(localCheckoutPayment?.resolved_payment_intent_id).toBe('pi_local_checkout_1');
		expect(localCheckoutPayment?.charge_id).toBe('ch_local_checkout_1');
		expect(localCheckoutPayment?.refundable_via_payment_intent).toBe(true);
		expect(overview.payment_methods[0]?.id).toBe('pm_billing_target_1');
		const invoices = await createBuilder<AdminInvoiceListResponse>(harness, `${admin.token}`)
			.get(`/admin/billing/users/${targetUser.userId}/invoices`)
			.execute();
		expect(invoices.invoices).toHaveLength(2);
		expect(invoices.invoices[0]?.id).toBe('in_renewal_1');
		expect(invoices.invoices[0]?.payment_intent_id).toBe('pi_renewal_1');
		expect(invoices.invoices[0]?.charge_id).toBe('ch_renewal_1');
		expect(invoices.invoices[0]?.billing_reason).toBe('subscription_cycle');
		expect(invoices.invoices[1]?.payment_intent_id).toBe('pi_local_checkout_1');
	});
	test('resolves billing overview from Stripe metadata even when local Stripe linkage is missing', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), ['admin:authenticate', 'billing:view']);
		const targetUser = await createTestAccount(harness);
		const stripeCustomerId = 'cus_billing_metadata_only';
		const stripeSubscriptionId = 'sub_billing_metadata_only';
		const invoiceId = 'in_billing_metadata_only';
		const now = Math.floor(Date.now() / 1000);
		const stripeHandlers = createStripeApiHandlers({
			customers: {
				[stripeCustomerId]: {
					email: 'target@example.com',
					metadata: {
						userId: targetUser.userId,
					},
				},
			},
			invoices: {
				[invoiceId]: {
					customer: stripeCustomerId,
					subscriptionId: stripeSubscriptionId,
					amount_due: 499,
					amount_paid: 499,
					billing_reason: 'subscription_create',
					currency: 'eur',
					created: now - 300,
					status: 'paid',
				},
			},
			paymentMethods: {
				pm_billing_metadata_only: {
					customer: stripeCustomerId,
					type: 'card',
					card: {
						brand: 'visa',
						last4: '1111',
						exp_month: 8,
						exp_year: 2031,
						country: 'FR',
					},
				},
			},
			subscriptions: {
				[stripeSubscriptionId]: {
					customer: stripeCustomerId,
					latest_invoice: invoiceId,
					status: 'active',
					current_period_start: now - DAY_SECONDS,
					current_period_end: now + 29 * DAY_SECONDS,
				},
			},
		});
		server.use(...stripeHandlers.handlers);
		await mirrorCustomer({stripeCustomerId, userId: targetUser.userId});
		await mirrorSubscription({
			currentPeriodEnd: now + 29 * DAY_SECONDS,
			currentPeriodStart: now - DAY_SECONDS,
			latestInvoiceId: invoiceId,
			stripeCustomerId,
			stripeSubscriptionId,
			userId: targetUser.userId,
		});
		await mirrorInvoice({
			amountPaidCents: 499,
			chargeId: `ch_${invoiceId}`,
			created: now - 300,
			invoiceId,
			paymentIntentId: `pi_${invoiceId}`,
			stripeCustomerId,
			stripeSubscriptionId,
			userId: targetUser.userId,
		});
		await mirrorPaymentIntent({
			chargeId: `ch_${invoiceId}`,
			invoiceId,
			paymentIntentId: `pi_${invoiceId}`,
			stripeCustomerId,
		});
		await mirrorCharge({
			chargeId: `ch_${invoiceId}`,
			invoiceId,
			paymentIntentId: `pi_${invoiceId}`,
			stripeCustomerId,
		});
		await mirrorPaymentMethod({paymentMethodId: 'pm_billing_metadata_only', stripeCustomerId});
		const overview = await createBuilder<AdminBillingOverviewResponse>(harness, `${admin.token}`)
			.get(`/admin/billing/users/${targetUser.userId}/overview`)
			.execute();
		expect(overview.stripe_customer_id).toBe(stripeCustomerId);
		expect(overview.subscription?.id).toBe(stripeSubscriptionId);
		expect(overview.subscription?.status).toBe('active');
		expect(overview.payment_methods[0]?.id).toBe('pm_billing_metadata_only');
		expect(overview.payments[0]?.invoice_id).toBe(invoiceId);
		expect(overview.payments[0]?.resolved_payment_intent_id).toBe(`pi_${invoiceId}`);
	});
	test('cancels a Stripe subscription at period end after resolving missing local Stripe IDs from Stripe metadata', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'billing:manage_subscription',
		]);
		const targetUser = await createTestAccount(harness);
		const stripeCustomerId = 'cus_billing_cancel_metadata';
		const stripeSubscriptionId = 'sub_billing_cancel_metadata';
		const now = Math.floor(Date.now() / 1000);
		const stripeHandlers = createStripeApiHandlers({
			customers: {
				[stripeCustomerId]: {
					metadata: {
						userId: targetUser.userId,
					},
				},
			},
			subscriptions: {
				[stripeSubscriptionId]: {
					customer: stripeCustomerId,
					status: 'active',
					current_period_start: now - DAY_SECONDS,
					current_period_end: now + 29 * DAY_SECONDS,
				},
			},
		});
		server.use(...stripeHandlers.handlers);
		await mirrorCustomer({stripeCustomerId, userId: targetUser.userId});
		await mirrorSubscription({
			currentPeriodEnd: now + 29 * DAY_SECONDS,
			currentPeriodStart: now - DAY_SECONDS,
			stripeCustomerId,
			stripeSubscriptionId,
			userId: targetUser.userId,
		});
		await createBuilder(harness, `${admin.token}`)
			.post(`/admin/billing/users/${targetUser.userId}/cancel-subscription`)
			.body({})
			.expect(204)
			.execute();
		expect(stripeHandlers.spies.updatedSubscriptions).toContainEqual({
			id: stripeSubscriptionId,
			params: {
				cancel_at_period_end: 'true',
			},
		});
	});
	test('allows admin refunds when the payment intent belongs to the target Stripe customer even without a local payment-intent index', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'billing:refund',
		]);
		const targetUser = await createTestAccount(harness);
		const stripeCustomerId = 'cus_refund_target';
		await setStripeCustomerId(targetUser.userId, stripeCustomerId);
		const stripeHandlers = createStripeApiHandlers({
			paymentIntents: {
				pi_remote_only_refund: {
					customer: stripeCustomerId,
					latest_charge: 'ch_remote_only_refund',
				},
			},
		});
		server.use(...stripeHandlers.handlers);
		await mirrorPaymentIntent({
			chargeId: 'ch_remote_only_refund',
			paymentIntentId: 'pi_remote_only_refund',
			stripeCustomerId,
		});
		await createBuilder(harness, `${admin.token}`)
			.post(`/admin/billing/users/${targetUser.userId}/refund`)
			.body({
				payment_intent_id: 'pi_remote_only_refund',
				reason: 'Customer requested a refund',
			})
			.expect(204)
			.execute();
		expect(stripeHandlers.spies.createdRefunds).toHaveLength(1);
		expect(stripeHandlers.spies.createdRefunds[0]).toMatchObject({
			payment_intent: 'pi_remote_only_refund',
			reason: 'requested_by_customer',
			metadata: {
				admin_user_id: admin.userId,
				target_user_id: targetUser.userId,
				admin_reason: 'Customer requested a refund',
			},
		});
	});
	test('forces a full refund when the latest invoice is inside the EU withdrawal window without a waiver', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'billing:refund',
			'billing:manage_subscription',
		]);
		const targetUser = await createTestAccount(harness);
		const stripeCustomerId = 'cus_eu_missing_waiver';
		const stripeSubscriptionId = 'sub_eu_missing_waiver';
		const invoiceId = 'in_eu_missing_waiver';
		await setStripeSubscriptionState({userId: targetUser.userId, stripeCustomerId, stripeSubscriptionId});
		await createPaymentRecord({
			userId: targetUser.userId,
			checkoutSessionId: 'cs_eu_missing_waiver',
			completedAt: new Date(Date.now() - 6 * DAY_SECONDS * 1000),
			euWithdrawalWaiverRequired: true,
			euWithdrawalWaiverAccepted: false,
			euWithdrawalWaiverTextVersion: '2026-04-23',
			invoiceId,
			purchaseClientCountryCode: 'DE',
			purchaseGeoipCountryCode: 'DE',
			subscriptionId: stripeSubscriptionId,
			stripeCustomerId,
		});
		const stripeHandlers = createRefundPolicyStripeHandlers({
			amountPaidCents: 499,
			elapsedDays: 6,
			invoiceId,
			stripeCustomerId,
			stripeSubscriptionId,
		});
		server.use(...stripeHandlers.handlers);
		const now = Math.floor(Date.now() / 1000);
		await mirrorSubscription({
			currentPeriodEnd: now + 24 * DAY_SECONDS,
			currentPeriodStart: now - 6 * DAY_SECONDS,
			latestInvoiceId: invoiceId,
			stripeCustomerId,
			stripeSubscriptionId,
			userId: targetUser.userId,
		});
		await mirrorInvoice({
			amountPaidCents: 499,
			chargeId: 'ch_eu_missing_waiver',
			invoiceId,
			paymentIntentId: 'pi_eu_missing_waiver',
			stripeCustomerId,
			stripeSubscriptionId,
			userId: targetUser.userId,
		});
		await mirrorCharge({
			chargeId: 'ch_eu_missing_waiver',
			invoiceId,
			paymentIntentId: 'pi_eu_missing_waiver',
			stripeCustomerId,
		});
		const result = await createBuilder<AdminBillingRefundLatestInvoiceCancelResponse>(harness, `${admin.token}`)
			.post(`/admin/billing/users/${targetUser.userId}/refund-policy-cancel-now`)
			.body({reason: 'Withdrawal waiver missing'})
			.execute();
		expect(result.refund_policy).toBe('full_refund');
		expect(result.refund_policy_basis).toBe('eu_eea_withdrawal_no_waiver');
		expect(result.refunded_amount_cents).toBe(499);
		expect(result.eu_withdrawal_waiver_required).toBe(true);
		expect(result.eu_withdrawal_waiver_accepted).toBe(false);
		expect(result.purchase_geoip_country_code).toBe('DE');
		expect(stripeHandlers.spies.createdRefunds[0]).toMatchObject({
			amount: '499',
			metadata: {
				refund_policy: 'full_refund',
				refund_policy_basis: 'eu_eea_withdrawal_no_waiver',
				eu_withdrawal_waiver_required: 'true',
				eu_withdrawal_waiver_accepted: 'false',
			},
		});
		expect(stripeHandlers.spies.cancelledSubscriptions).toContain(stripeSubscriptionId);
	});
	test('uses the support prorate policy when an EU waiver was accepted', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'billing:refund',
			'billing:manage_subscription',
		]);
		const targetUser = await createTestAccount(harness);
		const stripeCustomerId = 'cus_eu_accepted_waiver';
		const stripeSubscriptionId = 'sub_eu_accepted_waiver';
		const invoiceId = 'in_eu_accepted_waiver';
		await setStripeSubscriptionState({userId: targetUser.userId, stripeCustomerId, stripeSubscriptionId});
		await createPaymentRecord({
			userId: targetUser.userId,
			checkoutSessionId: 'cs_eu_accepted_waiver',
			completedAt: new Date(Date.now() - 6 * DAY_SECONDS * 1000),
			euWithdrawalWaiverRequired: true,
			euWithdrawalWaiverAccepted: true,
			euWithdrawalWaiverAcceptedAt: new Date(Date.now() - 6 * DAY_SECONDS * 1000),
			euWithdrawalWaiverTextVersion: '2026-04-23',
			invoiceId,
			purchaseClientCountryCode: 'DE',
			purchaseGeoipCountryCode: 'DE',
			subscriptionId: stripeSubscriptionId,
			stripeCustomerId,
		});
		const stripeHandlers = createRefundPolicyStripeHandlers({
			amountPaidCents: 499,
			elapsedDays: 6,
			invoiceId,
			stripeCustomerId,
			stripeSubscriptionId,
		});
		server.use(...stripeHandlers.handlers);
		const now = Math.floor(Date.now() / 1000);
		await mirrorSubscription({
			currentPeriodEnd: now + 24 * DAY_SECONDS,
			currentPeriodStart: now - 6 * DAY_SECONDS,
			latestInvoiceId: invoiceId,
			stripeCustomerId,
			stripeSubscriptionId,
			userId: targetUser.userId,
		});
		await mirrorInvoice({
			amountPaidCents: 499,
			chargeId: 'ch_eu_accepted_waiver',
			invoiceId,
			paymentIntentId: 'pi_eu_accepted_waiver',
			stripeCustomerId,
			stripeSubscriptionId,
			userId: targetUser.userId,
		});
		await mirrorCharge({
			chargeId: 'ch_eu_accepted_waiver',
			invoiceId,
			paymentIntentId: 'pi_eu_accepted_waiver',
			stripeCustomerId,
		});
		const result = await createBuilder<AdminBillingRefundLatestInvoiceCancelResponse>(harness, `${admin.token}`)
			.post(`/admin/billing/users/${targetUser.userId}/refund-policy-cancel-now`)
			.body({})
			.execute();
		expect(result.refund_policy).toBe('prorated_refund');
		expect(result.refund_policy_basis).toBe('support_policy');
		expect(result.refunded_amount_cents).toBe(400);
		expect(stripeHandlers.spies.createdRefunds[0]).toMatchObject({
			amount: '400',
			metadata: {
				refund_policy: 'prorated_refund',
				refund_policy_basis: 'support_policy',
				eu_withdrawal_waiver_required: 'true',
				eu_withdrawal_waiver_accepted: 'true',
			},
		});
		expect(stripeHandlers.spies.cancelledSubscriptions).toContain(stripeSubscriptionId);
	});
	test('cancels without refund after the support refund window', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'billing:refund',
			'billing:manage_subscription',
		]);
		const targetUser = await createTestAccount(harness);
		const stripeCustomerId = 'cus_cancel_only';
		const stripeSubscriptionId = 'sub_cancel_only';
		const invoiceId = 'in_cancel_only';
		await setStripeSubscriptionState({userId: targetUser.userId, stripeCustomerId, stripeSubscriptionId});
		await createPaymentRecord({
			userId: targetUser.userId,
			checkoutSessionId: 'cs_cancel_only',
			completedAt: new Date(Date.now() - 20 * DAY_SECONDS * 1000),
			invoiceId,
			subscriptionId: stripeSubscriptionId,
			stripeCustomerId,
		});
		const stripeHandlers = createRefundPolicyStripeHandlers({
			amountPaidCents: 499,
			elapsedDays: 20,
			invoiceId,
			stripeCustomerId,
			stripeSubscriptionId,
		});
		server.use(...stripeHandlers.handlers);
		const now = Math.floor(Date.now() / 1000);
		await mirrorSubscription({
			currentPeriodEnd: now + 10 * DAY_SECONDS,
			currentPeriodStart: now - 20 * DAY_SECONDS,
			latestInvoiceId: invoiceId,
			stripeCustomerId,
			stripeSubscriptionId,
			userId: targetUser.userId,
		});
		await mirrorInvoice({
			amountPaidCents: 499,
			chargeId: 'ch_cancel_only',
			invoiceId,
			paymentIntentId: 'pi_cancel_only',
			stripeCustomerId,
			stripeSubscriptionId,
			userId: targetUser.userId,
		});
		await mirrorCharge({
			chargeId: 'ch_cancel_only',
			invoiceId,
			paymentIntentId: 'pi_cancel_only',
			stripeCustomerId,
		});
		const result = await createBuilder<AdminBillingRefundLatestInvoiceCancelResponse>(harness, `${admin.token}`)
			.post(`/admin/billing/users/${targetUser.userId}/refund-policy-cancel-now`)
			.body({})
			.execute();
		expect(result.refund_policy).toBe('cancel_only');
		expect(result.refund_policy_basis).toBe('support_policy');
		expect(result.refunded_amount_cents).toBe(0);
		expect(stripeHandlers.spies.createdRefunds).toHaveLength(0);
		expect(stripeHandlers.spies.cancelledSubscriptions).toContain(stripeSubscriptionId);
	});
});
