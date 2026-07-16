// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpResponse, http} from 'msw';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {Config} from '../../Config';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createPwnedPasswordsRangeHandler} from '../../test/msw/handlers/PwnedPasswordsHandlers';
import {createStripeApiHandlers, type StripeApiHandlers} from '../../test/msw/handlers/StripeApiHandlers';
import {server} from '../../test/msw/server';
import {createBuilder} from '../../test/TestRequestBuilder';

const MOCK_PRICES = {
	monthlyUsd: 'price_monthly_usd',
	monthlyEur: 'price_monthly_eur',
	yearlyUsd: 'price_yearly_usd',
	yearlyEur: 'price_yearly_eur',
	gift1MonthUsd: 'price_gift_1_month_usd',
	gift1MonthEur: 'price_gift_1_month_eur',
	gift1YearUsd: 'price_gift_1_year_usd',
	gift1YearEur: 'price_gift_1_year_eur',
};

describe('StripeSubscriptionService', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	let originalPrices: typeof Config.stripe.prices | undefined;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		originalPrices = Config.stripe.prices;
		Config.stripe.prices = MOCK_PRICES;
		stripeHandlers = createStripeApiHandlers();
		server.use(...stripeHandlers.handlers);
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.prices = originalPrices;
	});
	beforeEach(async () => {
		await harness.resetData();
		stripeHandlers.resetAll();
		server.use(...stripeHandlers.handlers, createPwnedPasswordsRangeHandler());
	});
	describe('POST /premium/cancel-subscription', () => {
		test('cancels subscription at period end', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
					premium_will_cancel: false,
				})
				.execute();
			await createBuilder(harness, account.token).post('/premium/cancel-subscription').expect(204).execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
			const update = stripeHandlers.spies.updatedSubscriptions[0];
			expect(update?.id).toBe('sub_test_1');
			expect(update?.params.cancel_at_period_end).toBe('true');
			const me = await createBuilder<{
				premium_will_cancel: boolean;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_will_cancel).toBe(true);
		});
		test('cancels a subscription through its schedule when a yearly upgrade is pending', async () => {
			const account = await createTestAccount(harness);
			const currentPeriodStart = Math.floor(Date.now() / 1000) - 3 * 24 * 60 * 60;
			const currentPeriodEnd = currentPeriodStart + 30 * 24 * 60 * 60;
			stripeHandlers = createStripeApiHandlers({
				subscriptions: {
					sub_test_scheduled_upgrade: {
						price_id: MOCK_PRICES.monthlyUsd,
						interval: 'month',
						item_id: 'si_scheduled_upgrade_monthly',
						current_period_start: currentPeriodStart,
						current_period_end: currentPeriodEnd,
					},
				},
			});
			server.use(...stripeHandlers.handlers);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_scheduled_upgrade',
					premium_type: 1,
					premium_billing_cycle: 'monthly',
					premium_will_cancel: false,
				})
				.execute();
			await createBuilder(harness, account.token)
				.post('/premium/change-subscription')
				.body({billing_cycle: 'yearly', effective_at: 'period_end'})
				.expect(204)
				.execute();
			stripeHandlers.spies.updatedSubscriptions.length = 0;
			stripeHandlers.spies.createdSubscriptionSchedules.length = 0;
			stripeHandlers.spies.updatedSubscriptionSchedules.length = 0;
			stripeHandlers.spies.releasedSubscriptionSchedules.length = 0;
			await createBuilder(harness, account.token).post('/premium/cancel-subscription').expect(204).execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
			expect(stripeHandlers.spies.releasedSubscriptionSchedules).toHaveLength(0);
			expect(stripeHandlers.spies.updatedSubscriptionSchedules).toHaveLength(1);
			const scheduleUpdate = stripeHandlers.spies.updatedSubscriptionSchedules[0];
			expect(scheduleUpdate?.params.end_behavior).toBe('cancel');
			expect(scheduleUpdate?.params.proration_behavior).toBe('none');
			expect(scheduleUpdate?.params.phases).toHaveLength(1);
			expect(scheduleUpdate?.params.phases?.[0]?.end_date).toBe(String(currentPeriodEnd));
			expect(scheduleUpdate?.params.phases?.[0]?.items?.[0]?.price).toBe(MOCK_PRICES.monthlyUsd);
			const me = await createBuilder<{
				premium_will_cancel: boolean;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_will_cancel).toBe(true);
		});
		test('rejects when no active subscription', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post('/premium/cancel-subscription')
				.expect(400, APIErrorCodes.STRIPE_NO_ACTIVE_SUBSCRIPTION)
				.execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
		});
		test('rejects when subscription already canceling', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
					premium_will_cancel: true,
				})
				.execute();
			await createBuilder(harness, account.token)
				.post('/premium/cancel-subscription')
				.expect(400, APIErrorCodes.STRIPE_SUBSCRIPTION_ALREADY_CANCELING)
				.execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
		});
		test('rejects when user does not exist', async () => {
			await createBuilder(harness, 'invalid-token').post('/premium/cancel-subscription').expect(401).execute();
		});
		test('handles stripe api errors gracefully', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
					premium_will_cancel: false,
				})
				.execute();
			stripeHandlers.reset();
			server.use(...createStripeApiHandlers({subscriptionShouldFail: true}).handlers);
			await createBuilder(harness, account.token)
				.post('/premium/cancel-subscription')
				.expect(400, APIErrorCodes.STRIPE_ERROR)
				.execute();
		});
	});
	describe('POST /premium/reactivate-subscription', () => {
		test('reactivates subscription', async () => {
			const account = await createTestAccount(harness);
			const currentPeriodEnd = Math.floor(Date.now() / 1000) + 30 * 24 * 60 * 60;
			stripeHandlers = createStripeApiHandlers({
				subscriptions: {
					sub_test_1: {
						cancel_at: currentPeriodEnd,
						cancel_at_period_end: true,
					},
				},
			});
			server.use(...stripeHandlers.handlers);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
					premium_will_cancel: true,
				})
				.execute();
			await createBuilder(harness, account.token).post('/premium/reactivate-subscription').expect(204).execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
			const update = stripeHandlers.spies.updatedSubscriptions[0];
			expect(update?.id).toBe('sub_test_1');
			expect(update?.params.cancel_at_period_end).toBe('false');
			expect(update?.params.cancel_at).toBeUndefined();
			expect(update?.params.proration_behavior).toBe('none');
			const me = await createBuilder<{
				premium_will_cancel: boolean;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_will_cancel).toBe(false);
		});
		test('reactivates subscription with an explicit cancel date', async () => {
			const account = await createTestAccount(harness);
			const currentPeriodEnd = Math.floor(Date.now() / 1000) + 30 * 24 * 60 * 60;
			stripeHandlers = createStripeApiHandlers({
				subscriptions: {
					sub_test_explicit_cancel: {
						cancel_at: currentPeriodEnd,
						cancel_at_period_end: false,
					},
				},
			});
			server.use(...stripeHandlers.handlers);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_explicit_cancel',
					premium_type: 1,
					premium_will_cancel: true,
				})
				.execute();
			await createBuilder(harness, account.token).post('/premium/reactivate-subscription').expect(204).execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
			const update = stripeHandlers.spies.updatedSubscriptions[0];
			expect(update?.id).toBe('sub_test_explicit_cancel');
			expect(update?.params.cancel_at).toBe('');
			expect(update?.params.cancel_at_period_end).toBeUndefined();
			expect(update?.params.proration_behavior).toBe('none');
		});
		test('rejects when no subscription id', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post('/premium/reactivate-subscription')
				.expect(400, APIErrorCodes.STRIPE_NO_SUBSCRIPTION)
				.execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
		});
		test('rejects when subscription not canceling', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
					premium_will_cancel: false,
				})
				.execute();
			await createBuilder(harness, account.token)
				.post('/premium/reactivate-subscription')
				.expect(400, APIErrorCodes.STRIPE_SUBSCRIPTION_NOT_CANCELING)
				.execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
		});
		test('rejects when user does not exist', async () => {
			await createBuilder(harness, 'invalid-token').post('/premium/reactivate-subscription').expect(401).execute();
		});
		test('handles stripe api errors gracefully', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
					premium_will_cancel: true,
				})
				.execute();
			stripeHandlers.reset();
			server.use(...createStripeApiHandlers({subscriptionShouldFail: true}).handlers);
			await createBuilder(harness, account.token)
				.post('/premium/reactivate-subscription')
				.expect(400, APIErrorCodes.STRIPE_ERROR)
				.execute();
		});
	});
	describe('POST /premium/change-subscription', () => {
		test('switches an active monthly subscription to yearly billing', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
					premium_billing_cycle: 'monthly',
					premium_will_cancel: false,
				})
				.execute();
			await createBuilder(harness, account.token)
				.post('/premium/change-subscription')
				.body({billing_cycle: 'yearly'})
				.expect(204)
				.execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
			const update = stripeHandlers.spies.updatedSubscriptions[0];
			expect(update?.id).toBe('sub_test_1');
			expect(update?.params.cancel_at_period_end).toBeUndefined();
			expect(update?.params.cancel_at).toBeUndefined();
			expect(update?.params.payment_behavior).toBe('error_if_incomplete');
			expect(update?.params.proration_behavior).toBe('always_invoice');
			expect(update?.params.items).toEqual([{id: 'si_test_1', price: MOCK_PRICES.yearlyUsd, quantity: '1'}]);
			const me = await createBuilder<{
				premium_billing_cycle: string | null;
				premium_will_cancel: boolean;
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_billing_cycle).toBe('yearly');
			expect(me.premium_will_cancel).toBe(false);
			expect(me.premium_until).not.toBeNull();
		});
		test('switches an active yearly subscription to monthly billing', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
					premium_billing_cycle: 'yearly',
					premium_will_cancel: false,
				})
				.execute();
			server.use(
				http.get('https://api.stripe.com/v1/subscriptions/:id', ({params}) => {
					const currentPeriodEnd = Math.floor(Date.now() / 1000) + 365 * 24 * 60 * 60;
					return HttpResponse.json({
						id: params.id,
						object: 'subscription',
						customer: 'cus_test_1',
						status: 'active',
						trial_end: null,
						items: {
							object: 'list',
							data: [
								{
									id: 'si_test_yearly',
									object: 'subscription_item',
									price: {
										id: MOCK_PRICES.yearlyUsd,
										object: 'price',
										unit_amount: 2500,
										currency: 'usd',
										recurring: {
											interval: 'year',
											interval_count: 1,
										},
										type: 'recurring',
										active: true,
										livemode: false,
									},
									quantity: 1,
									current_period_end: currentPeriodEnd,
								},
							],
							has_more: false,
							url: `/v1/subscription_items?subscription=${params.id}`,
						},
						cancel_at: null,
						cancel_at_period_end: false,
						canceled_at: null,
						collection_method: 'charge_automatically',
						created: Math.floor(Date.now() / 1000) - 90 * 24 * 60 * 60,
						livemode: false,
						metadata: {},
						start_date: Math.floor(Date.now() / 1000) - 90 * 24 * 60 * 60,
					});
				}),
			);
			await createBuilder(harness, account.token)
				.post('/premium/change-subscription')
				.body({billing_cycle: 'monthly'})
				.expect(204)
				.execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
			const update = stripeHandlers.spies.updatedSubscriptions[0];
			expect(update?.params.items).toEqual([{id: 'si_test_yearly', price: MOCK_PRICES.monthlyUsd, quantity: '1'}]);
			const me = await createBuilder<{
				premium_billing_cycle: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_billing_cycle).toBe('monthly');
		});
		test('schedules a canceling monthly subscription to switch to yearly at period end without immediate invoice', async () => {
			const account = await createTestAccount(harness);
			const currentPeriodStart = Math.floor(Date.now() / 1000) - 3 * 24 * 60 * 60;
			const currentPeriodEnd = currentPeriodStart + 30 * 24 * 60 * 60;
			stripeHandlers = createStripeApiHandlers({
				subscriptions: {
					sub_test_canceling: {
						price_id: MOCK_PRICES.monthlyUsd,
						interval: 'month',
						item_id: 'si_canceling_monthly',
						current_period_start: currentPeriodStart,
						current_period_end: currentPeriodEnd,
						cancel_at: currentPeriodEnd,
						cancel_at_period_end: false,
					},
				},
			});
			server.use(...stripeHandlers.handlers);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_canceling',
					premium_type: 1,
					premium_billing_cycle: 'monthly',
					premium_will_cancel: true,
				})
				.execute();
			await createBuilder(harness, account.token)
				.post('/premium/change-subscription')
				.body({billing_cycle: 'yearly', effective_at: 'period_end'})
				.expect(204)
				.execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
			const clearCancelUpdate = stripeHandlers.spies.updatedSubscriptions[0];
			expect(clearCancelUpdate?.params.cancel_at).toBe('');
			expect(clearCancelUpdate?.params.cancel_at_period_end).toBeUndefined();
			expect(clearCancelUpdate?.params.proration_behavior).toBe('none');
			expect(clearCancelUpdate?.params.items).toBeUndefined();
			expect(stripeHandlers.spies.createdSubscriptionSchedules).toHaveLength(1);
			expect(stripeHandlers.spies.createdSubscriptionSchedules[0]?.from_subscription).toBe('sub_test_canceling');
			expect(stripeHandlers.spies.updatedSubscriptionSchedules).toHaveLength(1);
			const scheduleUpdate = stripeHandlers.spies.updatedSubscriptionSchedules[0];
			expect(scheduleUpdate?.params.proration_behavior).toBe('none');
			expect(scheduleUpdate?.params.phases?.[0]?.end_date).toBe(String(currentPeriodEnd));
			expect(scheduleUpdate?.params.phases?.[0]?.items?.[0]?.price).toBe(MOCK_PRICES.monthlyUsd);
			expect(scheduleUpdate?.params.phases?.[1]?.start_date).toBe(String(currentPeriodEnd));
			expect(scheduleUpdate?.params.phases?.[1]?.billing_cycle_anchor).toBe('phase_start');
			expect(scheduleUpdate?.params.phases?.[1]?.items?.[0]?.price).toBe(MOCK_PRICES.yearlyUsd);
			expect(scheduleUpdate?.params.phases?.[1]?.add_invoice_items?.[0]?.discountable).toBe('false');
			expect(scheduleUpdate?.params.phases?.[1]?.add_invoice_items?.[0]?.price_data?.unit_amount).toBe('-2500');
			expect(scheduleUpdate?.params.phases?.[1]?.add_invoice_items?.[0]?.price_data?.currency).toBe('usd');
			const me = await createBuilder<{
				premium_billing_cycle: string | null;
				premium_will_cancel: boolean;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_billing_cycle).toBe('monthly');
			expect(me.premium_will_cancel).toBe(false);
			const premiumState = await createBuilder<{
				billing: {
					pending_subscription_change: {
						target_billing_cycle: string;
						effective_at: string;
						initial_amount_minor: number | null;
						recurring_amount_minor: number | null;
						credit_amount_minor: number | null;
						currency: string | null;
					} | null;
				};
			}>(harness, account.token)
				.get('/premium/state')
				.expect(200)
				.execute();
			expect(premiumState.billing.pending_subscription_change).toEqual(
				expect.objectContaining({
					target_billing_cycle: 'yearly',
					effective_at: new Date(currentPeriodEnd * 1000).toISOString(),
					initial_amount_minor: 2499,
					recurring_amount_minor: 4999,
					credit_amount_minor: 2500,
					currency: 'USD',
				}),
			);
			stripeHandlers.spies.updatedSubscriptions.length = 0;
			stripeHandlers.spies.createdSubscriptionSchedules.length = 0;
			stripeHandlers.spies.updatedSubscriptionSchedules.length = 0;
			await createBuilder(harness, account.token)
				.post('/premium/change-subscription')
				.body({billing_cycle: 'yearly'})
				.expect(204)
				.execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
			expect(stripeHandlers.spies.createdSubscriptionSchedules).toHaveLength(0);
			expect(stripeHandlers.spies.updatedSubscriptionSchedules).toHaveLength(0);
		});
		test('preserves gifted trial time when scheduling a monthly-to-yearly upgrade at period end', async () => {
			const account = await createTestAccount(harness);
			const currentPeriodStart = Math.floor(Date.now() / 1000) - 3 * 24 * 60 * 60;
			const trialEnd = Math.floor(Date.now() / 1000) + 60 * 24 * 60 * 60;
			stripeHandlers = createStripeApiHandlers({
				subscriptions: {
					sub_test_gift_trial: {
						price_id: MOCK_PRICES.monthlyUsd,
						interval: 'month',
						item_id: 'si_gift_trial_monthly',
						current_period_start: currentPeriodStart,
						current_period_end: trialEnd,
						status: 'trialing',
						trial_end: trialEnd,
					},
				},
			});
			server.use(...stripeHandlers.handlers);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_gift_trial',
					premium_type: 1,
					premium_billing_cycle: 'monthly',
					premium_will_cancel: false,
				})
				.execute();
			await createBuilder(harness, account.token)
				.post('/premium/change-subscription')
				.body({billing_cycle: 'yearly', effective_at: 'period_end'})
				.expect(204)
				.execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
			expect(stripeHandlers.spies.createdSubscriptionSchedules).toHaveLength(1);
			expect(stripeHandlers.spies.createdSubscriptionSchedules[0]?.from_subscription).toBe('sub_test_gift_trial');
			expect(stripeHandlers.spies.updatedSubscriptionSchedules).toHaveLength(1);
			const scheduleUpdate = stripeHandlers.spies.updatedSubscriptionSchedules[0];
			expect(scheduleUpdate?.params.phases?.[0]?.end_date).toBe(String(trialEnd));
			expect(scheduleUpdate?.params.phases?.[0]?.trial).toBe('true');
			expect(scheduleUpdate?.params.phases?.[0]?.trial_end).toBeUndefined();
			expect(scheduleUpdate?.params.phases?.[0]?.items?.[0]?.price).toBe(MOCK_PRICES.monthlyUsd);
			expect(scheduleUpdate?.params.phases?.[1]?.start_date).toBe(String(trialEnd));
			expect(scheduleUpdate?.params.phases?.[1]?.billing_cycle_anchor).toBe('phase_start');
			expect(scheduleUpdate?.params.phases?.[1]?.items?.[0]?.price).toBe(MOCK_PRICES.yearlyUsd);
			expect(scheduleUpdate?.params.phases?.[1]?.add_invoice_items).toBeUndefined();
			const premiumState = await createBuilder<{
				billing: {
					pending_subscription_change: {
						target_billing_cycle: string;
						effective_at: string;
						initial_amount_minor: number | null;
						recurring_amount_minor: number | null;
						credit_amount_minor: number | null;
					} | null;
				};
			}>(harness, account.token)
				.get('/premium/state')
				.expect(200)
				.execute();
			expect(premiumState.billing.pending_subscription_change).toEqual(
				expect.objectContaining({
					target_billing_cycle: 'yearly',
					effective_at: new Date(trialEnd * 1000).toISOString(),
					initial_amount_minor: 4999,
					recurring_amount_minor: 4999,
					credit_amount_minor: null,
				}),
			);
		});
		test('cancels a pending yearly upgrade without canceling the active subscription', async () => {
			const account = await createTestAccount(harness);
			const currentPeriodStart = Math.floor(Date.now() / 1000) - 3 * 24 * 60 * 60;
			const currentPeriodEnd = currentPeriodStart + 30 * 24 * 60 * 60;
			stripeHandlers = createStripeApiHandlers({
				subscriptions: {
					sub_test_pending_upgrade: {
						price_id: MOCK_PRICES.monthlyUsd,
						interval: 'month',
						item_id: 'si_pending_upgrade_monthly',
						current_period_start: currentPeriodStart,
						current_period_end: currentPeriodEnd,
					},
				},
			});
			server.use(...stripeHandlers.handlers);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_pending_upgrade',
					premium_type: 1,
					premium_billing_cycle: 'monthly',
					premium_will_cancel: false,
				})
				.execute();
			await createBuilder(harness, account.token)
				.post('/premium/change-subscription')
				.body({billing_cycle: 'yearly', effective_at: 'period_end'})
				.expect(204)
				.execute();
			stripeHandlers.spies.updatedSubscriptions.length = 0;
			stripeHandlers.spies.createdSubscriptionSchedules.length = 0;
			stripeHandlers.spies.updatedSubscriptionSchedules.length = 0;
			stripeHandlers.spies.releasedSubscriptionSchedules.length = 0;
			await createBuilder(harness, account.token)
				.post('/premium/cancel-pending-subscription-change')
				.expect(204)
				.execute();
			expect(stripeHandlers.spies.releasedSubscriptionSchedules).toEqual([
				expect.objectContaining({id: 'sub_sched_test_1'}),
			]);
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
			const premiumState = await createBuilder<{
				billing: {
					pending_subscription_change: unknown;
				};
			}>(harness, account.token)
				.get('/premium/state')
				.expect(200)
				.execute();
			expect(premiumState.billing.pending_subscription_change).toBeNull();
			const me = await createBuilder<{
				premium_will_cancel: boolean;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_will_cancel).toBe(false);
		});
		test('rejects when there is no active subscription', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post('/premium/change-subscription')
				.body({billing_cycle: 'yearly'})
				.expect(400, APIErrorCodes.STRIPE_NO_ACTIVE_SUBSCRIPTION)
				.execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
		});
	});
	describe('extendSubscriptionWithGiftTrialDuration', () => {
		test('updates subscription trial_end to extend by gift duration', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
				})
				.execute();
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/extend-subscription-trial`)
				.body({
					duration_type: 'months',
					duration_quantity: 3,
					idempotency_key: 'gift_code_123',
				})
				.expect(204)
				.execute();
			expect(stripeHandlers.spies.retrievedSubscriptions).toContain('sub_test_1');
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
			const update = stripeHandlers.spies.updatedSubscriptions[0];
			expect(update?.id).toBe('sub_test_1');
			expect(update?.params.trial_end).toBeDefined();
			expect(update?.params.proration_behavior).toBe('none');
		});
		test('stacks multiple gifts by reading current trial_end', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
				})
				.execute();
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/extend-subscription-trial`)
				.body({
					duration_type: 'months',
					duration_quantity: 3,
					idempotency_key: 'gift_1',
				})
				.expect(204)
				.execute();
			stripeHandlers.reset();
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/extend-subscription-trial`)
				.body({
					duration_type: 'months',
					duration_quantity: 6,
					idempotency_key: 'gift_2',
				})
				.expect(204)
				.execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
			const update = stripeHandlers.spies.updatedSubscriptions[0];
			expect(update?.params.trial_end).toBeDefined();
		});
		test('serialises concurrent gift trial extensions onto the same subscription', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
				})
				.execute();
			const [resultA, resultB] = await Promise.allSettled([
				createBuilder(harness, account.token)
					.post(`/test/users/${account.userId}/extend-subscription-trial`)
					.body({
						duration_type: 'months',
						duration_quantity: 3,
						idempotency_key: 'concurrent_gift_a',
					})
					.expect(204)
					.execute(),
				createBuilder(harness, account.token)
					.post(`/test/users/${account.userId}/extend-subscription-trial`)
					.body({
						duration_type: 'months',
						duration_quantity: 6,
						idempotency_key: 'concurrent_gift_b',
					})
					.expect(204)
					.execute(),
			]);
			expect(resultA.status).toBe('fulfilled');
			expect(resultB.status).toBe('fulfilled');
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(2);
			const firstUpdate = stripeHandlers.spies.updatedSubscriptions[0];
			const secondUpdate = stripeHandlers.spies.updatedSubscriptions[1];
			expect(firstUpdate?.params.trial_end).toBeDefined();
			expect(secondUpdate?.params.trial_end).toBeDefined();
			const firstTrialEnd = Number(firstUpdate?.params.trial_end);
			const secondTrialEnd = Number(secondUpdate?.params.trial_end);
			expect(secondTrialEnd).toBeGreaterThan(firstTrialEnd);
		});
		test('enforces idempotency with cache (gift already applied)', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
				})
				.execute();
			const idempotencyKey = 'gift_code_unique_123';
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/extend-subscription-trial`)
				.body({
					duration_months: 3,
					idempotency_key: idempotencyKey,
				})
				.expect(204)
				.execute();
			stripeHandlers.reset();
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/extend-subscription-trial`)
				.body({
					duration_months: 3,
					idempotency_key: idempotencyKey,
				})
				.expect(204)
				.execute();
			expect(stripeHandlers.spies.retrievedSubscriptions).toHaveLength(0);
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
		});
		test('rejects when user has no active subscription', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/extend-subscription-trial`)
				.body({
					duration_months: 3,
					idempotency_key: 'gift_no_sub',
				})
				.expect(400, APIErrorCodes.NO_ACTIVE_SUBSCRIPTION)
				.execute();
			expect(stripeHandlers.spies.retrievedSubscriptions).toHaveLength(0);
		});
		test('handles stripe api errors gracefully', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_test_1',
					premium_type: 1,
				})
				.execute();
			stripeHandlers.reset();
			server.use(...createStripeApiHandlers({subscriptionShouldFail: true}).handlers);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/extend-subscription-trial`)
				.body({
					duration_months: 3,
					idempotency_key: 'gift_fail',
				})
				.expect(400, APIErrorCodes.STRIPE_ERROR)
				.execute();
		});
	});
});
