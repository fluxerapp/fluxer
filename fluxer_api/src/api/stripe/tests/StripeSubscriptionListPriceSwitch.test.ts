// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	ListPriceSwitchIneligibilityReason,
	PremiumStateResponse,
	SwitchToListPriceResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {HttpResponse, http} from 'msw';
import Stripe from 'stripe';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createPwnedPasswordsRangeHandler} from '../../test/msw/handlers/PwnedPasswordsHandlers';
import {createStripeApiHandlers, type StripeApiHandlers} from '../../test/msw/handlers/StripeApiHandlers';
import {server} from '../../test/msw/server';
import {createBuilder} from '../../test/TestRequestBuilder';
import {STRIPE_API_VERSION} from '../StripeApiVersion';

const MOCK_PRICES = {
	monthlyUsd: 'price_list_monthly_usd',
	yearlyUsd: 'price_list_yearly_usd',
	monthlyBrl: 'price_list_monthly_brl',
	yearlyBrl: 'price_list_yearly_brl',
};

const RETIRED_MONTHLY_BRL = 'price_retired_monthly_brl';
const RETIRED_CHEAP_MONTHLY_BRL = 'price_retired_cheap_monthly_brl';
const RETIRED_MONTHLY_TRY = 'price_retired_monthly_try';
const RETIRED_EQUAL_MONTHLY_BRL = 'price_retired_equal_monthly_brl';

const LIST_MONTHLY_BRL_MINOR = 1890;
const LIST_YEARLY_BRL_MINOR = 18900;
const RETIRED_MONTHLY_BRL_MINOR = 2499;
const RETIRED_CHEAP_MONTHLY_BRL_MINOR = 990;
const RETIRED_MONTHLY_TRY_MINOR = 2999;

const MOCK_PRICE_SEEDS: Record<
	string,
	{
		unit_amount: number;
		currency: string;
		interval: 'month' | 'year';
	}
> = {
	[MOCK_PRICES.monthlyUsd]: {unit_amount: 499, currency: 'usd', interval: 'month'},
	[MOCK_PRICES.yearlyUsd]: {unit_amount: 4999, currency: 'usd', interval: 'year'},
	[MOCK_PRICES.monthlyBrl]: {unit_amount: LIST_MONTHLY_BRL_MINOR, currency: 'brl', interval: 'month'},
	[MOCK_PRICES.yearlyBrl]: {unit_amount: LIST_YEARLY_BRL_MINOR, currency: 'brl', interval: 'year'},
	[RETIRED_MONTHLY_BRL]: {unit_amount: RETIRED_MONTHLY_BRL_MINOR, currency: 'brl', interval: 'month'},
	[RETIRED_CHEAP_MONTHLY_BRL]: {unit_amount: RETIRED_CHEAP_MONTHLY_BRL_MINOR, currency: 'brl', interval: 'month'},
	[RETIRED_MONTHLY_TRY]: {unit_amount: RETIRED_MONTHLY_TRY_MINOR, currency: 'try', interval: 'month'},
	[RETIRED_EQUAL_MONTHLY_BRL]: {unit_amount: LIST_MONTHLY_BRL_MINOR, currency: 'brl', interval: 'month'},
};

interface SubscriberFixture {
	subscriptionId: string;
	priceId: string;
	status?: 'active' | 'trialing' | 'past_due';
	cancelAt?: number | null;
	cancelAtPeriodEnd?: boolean;
	periodStart?: number;
	periodEnd?: number;
}

interface ResolvedSubscriber {
	token: string;
	userId: UserID;
	subscriptionId: string;
	priceId: string;
	periodStart: number;
	periodEnd: number;
}

describe('StripeSubscriptionListPriceSwitch', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	let originalPrices: typeof Config.stripe.prices | undefined;

	function applyStripeHandlers(fixture: Required<SubscriberFixture>): void {
		const seed = MOCK_PRICE_SEEDS[fixture.priceId];
		if (!seed) {
			throw new Error(`Missing explicit price seed for ${fixture.priceId}`);
		}
		stripeHandlers = createStripeApiHandlers({
			subscriptions: {
				[fixture.subscriptionId]: {
					customer: `cus_${fixture.subscriptionId}`,
					price_id: fixture.priceId,
					unit_amount: seed.unit_amount,
					currency: seed.currency,
					interval: seed.interval,
					item_id: `si_${fixture.subscriptionId}`,
					current_period_start: fixture.periodStart,
					current_period_end: fixture.periodEnd,
					status: fixture.status,
					cancel_at: fixture.cancelAt,
					cancel_at_period_end: fixture.cancelAtPeriodEnd,
				},
			},
			prices: MOCK_PRICE_SEEDS,
		});
		server.use(...stripeHandlers.handlers, createPwnedPasswordsRangeHandler());
	}

	async function mirrorStripeState(userId: UserID, subscriptionId: string): Promise<void> {
		const stripe = new Stripe(Config.stripe.secretKey ?? 'sk_test_fluxer', {
			apiVersion: STRIPE_API_VERSION,
			httpClient: Stripe.createFetchHttpClient(),
		});
		const subscription = await stripe.subscriptions.retrieve(subscriptionId, {expand: ['items.data.price']});
		await getBillingRepository().subscriptions.upsertFromStripe(subscription, {
			knownUserId: userId,
			snapshotCapturedAt: new Date(),
		});
		for (const priceId of Object.keys(MOCK_PRICE_SEEDS)) {
			const price = await stripe.prices.retrieve(priceId);
			await getBillingRepository().prices.upsertFromStripe(price);
		}
	}

	async function createSubscriber(fixture: SubscriberFixture): Promise<ResolvedSubscriber> {
		const periodStart = fixture.periodStart ?? Math.floor(Date.now() / 1000) - 3 * 24 * 60 * 60;
		const periodEnd = fixture.periodEnd ?? periodStart + 30 * 24 * 60 * 60;
		const resolved: Required<SubscriberFixture> = {
			subscriptionId: fixture.subscriptionId,
			priceId: fixture.priceId,
			status: fixture.status ?? 'active',
			cancelAt: fixture.cancelAt ?? null,
			cancelAtPeriodEnd: fixture.cancelAtPeriodEnd ?? false,
			periodStart,
			periodEnd,
		};
		const account = await createTestAccount(harness);
		applyStripeHandlers(resolved);
		await createBuilder(harness, account.token)
			.post(`/test/users/${account.userId}/premium`)
			.body({
				stripe_subscription_id: resolved.subscriptionId,
				premium_type: 1,
				premium_billing_cycle: MOCK_PRICE_SEEDS[resolved.priceId]?.interval === 'year' ? 'yearly' : 'monthly',
				premium_until: new Date(periodEnd * 1000).toISOString(),
				premium_will_cancel: Boolean(resolved.cancelAt) || resolved.cancelAtPeriodEnd,
			})
			.execute();
		const userId = createUserID(BigInt(account.userId));
		await mirrorStripeState(userId, resolved.subscriptionId);
		return {
			token: account.token,
			userId,
			subscriptionId: resolved.subscriptionId,
			priceId: resolved.priceId,
			periodStart,
			periodEnd,
		};
	}

	function switchToListPrice(subscriber: ResolvedSubscriber): Promise<SwitchToListPriceResponse> {
		return createBuilder<SwitchToListPriceResponse>(harness, subscriber.token)
			.post('/premium/switch-to-list-price')
			.expect(200)
			.execute();
	}

	function getPremiumState(subscriber: ResolvedSubscriber): Promise<PremiumStateResponse> {
		return createBuilder<PremiumStateResponse>(harness, subscriber.token).get('/premium/state').expect(200).execute();
	}

	function expectNoStripeWrites(): void {
		expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
		expect(stripeHandlers.spies.cancelledSubscriptions).toHaveLength(0);
		expect(stripeHandlers.spies.createdSubscriptionSchedules).toHaveLength(0);
		expect(stripeHandlers.spies.updatedSubscriptionSchedules).toHaveLength(0);
		expect(stripeHandlers.spies.releasedSubscriptionSchedules).toHaveLength(0);
	}

	async function expectRefusal(
		subscriber: ResolvedSubscriber,
		reason: ListPriceSwitchIneligibilityReason,
	): Promise<void> {
		const result = await switchToListPrice(subscriber);
		expect(result).toEqual({status: 'ineligible', reason});
		const state = await getPremiumState(subscriber);
		expect(state.billing.list_price_switch.available).toBe(false);
		expect(state.billing.list_price_switch.reason).toBe(reason);
	}

	beforeAll(async () => {
		originalPrices = Config.stripe.prices;
		Config.stripe.prices = MOCK_PRICES;
		harness = await createApiTestHarness();
		stripeHandlers = createStripeApiHandlers({prices: MOCK_PRICE_SEEDS});
		server.use(...stripeHandlers.handlers);
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.prices = originalPrices;
	});
	beforeEach(async () => {
		await harness.resetData();
		Config.stripe.prices = MOCK_PRICES;
		stripeHandlers.resetAll();
		server.use(...stripeHandlers.handlers, createPwnedPasswordsRangeHandler());
	});

	describe('POST /premium/switch-to-list-price', () => {
		test('schedules a grandfathered BRL monthly subscriber onto the current list price at period end', async () => {
			const moneyMoves: Array<string> = [];
			server.use(
				http.post('https://api.stripe.com/v1/invoices', () => {
					moneyMoves.push('invoices.create');
					return HttpResponse.json({});
				}),
				http.post('https://api.stripe.com/v1/charges', () => {
					moneyMoves.push('charges.create');
					return HttpResponse.json({});
				}),
				http.post('https://api.stripe.com/v1/payment_intents', () => {
					moneyMoves.push('payment_intents.create');
					return HttpResponse.json({});
				}),
			);
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_grandfathered',
				priceId: RETIRED_MONTHLY_BRL,
			});
			const result = await switchToListPrice(subscriber);
			expect(result).toEqual({
				status: 'scheduled',
				effective_at: new Date(subscriber.periodEnd * 1000).toISOString(),
				target_price_id: MOCK_PRICES.monthlyBrl,
				target_amount_minor: LIST_MONTHLY_BRL_MINOR,
				current_amount_minor: RETIRED_MONTHLY_BRL_MINOR,
				currency: 'BRL',
			});
			expect(stripeHandlers.spies.createdSubscriptionSchedules).toHaveLength(1);
			expect(stripeHandlers.spies.createdSubscriptionSchedules[0]?.from_subscription).toBe('sub_brl_grandfathered');
			expect(stripeHandlers.spies.updatedSubscriptionSchedules).toHaveLength(1);
			const scheduleUpdate = stripeHandlers.spies.updatedSubscriptionSchedules[0];
			expect(scheduleUpdate?.params.end_behavior).toBe('release');
			expect(scheduleUpdate?.params.proration_behavior).toBe('none');
			expect(scheduleUpdate?.params.phases).toHaveLength(2);
			expect(scheduleUpdate?.params.phases?.[0]?.end_date).toBe(String(subscriber.periodEnd));
			expect(scheduleUpdate?.params.phases?.[0]?.items?.[0]?.price).toBe(RETIRED_MONTHLY_BRL);
			expect(scheduleUpdate?.params.phases?.[1]?.start_date).toBe(String(subscriber.periodEnd));
			expect(scheduleUpdate?.params.phases?.[1]?.items?.[0]?.price).toBe(MOCK_PRICES.monthlyBrl);
			expect(scheduleUpdate?.params.phases?.[1]?.proration_behavior).toBe('none');
			expect(scheduleUpdate?.params.phases?.[1]?.billing_cycle_anchor).toBeUndefined();
			expect(scheduleUpdate?.params.phases?.[1]?.add_invoice_items).toBeUndefined();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
			expect(moneyMoves).toEqual([]);
			const me = await createBuilder<{
				premium_will_cancel: boolean;
			}>(harness, subscriber.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_will_cancel).toBe(false);
			const state = await getPremiumState(subscriber);
			expect(state.billing.pending_subscription_change).toBeNull();
			expect(state.billing.list_price_switch).toEqual(
				expect.objectContaining({
					available: false,
					reason: null,
					pending: true,
					effective_at: new Date(subscriber.periodEnd * 1000).toISOString(),
					current_price_id: RETIRED_MONTHLY_BRL,
					current_amount_minor: RETIRED_MONTHLY_BRL_MINOR,
					list_price_id: MOCK_PRICES.monthlyBrl,
					list_amount_minor: LIST_MONTHLY_BRL_MINOR,
					currency: 'BRL',
					billing_cycle: 'monthly',
				}),
			);
		});

		test('preserves the phase-level settings Stripe would otherwise unset on the live subscription', async () => {
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_phase_settings',
				priceId: RETIRED_MONTHLY_BRL,
			});
			const scheduleId = 'sub_sched_phase_settings';
			const richSchedule = {
				id: scheduleId,
				object: 'subscription_schedule',
				status: 'active',
				subscription: subscriber.subscriptionId,
				end_behavior: 'release',
				metadata: {},
				current_phase: {start_date: subscriber.periodStart, end_date: subscriber.periodEnd},
				phases: [
					{
						start_date: subscriber.periodStart,
						end_date: subscriber.periodEnd,
						currency: 'brl',
						collection_method: 'send_invoice',
						description: 'Grandfathered Plutonium',
						metadata: {campaign: 'brl_2026'},
						discounts: [{coupon: {id: 'co_grandfather_10'}, discount: null, promotion_code: null}],
						default_tax_rates: [{id: 'txr_br_icms'}],
						automatic_tax: {enabled: true, disabled_reason: null, liability: {type: 'self'}},
						invoice_settings: {account_tax_ids: null, days_until_due: 14, issuer: null},
						billing_cycle_anchor: 'automatic',
						add_invoice_items: [],
						items: [
							{
								price: RETIRED_MONTHLY_BRL,
								quantity: 1,
								metadata: {seat: 'primary'},
								discounts: [{coupon: null, discount: 'di_existing', promotion_code: null}],
								tax_rates: [{id: 'txr_br_iss'}],
								billing_thresholds: null,
							},
						],
						proration_behavior: 'none',
					},
				],
				livemode: false,
				created: Math.floor(Date.now() / 1000),
			};
			const scheduleUpdates: Array<Record<string, string>> = [];
			server.use(
				http.post('https://api.stripe.com/v1/subscription_schedules', () => HttpResponse.json(richSchedule)),
				http.post(`https://api.stripe.com/v1/subscription_schedules/${scheduleId}`, async ({request}) => {
					const formData = await request.formData();
					const entries: Record<string, string> = {};
					for (const [key, value] of formData.entries()) {
						entries[key] = String(value);
					}
					scheduleUpdates.push(entries);
					return HttpResponse.json(richSchedule);
				}),
			);
			const result = await switchToListPrice(subscriber);
			expect(result.status).toBe('scheduled');
			expect(scheduleUpdates).toHaveLength(1);
			const update = scheduleUpdates[0]!;
			expect(update['phases[0][items][0][price]']).toBe(RETIRED_MONTHLY_BRL);
			expect(update['phases[0][discounts][0][coupon]']).toBe('co_grandfather_10');
			expect(update['phases[0][default_tax_rates][0]']).toBe('txr_br_icms');
			expect(update['phases[0][items][0][tax_rates][0]']).toBe('txr_br_iss');
			expect(update['phases[0][items][0][discounts][0][discount]']).toBe('di_existing');
			expect(update['phases[0][items][0][metadata][seat]']).toBe('primary');
			expect(update['phases[0][metadata][campaign]']).toBe('brl_2026');
			expect(update['phases[0][collection_method]']).toBe('send_invoice');
			expect(update['phases[0][description]']).toBe('Grandfathered Plutonium');
			expect(update['phases[0][currency]']).toBe('brl');
			expect(update['phases[0][automatic_tax][enabled]']).toBe('true');
			expect(update['phases[0][automatic_tax][liability][type]']).toBe('self');
			expect(update['phases[0][invoice_settings][days_until_due]']).toBe('14');
			expect(update['phases[0][billing_cycle_anchor]']).toBe('automatic');
			expect(update['phases[0][end_date]']).toBe(String(subscriber.periodEnd));
			expect(update['phases[1][items][0][price]']).toBe(MOCK_PRICES.monthlyBrl);
			expect(update['phases[1][discounts][0][coupon]']).toBeUndefined();
		});

		test('refuses to move a subscriber up to a more expensive list price', async () => {
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_below_list',
				priceId: RETIRED_CHEAP_MONTHLY_BRL,
			});
			await expectRefusal(subscriber, 'not_a_price_decrease');
			expectNoStripeWrites();
		});

		test('refuses a subscriber who is cancelling at period end', async () => {
			const periodStart = Math.floor(Date.now() / 1000) - 3 * 24 * 60 * 60;
			const periodEnd = periodStart + 30 * 24 * 60 * 60;
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_cancel_at_period_end',
				priceId: RETIRED_MONTHLY_BRL,
				cancelAtPeriodEnd: true,
				periodStart,
				periodEnd,
			});
			await expectRefusal(subscriber, 'subscription_cancelling');
			expectNoStripeWrites();
			const me = await createBuilder<{
				premium_will_cancel: boolean;
			}>(harness, subscriber.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_will_cancel).toBe(true);
		});

		test('refuses a subscriber who is cancelling on an explicit cancel_at date', async () => {
			const periodStart = Math.floor(Date.now() / 1000) - 3 * 24 * 60 * 60;
			const periodEnd = periodStart + 30 * 24 * 60 * 60;
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_cancel_at',
				priceId: RETIRED_MONTHLY_BRL,
				cancelAt: periodEnd,
				cancelAtPeriodEnd: false,
				periodStart,
				periodEnd,
			});
			await expectRefusal(subscriber, 'subscription_cancelling');
			expectNoStripeWrites();
			const me = await createBuilder<{
				premium_will_cancel: boolean;
			}>(harness, subscriber.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_will_cancel).toBe(true);
		});

		test('is idempotent and reports an already scheduled switch on the second call', async () => {
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_idempotent',
				priceId: RETIRED_MONTHLY_BRL,
			});
			const first = await switchToListPrice(subscriber);
			expect(first.status).toBe('scheduled');
			const second = await switchToListPrice(subscriber);
			expect(second).toEqual({
				status: 'already_scheduled',
				effective_at: new Date(subscriber.periodEnd * 1000).toISOString(),
				target_price_id: MOCK_PRICES.monthlyBrl,
				target_amount_minor: LIST_MONTHLY_BRL_MINOR,
				current_amount_minor: RETIRED_MONTHLY_BRL_MINOR,
				currency: 'BRL',
			});
			expect(stripeHandlers.spies.createdSubscriptionSchedules).toHaveLength(1);
			expect(stripeHandlers.spies.updatedSubscriptionSchedules).toHaveLength(1);
		});

		test('refuses a retired price that costs exactly the same as the list price', async () => {
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_equal_price',
				priceId: RETIRED_EQUAL_MONTHLY_BRL,
			});
			await expectRefusal(subscriber, 'not_a_price_decrease');
			expectNoStripeWrites();
		});

		test('lets a scheduled switch be withdrawn again and restores eligibility', async () => {
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_withdrawn',
				priceId: RETIRED_MONTHLY_BRL,
			});
			expect((await switchToListPrice(subscriber)).status).toBe('scheduled');
			await createBuilder(harness, subscriber.token)
				.post('/premium/cancel-pending-subscription-change')
				.expect(204)
				.execute();
			expect(stripeHandlers.spies.releasedSubscriptionSchedules).toHaveLength(1);
			expect(stripeHandlers.spies.releasedSubscriptionSchedules[0]?.params.preserve_cancel_date).toBe('false');
			const state = await getPremiumState(subscriber);
			expect(state.billing.pending_subscription_change).toBeNull();
			expect(state.billing.list_price_switch).toEqual(
				expect.objectContaining({
					available: true,
					reason: null,
					pending: false,
					current_price_id: RETIRED_MONTHLY_BRL,
					current_amount_minor: RETIRED_MONTHLY_BRL_MINOR,
					list_price_id: MOCK_PRICES.monthlyBrl,
					list_amount_minor: LIST_MONTHLY_BRL_MINOR,
				}),
			);
		});

		test('refuses a subscriber who is already on the current list price', async () => {
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_on_list_price',
				priceId: MOCK_PRICES.monthlyBrl,
			});
			await expectRefusal(subscriber, 'already_on_list_price');
			expectNoStripeWrites();
		});

		test('refuses a currency that has no configured list price', async () => {
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_try_unconfigured',
				priceId: RETIRED_MONTHLY_TRY,
			});
			await expectRefusal(subscriber, 'no_list_price');
			expectNoStripeWrites();
		});

		test('refuses a subscription that is not chargeable', async () => {
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_past_due',
				priceId: RETIRED_MONTHLY_BRL,
				status: 'past_due',
			});
			await expectRefusal(subscriber, 'subscription_not_chargeable');
			expectNoStripeWrites();
		});

		test('refuses a subscriber whose cancellation is managed by a subscription schedule', async () => {
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_schedule_cancel',
				priceId: RETIRED_MONTHLY_BRL,
			});
			await createBuilder(harness, subscriber.token)
				.post('/premium/change-subscription')
				.body({billing_cycle: 'yearly', effective_at: 'period_end'})
				.expect(204)
				.execute();
			await createBuilder(harness, subscriber.token).post('/premium/cancel-subscription').expect(204).execute();
			stripeHandlers.spies.updatedSubscriptions.length = 0;
			stripeHandlers.spies.createdSubscriptionSchedules.length = 0;
			stripeHandlers.spies.updatedSubscriptionSchedules.length = 0;
			stripeHandlers.spies.releasedSubscriptionSchedules.length = 0;
			const result = await switchToListPrice(subscriber);
			expect(result).toEqual({status: 'ineligible', reason: 'subscription_cancelling'});
			expectNoStripeWrites();
			const me = await createBuilder<{
				premium_will_cancel: boolean;
			}>(harness, subscriber.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_will_cancel).toBe(true);
		});

		test('refuses when a billing cycle change is already pending', async () => {
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_pending_cycle_change',
				priceId: RETIRED_MONTHLY_BRL,
			});
			await createBuilder(harness, subscriber.token)
				.post('/premium/change-subscription')
				.body({billing_cycle: 'yearly', effective_at: 'period_end'})
				.expect(204)
				.execute();
			stripeHandlers.spies.updatedSubscriptions.length = 0;
			stripeHandlers.spies.createdSubscriptionSchedules.length = 0;
			stripeHandlers.spies.updatedSubscriptionSchedules.length = 0;
			await expectRefusal(subscriber, 'conflicting_pending_change');
			expectNoStripeWrites();
		});

		test('rejects a user without an active subscription and mirrors that in premium state', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post('/premium/switch-to-list-price')
				.expect(400, APIErrorCodes.STRIPE_NO_ACTIVE_SUBSCRIPTION)
				.execute();
			const state = await createBuilder<PremiumStateResponse>(harness, account.token)
				.get('/premium/state')
				.expect(200)
				.execute();
			expect(state.billing.list_price_switch.available).toBe(false);
			expect(state.billing.list_price_switch.reason).toBe('no_active_subscription');
			expectNoStripeWrites();
		});
	});

	describe('POST /premium/change-subscription (unchanged behaviour)', () => {
		test('still clears a pending cancellation and re-anchors billing when the cycle actually changes', async () => {
			const periodStart = Math.floor(Date.now() / 1000) - 3 * 24 * 60 * 60;
			const periodEnd = periodStart + 30 * 24 * 60 * 60;
			const subscriber = await createSubscriber({
				subscriptionId: 'sub_brl_cycle_change',
				priceId: RETIRED_MONTHLY_BRL,
				cancelAt: periodEnd,
				cancelAtPeriodEnd: false,
				periodStart,
				periodEnd,
			});
			await createBuilder(harness, subscriber.token)
				.post('/premium/change-subscription')
				.body({billing_cycle: 'yearly', effective_at: 'period_end'})
				.expect(204)
				.execute();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
			const clearCancelUpdate = stripeHandlers.spies.updatedSubscriptions[0];
			expect(clearCancelUpdate?.id).toBe('sub_brl_cycle_change');
			expect(clearCancelUpdate?.params.cancel_at).toBe('');
			expect(clearCancelUpdate?.params.proration_behavior).toBe('none');
			expect(clearCancelUpdate?.params.items).toBeUndefined();
			expect(stripeHandlers.spies.updatedSubscriptionSchedules).toHaveLength(1);
			const scheduleUpdate = stripeHandlers.spies.updatedSubscriptionSchedules[0];
			expect(scheduleUpdate?.params.end_behavior).toBe('release');
			expect(scheduleUpdate?.params.phases?.[1]?.start_date).toBe(String(periodEnd));
			expect(scheduleUpdate?.params.phases?.[1]?.billing_cycle_anchor).toBe('phase_start');
			expect(scheduleUpdate?.params.phases?.[1]?.items?.[0]?.price).toBe(MOCK_PRICES.yearlyBrl);
			const me = await createBuilder<{
				premium_will_cancel: boolean;
			}>(harness, subscriber.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_will_cancel).toBe(false);
		});
	});
});
