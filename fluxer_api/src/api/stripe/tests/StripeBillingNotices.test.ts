// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {addCalendarMonths, type BillingNotice} from '@app/api/stripe/services/BillingNoticeSchedule';
import {
	BILLING_EMAIL_PRICES,
	billingEmailTestContext,
	cardPaymentMethod,
	DAY_SECONDS,
	invoicePreview,
	namedPaymentMethod,
	plutoniumSubscription,
	type SubscriptionPatch,
	subscriptionDiscount,
	unixNow,
} from '@app/api/stripe/tests/BillingEmailTestUtils';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import scanBillingNotices from '@app/api/worker/tasks/ScanBillingNotices';
import sendBillingEmail from '@app/api/worker/tasks/SendBillingEmail';
import {describe, expect, test} from 'vitest';

const DONOR = 'donor@example.com';

function cardExpiringBefore(chargeAt: number) {
	const charge = new Date(chargeAt * 1000);
	const january = charge.getUTCMonth() === 0;
	return {
		expMonth: january ? 12 : charge.getUTCMonth(),
		expYear: january ? charge.getUTCFullYear() - 1 : charge.getUTCFullYear(),
	};
}

describe('billing notices', () => {
	const context = billingEmailTestContext();
	const {createSubscriber, emailsOfType} = context;

	async function subscriber(customerId: string, patch: SubscriptionPatch = {}) {
		const account = await createSubscriber(customerId, {timezone: 'Europe/Stockholm'});
		context.stripe.put.subscription(`sub_${customerId}`, plutoniumSubscription(customerId, patch));
		return account;
	}

	function monthly(daysAway: number, patch: SubscriptionPatch = {}): SubscriptionPatch {
		return {
			price_id: BILLING_EMAIL_PRICES.monthlyUsd,
			unit_amount: 499,
			interval: 'month',
			current_period_end: unixNow() + daysAway * DAY_SECONDS,
			...patch,
		};
	}

	async function scan() {
		await scanBillingNotices({}, createTestWorkerHelpers());
	}

	async function deliver(customerId: string, notice: BillingNotice, chargeAt: number) {
		await sendBillingEmail(
			{kind: 'subscription_notice', id: `sub_${customerId}`, notice, chargeAt},
			createTestWorkerHelpers(),
		);
	}

	async function renewalNotice(email: string) {
		const notices = await emailsOfType(email, 'plutonium_renewal_notice');
		expect(notices).toHaveLength(1);
		return notices[0]!.metadata;
	}

	describe('renewal', () => {
		test('sends one notice, and none on the next scan', async () => {
			const account = await subscriber('cus_yearly');
			await scan();
			await scan();
			expect((await renewalNotice(account.email)).variant).toBe('renewal');
		});

		test('sends nothing when the plan is canceled before the notice goes out', async () => {
			const chargeAt = unixNow() + 33 * DAY_SECONDS;
			const account = await subscriber('cus_canceled', {current_period_end: chargeAt, cancel_at_period_end: true});
			await deliver('cus_canceled', 'renewal', chargeAt);
			expect(await emailsOfType(account.email, 'plutonium_renewal_notice')).toHaveLength(0);
		});

		test('quotes the upcoming invoice and the last four digits of the card', async () => {
			const chargeAt = unixNow() + 33 * DAY_SECONDS;
			const account = await subscriber('cus_preview', {
				current_period_end: chargeAt,
				default_payment_method: 'pm_1881',
			});
			context.stripe.put.paymentMethod(cardPaymentMethod('pm_1881', {last4: '1881'}));
			context.stripe.put.invoicePreview(
				'sub_cus_preview',
				invoicePreview({chargeAt, amountDue: 4999, tax: {amount: 833, behavior: 'inclusive'}}),
			);
			await scan();
			expect(await renewalNotice(account.email)).toMatchObject({
				next_total_minor: '4999',
				next_tax_minor: '833',
				tax_included: 'true',
				card_last4: '1881',
			});
		});

		test('quotes the regular price when the upcoming invoice is not available', async () => {
			const account = await subscriber('cus_no_preview');
			context.stripe.put.invoicePreview('sub_cus_no_preview', 'error');
			await scan();
			expect(await renewalNotice(account.email)).toMatchObject({
				next_total_minor: '',
				regular_total_minor: '4999',
				tax_included: 'false',
			});
		});

		test('shows both amounts when the next charge differs from the regular price', async () => {
			const chargeAt = unixNow() + 33 * DAY_SECONDS;
			const account = await subscriber('cus_discount', {current_period_end: chargeAt});
			context.stripe.put.invoicePreview(
				'sub_cus_discount',
				invoicePreview({chargeAt, amountDue: 2499, unitAmount: 4999}),
			);
			await scan();
			expect(await renewalNotice(account.email)).toMatchObject({
				next_total_minor: '2499',
				regular_total_minor: '4999',
				regular_differs: 'true',
			});
		});

		test('quotes the discounted price after the next charge while a coupon lasts forever', async () => {
			const chargeAt = unixNow() + 33 * DAY_SECONDS;
			const account = await subscriber('cus_forever', {
				current_period_end: chargeAt,
				discounts: [subscriptionDiscount('di_forever', {duration: 'forever'})],
			});
			context.stripe.put.invoicePreview(
				'sub_cus_forever',
				invoicePreview({chargeAt, amountDue: 2499, unitAmount: 4999, discount: {id: 'di_forever', amount: 2500}}),
			);
			await scan();
			expect(await renewalNotice(account.email)).toMatchObject({regular_total_minor: '2499', regular_differs: 'false'});
		});

		test('quotes the full price after the next charge when the coupon ends before the following one', async () => {
			const chargeAt = unixNow() + 33 * DAY_SECONDS;
			const account = await subscriber('cus_repeating', {
				current_period_end: chargeAt,
				discounts: [subscriptionDiscount('di_repeating', {duration: 'repeating'}, chargeAt + 30 * DAY_SECONDS)],
			});
			context.stripe.put.invoicePreview(
				'sub_cus_repeating',
				invoicePreview({chargeAt, amountDue: 2499, unitAmount: 4999, discount: {id: 'di_repeating', amount: 2500}}),
			);
			await scan();
			expect(await renewalNotice(account.email)).toMatchObject({regular_total_minor: '4999', regular_differs: 'true'});
		});

		test('names a payment method that has no card', async () => {
			const account = await subscriber('cus_pix', {default_payment_method: 'pm_pix'});
			context.stripe.put.paymentMethod(namedPaymentMethod('pm_pix', 'pix'));
			await scan();
			expect(await renewalNotice(account.email)).toMatchObject({payment_method: 'named', payment_method_name: 'Pix'});
		});
	});

	describe('yearly reminder for monthly plans', () => {
		function anniversaryAfterNextCharge(): SubscriptionPatch {
			const chargeAt = unixNow() + 3 * DAY_SECONDS;
			return monthly(3, {start_date: addCalendarMonths(addCalendarMonths(chargeAt, 1), -12)});
		}

		test('quotes the next monthly charge when the anniversary is the charge after it', async () => {
			const patch = anniversaryAfterNextCharge();
			const account = await subscriber('cus_annual', patch);
			await scan();
			expect(await renewalNotice(account.email)).toMatchObject({
				variant: 'annual',
				charge_at: new Date(patch.current_period_end! * 1000).toISOString(),
				year_starts_at: new Date(addCalendarMonths(patch.current_period_end!, 1) * 1000).toISOString(),
				next_total_minor: '499',
			});
		});

		test('sends one reminder per year even when the charge date moves', async () => {
			const patch = anniversaryAfterNextCharge();
			const account = await subscriber('cus_annual_once', patch);
			await scan();
			context.stripe.put.subscription('sub_cus_annual_once', {
				current_period_end: addCalendarMonths(patch.current_period_end!, 1) + DAY_SECONDS,
			});
			await scan();
			expect(await emailsOfType(account.email, 'plutonium_renewal_notice')).toHaveLength(1);
		});
	});

	describe('gift trials', () => {
		test('a yearly plan in a gift trial gets the renewal notice with the gift end date', async () => {
			const trialEnd = unixNow() + 33 * DAY_SECONDS;
			const account = await subscriber('cus_yearly_gift', {trial_end: trialEnd, current_period_end: trialEnd});
			await scan();
			expect(await renewalNotice(account.email)).toMatchObject({
				variant: 'renewal',
				gift_ends_at: new Date(trialEnd * 1000).toISOString(),
			});
		});

		test('a monthly plan in a gift trial gets the gift notice', async () => {
			const trialEnd = unixNow() + 8 * DAY_SECONDS - 60;
			const account = await subscriber(
				'cus_monthly_gift',
				monthly(8, {trial_end: trialEnd, current_period_end: trialEnd}),
			);
			await scan();
			expect(await renewalNotice(account.email)).toMatchObject({variant: 'gift', plan: 'monthly'});
		});

		test('a yearly plan in a gift trial gets the gift notice 8 days before it ends', async () => {
			const trialEnd = unixNow() + 8 * DAY_SECONDS - 60;
			const account = await subscriber('cus_yearly_gift_late', {trial_end: trialEnd, current_period_end: trialEnd});
			await scan();
			expect(await renewalNotice(account.email)).toMatchObject({variant: 'gift', plan: 'yearly'});
		});

		test('sends no gift notice after the gift confirmation gave the same date', async () => {
			const trialEnd = unixNow() + 7 * DAY_SECONDS;
			const account = await subscriber(
				'cus_gift_confirmed',
				monthly(7, {trial_end: trialEnd, current_period_end: trialEnd}),
			);
			await sendBillingEmail({kind: 'gift_applied', id: 'sub_cus_gift_confirmed', trialEnd}, createTestWorkerHelpers());
			await scan();
			expect(await emailsOfType(account.email, 'plutonium_gift_applied')).toHaveLength(1);
			expect(await emailsOfType(account.email, 'plutonium_renewal_notice')).toHaveLength(0);
		});
	});

	describe('expiring cards', () => {
		const chargeAt = unixNow() + 10 * DAY_SECONDS;

		async function subscriberWithCard(customerId: string, card: Parameters<typeof cardPaymentMethod>[1]) {
			const account = await subscriber(customerId, monthly(10, {default_payment_method: `pm_${customerId}`}));
			context.stripe.put.paymentMethod(cardPaymentMethod(`pm_${customerId}`, card));
			return account;
		}

		test('warns about a card that expires before the next charge', async () => {
			const account = await subscriberWithCard('cus_expiring', cardExpiringBefore(chargeAt));
			await scan();
			const [notice] = await emailsOfType(account.email, 'plutonium_card_expiring');
			expect(notice!.metadata.card_last4).toBe('4242');
		});

		test('sends nothing once the card is replaced', async () => {
			const account = await subscriberWithCard('cus_replaced', cardExpiringBefore(chargeAt));
			context.stripe.put.subscription('sub_cus_replaced', {default_payment_method: 'pm_test_1'});
			await deliver('cus_replaced', 'card_expiring', chargeAt);
			expect(await emailsOfType(account.email, 'plutonium_card_expiring')).toHaveLength(0);
		});

		test('sends nothing for a card in a wallet', async () => {
			const account = await subscriberWithCard('cus_wallet', {...cardExpiringBefore(chargeAt), wallet: 'apple_pay'});
			await scan();
			expect(await emailsOfType(account.email, 'plutonium_card_expiring')).toHaveLength(0);
		});

		test('sends nothing for a plan paid from a saved source', async () => {
			const account = await subscriber(
				'cus_source',
				monthly(10, {default_payment_method: null, default_source: 'card_legacy'}),
			);
			context.stripe.put.paymentMethod(cardPaymentMethod('pm_test_1', cardExpiringBefore(chargeAt)));
			await scan();
			expect(await emailsOfType(account.email, 'plutonium_card_expiring')).toHaveLength(0);
		});
	});

	test('a donor gets the donation notice with a link to manage the donation', async () => {
		context.stripe.put.subscription(
			'sub_donor',
			plutoniumSubscription('cus_donor', {
				price_id: 'price_donation_yearly',
				metadata: {is_donation: 'true', donation_email: DONOR},
			}),
		);
		await scan();
		const [notice] = await emailsOfType(DONOR, 'donation_renewal_notice');
		expect(notice!.metadata.manage_url).toContain('/donate/manage?email=donor%40example.com');
	});

	test('sends nothing when billing emails are turned off', async () => {
		const account = await subscriber('cus_self_hosted');
		Config.instance.selfHosted = true;
		try {
			await scan();
		} finally {
			Config.instance.selfHosted = false;
		}
		expect(await emailsOfType(account.email, 'plutonium_renewal_notice')).toHaveLength(0);
	});
});
