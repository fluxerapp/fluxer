// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {OperatorPassCheckoutService} from '@app/api/operator_pass/OperatorPassCheckoutService';
import {OperatorPassEntitlementService} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {
	isOperatorPassBusinessCheckout,
	recordOperatorPassDeliveryOnPayment,
	resolveOperatorPassPaidAt,
	resolveOperatorPassPurchaseWithdrawalRule,
} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {
	resolveOperatorPassDisabledCurrencies,
	resolveOperatorPassWithdrawalRule,
} from '@app/api/operator_pass/OperatorPassPrices';
import {
	getOperatorPassWithdrawalDeadline,
	getOperatorPassWithdrawalEndsAt,
	getOperatorPassWithdrawalEndsOn,
} from '@app/api/operator_pass/OperatorPassPurchaseRules';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {OperatorPassAgeConfirmationRequiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassAgeConfirmationRequiredError';
import {OperatorPassCertificateNameRejectedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassCertificateNameRejectedError';
import {OperatorPassDeliveryConsentRequiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassDeliveryConsentRequiredError';
import {OperatorPassEmailUndeliverableError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassEmailUndeliverableError';
import {
	OPERATOR_PASS_CERTIFICATE_NAME_MAX_LENGTH,
	OPERATOR_PASS_DELIVERY_BUSINESS_VERSION,
	OPERATOR_PASS_DELIVERY_COUNTRY_VERSION,
	OPERATOR_PASS_DELIVERY_WAIVER_VERSION,
	OperatorPassCertificateNameType,
	OperatorPassCheckoutRequest,
	type OperatorPassCurrency,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type Stripe from 'stripe';
import {afterAll, beforeAll, describe, expect, test, vi} from 'vitest';

const DAY_MS = 24 * 60 * 60 * 1000;

function boxTextOf(params: Stripe.Checkout.SessionCreateParams): string | undefined {
	const acceptance = params.custom_text?.terms_of_service_acceptance;
	return acceptance ? acceptance.message : undefined;
}

describe('Operator Pass checkout session', () => {
	let harness: ApiTestHarness;
	let priceRisesAt: Date | null;
	let disabledCurrencies: ReadonlySet<OperatorPassCurrency>;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		priceRisesAt = Config.operatorPass.priceRisesAt;
		disabledCurrencies = Config.operatorPass.disabledCurrencies;
		Config.operatorPass.priceRisesAt = new Date(Date.now() + 30 * DAY_MS);
		Config.operatorPass.disabledCurrencies = new Set();
	});

	afterAll(async () => {
		Config.operatorPass.priceRisesAt = priceRisesAt;
		Config.operatorPass.disabledCurrencies = disabledCurrencies;
		await harness.shutdown();
	});

	async function setUp(flags: {email_bounced?: boolean} = {}) {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: true, ...flags})
			.execute();
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		const create = vi.fn(async (params: Stripe.Checkout.SessionCreateParams) => ({
			id: `cs_test_${account.userId}_${create.mock.calls.length}`,
			url: 'https://checkout.stripe.test/session',
			params,
		}));
		const stripe = {checkout: {sessions: {create}}} as unknown as Stripe;
		const repository = new OperatorPassRepository();
		const service = new OperatorPassCheckoutService(stripe, repository, new OperatorPassEntitlementService(repository));
		const checkout = (request: Partial<OperatorPassCheckoutRequest>, geoipCountryCode: string) =>
			service.createCheckoutSession({
				user,
				request: {
					quantity: 1,
					certificate_names: ['Hampus Lindqvist'],
					wall_opt_out: true,
					age_confirmed: true,
					...request,
				} as OperatorPassCheckoutRequest,
				geoipCountryCode,
				clientIp: '203.0.113.7',
				userAgent: 'vitest',
			});
		return {create, repository, checkout};
	}

	test('a visitor from Brazil gets the price in reais and the 7-day withdrawal rule', async () => {
		const repository = new OperatorPassRepository();
		const service = new OperatorPassCheckoutService(null, repository, new OperatorPassEntitlementService(repository));
		const state = await service.getState({user: null, countryCode: 'BR'});
		expect(state).toMatchObject({currency: 'BRL', withdrawal_rule: {country: 'BR', days: 7}});
	});

	test('sells only the pass in payment mode and records the waiver box', async () => {
		const {create, repository, checkout} = await setUp();
		await expect(checkout({}, 'SE')).rejects.toBeInstanceOf(OperatorPassDeliveryConsentRequiredError);
		await expect(checkout({delivery_consent: true, age_confirmed: undefined}, 'SE')).rejects.toBeInstanceOf(
			OperatorPassAgeConfirmationRequiredError,
		);
		expect(create).not.toHaveBeenCalled();

		const created = await checkout({delivery_consent: true}, 'SE');
		const params = create.mock.calls[0]![0];
		expect(created).toMatchObject({
			url: 'https://checkout.stripe.test/session',
			sessionId: expect.stringMatching(/^cs_test_/),
		});
		expect(created.expiresAt.getTime()).toBe((params.expires_at ?? 0) * 1000);
		expect(await repository.findPurchase(created.sessionId)).toMatchObject({status: 'pending'});
		expect(params.mode).toBe('payment');
		expect(params.line_items).toHaveLength(1);
		expect(params.line_items?.[0]?.price_data?.recurring).toBeUndefined();
		expect(params).not.toHaveProperty('subscription_data');
		expect(params).not.toHaveProperty('invoice_creation');
		expect(params.metadata?.operator_pass_waiver_consent_version).toBe(OPERATOR_PASS_DELIVERY_WAIVER_VERSION);
		expect(params.payment_method_options).toBeUndefined();
		expect(JSON.stringify(params)).not.toContain('Hampus Lindqvist');
		const boxText = boxTextOf(params);
		expect(boxText).toContain('lose my right to withdraw');

		const purchase = await repository.findPurchase(`cs_test_${params.client_reference_id}_1`);
		expect(params.metadata?.operator_pass_age_confirmed).toBe('true');
		expect(purchase?.age_confirmed_at).toBeInstanceOf(Date);
		expect(purchase).toMatchObject({
			waiver_consent_text: boxText,
			waiver_consent_version: OPERATOR_PASS_DELIVERY_WAIVER_VERSION,
			withdrawal_days: null,
			withdrawal_country: null,
		});
	});

	test('says which certificate name it cannot print', async () => {
		const {create, checkout} = await setUp();
		const attempt = checkout(
			{
				quantity: 3,
				certificate_names: ['李小龍', 'محمد بن سلمان', 'Ada 🚀'],
				delivery_consent: true,
			},
			'SE',
		);
		await expect(attempt).rejects.toBeInstanceOf(OperatorPassCertificateNameRejectedError);
		await expect(attempt).rejects.toMatchObject({data: {index: 2}});
		expect(create).not.toHaveBeenCalled();
	});

	test('an empty list of certificate names prints the account name on every page, as the API describes', async () => {
		const {create, repository, checkout} = await setUp();
		expect(OperatorPassCheckoutRequest.shape.certificate_names.safeParse([]).success).toBe(true);
		await checkout({quantity: 2, certificate_names: [], delivery_consent: true}, 'SE');
		const params = create.mock.calls[0]![0];
		const purchase = await repository.findPurchase(`cs_test_${params.client_reference_id}_1`);
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(params.client_reference_id!)));
		const accountName = user.globalName ?? user.username;
		expect(purchase?.certificate_names).toEqual([accountName, accountName]);
	});

	test('refuses a checkout when the confirmation email cannot reach the buyer', async () => {
		const {create, checkout} = await setUp({email_bounced: true});
		await expect(checkout({delivery_consent: true}, 'SE')).rejects.toBeInstanceOf(OperatorPassEmailUndeliverableError);
		expect(create).not.toHaveBeenCalled();
	});

	test('asks for a billing address only when the pass is bought for a business', async () => {
		const {create, checkout} = await setUp();
		await checkout({delivery_consent: true}, 'SE');
		await checkout({delivery_consent: true, is_business: true}, 'SE');
		expect(create.mock.calls[0]![0].billing_address_collection).toBe('auto');
		expect(create.mock.calls[1]![0].billing_address_collection).toBe('required');
		expect(create.mock.calls[0]![0].tax_id_collection).toEqual({enabled: true});
	});

	test('turns adaptive pricing off so the buyer pays the price on the pass page', async () => {
		const {create, checkout} = await setUp();
		await checkout({delivery_consent: true}, 'US');
		expect(create.mock.calls[0]![0].adaptive_pricing).toEqual({enabled: false});
	});

	test('records a Brazilian company purchase with a CNPJ without the 7 days', async () => {
		const {create, repository, checkout} = await setUp();
		await checkout({delivery_consent: true, is_business: true, cnpj: '11444777000161'}, 'BR');
		const params = create.mock.calls[0]![0];
		expect(params.metadata?.operator_pass_waiver_consent_version).toBe(OPERATOR_PASS_DELIVERY_BUSINESS_VERSION);
		expect(params.metadata?.operator_pass_cnpj).toBe('11444777000161');
		expect(params.metadata?.operator_pass_withdrawal_country).toBeUndefined();
		const boxText = boxTextOf(params);
		expect(boxText).toContain('CNPJ');
		expect(boxText).not.toContain('7 days');
		const purchase = await repository.findPurchase(`cs_test_${params.client_reference_id}_1`);
		expect(purchase).toMatchObject({withdrawal_country: null, withdrawal_days: null, waiver_consent_text: boxText});
	});

	test('shows a buyer checking out from Brazil the 7 days and no waiver', async () => {
		const {create, repository, checkout} = await setUp();
		await checkout({delivery_consent: true}, 'BR');
		const params = create.mock.calls[0]![0];
		expect(params.mode).toBe('payment');
		expect(params.metadata?.operator_pass_waiver_consent_version).toBe(OPERATOR_PASS_DELIVERY_COUNTRY_VERSION);
		expect(params.metadata?.operator_pass_currency).toBe('BRL');
		expect(params.payment_method_options).toEqual({pix: {amount_includes_iof: 'always'}});
		const boxText = boxTextOf(params);
		expect(boxText).not.toContain('lose my right');
		expect(boxText).toContain('billing address in Brazil or pay in reais');
		expect(boxText).toContain('7 days');
		const purchase = await repository.findPurchase(`cs_test_${params.client_reference_id}_1`);
		expect(purchase).toMatchObject({withdrawal_country: 'BR', withdrawal_days: 7});
	});
});

test('dates the purchase from the charge, not from when fulfilment ran', () => {
	const paidAt = new Date('2026-12-31T23:58:00Z');
	const fulfilledAt = new Date('2027-01-01T00:20:00Z');
	const charge = {created: Math.floor(paidAt.getTime() / 1000)} as Stripe.Charge;
	expect(resolveOperatorPassPaidAt(charge, fulfilledAt)).toEqual(paidAt);
	expect(resolveOperatorPassPaidAt(null, fulfilledAt)).toEqual(fulfilledAt);
});

test('fails the delivery so the webhook retries until the payment carries the delivery time', async () => {
	const metadata: Record<string, string> = {};
	let failuresLeft = 3;
	const update = vi.fn(async (_id: string, params: Stripe.PaymentIntentUpdateParams) => {
		if (failuresLeft-- > 0) {
			throw new Error('Stripe is down');
		}
		Object.assign(metadata, params.metadata);
		return {} as Stripe.PaymentIntent;
	});
	const stripe = {paymentIntents: {update}} as unknown as Stripe;
	const paidAt = new Date('2026-11-01T09:59:00Z');
	const purchase = {
		payment_intent_id: 'pi_test',
		checkout_session_id: 'cs_test',
		purchased_at: paidAt,
		withdrawal_country: null,
	};
	const deliveredAt = new Date('2026-11-01T10:00:00Z');
	await expect(recordOperatorPassDeliveryOnPayment(stripe, purchase, deliveredAt)).rejects.toThrow();
	expect(metadata.operator_pass_delivered_at).toBeUndefined();
	await recordOperatorPassDeliveryOnPayment(stripe, purchase, deliveredAt);
	expect(metadata.operator_pass_delivered_at).toBe(deliveredAt.toISOString());
	expect(metadata.operator_pass_paid_at).toBe(paidAt.toISOString());
	expect(metadata.operator_pass_withdrawal_country).toBe('');
});

test('records the withdrawal country decided at fulfilment on the payment', async () => {
	const metadata: Record<string, string> = {operator_pass_currency: 'USD'};
	const update = vi.fn(async (_id: string, params: Stripe.PaymentIntentUpdateParams) => {
		Object.assign(metadata, params.metadata);
		return {} as Stripe.PaymentIntent;
	});
	const stripe = {paymentIntents: {update}} as unknown as Stripe;
	const rule = resolveOperatorPassWithdrawalRule(Config.operatorPass.withdrawalCountries, [null, 'BR'], 'USD');
	expect(rule?.country).toBe('BR');
	const purchase = {
		payment_intent_id: 'pi_test_br',
		checkout_session_id: 'cs_test_br',
		purchased_at: new Date('2026-11-01T09:59:00Z'),
		withdrawal_country: rule?.country ?? null,
	};
	await recordOperatorPassDeliveryOnPayment(stripe, purchase, new Date('2026-11-01T10:00:00Z'));
	expect(metadata.operator_pass_withdrawal_country).toBe('BR');
	await recordOperatorPassDeliveryOnPayment(
		stripe,
		{...purchase, withdrawal_country: null},
		new Date('2026-11-01T10:00:00Z'),
	);
	expect(metadata.operator_pass_withdrawal_country).toBe('');
});

test('treats a checkout with a company name or tax ID as a business purchase', () => {
	const session = (customer_details: Partial<Stripe.Checkout.Session.CustomerDetails>) =>
		({customer_details}) as Stripe.Checkout.Session;
	expect(isOperatorPassBusinessCheckout(session({business_name: null, tax_ids: []}))).toBe(false);
	expect(isOperatorPassBusinessCheckout(session({business_name: 'Acme Ltda', tax_ids: []}))).toBe(true);
	expect(isOperatorPassBusinessCheckout(session({business_name: null, tax_ids: [{type: 'eu_vat', value: '1'}]}))).toBe(
		true,
	);
});

test('gives a Brazilian purchase the 7 days unless it was bought for a company with its CNPJ', () => {
	const session = (
		customer_details: Partial<Stripe.Checkout.Session.CustomerDetails>,
		metadata: Record<string, string> = {},
	) => ({customer_details: {address: {country: 'BR'}, ...customer_details}, metadata}) as Stripe.Checkout.Session;
	const purchase = {withdrawal_country: null, currency: 'BRL'};
	expect(
		resolveOperatorPassPurchaseWithdrawalRule(session({business_name: null, tax_ids: []}), purchase)?.country,
	).toBe('BR');
	expect(
		resolveOperatorPassPurchaseWithdrawalRule(session({business_name: 'Acme Ltda', tax_ids: []}), purchase)?.country,
	).toBe('BR');
	expect(
		resolveOperatorPassPurchaseWithdrawalRule(
			session({business_name: 'Acme Ltda', tax_ids: []}, {operator_pass_cnpj: '11444777000161'}),
			{withdrawal_country: 'BR', currency: 'BRL'},
		),
	).toBeNull();
});

test('ends the Brazilian 7 days at the end of the seventh calendar day after the purchase day', () => {
	for (const [paidAt, endsOn] of [
		['2026-11-02T03:30:00Z', '2026-11-09'],
		['2026-11-03T02:30:00Z', '2026-11-10'],
		['2026-11-02T14:00:00Z', '2026-11-09'],
	] as const) {
		const endsAt = getOperatorPassWithdrawalEndsAt(new Date(paidAt), 7);
		expect(getOperatorPassWithdrawalEndsOn(endsAt), paidAt).toBe(endsOn);
		const deadline = getOperatorPassWithdrawalDeadline({
			withdrawal_days: 7,
			withdrawal_ends_at: endsAt,
		} as OperatorPassPurchaseRow);
		expect(deadline?.getTime(), paidAt).toBeGreaterThanOrEqual(new Date(`${endsOn}T23:59:59-03:00`).getTime());
	}
});

test('sells in reais unless the real price is turned off', () => {
	expect(resolveOperatorPassDisabledCurrencies(undefined).has('BRL')).toBe(false);
	expect(resolveOperatorPassDisabledCurrencies({brl: false}).has('BRL')).toBe(true);
});

test('a decomposed certificate name passes the length limit just like the same name composed', () => {
	const composed = 'José María Álvarez Núñez García Fernández López';
	const decomposed = composed.normalize('NFD');
	expect(composed).toHaveLength(47);
	expect(decomposed.length).toBeGreaterThan(OPERATOR_PASS_CERTIFICATE_NAME_MAX_LENGTH);
	expect(OperatorPassCertificateNameType.parse(decomposed)).toBe(composed);
	expect(OperatorPassCertificateNameType.parse(composed)).toBe(composed);
});
