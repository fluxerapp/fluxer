// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {
	getEffectiveBillingConfig,
	getStoredBillingConfig,
	isBillingActive,
	type StoredBillingConfig,
	setStoredBillingConfig,
} from '@app/api/stripe/BillingConfigCache';
import {getProductRegistry, ProductRegistry, ProductType} from '@app/api/stripe/ProductRegistry';
import {getStripeClient} from '@app/api/stripe/StripeClient';
import {setupSyncStripeWebhookWorker} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {
	createMockWebhookPayload,
	createStripeApiHandlers,
	type StripeApiHandlers,
} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {getCurrencyPreferences, getGiftCurrencyPreferences, isLocalizedCurrency} from '@app/api/utils/CurrencyUtils';
import processStripeWebhook from '@app/api/worker/tasks/ProcessStripeWebhook';
import {setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {PremiumStateResponse, PriceIdsResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const ENV_CURRENCIES = ['USD', 'EUR', 'BRL', 'DKK', 'INR', 'NOK', 'PLN', 'SEK', 'TRY'] as const;

function buildFullEnvPrices(): NonNullable<typeof Config.stripe.prices> {
	const prices: Record<string, string> = {};
	for (const currency of ENV_CURRENCIES) {
		const suffix = currency.charAt(0) + currency.slice(1).toLowerCase();
		const lower = currency.toLowerCase();
		prices[`monthly${suffix}`] = `price_envmonthly${lower}`;
		prices[`yearly${suffix}`] = `price_envyearly${lower}`;
		prices[`gift1Month${suffix}`] = `price_envgiftmonth${lower}`;
		prices[`gift1Year${suffix}`] = `price_envgiftyear${lower}`;
	}
	return prices as NonNullable<typeof Config.stripe.prices>;
}

const OPERATOR_PRICES = {
	GBP: {
		monthly: 'price_opgbpmonthly',
		yearly: 'price_opgbpyearly',
		gift_1_month: 'price_opgbpgiftmonth',
		gift_1_year: 'price_opgbpgiftyear',
	},
	SEK: {
		monthly: 'price_opsekmonthly',
		yearly: 'price_opsekyearly',
		gift_1_month: null,
		gift_1_year: null,
	},
	CHF: {
		monthly: 'price_opchfmonthly',
		yearly: 'price_opchfyearly',
		gift_1_month: 'price_opchfgiftmonth',
		gift_1_year: 'price_opchfgiftyear',
	},
};

const STORED_WEBHOOK_SECRET = 'whsec_operator_stored';
const ENV_WEBHOOK_SECRET = 'whsec_env_only';

function operatorBilling(overrides: Partial<StoredBillingConfig> = {}): StoredBillingConfig {
	return {
		enabled: true,
		stripe_secret_key: 'sk_test_operator_a',
		stripe_webhook_secret: STORED_WEBHOOK_SECRET,
		automatic_tax: null,
		tax_id_collection: null,
		terms_consent_required: null,
		default_currency: 'GBP',
		prices: OPERATOR_PRICES,
		country_currencies: {SE: 'SEK', CH: 'CHF'},
		legacy_prices: {monthly_GBP: ['price_opgbplegacymonthly']},
		...overrides,
	};
}

interface GlobalState {
	prices: typeof Config.stripe.prices;
	legacyPrices: typeof Config.stripe.legacyPrices;
	webhookSecret: typeof Config.stripe.webhookSecret;
	selfHosted: boolean;
	premiumMode: ReturnType<typeof getCachedInstancePremiumMode>;
	storedBilling: StoredBillingConfig | null;
}

function captureGlobalState(): GlobalState {
	return {
		prices: Config.stripe.prices,
		legacyPrices: Config.stripe.legacyPrices,
		webhookSecret: Config.stripe.webhookSecret,
		selfHosted: Config.instance.selfHosted,
		premiumMode: getCachedInstancePremiumMode(),
		storedBilling: getStoredBillingConfig(),
	};
}

function restoreGlobalState(state: GlobalState): void {
	Config.stripe.prices = state.prices;
	Config.stripe.legacyPrices = state.legacyPrices;
	Config.stripe.webhookSecret = state.webhookSecret;
	Config.instance.selfHosted = state.selfHosted;
	setCachedInstancePremiumMode(state.premiumMode);
	setStoredBillingConfig(state.storedBilling);
}

const WORKER_HELPERS = {logger: new NoopLogger()} as unknown as WorkerTaskHelpers;

function useOperatorBilling(overrides: Partial<StoredBillingConfig> = {}): void {
	Config.instance.selfHosted = true;
	setCachedInstancePremiumMode('mirror');
	setStoredBillingConfig(operatorBilling(overrides));
}

function sessionField(session: object | undefined, key: string): unknown {
	return session ? (session as Record<string, unknown>)[key] : undefined;
}

function signWebhook(payload: string, timestamp: number, secret: string): string {
	const signature = crypto.createHmac('sha256', secret).update(`${timestamp}.${payload}`).digest('hex');
	return `t=${timestamp},v1=${signature}`;
}

describe('operator billing catalog', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	let initialState: GlobalState;

	async function createAccount(): Promise<Awaited<ReturnType<typeof createTestAccount>>> {
		const storedBilling = getStoredBillingConfig();
		const premiumMode = getCachedInstancePremiumMode();
		const account = await createTestAccount(harness);
		setStoredBillingConfig(storedBilling);
		setCachedInstancePremiumMode(premiumMode);
		return account;
	}

	async function createPurchaser(): Promise<string> {
		const storedBilling = getStoredBillingConfig();
		const premiumMode = getCachedInstancePremiumMode();
		const account = await createAccount();
		await createBuilder(harness, account.token)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: true})
			.execute();
		setStoredBillingConfig(storedBilling);
		setCachedInstancePremiumMode(premiumMode);
		return account.token;
	}

	beforeAll(async () => {
		initialState = captureGlobalState();
		harness = await createApiTestHarness();
	});

	afterAll(async () => {
		restoreGlobalState(initialState);
		await harness.shutdown();
	});

	beforeEach(() => {
		restoreGlobalState(initialState);
		Config.stripe.prices = buildFullEnvPrices();
		Config.stripe.legacyPrices = {};
		stripeHandlers = createStripeApiHandlers({subscriptionsListEmpty: true});
		server.use(...stripeHandlers.handlers);
	});

	afterEach(() => {
		restoreGlobalState(initialState);
	});

	describe('env catalog mode', () => {
		test('registers every env price with the same product shape as before', () => {
			const registry = new ProductRegistry();
			expect(getEffectiveBillingConfig().catalogMode).toBe('env');
			for (const currency of ENV_CURRENCIES) {
				const lower = currency.toLowerCase();
				expect(registry.getProduct(`price_envmonthly${lower}`)).toEqual({
					type: ProductType.MONTHLY_SUBSCRIPTION,
					premiumType: UserPremiumTypes.SUBSCRIPTION,
					durationMonths: 1,
					isGift: false,
					currency,
					billingCycle: 'monthly',
				});
				expect(registry.getProduct(`price_envyearly${lower}`)).toEqual({
					type: ProductType.YEARLY_SUBSCRIPTION,
					premiumType: UserPremiumTypes.SUBSCRIPTION,
					durationMonths: 12,
					isGift: false,
					currency,
					billingCycle: 'yearly',
				});
				expect(registry.getProduct(`price_envgiftmonth${lower}`)).toEqual({
					type: ProductType.GIFT_1_MONTH,
					premiumType: UserPremiumTypes.SUBSCRIPTION,
					durationMonths: 1,
					isGift: true,
					currency,
				});
				expect(registry.getProduct(`price_envgiftyear${lower}`)).toEqual({
					type: ProductType.GIFT_1_YEAR,
					premiumType: UserPremiumTypes.SUBSCRIPTION,
					durationMonths: 12,
					isGift: true,
					currency,
				});
				expect(registry.getRecurringSubscriptionPriceId('monthly', lower)).toBe(`price_envmonthly${lower}`);
				expect(registry.getRecurringSubscriptionPriceId('yearly', currency)).toBe(`price_envyearly${lower}`);
				expect(registry.getGiftPriceId('gift_1_month', lower)).toBe(`price_envgiftmonth${lower}`);
				expect(registry.getGiftPriceId('gift_1_year', currency)).toBe(`price_envgiftyear${lower}`);
			}
			expect(registry.getRecurringSubscriptionPriceId('monthly', 'GBP')).toBeNull();
		});

		test('keeps the hosted country routing and localized currency rules', () => {
			expect(getCurrencyPreferences('SE')).toEqual(['SEK', 'EUR', 'USD']);
			expect(getCurrencyPreferences('GB')).toEqual(['USD', 'EUR']);
			expect(getGiftCurrencyPreferences('BR')).toEqual(['USD', 'EUR']);
			expect(isLocalizedCurrency('BRL')).toBe(true);
			expect(isLocalizedCurrency('USD')).toBe(false);
			expect(isLocalizedCurrency('EUR')).toBe(false);
		});

		test('ignores legacy slots outside the hosted currencies', () => {
			Config.stripe.legacyPrices = {monthly_gbp: ['price_legacygbp'], monthly_brl: ['price_legacybrl']};
			const registry = getProductRegistry();
			expect(registry.getProduct('price_legacygbp')).toBeNull();
			expect(registry.getProduct('price_legacybrl')?.currency).toBe('BRL');
		});

		test('serves the localized SEK price ids for a Swedish request', async () => {
			const prices = await createBuilder<PriceIdsResponse>(harness, '')
				.get('/premium/price-ids?country_code=SE')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(prices.currency).toBe('SEK');
			expect(prices.monthly).toBe('price_envmonthlysek');
			expect(prices.gift_currency).toBe('SEK');
		});

		test('keeps the hosted tier name, required terms consent and tax collection on hosted', async () => {
			setStoredBillingConfig(operatorBilling({terms_consent_required: false, automatic_tax: false}));
			const token = await createPurchaser();
			await createBuilder(harness, token)
				.post('/stripe/checkout/gift')
				.body({price_id: 'price_envgiftmonthusd'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const session = stripeHandlers.spies.createdCheckoutSessions[0];
			expect(session?.line_items?.[0]?.price).toBe('price_envgiftmonthusd');
			expect(session?.custom_text?.terms_of_service_acceptance?.message).toContain('Fluxer Plutonium');
			expect(session?.consent_collection?.terms_of_service).toBe('required');
			expect(sessionField(session, 'automatic_tax')).toEqual({enabled: 'true'});
			expect(sessionField(session, 'tax_id_collection')).toEqual({enabled: 'true'});
		});

		test('opens the portal with the stored customer on hosted without checking it first', async () => {
			server.use(...createStripeApiHandlers({customerShouldFail: true}).handlers);
			const account = await createAccount();
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({stripe_customer_id: 'cus_hosted_existing'})
				.execute();
			await createBuilder<{url: string}>(harness, account.token)
				.post('/premium/customer-portal')
				.expect(HTTP_STATUS.OK)
				.execute();
			const user = await getUserRepository().findUnique(createUserID(BigInt(account.userId)));
			expect(user?.stripeCustomerId).toBe('cus_hosted_existing');
		});

		test('still rejects a localized recurring price without a country code', async () => {
			const token = await createPurchaser();
			await createBuilder(harness, token)
				.post('/stripe/checkout/subscription')
				.body({price_id: 'price_envmonthlybrl'})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.STRIPE_INVALID_PRODUCT_CONFIGURATION)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
		});
	});

	describe('operator catalog mode', () => {
		beforeEach(() => {
			useOperatorBilling();
		});

		test('builds the catalog from the stored prices only', () => {
			const registry = getProductRegistry();
			expect(getEffectiveBillingConfig().catalogMode).toBe('operator');
			expect(registry.getProduct('price_opgbpmonthly')).toEqual({
				type: ProductType.MONTHLY_SUBSCRIPTION,
				premiumType: UserPremiumTypes.SUBSCRIPTION,
				durationMonths: 1,
				isGift: false,
				currency: 'GBP',
				billingCycle: 'monthly',
			});
			expect(registry.getProduct('price_opgbplegacymonthly')?.currency).toBe('GBP');
			expect(registry.getProduct('price_envmonthlyusd')).toBeNull();
			expect(registry.getRecurringSubscriptionPriceId('yearly', 'gbp')).toBe('price_opgbpyearly');
			expect(registry.getGiftPriceId('gift_1_month', 'SEK')).toBeNull();
			expect(isLocalizedCurrency('GBP')).toBe(false);
			expect(isLocalizedCurrency('BRL')).toBe(false);
		});

		test('routes countries through country_currencies, then the default currency', async () => {
			const swedish = await createBuilder<PriceIdsResponse>(harness, '')
				.get('/premium/price-ids?country_code=SE')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(swedish.currency).toBe('SEK');
			expect(swedish.monthly).toBe('price_opsekmonthly');
			expect(swedish.yearly).toBe('price_opsekyearly');
			expect(swedish.gift_currency).toBe('GBP');
			expect(swedish.gift_1_month).toBe('price_opgbpgiftmonth');

			const swiss = await createBuilder<PriceIdsResponse>(harness, '')
				.get('/premium/price-ids?country_code=CH')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(swiss.currency).toBe('CHF');
			expect(swiss.gift_currency).toBe('CHF');

			const american = await createBuilder<PriceIdsResponse>(harness, '')
				.get('/premium/price-ids?country_code=US')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(american.currency).toBe('GBP');
			expect(american.yearly).toBe('price_opgbpyearly');
			expect(american.gift_1_year).toBe('price_opgbpgiftyear');
		});

		test('falls back to the first configured currency without a default', () => {
			setStoredBillingConfig(operatorBilling({default_currency: null, country_currencies: null}));
			expect(getCurrencyPreferences('US')).toEqual(['GBP', 'SEK', 'CHF']);
			expect(getGiftCurrencyPreferences('US')).toEqual(['GBP', 'CHF']);
		});

		test('creates a GBP checkout with the stored price id and no country enforcement', async () => {
			const token = await createPurchaser();
			const withoutCountry = await createBuilder<{url: string}>(harness, token)
				.post('/stripe/checkout/subscription')
				.body({price_id: 'price_opgbpmonthly'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(withoutCountry.url).toContain('checkout.stripe.com');
			const session = stripeHandlers.spies.createdCheckoutSessions[0];
			expect(session?.mode).toBe('subscription');
			expect(session?.line_items?.[0]?.price).toBe('price_opgbpmonthly');
			expect(session?.metadata?.price_id).toBe('price_opgbpmonthly');
			expect(session?.payment_method_types).toBeUndefined();
			expect(session?.payment_method_options).toBeUndefined();
			expect(session?.consent_collection).toBeUndefined();
			expect(session?.custom_text).toBeUndefined();
			expect(sessionField(session, 'automatic_tax')).toEqual({enabled: 'false'});
			expect(sessionField(session, 'tax_id_collection')).toEqual({enabled: 'false'});
		});

		test('sends terms consent and tax collection when the operator turns them on', async () => {
			useOperatorBilling({automatic_tax: true, tax_id_collection: true, terms_consent_required: true});
			const token = await createPurchaser();
			await createBuilder<{url: string}>(harness, token)
				.post('/stripe/checkout/subscription')
				.body({price_id: 'price_opgbpyearly'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const session = stripeHandlers.spies.createdCheckoutSessions[0];
			expect(session?.consent_collection?.terms_of_service).toBe('required');
			expect(session?.custom_text?.terms_of_service_acceptance?.message).toBeTruthy();
			expect(sessionField(session, 'automatic_tax')).toEqual({enabled: 'true'});
			expect(sessionField(session, 'tax_id_collection')).toEqual({enabled: 'true'});
		});

		test('rejects legacy price ids for new checkouts', async () => {
			const token = await createPurchaser();
			expect(getProductRegistry().getProduct('price_opgbplegacymonthly')).not.toBeNull();
			await createBuilder(harness, token)
				.post('/stripe/checkout/subscription')
				.body({price_id: 'price_opgbplegacymonthly'})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.STRIPE_INVALID_PRODUCT)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
		});

		test('serves subscription prices without gifts when no currency has gift prices', async () => {
			useOperatorBilling({
				prices: {
					GBP: {monthly: 'price_opgbpmonthly', yearly: 'price_opgbpyearly', gift_1_month: null, gift_1_year: null},
				},
			});
			const prices = await createBuilder<PriceIdsResponse>(harness, '')
				.get('/premium/price-ids?country_code=GB')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(prices.currency).toBe('GBP');
			expect(prices.monthly).toBe('price_opgbpmonthly');
			expect(prices.yearly).toBe('price_opgbpyearly');
			expect(prices.gift_1_month ?? null).toBeNull();
			expect(prices.gift_1_year ?? null).toBeNull();
			expect(prices.gift_currency).toBeNull();
			const account = await createAccount();
			const state = await createBuilder<PremiumStateResponse>(harness, account.token)
				.get('/premium/state')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(state.pricing.localized?.monthly).toBe('price_opgbpmonthly');
			expect(state.pricing.localized?.yearly).toBe('price_opgbpyearly');
			expect(state.pricing.localized?.gift_1_month ?? null).toBeNull();
			expect(state.pricing.localized?.gift_currency).toBeNull();
		});

		test('fills premium state amounts from Stripe when the price mirror has no row', async () => {
			const suffix = crypto.randomBytes(4).toString('hex');
			const monthly = `price_mirrormiss${suffix}monthly`;
			const yearly = `price_mirrormiss${suffix}yearly`;
			useOperatorBilling({prices: {GBP: {monthly, yearly, gift_1_month: null, gift_1_year: null}}});
			expect(await getBillingRepository().prices.findById(monthly)).toBeNull();
			const account = await createAccount();
			const state = await createBuilder<PremiumStateResponse>(harness, account.token)
				.get('/premium/state')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(state.pricing.localized?.monthly_amount_minor).toBe(499);
			expect(state.pricing.localized?.yearly_amount_minor).toBe(4999);
			expect(Number((await getBillingRepository().prices.findById(monthly))?.unit_amount)).toBe(499);
		});

		test('replaces a stored customer that no longer exists in the configured Stripe account', async () => {
			server.use(
				...createStripeApiHandlers({
					subscriptionsListEmpty: true,
					customerShouldFail: true,
					subscriptionShouldFail: true,
				}).handlers,
			);
			const account = await createAccount();
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/security-flags`)
				.body({email_verified: true})
				.execute();
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({stripe_customer_id: 'cus_from_old_account', stripe_subscription_id: 'sub_from_old_account'})
				.execute();
			useOperatorBilling();
			const portal = await createBuilder<{url: string}>(harness, account.token)
				.post('/premium/customer-portal')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(portal.url).toContain('billing.stripe.com');
			const user = await getUserRepository().findUnique(createUserID(BigInt(account.userId)));
			expect(user?.stripeCustomerId).toBeTruthy();
			expect(user?.stripeCustomerId).not.toBe('cus_from_old_account');
			expect(user?.stripeSubscriptionId).toBeNull();
			await createBuilder<{url: string}>(harness, account.token)
				.post('/stripe/checkout/subscription')
				.body({price_id: 'price_opgbpmonthly'})
				.expect(HTTP_STATUS.OK)
				.execute();
		});

		test('accepts an operator price that does not match the buyer country', async () => {
			const token = await createPurchaser();
			await createBuilder<{url: string}>(harness, token)
				.post('/stripe/checkout/subscription')
				.body({price_id: 'price_opchfyearly', country_code: 'SE'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions[0]?.line_items?.[0]?.price).toBe('price_opchfyearly');
		});

		test('never offers the localized card preapproval flow', async () => {
			const token = await createPurchaser();
			await createBuilder(harness, token)
				.post('/stripe/checkout/subscription/preapproval')
				.body({price_id: 'price_opsekmonthly', country_code: 'SE'})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.STRIPE_INVALID_PRODUCT_CONFIGURATION)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
		});

		test('rejects pix and upi for operator prices', async () => {
			const token = await createPurchaser();
			await createBuilder(harness, token)
				.post('/stripe/checkout/subscription')
				.body({price_id: 'price_opgbpmonthly', payment_method: 'pix'})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.STRIPE_INVALID_PRODUCT_CONFIGURATION)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
		});

		test('names the configured premium tier in the checkout terms on a self-hosted instance', async () => {
			useOperatorBilling({terms_consent_required: true});
			const token = await createPurchaser();
			await createBuilder(harness, token)
				.post('/stripe/checkout/gift')
				.body({price_id: 'price_opgbpgiftmonth'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const message =
				stripeHandlers.spies.createdCheckoutSessions[0]?.custom_text?.terms_of_service_acceptance?.message;
			expect(message).toContain('Premium');
			expect(message).not.toContain('Plutonium');
		});
	});

	describe('stripe client', () => {
		test('is shared while the secret stays the same and rebuilt when it changes', () => {
			useOperatorBilling();
			const first = getStripeClient();
			expect(first).not.toBeNull();
			expect(getStripeClient()).toBe(first);
			setStoredBillingConfig(operatorBilling({stripe_secret_key: 'sk_test_operator_b'}));
			const second = getStripeClient();
			expect(second).not.toBeNull();
			expect(second).not.toBe(first);
		});

		test('stays available on self-hosted after billing is disabled so subscriptions can be serviced', () => {
			useOperatorBilling({enabled: false});
			expect(isBillingActive()).toBe(false);
			expect(getStripeClient()).not.toBeNull();
		});

		test('is unavailable on hosted when env billing is disabled', () => {
			const enabled = Config.stripe.enabled;
			Config.stripe.enabled = false;
			try {
				setStoredBillingConfig(operatorBilling({enabled: true}));
				expect(getStripeClient()).toBeNull();
			} finally {
				Config.stripe.enabled = enabled;
			}
		});

		test('sends requests with the currently stored secret key', async () => {
			const authorizations: Array<string | null> = [];
			server.use(
				http.get('https://api.stripe.com/v1/prices/:id', ({request, params}) => {
					authorizations.push(request.headers.get('authorization'));
					return HttpResponse.json({id: params.id, object: 'price', unit_amount: 500, currency: 'gbp'});
				}),
			);
			const suffix = crypto.randomBytes(4).toString('hex');
			const pricesFor = (tag: string) => ({
				GBP: {
					monthly: `price_${tag}${suffix}monthly`,
					yearly: `price_${tag}${suffix}yearly`,
					gift_1_month: `price_${tag}${suffix}giftmonth`,
					gift_1_year: `price_${tag}${suffix}giftyear`,
				},
			});
			useOperatorBilling({stripe_secret_key: 'sk_test_rotate_a', prices: pricesFor('a')});
			await createBuilder(harness, '').get('/premium/price-ids').expect(HTTP_STATUS.OK).execute();
			setStoredBillingConfig(operatorBilling({stripe_secret_key: 'sk_test_rotate_b', prices: pricesFor('b')}));
			await createBuilder(harness, '').get('/premium/price-ids').expect(HTTP_STATUS.OK).execute();
			expect(authorizations.slice(0, 4).every((value) => value === 'Bearer sk_test_rotate_a')).toBe(true);
			expect(authorizations.slice(4)).toHaveLength(4);
			expect(authorizations.slice(4).every((value) => value === 'Bearer sk_test_rotate_b')).toBe(true);
		});
	});

	describe('premium purchase availability', () => {
		test('reports purchases disabled while billing is inactive', async () => {
			Config.stripe.prices = {};
			expect(isBillingActive()).toBe(false);
			const account = await createAccount();
			const state = await createBuilder<PremiumStateResponse>(harness, account.token)
				.get('/premium/state')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(state.effective.premium_purchase_disabled).toBe(true);
			expect(state.billing.list_price_switch.reason).toBe('feature_unavailable');
		});

		test('reports purchases enabled while billing is active', async () => {
			expect(isBillingActive()).toBe(true);
			const account = await createAccount();
			const state = await createBuilder<PremiumStateResponse>(harness, account.token)
				.get('/premium/state')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(state.effective.premium_purchase_disabled).toBe(false);
		});

		test('reports purchases disabled on a self-hosted instance where everyone is premium', async () => {
			Config.instance.selfHosted = true;
			setCachedInstancePremiumMode('everyone');
			setStoredBillingConfig(operatorBilling());
			expect(isBillingActive()).toBe(false);
			const account = await createAccount();
			const state = await createBuilder<PremiumStateResponse>(harness, account.token)
				.get('/premium/state')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(state.effective.premium_purchase_disabled).toBe(true);
		});
	});
	describe('webhook secret', () => {
		beforeEach(() => {
			setupSyncStripeWebhookWorker();
			Config.stripe.webhookSecret = ENV_WEBHOOK_SECRET;
			useOperatorBilling();
		});

		async function sendWebhook(secret: string): Promise<number> {
			const {payload, timestamp} = createMockWebhookPayload({
				type: 'customer.created',
				data: {object: {id: `cus_operator_${crypto.randomBytes(4).toString('hex')}`}},
			});
			const {response} = await createBuilder(harness, '')
				.post('/stripe/webhook')
				.header('stripe-signature', signWebhook(payload, timestamp, secret))
				.header('content-type', 'application/json')
				.body(payload)
				.executeRaw();
			return response.status;
		}

		test('verifies the signature with the stored webhook secret in the api and the worker', async () => {
			expect(await sendWebhook(STORED_WEBHOOK_SECRET)).toBe(200);
		});

		test('rejects a signature made with the overridden env secret', async () => {
			expect(await sendWebhook(ENV_WEBHOOK_SECRET)).toBe(401);
		});

		function signedJob(secret: string): {body: string; signature: string} {
			const {payload, timestamp} = createMockWebhookPayload({
				type: 'customer.created',
				data: {object: {id: `cus_operator_${crypto.randomBytes(4).toString('hex')}`}},
			});
			return {body: payload, signature: signWebhook(payload, timestamp, secret)};
		}

		test('accepts a queued event signed with the previous secret in the worker after a rotation', async () => {
			setStoredBillingConfig(operatorBilling({stripe_webhook_secret: 'whsec_operator_rotated'}));
			await expect(processStripeWebhook(signedJob(STORED_WEBHOOK_SECRET), WORKER_HELPERS)).resolves.toBeUndefined();
			await expect(processStripeWebhook(signedJob('whsec_operator_rotated'), WORKER_HELPERS)).resolves.toBeUndefined();
			await expect(processStripeWebhook(signedJob('whsec_never_configured'), WORKER_HELPERS)).rejects.toThrow();
		});

		test('retries instead of discarding a queued event when the self-hosted worker has no Stripe client', async () => {
			setWorkerDependenciesForTest({stripe: null});
			await expect(processStripeWebhook(signedJob(STORED_WEBHOOK_SECRET), WORKER_HELPERS)).rejects.toThrow();
		});

		test('still discards a queued event on hosted when Stripe is not configured', async () => {
			Config.instance.selfHosted = false;
			setWorkerDependenciesForTest({stripe: null});
			await expect(processStripeWebhook(signedJob(ENV_WEBHOOK_SECRET), WORKER_HELPERS)).resolves.toBeUndefined();
		});
	});
});
