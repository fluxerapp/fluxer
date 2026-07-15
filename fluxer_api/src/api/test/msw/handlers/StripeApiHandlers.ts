// SPDX-License-Identifier: AGPL-3.0-or-later

import {HttpResponse, http, type RequestHandler} from 'msw';
import {STRIPE_API_VERSION} from '../../../stripe/StripeApiVersion';

const STRIPE_API_BASE = 'https://api.stripe.com';

interface CheckoutSessionParams {
	billing_address_collection?: string;
	customer?: string;
	customer_email?: string;
	line_items?: Array<{
		price_data?: unknown;
		price?: string;
		quantity?: number;
	}>;
	mode?: string;
	payment_method_types?: Array<string>;
	payment_method_options?: {
		card?: {
			request_three_d_secure?: string;
		};
		pix?: Record<string, unknown>;
		[key: string]: unknown;
	};
	phone_number_collection?: {
		enabled?: string;
	};
	success_url?: string;
	cancel_url?: string;
	metadata?: Record<string, string>;
	consent_collection?: {
		terms_of_service?: string;
	};
	custom_text?: {
		terms_of_service_acceptance?: {
			message?: string;
		};
	};
	subscription_data?: {
		metadata?: Record<string, string>;
	};
	payment_intent_data?: {
		metadata?: Record<string, string>;
	};
	allow_promotion_codes?: string;
}

interface PortalSessionParams {
	customer: string;
	return_url?: string;
}

interface StripeApiMockConfig {
	checkoutShouldFail?: boolean;
	portalShouldFail?: boolean;
	subscriptionShouldFail?: boolean;
	customerShouldFail?: boolean;
	subscriptionsListEmpty?: boolean;
	charges?: Record<string, Partial<MockStripeCharge>>;
	customers?: Record<string, Partial<MockStripeCustomer>>;
	invoices?: Record<string, Partial<MockStripeInvoice>>;
	paymentIntents?: Record<string, Partial<MockStripePaymentIntent>>;
	paymentMethods?: Record<string, Partial<MockStripePaymentMethod>>;
	setupIntents?: Record<string, Partial<MockStripeSetupIntent>>;
	subscriptions?: Record<string, Partial<MockStripeSubscriptionState>>;
}

interface SubscriptionScheduleParams {
	from_subscription?: string;
	end_behavior?: string;
	metadata?: Record<string, string>;
	phases?: Array<{
		start_date?: string;
		end_date?: string;
		billing_cycle_anchor?: string;
		trial?: string;
		trial_end?: string;
		metadata?: Record<string, string>;
		add_invoice_items?: Array<{
			discountable?: string;
			price_data?: {
				currency?: string;
				product?: string;
				tax_behavior?: string;
				unit_amount?: string;
			};
			quantity?: string;
			metadata?: Record<string, string>;
		}>;
		items?: Array<{
			price?: string;
			quantity?: string;
		}>;
		proration_behavior?: string;
	}>;
	proration_behavior?: string;
}

export interface StripeApiMockSpies {
	createdCheckoutSessions: Array<CheckoutSessionParams>;
	createdPortalSessions: Array<PortalSessionParams>;
	createdCustomers: Array<{
		id: string;
		email: string | null;
	}>;
	retrievedSubscriptions: Array<string>;
	cancelledSubscriptions: Array<string>;
	retrievedCustomers: Array<string>;
	retrievedPaymentIntents: Array<string>;
	retrievedSetupIntents: Array<string>;
	retrievedCharges: Array<string>;
	updatedCustomers: Array<{
		id: string;
		params: Record<string, unknown>;
	}>;
	updatedCharges: Array<{
		id: string;
		params: Record<string, unknown>;
	}>;
	createdRefunds: Array<Record<string, unknown>>;
	createdValueListItems: Array<{
		value: string;
		valueListId: string;
	}>;
	createdValueLists: Array<{
		alias: string;
		itemType: string;
		name: string;
	}>;
	updatedSubscriptions: Array<{
		id: string;
		params: Record<string, unknown>;
	}>;
	createdSubscriptionSchedules: Array<SubscriptionScheduleParams>;
	updatedSubscriptionSchedules: Array<{
		id: string;
		params: SubscriptionScheduleParams;
	}>;
	releasedSubscriptionSchedules: Array<{
		id: string;
		params: Record<string, unknown>;
	}>;
}

interface MockStripeCharge {
	id: string;
	amount: number;
	amount_captured: number;
	amount_refunded: number;
	billing_details: {
		email: string | null;
	};
	captured: boolean;
	created: number;
	currency: string;
	customer: string | null;
	disputed: boolean;
	fraud_details: Record<string, unknown>;
	livemode: boolean;
	metadata: Record<string, unknown>;
	object: 'charge';
	paid: boolean;
	payment_intent: string | null;
	payment_method: string | null;
	payment_method_details: {
		type?: string;
		card?: {
			fingerprint: string | null;
			country?: string | null;
		};
		sepa_debit?: {
			fingerprint: string | null;
		};
		us_bank_account?: {
			fingerprint: string | null;
		};
		pix?: Record<string, unknown>;
		upi?: Record<string, unknown>;
	} | null;
	receipt_email: string | null;
	refunded: boolean;
	status: 'failed' | 'pending' | 'succeeded';
}

interface MockStripePaymentIntent {
	customer: string | null;
	id: string;
	object: 'payment_intent';
	currency: string;
	latest_charge: string | null;
	status: 'canceled' | 'processing' | 'requires_payment_method' | 'requires_action' | 'succeeded';
}

interface MockStripePaymentMethod {
	id: string;
	object: 'payment_method';
	type: string;
	card: {
		brand?: string | null;
		country: string | null;
		exp_month?: number | null;
		exp_year?: number | null;
		last4?: string | null;
	};
	created?: number;
	customer: string | null;
}

interface MockStripeSetupIntent {
	id: string;
	object: 'setup_intent';
	customer: string | null;
	payment_method: MockStripePaymentMethod | string | null;
	status: 'canceled' | 'processing' | 'requires_action' | 'requires_payment_method' | 'succeeded';
}

interface MockStripeRefund {
	id: string;
	object: 'refund';
	amount: number;
	charge: string | null;
	created: number;
	currency: string;
	metadata: Record<string, unknown>;
	payment_intent: string | null;
	reason: string | null;
	status: 'canceled' | 'failed' | 'pending' | 'requires_action' | 'succeeded';
}

interface MockStripeCustomer {
	id: string;
	object: 'customer';
	email: string | null;
	name: string | null;
	created: number;
	livemode: boolean;
	metadata: Record<string, unknown>;
	description: string | null;
	currency: string | null;
	default_source: string | null;
	invoice_settings: {
		default_payment_method: string | null;
	};
}

interface MockStripeInvoicePayment {
	id: string;
	object: 'invoice_payment';
	amount_paid: number | null;
	amount_requested: number;
	created: number;
	currency: string;
	invoice: string;
	is_default: boolean;
	livemode: boolean;
	payment: {
		charge: MockStripeCharge | string | null;
		payment_intent: MockStripePaymentIntent | string | null;
		type: 'charge' | 'payment_intent' | 'payment_record';
	};
	status: 'canceled' | 'open' | 'paid';
	status_transitions: {
		canceled_at: number | null;
		paid_at: number | null;
	};
}

interface MockStripeInvoice {
	id: string;
	object: 'invoice';
	amount_due: number;
	amount_paid: number;
	billing_reason: string | null;
	created: number;
	currency: string;
	customer: string | null;
	hosted_invoice_url: string | null;
	invoice_pdf: string | null;
	payments: {
		object: 'list';
		data: Array<MockStripeInvoicePayment>;
		has_more: boolean;
		url: string;
	};
	status: 'draft' | 'open' | 'paid' | 'uncollectible' | 'void' | null;
	subscription: string | null;
}

interface MockStripeValueList {
	alias: string;
	id: string;
	itemType: string;
	items: Array<string>;
	name: string;
}

interface MockStripeSubscriptionState {
	customer: string;
	trial_end: number | null;
	price_id: string;
	currency: string;
	interval: 'month' | 'year';
	item_id: string;
	current_period_start: number;
	current_period_end: number;
	cancel_at: number | null;
	cancel_at_period_end: boolean;
	default_payment_method: string | null;
	latest_invoice: string | null;
	status: 'active' | 'canceled' | 'incomplete' | 'past_due' | 'trialing';
	schedule_id: string | null;
}

interface MockStripeSubscriptionSchedule {
	id: string;
	subscription: string;
	end_behavior: string;
	metadata: Record<string, string>;
	phases: Array<{
		start_date: number;
		end_date: number;
		billing_cycle_anchor?: string | null;
		metadata?: Record<string, string>;
		add_invoice_items?: Array<{
			discountable?: boolean;
			price_data?: {
				currency?: string;
				product?: string;
				tax_behavior?: string;
				unit_amount?: number;
			};
			quantity: number;
			metadata?: Record<string, string>;
		}>;
		items: Array<{
			price: string;
			quantity: number;
		}>;
		proration_behavior: string;
	}>;
}

function parseFormDataToObject<T extends object = Record<string, unknown>>(formData: FormData): T {
	const result: Record<string, unknown> = {};
	const isArrayIndex = (segment: string): boolean => /^\d+$/.test(segment);
	const setNestedValue = (target: Record<string, unknown>, key: string, value: string): void => {
		const segments = key.match(/[^[\]]+/g);
		if (!segments || segments.length === 0) {
			return;
		}
		let current: Record<string, unknown> | Array<unknown> = target;
		for (let i = 0; i < segments.length; i++) {
			const segment = segments[i]!;
			const isLast = i === segments.length - 1;
			const nextIsArray = !isLast && isArrayIndex(segments[i + 1]!);
			if (Array.isArray(current)) {
				const index = parseInt(segment, 10);
				if (isLast) {
					current[index] = value;
					return;
				}
				if (current[index] === undefined) {
					current[index] = nextIsArray ? [] : {};
				}
				current = current[index] as Record<string, unknown> | Array<unknown>;
				continue;
			}
			if (isLast) {
				current[segment] = value;
				return;
			}
			if (current[segment] === undefined) {
				current[segment] = nextIsArray ? [] : {};
			}
			current[segment] = current[segment] as Record<string, unknown> | Array<unknown>;
			current = current[segment] as Record<string, unknown> | Array<unknown>;
		}
	};
	for (const [key, value] of formData.entries()) {
		setNestedValue(result, key, typeof value === 'string' ? value : String(value));
	}
	return result as T;
}

export interface StripeApiHandlers {
	handlers: Array<RequestHandler>;
	spies: StripeApiMockSpies;
	reset: () => void;
	resetAll: () => void;
}

export function createStripeApiHandlers(config: StripeApiMockConfig = {}): StripeApiHandlers {
	const spies: StripeApiMockSpies = {
		createdCheckoutSessions: [],
		createdPortalSessions: [],
		createdCustomers: [],
		createdRefunds: [],
		createdValueListItems: [],
		createdValueLists: [],
		retrievedCharges: [],
		retrievedSubscriptions: [],
		cancelledSubscriptions: [],
		retrievedCustomers: [],
		retrievedPaymentIntents: [],
		retrievedSetupIntents: [],
		updatedCharges: [],
		updatedCustomers: [],
		updatedSubscriptions: [],
		createdSubscriptionSchedules: [],
		updatedSubscriptionSchedules: [],
		releasedSubscriptionSchedules: [],
	};
	let sessionCounter = 0;
	let portalCounter = 0;
	let refundCounter = 0;
	let valueListCounter = 0;
	const subscriptionStore = new Map<string, MockStripeSubscriptionState>();
	const chargeStore = new Map<string, MockStripeCharge>();
	const invoiceStore = new Map<string, MockStripeInvoice>();
	const paymentIntentStore = new Map<string, MockStripePaymentIntent>();
	const paymentMethodStore = new Map<string, MockStripePaymentMethod>();
	const refundStore = new Map<string, MockStripeRefund>();
	const setupIntentStore = new Map<string, MockStripeSetupIntent>();
	const customerStore = new Map<string, MockStripeCustomer>();
	const valueListStore = new Map<string, MockStripeValueList>();
	const subscriptionScheduleStore = new Map<string, MockStripeSubscriptionSchedule>();
	let subscriptionScheduleCounter = 0;
	function createDefaultCharge(id: string, paymentIntentId: string | null = 'pi_test_1'): MockStripeCharge {
		return {
			id,
			amount: 2500,
			amount_captured: 2500,
			amount_refunded: 0,
			billing_details: {
				email: 'test@example.com',
			},
			captured: true,
			created: Math.floor(Date.now() / 1000) - 3600,
			currency: 'usd',
			customer: 'cus_test_1',
			disputed: false,
			fraud_details: {},
			livemode: false,
			metadata: {},
			object: 'charge',
			paid: true,
			payment_intent: paymentIntentId,
			payment_method: 'pm_test_1',
			payment_method_details: {
				type: 'card',
				card: {
					fingerprint: 'fp_test_1',
					country: 'US',
				},
			},
			receipt_email: null,
			refunded: false,
			status: 'succeeded',
		};
	}
	function hydrateStores(): void {
		subscriptionStore.clear();
		chargeStore.clear();
		invoiceStore.clear();
		paymentIntentStore.clear();
		paymentMethodStore.clear();
		refundStore.clear();
		setupIntentStore.clear();
		customerStore.clear();
		valueListStore.clear();
		subscriptionScheduleStore.clear();
		for (const [chargeId, overrides] of Object.entries(config.charges ?? {})) {
			chargeStore.set(chargeId, {
				...createDefaultCharge(chargeId),
				...overrides,
				billing_details: {
					...createDefaultCharge(chargeId).billing_details,
					...(overrides.billing_details ?? {}),
				},
				fraud_details: {
					...createDefaultCharge(chargeId).fraud_details,
					...(overrides.fraud_details ?? {}),
				},
				metadata: {
					...createDefaultCharge(chargeId).metadata,
					...(overrides.metadata ?? {}),
				},
				object: 'charge',
				payment_method_details:
					overrides.payment_method_details ?? createDefaultCharge(chargeId).payment_method_details,
			});
		}
		for (const [paymentIntentId, overrides] of Object.entries(config.paymentIntents ?? {})) {
			paymentIntentStore.set(paymentIntentId, {
				...createDefaultPaymentIntent(paymentIntentId),
				...overrides,
				id: paymentIntentId,
				object: 'payment_intent',
			});
		}
		for (const [paymentMethodId, overrides] of Object.entries(config.paymentMethods ?? {})) {
			const defaultPaymentMethod = createDefaultPaymentMethod(paymentMethodId, overrides.customer ?? 'cus_test_1');
			paymentMethodStore.set(paymentMethodId, {
				...defaultPaymentMethod,
				...overrides,
				id: paymentMethodId,
				object: 'payment_method',
				card: {
					brand: overrides.card?.brand ?? defaultPaymentMethod.card.brand,
					country: overrides.card?.country ?? defaultPaymentMethod.card.country,
					exp_month: overrides.card?.exp_month ?? defaultPaymentMethod.card.exp_month,
					exp_year: overrides.card?.exp_year ?? defaultPaymentMethod.card.exp_year,
					last4: overrides.card?.last4 ?? defaultPaymentMethod.card.last4,
				},
			});
		}
		for (const [setupIntentId, overrides] of Object.entries(config.setupIntents ?? {})) {
			const defaultSetupIntent = createDefaultSetupIntent(setupIntentId);
			const paymentMethodOverride = overrides.payment_method;
			const paymentMethod =
				typeof paymentMethodOverride === 'string' || paymentMethodOverride == null
					? (paymentMethodOverride ?? defaultSetupIntent.payment_method)
					: (() => {
							const defaultPaymentMethod = createDefaultPaymentMethod(
								paymentMethodOverride.id ?? `pm_${setupIntentId}`,
							);
							const mergedPaymentMethod: MockStripePaymentMethod = {
								...defaultPaymentMethod,
								...paymentMethodOverride,
								id: paymentMethodOverride.id ?? `pm_${setupIntentId}`,
								object: 'payment_method',
								card: {
									brand: paymentMethodOverride.card?.brand ?? defaultPaymentMethod.card.brand,
									country: paymentMethodOverride.card?.country ?? defaultPaymentMethod.card.country,
									exp_month: paymentMethodOverride.card?.exp_month ?? defaultPaymentMethod.card.exp_month,
									exp_year: paymentMethodOverride.card?.exp_year ?? defaultPaymentMethod.card.exp_year,
									last4: paymentMethodOverride.card?.last4 ?? defaultPaymentMethod.card.last4,
								},
							};
							return mergedPaymentMethod;
						})();
			if (paymentMethod && typeof paymentMethod !== 'string') {
				paymentMethodStore.set(paymentMethod.id, paymentMethod);
			}
			setupIntentStore.set(setupIntentId, {
				...defaultSetupIntent,
				...overrides,
				id: setupIntentId,
				object: 'setup_intent',
				payment_method: paymentMethod,
			});
		}
		for (const [customerId, overrides] of Object.entries(config.customers ?? {})) {
			const defaultCustomer = createDefaultCustomer(customerId, overrides.email ?? 'test@example.com');
			customerStore.set(customerId, {
				...defaultCustomer,
				...overrides,
				id: customerId,
				object: 'customer',
				invoice_settings: {
					default_payment_method:
						overrides.invoice_settings && 'default_payment_method' in overrides.invoice_settings
							? overrides.invoice_settings.default_payment_method
							: defaultCustomer.invoice_settings.default_payment_method,
				},
				metadata: {
					...defaultCustomer.metadata,
					...(overrides.metadata ?? {}),
				},
			});
		}
		for (const [invoiceId, overrides] of Object.entries(config.invoices ?? {})) {
			const defaultInvoice = createDefaultInvoice(invoiceId, {
				customerId: overrides.customer ?? 'cus_test_1',
				subscriptionId: overrides.subscription ?? 'sub_test_1',
			});
			invoiceStore.set(invoiceId, {
				...defaultInvoice,
				...overrides,
				id: invoiceId,
				object: 'invoice',
				payments: overrides.payments ?? defaultInvoice.payments,
			});
		}
		for (const [subscriptionId, overrides] of Object.entries(config.subscriptions ?? {})) {
			subscriptionStore.set(subscriptionId, {
				...createDefaultSubscriptionState(),
				...overrides,
			});
		}
	}
	function getCharge(chargeId: string): MockStripeCharge {
		const existingCharge = chargeStore.get(chargeId);
		if (existingCharge) {
			return existingCharge;
		}
		const newCharge = createDefaultCharge(chargeId);
		chargeStore.set(chargeId, newCharge);
		return newCharge;
	}
	function createDefaultPaymentIntent(id: string): MockStripePaymentIntent {
		const defaultChargeId = `ch_${id}`;
		if (!chargeStore.has(defaultChargeId)) {
			chargeStore.set(defaultChargeId, createDefaultCharge(defaultChargeId, id));
		}
		return {
			customer: 'cus_test_1',
			id,
			object: 'payment_intent',
			currency: 'usd',
			latest_charge: defaultChargeId,
			status: 'succeeded',
		};
	}
	function createDefaultPaymentMethod(id: string, customerId: string | null = 'cus_test_1'): MockStripePaymentMethod {
		return {
			id,
			object: 'payment_method',
			type: 'card',
			card: {
				brand: 'visa',
				country: 'US',
				exp_month: 1,
				exp_year: 2030,
				last4: '4242',
			},
			created: Math.floor(Date.now() / 1000) - 3600,
			customer: customerId,
		};
	}
	function createDefaultSetupIntent(
		id: string,
		{
			customerId = 'cus_test_1',
			paymentMethodId = `pm_${id}`,
		}: {
			customerId?: string | null;
			paymentMethodId?: string;
		} = {},
	): MockStripeSetupIntent {
		const paymentMethod = createDefaultPaymentMethod(paymentMethodId, customerId);
		paymentMethodStore.set(paymentMethod.id, paymentMethod);
		return {
			id,
			object: 'setup_intent',
			customer: customerId,
			payment_method: paymentMethod,
			status: 'succeeded',
		};
	}
	function createDefaultCustomer(id: string, email: string | null = 'test@example.com'): MockStripeCustomer {
		return {
			id,
			object: 'customer',
			email,
			name: 'Test Customer',
			created: Math.floor(Date.now() / 1000) - 365 * 24 * 60 * 60,
			livemode: false,
			metadata: {},
			description: null,
			currency: 'usd',
			default_source: null,
			invoice_settings: {
				default_payment_method: 'pm_test_1',
			},
		};
	}
	function createDefaultInvoicePayment(
		id: string,
		invoiceId: string,
		paymentIntentId: string,
		chargeId: string,
	): MockStripeInvoicePayment {
		const paymentIntent = getPaymentIntent(paymentIntentId);
		const charge = getCharge(chargeId);
		return {
			id,
			object: 'invoice_payment',
			amount_paid: 2500,
			amount_requested: 2500,
			created: Math.floor(Date.now() / 1000) - 1800,
			currency: 'usd',
			invoice: invoiceId,
			is_default: true,
			livemode: false,
			payment: {
				charge,
				payment_intent: paymentIntent,
				type: 'payment_intent',
			},
			status: 'paid',
			status_transitions: {
				canceled_at: null,
				paid_at: Math.floor(Date.now() / 1000) - 1800,
			},
		};
	}
	function createDefaultInvoice(
		id: string,
		{
			customerId = 'cus_test_1',
			subscriptionId = 'sub_test_1',
		}: {
			customerId?: string | null;
			subscriptionId?: string | null;
		} = {},
	): MockStripeInvoice {
		const paymentIntentId = `pi_${id}`;
		const chargeId = `ch_${paymentIntentId}`;
		if (!paymentIntentStore.has(paymentIntentId)) {
			paymentIntentStore.set(paymentIntentId, {
				...createDefaultPaymentIntent(paymentIntentId),
				customer: customerId,
				latest_charge: chargeId,
			});
		}
		if (!chargeStore.has(chargeId)) {
			chargeStore.set(chargeId, {
				...createDefaultCharge(chargeId, paymentIntentId),
				customer: customerId,
			});
		}
		return {
			id,
			object: 'invoice',
			amount_due: 2500,
			amount_paid: 2500,
			billing_reason: 'subscription_cycle',
			created: Math.floor(Date.now() / 1000) - 1800,
			currency: 'usd',
			customer: customerId,
			hosted_invoice_url: `https://invoice.stripe.com/i/${id}`,
			invoice_pdf: `https://pay.stripe.com/invoice/${id}/pdf`,
			payments: {
				object: 'list',
				data: [createDefaultInvoicePayment(`inpay_${id}`, id, paymentIntentId, chargeId)],
				has_more: false,
				url: `/v1/invoices/${id}/payments`,
			},
			status: 'paid',
			subscription: subscriptionId,
		};
	}
	function getPaymentIntent(paymentIntentId: string): MockStripePaymentIntent {
		const existingPaymentIntent = paymentIntentStore.get(paymentIntentId);
		if (existingPaymentIntent) {
			return existingPaymentIntent;
		}
		const newPaymentIntent = createDefaultPaymentIntent(paymentIntentId);
		paymentIntentStore.set(paymentIntentId, newPaymentIntent);
		return newPaymentIntent;
	}
	function getSetupIntent(setupIntentId: string): MockStripeSetupIntent {
		const existingSetupIntent = setupIntentStore.get(setupIntentId);
		if (existingSetupIntent) {
			return existingSetupIntent;
		}
		const newSetupIntent = createDefaultSetupIntent(setupIntentId);
		setupIntentStore.set(setupIntentId, newSetupIntent);
		return newSetupIntent;
	}
	function getCustomer(customerId: string): MockStripeCustomer {
		const existingCustomer = customerStore.get(customerId);
		if (existingCustomer) {
			return existingCustomer;
		}
		const newCustomer = createDefaultCustomer(customerId);
		customerStore.set(customerId, newCustomer);
		return newCustomer;
	}
	function getInvoice(invoiceId: string): MockStripeInvoice {
		const existingInvoice = invoiceStore.get(invoiceId);
		if (existingInvoice) {
			return existingInvoice;
		}
		const newInvoice = createDefaultInvoice(invoiceId);
		invoiceStore.set(invoiceId, newInvoice);
		return newInvoice;
	}
	function inferSubscriptionPriceState(priceId: string): Pick<MockStripeSubscriptionState, 'currency' | 'interval'> {
		const normalizedPriceId = priceId.toLowerCase();
		return {
			currency: normalizedPriceId.includes('eur')
				? 'eur'
				: normalizedPriceId.includes('brl')
					? 'brl'
					: normalizedPriceId.includes('inr')
						? 'inr'
						: normalizedPriceId.includes('pln')
							? 'pln'
							: normalizedPriceId.includes('try')
								? 'try'
								: 'usd',
			interval: normalizedPriceId.includes('year') ? 'year' : 'month',
		};
	}
	function createDefaultSubscriptionState(): MockStripeSubscriptionState {
		const currentPeriodStart = Math.floor(Date.now() / 1000) - 2 * 24 * 60 * 60;
		return {
			customer: 'cus_test_1',
			trial_end: null,
			price_id: 'price_test_1',
			currency: 'usd',
			interval: 'month',
			item_id: 'si_test_1',
			current_period_start: currentPeriodStart,
			current_period_end: currentPeriodStart + 30 * 24 * 60 * 60,
			cancel_at: null,
			cancel_at_period_end: false,
			default_payment_method: 'pm_test_1',
			latest_invoice: null,
			status: 'active',
			schedule_id: null,
		};
	}
	function getOrCreateSubscriptionState(subscriptionId: string): MockStripeSubscriptionState {
		const existingState = subscriptionStore.get(subscriptionId);
		if (existingState) {
			return existingState;
		}
		const newState = createDefaultSubscriptionState();
		subscriptionStore.set(subscriptionId, newState);
		return newState;
	}
	function getPaymentMethod(paymentMethodId: string): MockStripePaymentMethod {
		const existingPaymentMethod = paymentMethodStore.get(paymentMethodId);
		if (existingPaymentMethod) {
			return existingPaymentMethod;
		}
		const paymentMethod = createDefaultPaymentMethod(paymentMethodId);
		paymentMethodStore.set(paymentMethodId, paymentMethod);
		return paymentMethod;
	}
	function mapSubscriptionStateToStripeSubscription(id: string, subState: MockStripeSubscriptionState) {
		return {
			id,
			object: 'subscription',
			customer: subState.customer,
			status: subState.status,
			current_period_start: subState.current_period_start,
			current_period_end: subState.current_period_end,
			latest_invoice: subState.latest_invoice ? getInvoice(subState.latest_invoice) : null,
			trial_end: subState.trial_end,
			items: {
				object: 'list',
				data: [
					{
						id: subState.item_id,
						object: 'subscription_item',
						price: {
							id: subState.price_id,
							object: 'price',
							unit_amount: 2500,
							currency: subState.currency,
							recurring: {
								interval: subState.interval,
								interval_count: 1,
							},
							type: 'recurring',
							active: true,
							livemode: false,
						},
						quantity: 1,
						current_period_start: subState.current_period_start,
						current_period_end: subState.current_period_end,
					},
				],
				has_more: false,
				url: `/v1/subscription_items?subscription=${id}`,
			},
			cancel_at: subState.cancel_at,
			cancel_at_period_end: subState.cancel_at_period_end,
			canceled_at: subState.status === 'canceled' ? Math.floor(Date.now() / 1000) : null,
			collection_method: 'charge_automatically',
			created: Math.floor(Date.now() / 1000) - 90 * 24 * 60 * 60,
			default_payment_method: subState.default_payment_method
				? getPaymentMethod(subState.default_payment_method)
				: null,
			livemode: false,
			metadata: {},
			schedule: subState.schedule_id,
			start_date: Math.floor(Date.now() / 1000) - 90 * 24 * 60 * 60,
		};
	}
	function mapScheduleToStripeSchedule(schedule: MockStripeSubscriptionSchedule) {
		return {
			id: schedule.id,
			object: 'subscription_schedule',
			status: 'active',
			subscription: schedule.subscription,
			end_behavior: schedule.end_behavior,
			metadata: schedule.metadata,
			current_phase: schedule.phases[0]
				? {
						start_date: schedule.phases[0].start_date,
						end_date: schedule.phases[0].end_date,
					}
				: null,
			phases: schedule.phases.map((phase) => ({
				start_date: phase.start_date,
				end_date: phase.end_date,
				billing_cycle_anchor: phase.billing_cycle_anchor,
				metadata: phase.metadata ?? {},
				add_invoice_items:
					phase.add_invoice_items?.map((item) => ({
						discountable: item.discountable,
						price_data: item.price_data,
						quantity: item.quantity,
						metadata: item.metadata ?? {},
					})) ?? [],
				items: phase.items.map((item) => ({
					price: item.price,
					quantity: item.quantity,
				})),
				proration_behavior: phase.proration_behavior,
				discounts: [],
			})),
			livemode: false,
			created: Math.floor(Date.now() / 1000),
		};
	}
	function createScheduleFromSubscription(subscriptionId: string, metadata: Record<string, string> = {}) {
		const subscription = getOrCreateSubscriptionState(subscriptionId);
		subscriptionScheduleCounter++;
		const scheduleId = `sub_sched_test_${subscriptionScheduleCounter}`;
		const schedule: MockStripeSubscriptionSchedule = {
			id: scheduleId,
			subscription: subscriptionId,
			end_behavior: 'release',
			metadata,
			phases: [
				{
					start_date: subscription.current_period_start,
					end_date: subscription.current_period_end,
					billing_cycle_anchor: null,
					add_invoice_items: [],
					items: [
						{
							price: subscription.price_id,
							quantity: 1,
						},
					],
					proration_behavior: 'none',
				},
			],
		};
		subscription.schedule_id = scheduleId;
		subscriptionScheduleStore.set(scheduleId, schedule);
		return schedule;
	}
	hydrateStores();
	const handlers = [
		http.post(`${STRIPE_API_BASE}/v1/checkout/sessions`, async ({request}) => {
			if (config.checkoutShouldFail) {
				return HttpResponse.json(
					{
						error: {
							type: 'invalid_request_error',
							message: 'Mock checkout failure',
							code: 'resource_missing',
						},
					},
					{status: 400},
				);
			}
			const formData = await request.formData();
			const params = parseFormDataToObject<CheckoutSessionParams>(formData);
			spies.createdCheckoutSessions.push(params);
			sessionCounter++;
			const sessionId = `cs_test_${sessionCounter}_${Date.now()}`;
			const setupIntentId = params.mode === 'setup' ? `seti_test_${sessionCounter}_${Date.now()}` : null;
			if (setupIntentId) {
				setupIntentStore.set(
					setupIntentId,
					createDefaultSetupIntent(setupIntentId, {
						customerId: params.customer ?? null,
					}),
				);
			}
			return HttpResponse.json({
				id: sessionId,
				object: 'checkout.session',
				url: `https://checkout.stripe.com/c/pay/${sessionId}`,
				customer: params.customer ?? (params.customer_email ? `cus_test_${sessionCounter}` : null),
				customer_email: params.customer_email,
				mode: params.mode || 'subscription',
				metadata: params.metadata || {},
				status: 'open',
				success_url: params.success_url,
				cancel_url: params.cancel_url,
				amount_total: null,
				currency: null,
				livemode: false,
				payment_status: 'unpaid',
				created: Math.floor(Date.now() / 1000),
				expires_at: Math.floor(Date.now() / 1000) + 86400,
				setup_intent: setupIntentId,
			});
		}),
		http.get(`${STRIPE_API_BASE}/v1/checkout/sessions/:id`, ({params}) => {
			const {id} = params;
			return HttpResponse.json({
				id,
				object: 'checkout.session',
				customer: 'cus_test_1',
				customer_email: 'test@example.com',
				subscription: 'sub_test_1',
				payment_intent: 'pi_test_1',
				amount_total: 2500,
				currency: 'usd',
				status: 'complete',
				payment_status: 'paid',
				metadata: {},
				mode: 'subscription',
				livemode: false,
				created: Math.floor(Date.now() / 1000) - 3600,
			});
		}),
		http.get(`${STRIPE_API_BASE}/v1/checkout/sessions`, ({request}) => {
			const requestUrl = new URL(request.url);
			const paymentIntentId = requestUrl.searchParams.get('payment_intent');
			const subscriptionId = requestUrl.searchParams.get('subscription');
			const customerId = requestUrl.searchParams.get('customer');
			return HttpResponse.json({
				object: 'list',
				data: paymentIntentId || subscriptionId || customerId ? [] : [],
				has_more: false,
				url: '/v1/checkout/sessions',
			});
		}),
		http.get(`${STRIPE_API_BASE}/v1/payment_intents/:id`, ({params}) => {
			const {id} = params;
			const paymentIntentId = id as string;
			spies.retrievedPaymentIntents.push(paymentIntentId);
			return HttpResponse.json(getPaymentIntent(paymentIntentId));
		}),
		http.get(`${STRIPE_API_BASE}/v1/setup_intents/:id`, ({params}) => {
			const {id} = params;
			const setupIntentId = id as string;
			spies.retrievedSetupIntents.push(setupIntentId);
			return HttpResponse.json(getSetupIntent(setupIntentId));
		}),
		http.get(`${STRIPE_API_BASE}/v1/customers/:id/payment_methods`, ({params, request}) => {
			const customerId = params.id as string;
			const requestUrl = new URL(request.url);
			const paymentMethodType = requestUrl.searchParams.get('type');
			const paymentMethods = [...paymentMethodStore.values()].filter(
				(paymentMethod) =>
					paymentMethod.customer === customerId && (!paymentMethodType || paymentMethod.type === paymentMethodType),
			);
			return HttpResponse.json({
				object: 'list',
				data: paymentMethods,
				has_more: false,
				url: `/v1/customers/${customerId}/payment_methods`,
			});
		}),
		http.get(`${STRIPE_API_BASE}/v1/customers/:customerId/payment_methods/:paymentMethodId`, ({params}) => {
			const customerId = params.customerId as string;
			const paymentMethodId = params.paymentMethodId as string;
			const paymentMethod = getPaymentMethod(paymentMethodId);
			return HttpResponse.json({...paymentMethod, customer: customerId});
		}),
		http.get(`${STRIPE_API_BASE}/v1/invoices`, ({request}) => {
			const requestUrl = new URL(request.url);
			const customerId = requestUrl.searchParams.get('customer');
			const subscriptionId = requestUrl.searchParams.get('subscription');
			const startingAfter = requestUrl.searchParams.get('starting_after');
			const limit = Number.parseInt(requestUrl.searchParams.get('limit') ?? '10', 10);
			const sortedInvoices = [...invoiceStore.values()]
				.filter((invoice) => !customerId || invoice.customer === customerId)
				.filter((invoice) => !subscriptionId || invoice.subscription === subscriptionId)
				.sort((left, right) => right.created - left.created);
			const startIndex = startingAfter ? sortedInvoices.findIndex((invoice) => invoice.id === startingAfter) + 1 : 0;
			const pagedInvoices = sortedInvoices.slice(Math.max(startIndex, 0), Math.max(startIndex, 0) + limit);
			const hasMore = Math.max(startIndex, 0) + limit < sortedInvoices.length;
			return HttpResponse.json({
				object: 'list',
				data: pagedInvoices,
				has_more: hasMore,
				url: '/v1/invoices',
			});
		}),
		http.get(`${STRIPE_API_BASE}/v1/invoices/:id`, ({params}) => {
			const invoiceId = params.id as string;
			return HttpResponse.json(getInvoice(invoiceId));
		}),
		http.get(`${STRIPE_API_BASE}/v1/charges/:id`, ({params}) => {
			const {id} = params;
			const chargeId = id as string;
			spies.retrievedCharges.push(chargeId);
			return HttpResponse.json(getCharge(chargeId));
		}),
		http.post(`${STRIPE_API_BASE}/v1/charges/:id`, async ({params, request}) => {
			const {id} = params;
			const chargeId = id as string;
			const charge = getCharge(chargeId);
			const formData = await request.formData();
			const updateParams = parseFormDataToObject(formData);
			spies.updatedCharges.push({id: chargeId, params: updateParams});
			const updatedCharge: MockStripeCharge = {
				...charge,
				fraud_details:
					updateParams.fraud_details && typeof updateParams.fraud_details === 'object'
						? {
								...charge.fraud_details,
								...(updateParams.fraud_details as Record<string, unknown>),
							}
						: charge.fraud_details,
			};
			chargeStore.set(chargeId, updatedCharge);
			return HttpResponse.json(updatedCharge);
		}),
		http.post(`${STRIPE_API_BASE}/v1/refunds`, async ({request}) => {
			const formData = await request.formData();
			const refundParams = parseFormDataToObject(formData);
			spies.createdRefunds.push(refundParams);
			const paymentIntentId = (refundParams.payment_intent as string | undefined) ?? null;
			const chargeId =
				(refundParams.charge as string | undefined) ??
				(paymentIntentId ? (getPaymentIntent(paymentIntentId).latest_charge ?? null) : null);
			if (!chargeId) {
				return HttpResponse.json(
					{
						error: {
							type: 'invalid_request_error',
							message: 'Missing charge or payment_intent',
							code: 'parameter_missing',
						},
					},
					{status: 400},
				);
			}
			const charge = getCharge(chargeId);
			const refundAmount =
				typeof refundParams.amount === 'string' ? Number.parseInt(refundParams.amount, 10) : charge.amount_captured;
			const updatedCharge: MockStripeCharge = {
				...charge,
				amount_refunded: refundAmount,
				refunded: refundAmount >= charge.amount_captured,
			};
			chargeStore.set(chargeId, updatedCharge);
			refundCounter++;
			const refund: MockStripeRefund = {
				id: `re_test_${refundCounter}`,
				object: 'refund',
				amount: refundAmount,
				charge: chargeId,
				created: Math.floor(Date.now() / 1000),
				currency: updatedCharge.currency,
				metadata:
					refundParams.metadata && typeof refundParams.metadata === 'object'
						? (refundParams.metadata as Record<string, unknown>)
						: {},
				payment_intent: paymentIntentId,
				reason: typeof refundParams.reason === 'string' ? refundParams.reason : null,
				status: 'succeeded',
			};
			refundStore.set(refund.id, refund);
			return HttpResponse.json(refund);
		}),
		http.get(`${STRIPE_API_BASE}/v1/refunds`, ({request}) => {
			const requestUrl = new URL(request.url);
			const chargeId = requestUrl.searchParams.get('charge');
			const paymentIntentId = requestUrl.searchParams.get('payment_intent');
			const limit = Number.parseInt(requestUrl.searchParams.get('limit') ?? '10', 10);
			const startingAfter = requestUrl.searchParams.get('starting_after');
			const refunds = [...refundStore.values()]
				.filter((refund) => !chargeId || refund.charge === chargeId)
				.filter((refund) => !paymentIntentId || refund.payment_intent === paymentIntentId)
				.sort((left, right) => right.created - left.created);
			const startIndex = startingAfter ? refunds.findIndex((refund) => refund.id === startingAfter) + 1 : 0;
			const pagedRefunds = refunds.slice(Math.max(startIndex, 0), Math.max(startIndex, 0) + limit);
			return HttpResponse.json({
				object: 'list',
				data: pagedRefunds,
				has_more: Math.max(startIndex, 0) + limit < refunds.length,
				url: '/v1/refunds',
			});
		}),
		http.post(`${STRIPE_API_BASE}/v1/billing_portal/sessions`, async ({request}) => {
			if (config.portalShouldFail) {
				return HttpResponse.json(
					{
						error: {
							type: 'invalid_request_error',
							message: 'Mock portal failure',
							code: 'resource_missing',
						},
					},
					{status: 400},
				);
			}
			const formData = await request.formData();
			const customer = formData.get('customer') as string;
			const return_url = formData.get('return_url') as string | undefined;
			spies.createdPortalSessions.push({customer, return_url});
			portalCounter++;
			return HttpResponse.json({
				id: `bps_test_${portalCounter}`,
				object: 'billing_portal.session',
				url: `https://billing.stripe.com/p/session/test_${portalCounter}_${Date.now()}`,
				customer,
				return_url: return_url || null,
				livemode: false,
				created: Math.floor(Date.now() / 1000),
				configuration: 'bpc_test_config',
			});
		}),
		http.post(`${STRIPE_API_BASE}/v1/subscription_schedules`, async ({request}) => {
			const formData = await request.formData();
			const params = parseFormDataToObject<SubscriptionScheduleParams>(formData);
			spies.createdSubscriptionSchedules.push(params);
			if (!params.from_subscription) {
				return HttpResponse.json(
					{
						error: {
							type: 'invalid_request_error',
							message: 'from_subscription is required in the mock',
						},
					},
					{status: 400},
				);
			}
			const schedule = createScheduleFromSubscription(params.from_subscription, params.metadata ?? {});
			return HttpResponse.json(mapScheduleToStripeSchedule(schedule));
		}),
		http.get(`${STRIPE_API_BASE}/v1/subscription_schedules/:id`, ({params}) => {
			const schedule = subscriptionScheduleStore.get(params.id as string);
			if (!schedule) {
				return HttpResponse.json(
					{
						error: {
							type: 'invalid_request_error',
							message: 'No such subscription schedule',
						},
					},
					{status: 404},
				);
			}
			return HttpResponse.json(mapScheduleToStripeSchedule(schedule));
		}),
		http.post(`${STRIPE_API_BASE}/v1/subscription_schedules/:id`, async ({params, request}) => {
			const schedule = subscriptionScheduleStore.get(params.id as string);
			if (!schedule) {
				return HttpResponse.json(
					{
						error: {
							type: 'invalid_request_error',
							message: 'No such subscription schedule',
						},
					},
					{status: 404},
				);
			}
			const formData = await request.formData();
			const updateParams = parseFormDataToObject<SubscriptionScheduleParams>(formData);
			spies.updatedSubscriptionSchedules.push({id: params.id as string, params: updateParams});
			schedule.end_behavior = updateParams.end_behavior ?? schedule.end_behavior;
			schedule.metadata = updateParams.metadata ?? schedule.metadata;
			if (Array.isArray(updateParams.phases)) {
				schedule.phases = updateParams.phases.map((phase, index) => ({
					start_date: Number(
						phase.start_date ?? (index === 0 ? schedule.phases[0]?.start_date : schedule.phases[index - 1]?.end_date),
					),
					end_date: Number(phase.end_date ?? 0),
					billing_cycle_anchor: phase.billing_cycle_anchor ?? null,
					add_invoice_items: (phase.add_invoice_items ?? []).map((item) => ({
						discountable: item.discountable === 'false' ? false : item.discountable === 'true' ? true : undefined,
						price_data: item.price_data
							? {
									currency: item.price_data.currency,
									product: item.price_data.product,
									tax_behavior: item.price_data.tax_behavior,
									unit_amount: item.price_data.unit_amount == null ? undefined : Number(item.price_data.unit_amount),
								}
							: undefined,
						quantity: Number(item.quantity ?? 1),
						metadata: item.metadata ?? {},
					})),
					items: (phase.items ?? []).map((item) => ({
						price: item.price ?? 'price_test_1',
						quantity: Number(item.quantity ?? 1),
					})),
					metadata: phase.metadata ?? {},
					proration_behavior: phase.proration_behavior ?? 'none',
				}));
			}
			if (schedule.end_behavior === 'cancel') {
				const subscription = getOrCreateSubscriptionState(schedule.subscription);
				const lastPhase = schedule.phases[schedule.phases.length - 1] ?? null;
				subscription.cancel_at = lastPhase?.end_date ?? null;
				subscription.cancel_at_period_end = false;
			}
			return HttpResponse.json(mapScheduleToStripeSchedule(schedule));
		}),
		http.post(`${STRIPE_API_BASE}/v1/subscription_schedules/:id/release`, async ({params, request}) => {
			const schedule = subscriptionScheduleStore.get(params.id as string);
			if (!schedule) {
				return HttpResponse.json(
					{
						error: {
							type: 'invalid_request_error',
							message: 'No such subscription schedule',
						},
					},
					{status: 404},
				);
			}
			const formData = await request.formData();
			const releaseParams = parseFormDataToObject(formData);
			spies.releasedSubscriptionSchedules.push({id: params.id as string, params: releaseParams});
			const subscription = getOrCreateSubscriptionState(schedule.subscription);
			subscription.schedule_id = null;
			if (releaseParams.preserve_cancel_date !== 'true') {
				subscription.cancel_at = null;
				subscription.cancel_at_period_end = false;
			}
			subscriptionScheduleStore.delete(params.id as string);
			return HttpResponse.json({
				...mapScheduleToStripeSchedule(schedule),
				status: 'released',
				released_subscription: schedule.subscription,
				subscription: null,
			});
		}),
		http.get(`${STRIPE_API_BASE}/v1/subscriptions/:id`, ({params}) => {
			if (config.subscriptionShouldFail) {
				return HttpResponse.json(
					{
						error: {
							type: 'invalid_request_error',
							message: 'No such subscription',
							code: 'resource_missing',
							param: 'id',
						},
					},
					{status: 404},
				);
			}
			const {id} = params;
			spies.retrievedSubscriptions.push(id as string);
			const subState = getOrCreateSubscriptionState(id as string);
			return HttpResponse.json(mapSubscriptionStateToStripeSubscription(id as string, subState));
		}),
		http.post(`${STRIPE_API_BASE}/v1/subscriptions/:id`, async ({params, request}) => {
			if (config.subscriptionShouldFail) {
				return HttpResponse.json(
					{
						error: {
							type: 'invalid_request_error',
							message: 'Mock subscription update failure',
							code: 'resource_missing',
						},
					},
					{status: 400},
				);
			}
			const {id} = params;
			const formData = await request.formData();
			const updateParams = parseFormDataToObject(formData);
			if ('cancel_at' in updateParams && 'cancel_at_period_end' in updateParams) {
				return HttpResponse.json(
					{
						error: {
							type: 'invalid_request_error',
							message: 'Received both cancel_at_period_end and cancel_at parameters. Please pass in only one.',
							code: 'parameter_unknown',
						},
					},
					{status: 400},
				);
			}
			spies.updatedSubscriptions.push({id: id as string, params: updateParams});
			const subState = getOrCreateSubscriptionState(id as string);
			if (updateParams.trial_end) {
				subState.trial_end = Number(updateParams.trial_end);
			}
			if ('cancel_at' in updateParams) {
				subState.cancel_at =
					typeof updateParams.cancel_at === 'string' && updateParams.cancel_at !== ''
						? Number(updateParams.cancel_at)
						: null;
			}
			if (typeof updateParams.cancel_at_period_end === 'string') {
				subState.cancel_at_period_end = updateParams.cancel_at_period_end === 'true';
			}
			if (typeof updateParams.default_payment_method === 'string') {
				subState.default_payment_method = updateParams.default_payment_method;
			}
			const updatedItem = Array.isArray(updateParams.items) ? updateParams.items[0] : null;
			if (updatedItem && typeof updatedItem === 'object') {
				if (typeof updatedItem.id === 'string') {
					subState.item_id = updatedItem.id;
				}
				if (typeof updatedItem.price === 'string') {
					subState.price_id = updatedItem.price;
					const inferredState = inferSubscriptionPriceState(updatedItem.price);
					subState.currency = inferredState.currency;
					subState.interval = inferredState.interval;
					subState.current_period_start = Math.floor(Date.now() / 1000);
					subState.current_period_end =
						Math.floor(Date.now() / 1000) +
						(inferredState.interval === 'year' ? 365 * 24 * 60 * 60 : 30 * 24 * 60 * 60);
				}
			}
			subscriptionStore.set(id as string, subState);
			return HttpResponse.json({
				...mapSubscriptionStateToStripeSubscription(id as string, subState),
				metadata: updateParams.metadata || {},
			});
		}),
		http.delete(`${STRIPE_API_BASE}/v1/subscriptions/:id`, ({params}) => {
			const {id} = params;
			const subscriptionId = id as string;
			spies.cancelledSubscriptions.push(subscriptionId);
			const subState = getOrCreateSubscriptionState(subscriptionId);
			subState.status = 'canceled';
			subState.cancel_at_period_end = false;
			subscriptionStore.set(subscriptionId, subState);
			return HttpResponse.json({
				...mapSubscriptionStateToStripeSubscription(subscriptionId, subState),
				ended_at: Math.floor(Date.now() / 1000),
			});
		}),
		http.get(`${STRIPE_API_BASE}/v1/subscriptions`, ({request}) => {
			if (config.subscriptionsListEmpty) {
				return HttpResponse.json({
					object: 'list',
					url: '/v1/subscriptions',
					has_more: false,
					data: [],
				});
			}
			const requestUrl = new URL(request.url);
			const customer = requestUrl.searchParams.get('customer') ?? 'cus_test_1';
			const status = requestUrl.searchParams.get('status');
			const storedSubscriptions = [...subscriptionStore.entries()]
				.map(([subscriptionId, subState]) => mapSubscriptionStateToStripeSubscription(subscriptionId, subState))
				.filter((subscription) => subscription.customer === customer)
				.filter((subscription) => !status || status === 'all' || subscription.status === status)
				.sort((left, right) => right.items.data[0]!.current_period_end - left.items.data[0]!.current_period_end);
			if (storedSubscriptions.length > 0 || Object.keys(config.subscriptions ?? {}).length > 0) {
				return HttpResponse.json({
					object: 'list',
					url: '/v1/subscriptions',
					has_more: false,
					data: storedSubscriptions,
				});
			}
			const currentPeriodEnd = Math.floor(Date.now() / 1000) + 30 * 24 * 60 * 60;
			return HttpResponse.json({
				object: 'list',
				url: '/v1/subscriptions',
				has_more: false,
				data: [
					{
						id: 'sub_test_1',
						object: 'subscription',
						customer,
						status: 'active',
						current_period_start: Math.floor(Date.now() / 1000) - 30 * 24 * 60 * 60,
						trial_end: null,
						items: {
							object: 'list',
							data: [
								{
									id: 'si_test_1',
									object: 'subscription_item',
									price: {
										id: 'price_test_1',
										object: 'price',
										unit_amount: 2500,
										currency: 'usd',
										recurring: {
											interval: 'month',
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
							url: `/v1/subscription_items?subscription=sub_test_1`,
						},
						cancel_at: null,
						cancel_at_period_end: false,
						canceled_at: null,
						collection_method: 'charge_automatically',
						created: Math.floor(Date.now() / 1000) - 90 * 24 * 60 * 60,
						livemode: false,
						metadata: {},
						start_date: Math.floor(Date.now() / 1000) - 90 * 24 * 60 * 60,
					},
				],
			});
		}),
		http.get(`${STRIPE_API_BASE}/v1/customers/search`, ({request}) => {
			const requestUrl = new URL(request.url);
			const query = requestUrl.searchParams.get('query') ?? '';
			const metadataMatch = query.match(/metadata\['([^']+)'\]:'([^']+)'/);
			const matchedCustomers = metadataMatch
				? [...customerStore.values()].filter(
						(customer) => String(customer.metadata[metadataMatch[1]!] ?? '') === metadataMatch[2],
					)
				: [];
			return HttpResponse.json({
				object: 'search_result',
				url: '/v1/customers/search',
				has_more: false,
				next_page: null,
				data: matchedCustomers,
			});
		}),
		http.get(`${STRIPE_API_BASE}/v1/customers/:id`, ({params}) => {
			if (config.customerShouldFail) {
				return HttpResponse.json(
					{
						error: {
							type: 'invalid_request_error',
							message: 'No such customer',
							code: 'resource_missing',
							param: 'id',
						},
					},
					{status: 404},
				);
			}
			const {id} = params;
			const customerId = id as string;
			spies.retrievedCustomers.push(customerId);
			return HttpResponse.json(getCustomer(customerId));
		}),
		http.post(`${STRIPE_API_BASE}/v1/customers`, async ({request}) => {
			const formData = await request.formData();
			const email = formData.get('email') as string | null;
			const customerId = `cus_test_new_${Date.now()}`;
			spies.createdCustomers.push({id: customerId, email});
			customerStore.set(customerId, createDefaultCustomer(customerId, email));
			return HttpResponse.json(getCustomer(customerId));
		}),
		http.post(`${STRIPE_API_BASE}/v1/customers/:id`, async ({params, request}) => {
			const {id} = params;
			const customerId = id as string;
			const formData = await request.formData();
			const updateParams = parseFormDataToObject(formData);
			spies.updatedCustomers.push({id: customerId, params: updateParams});
			const customer = getCustomer(customerId);
			const invoiceSettingsUpdate =
				updateParams.invoice_settings && typeof updateParams.invoice_settings === 'object'
					? (updateParams.invoice_settings as Record<string, unknown>)
					: null;
			const updatedCustomer: MockStripeCustomer = {
				...customer,
				invoice_settings: invoiceSettingsUpdate
					? {
							...customer.invoice_settings,
							default_payment_method:
								(typeof invoiceSettingsUpdate.default_payment_method === 'string'
									? invoiceSettingsUpdate.default_payment_method
									: null) ?? customer.invoice_settings.default_payment_method,
						}
					: customer.invoice_settings,
			};
			customerStore.set(customerId, updatedCustomer);
			return HttpResponse.json(updatedCustomer);
		}),
		http.get(`${STRIPE_API_BASE}/v1/radar/value_lists`, ({request}) => {
			const requestUrl = new URL(request.url);
			const alias = requestUrl.searchParams.get('alias');
			const matchingLists = Array.from(valueListStore.values()).filter((valueList) => {
				if (!alias) {
					return true;
				}
				return valueList.alias === alias;
			});
			return HttpResponse.json({
				data: matchingLists.map((valueList) => {
					return {
						id: valueList.id,
						object: 'radar.value_list',
						alias: valueList.alias,
						created: Math.floor(Date.now() / 1000),
						created_by: 'API',
						item_type: valueList.itemType,
						list_items: {
							object: 'list',
							data: [],
							has_more: false,
							total_count: valueList.items.length,
							url: `/v1/radar/value_list_items?value_list=${valueList.id}`,
						},
						livemode: false,
						metadata: {},
						name: valueList.name,
					};
				}),
				has_more: false,
				object: 'list',
				url: '/v1/radar/value_lists',
			});
		}),
		http.post(`${STRIPE_API_BASE}/v1/radar/value_lists`, async ({request}) => {
			const formData = await request.formData();
			const alias = formData.get('alias') as string;
			const itemType = formData.get('item_type') as string;
			const name = formData.get('name') as string;
			valueListCounter++;
			const valueList: MockStripeValueList = {
				alias,
				id: `rsl_test_${valueListCounter}`,
				itemType,
				items: [],
				name,
			};
			valueListStore.set(valueList.id, valueList);
			spies.createdValueLists.push({alias, itemType, name});
			return HttpResponse.json({
				id: valueList.id,
				object: 'radar.value_list',
				alias: valueList.alias,
				created: Math.floor(Date.now() / 1000),
				created_by: 'API',
				item_type: valueList.itemType,
				list_items: {
					object: 'list',
					data: [],
					has_more: false,
					total_count: 0,
					url: `/v1/radar/value_list_items?value_list=${valueList.id}`,
				},
				livemode: false,
				metadata: {},
				name: valueList.name,
			});
		}),
		http.get(`${STRIPE_API_BASE}/v1/radar/value_list_items`, ({request}) => {
			const requestUrl = new URL(request.url);
			const valueListId = requestUrl.searchParams.get('value_list');
			const value = requestUrl.searchParams.get('value');
			const valueList = valueListId ? valueListStore.get(valueListId) : null;
			const matchingItems = (valueList?.items ?? []).filter((itemValue) => {
				if (!value) {
					return true;
				}
				return itemValue.includes(value);
			});
			return HttpResponse.json({
				data: matchingItems.map((itemValue, index) => {
					return {
						id: `rsli_test_${index + 1}`,
						object: 'radar.value_list_item',
						created: Math.floor(Date.now() / 1000),
						created_by: 'API',
						livemode: false,
						value: itemValue,
						value_list: valueListId,
					};
				}),
				has_more: false,
				object: 'list',
				url: '/v1/radar/value_list_items',
			});
		}),
		http.post(`${STRIPE_API_BASE}/v1/radar/value_list_items`, async ({request}) => {
			const formData = await request.formData();
			const value = formData.get('value') as string;
			const valueListId = formData.get('value_list') as string;
			const valueList = valueListStore.get(valueListId);
			if (!valueList) {
				return HttpResponse.json(
					{
						error: {
							type: 'invalid_request_error',
							message: 'No such value list',
						},
					},
					{status: 404},
				);
			}
			valueList.items.push(value);
			spies.createdValueListItems.push({value, valueListId});
			return HttpResponse.json({
				id: `rsli_test_${valueList.items.length}`,
				object: 'radar.value_list_item',
				created: Math.floor(Date.now() / 1000),
				created_by: 'API',
				livemode: false,
				value,
				value_list: valueListId,
			});
		}),
		http.get(`${STRIPE_API_BASE}/v1/prices/:id`, ({params}) => {
			const {id} = params;
			const normalizedPriceId = String(id).toLowerCase();
			return HttpResponse.json({
				id,
				object: 'price',
				active: true,
				currency: normalizedPriceId.includes('eur')
					? 'eur'
					: normalizedPriceId.includes('brl')
						? 'brl'
						: normalizedPriceId.includes('inr')
							? 'inr'
							: normalizedPriceId.includes('pln')
								? 'pln'
								: normalizedPriceId.includes('try')
									? 'try'
									: 'usd',
				unit_amount: normalizedPriceId.includes('year') ? 4999 : 499,
				type: 'recurring',
				recurring: {
					interval: normalizedPriceId.includes('year') ? 'year' : 'month',
					interval_count: 1,
				},
				product: 'prod_test_1',
				livemode: false,
				created: Math.floor(Date.now() / 1000) - 365 * 24 * 60 * 60,
			});
		}),
	];
	function reset() {
		spies.createdCheckoutSessions.length = 0;
		spies.createdPortalSessions.length = 0;
		spies.createdCustomers.length = 0;
		spies.createdRefunds.length = 0;
		spies.createdValueListItems.length = 0;
		spies.createdValueLists.length = 0;
		spies.retrievedCharges.length = 0;
		spies.retrievedSubscriptions.length = 0;
		spies.cancelledSubscriptions.length = 0;
		spies.retrievedCustomers.length = 0;
		spies.retrievedPaymentIntents.length = 0;
		spies.retrievedSetupIntents.length = 0;
		spies.updatedCustomers.length = 0;
		spies.updatedCharges.length = 0;
		spies.updatedSubscriptions.length = 0;
		spies.createdSubscriptionSchedules.length = 0;
		spies.updatedSubscriptionSchedules.length = 0;
		spies.releasedSubscriptionSchedules.length = 0;
		sessionCounter = 0;
		portalCounter = 0;
		refundCounter = 0;
		valueListCounter = 0;
		subscriptionScheduleCounter = 0;
		hydrateStores();
	}
	function resetAll() {
		reset();
	}
	return {handlers, spies, reset, resetAll};
}

export interface StripeWebhookEventData {
	id?: string;
	type: string;
	data: {
		object: Record<string, unknown>;
	};
	created?: number;
}

export function createMockWebhookPayload(eventData: StripeWebhookEventData): {
	payload: string;
	timestamp: number;
} {
	const timestamp = Math.floor(Date.now() / 1000);
	const event = {
		id: eventData.id ?? `evt_test_${Date.now()}`,
		object: 'event',
		api_version: STRIPE_API_VERSION,
		created: eventData.created ?? timestamp,
		type: eventData.type,
		data: eventData.data,
		livemode: false,
		pending_webhooks: 1,
		request: {
			id: `req_test_${Date.now()}`,
			idempotency_key: null,
		},
	};
	return {payload: JSON.stringify(event), timestamp};
}

export function createCheckoutCompletedEvent(options: {
	sessionId?: string;
	customerId?: string;
	customerEmail?: string;
	subscriptionId?: string;
	paymentIntentId?: string;
	setupIntentId?: string;
	amountTotal?: number;
	currency?: string;
	mode?: 'payment' | 'setup' | 'subscription';
	metadata?: Record<string, string>;
}): StripeWebhookEventData {
	const mode = options.mode ?? 'subscription';
	return {
		type: 'checkout.session.completed',
		data: {
			object: {
				id: options.sessionId ?? `cs_test_${Date.now()}`,
				object: 'checkout.session',
				customer: options.customerId ?? 'cus_test_1',
				customer_email: options.customerEmail ?? 'test@example.com',
				subscription: mode === 'subscription' ? (options.subscriptionId ?? 'sub_test_1') : null,
				payment_intent: mode === 'setup' ? null : (options.paymentIntentId ?? 'pi_test_1'),
				setup_intent: mode === 'setup' ? (options.setupIntentId ?? 'seti_test_1') : null,
				amount_total: mode === 'setup' ? null : (options.amountTotal ?? 2500),
				currency: mode === 'setup' ? null : (options.currency ?? 'usd'),
				mode,
				payment_status: mode === 'setup' ? 'no_payment_required' : 'paid',
				status: 'complete',
				metadata: options.metadata ?? {is_donation: 'true'},
			},
		},
	};
}

export function createSubscriptionUpdatedEvent(options: {
	subscriptionId?: string;
	customerId?: string;
	status?: string;
	cancelAtPeriodEnd?: boolean;
}): StripeWebhookEventData {
	const currentPeriodEnd = Math.floor(Date.now() / 1000) + 30 * 24 * 60 * 60;
	return {
		type: 'customer.subscription.updated',
		data: {
			object: {
				id: options.subscriptionId ?? 'sub_test_1',
				object: 'subscription',
				customer: options.customerId ?? 'cus_test_1',
				status: options.status ?? 'active',
				cancel_at_period_end: options.cancelAtPeriodEnd ?? false,
				items: {
					data: [{current_period_end: currentPeriodEnd}],
				},
			},
		},
	};
}

export function createSubscriptionDeletedEvent(options: {
	subscriptionId?: string;
	customerId?: string;
}): StripeWebhookEventData {
	return {
		type: 'customer.subscription.deleted',
		data: {
			object: {
				id: options.subscriptionId ?? 'sub_test_1',
				object: 'subscription',
				customer: options.customerId ?? 'cus_test_1',
				status: 'canceled',
				canceled_at: Math.floor(Date.now() / 1000),
			},
		},
	};
}

export function createInvoicePaidEvent(options: {
	invoiceId?: string;
	customerId?: string;
	subscriptionId?: string;
	amountPaid?: number;
	currency?: string;
}): StripeWebhookEventData {
	return {
		type: 'invoice.paid',
		data: {
			object: {
				id: options.invoiceId ?? `in_test_${Date.now()}`,
				object: 'invoice',
				customer: options.customerId ?? 'cus_test_1',
				subscription: options.subscriptionId ?? 'sub_test_1',
				amount_paid: options.amountPaid ?? 2500,
				currency: options.currency ?? 'usd',
				status: 'paid',
				paid: true,
			},
		},
	};
}

export function createInvoicePaymentFailedEvent(options: {
	invoiceId?: string;
	customerId?: string;
	subscriptionId?: string;
	amountDue?: number;
}): StripeWebhookEventData {
	return {
		type: 'invoice.payment_failed',
		data: {
			object: {
				id: options.invoiceId ?? `in_test_${Date.now()}`,
				object: 'invoice',
				customer: options.customerId ?? 'cus_test_1',
				subscription: options.subscriptionId ?? 'sub_test_1',
				amount_due: options.amountDue ?? 2500,
				status: 'open',
				paid: false,
				attempt_count: 1,
				next_payment_attempt: Math.floor(Date.now() / 1000) + 3 * 24 * 60 * 60,
			},
		},
	};
}

export function createInvoicePaymentActionRequiredEvent(options: {
	invoiceId?: string;
	customerId?: string;
	subscriptionId?: string;
	amountDue?: number;
}): StripeWebhookEventData {
	return {
		type: 'invoice.payment_action_required',
		data: {
			object: {
				id: options.invoiceId ?? `in_test_${Date.now()}`,
				object: 'invoice',
				customer: options.customerId ?? 'cus_test_1',
				subscription: options.subscriptionId ?? 'sub_test_1',
				amount_due: options.amountDue ?? 2500,
				status: 'open',
				paid: false,
				attempt_count: 1,
				attempted: true,
				next_payment_attempt: Math.floor(Date.now() / 1000) + 3 * 24 * 60 * 60,
			},
		},
	};
}

export function createInvoiceFinalizationFailedEvent(options: {
	invoiceId?: string;
	customerId?: string;
	subscriptionId?: string;
}): StripeWebhookEventData {
	return {
		type: 'invoice.finalization_failed',
		data: {
			object: {
				id: options.invoiceId ?? `in_test_${Date.now()}`,
				object: 'invoice',
				customer: options.customerId ?? 'cus_test_1',
				subscription: options.subscriptionId ?? 'sub_test_1',
				status: 'draft',
				paid: false,
				last_finalization_error: {
					type: 'invalid_request_error',
					message: 'Mock invoice finalization failure',
				},
			},
		},
	};
}

export function createInvoiceUpdatedEvent(options: {
	invoiceId?: string;
	customerId?: string;
	subscriptionId?: string;
	amountDue?: number;
	attemptCount?: number;
	attempted?: boolean;
	status?: string;
	paid?: boolean;
	nextPaymentAttempt?: number | null;
}): StripeWebhookEventData {
	return {
		type: 'invoice.updated',
		data: {
			object: {
				id: options.invoiceId ?? `in_test_${Date.now()}`,
				object: 'invoice',
				customer: options.customerId ?? 'cus_test_1',
				subscription: options.subscriptionId ?? 'sub_test_1',
				amount_due: options.amountDue ?? 2500,
				status: options.status ?? 'open',
				paid: options.paid ?? false,
				attempt_count: options.attemptCount ?? 1,
				attempted: options.attempted ?? true,
				next_payment_attempt:
					options.nextPaymentAttempt === undefined
						? Math.floor(Date.now() / 1000) + 3 * 24 * 60 * 60
						: options.nextPaymentAttempt,
			},
		},
	};
}
