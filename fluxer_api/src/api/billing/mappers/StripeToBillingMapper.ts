// SPDX-License-Identifier: AGPL-3.0-or-later

import type Stripe from 'stripe';
import type {
	BillingChargeByCustomerRow,
	BillingChargeRow,
	BillingCheckoutSessionByCustomerRow,
	BillingCheckoutSessionRow,
	BillingCustomerByUserIdRow,
	BillingCustomerRow,
	BillingDisputeByChargeRow,
	BillingDisputeRow,
	BillingInvoiceByCustomerRow,
	BillingInvoiceBySubscriptionRow,
	BillingInvoiceRow,
	BillingPaymentByInvoiceRow,
	BillingPaymentIntentByCustomerRow,
	BillingPaymentIntentRow,
	BillingPaymentMethodByCustomerRow,
	BillingPaymentMethodRow,
	BillingPaymentRow,
	BillingPriceRow,
	BillingProductRow,
	BillingRefundByChargeRow,
	BillingRefundByInvoiceRow,
	BillingRefundByPaymentIntentRow,
	BillingRefundRow,
	BillingSubscriptionByCustomerRow,
	BillingSubscriptionByUserRow,
	BillingSubscriptionRow,
} from '../../database/types/BillingTypes';

type BillingSubscriptionItemValue = NonNullable<BillingSubscriptionRow['items']>[number];
type BillingSubscriptionRowWithRawItems = Omit<BillingSubscriptionRow, 'items'> & {
	items: Array<unknown> | null;
};
type CassandraTupleLike = {
	readonly length: number;
	get(index: number): unknown;
};
type StripeExpandableId =
	| string
	| {
			id: string;
	  }
	| null
	| undefined;

export interface StripeSubscriptionPendingUpdatePayload {
	expires_at?: number | null;
	subscription_proration_subtotal?: number | null;
}

interface StripeSubscriptionItemPricePayload {
	id?: string | null;
	product?: StripeExpandableId;
	unit_amount?: number | null;
}

export interface StripeSubscriptionItemPayload {
	id: string;
	current_period_start?: number | null;
	current_period_end?: number | null;
	price?: StripeSubscriptionItemPricePayload | null;
	quantity?: number | null;
}

export interface StripeSubscriptionPayload {
	id?: string | null;
	customer?: StripeExpandableId;
	items?: {
		data?: ReadonlyArray<StripeSubscriptionItemPayload> | null;
	} | null;
	metadata?: Stripe.Metadata | null;
	cancellation_details?: {
		reason?: string | null;
		comment?: string | null;
	} | null;
	default_payment_method?: StripeExpandableId;
	latest_invoice?: StripeExpandableId;
	pending_update?: StripeSubscriptionPendingUpdatePayload | null;
	cancel_at?: number | null;
	cancel_at_period_end?: boolean | null;
	canceled_at?: number | null;
	collection_method?: string | null;
	created?: number | null;
	currency?: string | null;
	ended_at?: number | null;
	livemode?: boolean | null;
	start_date?: number | null;
	started_at?: number | null;
	status?: string | null;
	trial_end?: number | null;
	trial_start?: number | null;
}

function createBillingSubscriptionItemValue(
	itemId: string,
	priceId: string,
	quantity: number,
	unitAmount: bigint,
): BillingSubscriptionItemValue {
	return withCassandraTupleGetter([itemId, priceId, quantity, unitAmount]);
}

function withCassandraTupleGetter(value: BillingSubscriptionItemValue): BillingSubscriptionItemValue {
	if (!('get' in value)) {
		Object.defineProperty(value, 'get', {
			value(index: number): BillingSubscriptionItemValue[number] | undefined {
				return value[index];
			},
			enumerable: false,
		});
	}
	return value;
}

function isCassandraTupleLike(value: unknown): value is CassandraTupleLike {
	return (
		typeof value === 'object' &&
		value !== null &&
		typeof (value as {get?: unknown}).get === 'function' &&
		typeof (value as {length?: unknown}).length === 'number'
	);
}

function stringFromTupleValue(value: unknown): string {
	return typeof value === 'string' ? value : String(value ?? '');
}

function numberFromTupleValue(value: unknown): number {
	if (typeof value === 'number' && Number.isFinite(value)) {
		return value;
	}
	const parsed = Number(value);
	return Number.isFinite(parsed) ? parsed : 0;
}

function bigintFromTupleValue(value: unknown): bigint {
	if (typeof value === 'bigint') {
		return value;
	}
	if (typeof value === 'number' && Number.isFinite(value)) {
		return BigInt(value);
	}
	if (typeof value === 'string' && value.length > 0) {
		return BigInt(value);
	}
	if (typeof value === 'object' && value !== null && typeof value.toString === 'function') {
		const serialized = value.toString();
		return serialized.length > 0 ? BigInt(serialized) : 0n;
	}
	return 0n;
}

function normalizeBillingSubscriptionItemValue(value: unknown): BillingSubscriptionItemValue {
	if (Array.isArray(value)) {
		return createBillingSubscriptionItemValue(
			stringFromTupleValue(value[0]),
			stringFromTupleValue(value[1]),
			numberFromTupleValue(value[2]),
			bigintFromTupleValue(value[3]),
		);
	}
	if (isCassandraTupleLike(value)) {
		return createBillingSubscriptionItemValue(
			stringFromTupleValue(value.get(0)),
			stringFromTupleValue(value.get(1)),
			numberFromTupleValue(value.get(2)),
			bigintFromTupleValue(value.get(3)),
		);
	}
	return createBillingSubscriptionItemValue('', '', 0, 0n);
}

export function normalizeBillingSubscriptionRow(row: BillingSubscriptionRowWithRawItems): BillingSubscriptionRow {
	if (!row.items) {
		return {...row, items: null};
	}
	return {
		...row,
		items: row.items.map((item) => normalizeBillingSubscriptionItemValue(item)),
	};
}

class BillingMappingError extends Error {
	constructor(
		message: string,
		public readonly stripeId?: string,
	) {
		super(message);
		this.name = 'BillingMappingError';
	}
}

export function unixToDate(seconds: number | null | undefined): Date | null {
	if (seconds === null || seconds === undefined) {
		return null;
	}
	if (!Number.isFinite(seconds)) {
		return null;
	}
	return new Date(seconds * 1000);
}

export function safeMetadata(m: Stripe.Metadata | null | undefined): Map<string, string> | null {
	if (m === null || m === undefined) {
		return null;
	}
	const entries = Object.entries(m).filter(([, v]) => typeof v === 'string') as Array<[string, string]>;
	if (entries.length === 0) {
		return null;
	}
	return new Map(entries);
}

export function computeStripeUpdatedAt(
	obj: {
		created?: number | null;
		status_transitions?: object | null;
		canceled_at?: number | null;
		voided_at?: number | null;
		finalized_at?: number | null;
		paid_at?: number | null;
		refunded?: boolean | null;
		succeeded_at?: number | null;
		completed_at?: number | null;
	},
	options?: {floor?: Date | null},
): Date {
	const candidates: Array<number> = [];
	const push = (s: number | null | undefined) => {
		if (typeof s === 'number' && Number.isFinite(s) && s > 0) {
			candidates.push(s);
		}
	};
	push(obj.canceled_at);
	push(obj.voided_at);
	push(obj.finalized_at);
	push(obj.paid_at);
	push(obj.succeeded_at);
	push(obj.completed_at);
	if (options?.floor) {
		candidates.push(Math.floor(options.floor.getTime() / 1000));
	}
	if (obj.status_transitions) {
		for (const value of Object.values(obj.status_transitions)) {
			if (typeof value === 'number') {
				push(value);
			}
		}
	}
	push(obj.created);
	if (candidates.length === 0) {
		return new Date();
	}
	const max = Math.max(...candidates);
	return new Date(max * 1000);
}

function sumInvoiceTotalTaxes(inv: Stripe.Invoice): bigint | null {
	const taxes = inv.total_taxes ?? null;
	if (taxes === null) {
		return null;
	}
	return BigInt(taxes.reduce((total, tax) => total + (tax.amount ?? 0), 0));
}

function idOf(
	ref:
		| string
		| {
				id: string;
		  }
		| null
		| undefined,
): string | null {
	if (ref === null || ref === undefined) {
		return null;
	}
	if (typeof ref === 'string') {
		return ref;
	}
	if (
		typeof ref === 'object' &&
		typeof (
			ref as {
				id?: unknown;
			}
		).id === 'string'
	) {
		return (
			ref as {
				id: string;
			}
		).id;
	}
	return null;
}

export function mapStripeCustomerToRow(
	c: Stripe.Customer | Stripe.DeletedCustomer,
	hints?: {
		knownUserId?: bigint;
	},
): {
	primary: BillingCustomerRow;
	byUserId: BillingCustomerByUserIdRow | null;
} {
	if (!c.id) {
		throw new BillingMappingError('Customer is missing id');
	}
	const now = new Date();
	const isDeleted = (c as Stripe.DeletedCustomer).deleted === true;
	if (isDeleted) {
		const primary: BillingCustomerRow = {
			provider_id: c.id,
			user_id: hints?.knownUserId ?? null,
			email: null,
			name: null,
			description: null,
			phone: null,
			default_payment_method: null,
			invoice_prefix: null,
			currency: null,
			delinquent: null,
			balance: null,
			livemode: null,
			metadata: null,
			deleted: true,
			stripe_created_at: null,
			stripe_updated_at: now,
			mirrored_at: now,
			version: null,
		} as BillingCustomerRow;
		const byUserId: BillingCustomerByUserIdRow | null =
			hints?.knownUserId !== undefined
				? ({
						user_id: hints.knownUserId,
						provider_id: c.id,
						livemode: null,
					} as BillingCustomerByUserIdRow)
				: null;
		return {primary, byUserId};
	}
	const customer = c as Stripe.Customer;
	const defaultPaymentMethod = idOf(
		customer.invoice_settings?.default_payment_method as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const userIdFromMetadata = parseUserIdFromMetadata(customer.metadata);
	const userId = hints?.knownUserId ?? userIdFromMetadata;
	const primary: BillingCustomerRow = {
		provider_id: customer.id,
		user_id: userId,
		email: customer.email ?? null,
		name: customer.name ?? null,
		description: customer.description ?? null,
		default_payment_method: defaultPaymentMethod,
		invoice_prefix: customer.invoice_prefix ?? null,
		currency: customer.currency ?? null,
		delinquent: customer.delinquent ?? null,
		balance: typeof customer.balance === 'number' ? BigInt(customer.balance) : null,
		livemode: customer.livemode ?? null,
		metadata: safeMetadata(customer.metadata),
		deleted: false,
		stripe_created_at: unixToDate(customer.created),
		stripe_updated_at: computeStripeUpdatedAt({created: customer.created}),
		mirrored_at: now,
		version: null,
	} as BillingCustomerRow;
	const byUserId: BillingCustomerByUserIdRow | null =
		userId !== null && userId !== undefined
			? ({
					user_id: userId,
					provider_id: customer.id,
					livemode: customer.livemode ?? null,
				} as BillingCustomerByUserIdRow)
			: null;
	return {primary, byUserId};
}

function parseUserIdFromMetadata(metadata: Stripe.Metadata | null | undefined): bigint | null {
	if (!metadata) {
		return null;
	}
	const raw = metadata.user_id ?? metadata.userId;
	if (typeof raw !== 'string' || raw.length === 0) {
		return null;
	}
	try {
		return BigInt(raw);
	} catch {
		return null;
	}
}

export function mapStripeProductToRow(p: Stripe.Product): BillingProductRow {
	if (!p.id) {
		throw new BillingMappingError('Product is missing id');
	}
	const now = new Date();
	return {
		provider_id: p.id,
		name: p.name ?? null,
		description: p.description ?? null,
		active: p.active ?? null,
		livemode: p.livemode ?? null,
		metadata: safeMetadata(p.metadata),
		statement_descriptor: p.statement_descriptor ?? null,
		unit_label: p.unit_label ?? null,
		tax_code:
			typeof p.tax_code === 'string'
				? p.tax_code
				: (idOf(
						p.tax_code as
							| string
							| {
									id: string;
							  }
							| null,
					) ?? null),
		images: Array.isArray(p.images) ? p.images.slice() : null,
		stripe_created_at: unixToDate(p.created),
		stripe_updated_at: computeStripeUpdatedAt({created: p.created}),
		mirrored_at: now,
		version: null,
	} as BillingProductRow;
}

export function mapStripePriceToRow(p: Stripe.Price): BillingPriceRow {
	if (!p.id) {
		throw new BillingMappingError('Price is missing id');
	}
	const now = new Date();
	const productId = idOf(
		p.product as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	return {
		provider_id: p.id,
		product_id: productId,
		nickname: p.nickname ?? null,
		active: p.active ?? null,
		currency: p.currency ?? null,
		unit_amount: typeof p.unit_amount === 'number' ? BigInt(p.unit_amount) : null,
		billing_scheme: p.billing_scheme ?? null,
		type: p.type ?? null,
		interval: p.recurring?.interval ?? null,
		interval_count: p.recurring?.interval_count ?? null,
		usage_type: p.recurring?.usage_type ?? null,
		livemode: p.livemode ?? null,
		lookup_key: p.lookup_key ?? null,
		metadata: safeMetadata(p.metadata),
		tax_behavior: p.tax_behavior ?? null,
		stripe_created_at: unixToDate(p.created),
		stripe_updated_at: computeStripeUpdatedAt({created: p.created}),
		mirrored_at: now,
		version: null,
	} as BillingPriceRow;
}

export function mapStripePaymentMethodToRow(
	pm: Stripe.PaymentMethod,
	hints?: {
		isDefault?: boolean;
	},
): {
	primary: BillingPaymentMethodRow;
	byCustomer: BillingPaymentMethodByCustomerRow | null;
} {
	if (!pm.id) {
		throw new BillingMappingError('PaymentMethod is missing id');
	}
	const now = new Date();
	const customerId = idOf(
		pm.customer as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const card = pm.card ?? null;
	const cardWalletType =
		(
			card?.wallet as
				| {
						type?: string;
				  }
				| null
				| undefined
		)?.type ?? null;
	const usBank = pm.us_bank_account ?? null;
	const billingDetails = pm.billing_details ?? null;
	const primary: BillingPaymentMethodRow = {
		provider_id: pm.id,
		customer_id: customerId,
		type: pm.type ?? null,
		card_brand: card?.brand ?? null,
		card_last4: card?.last4 ?? null,
		card_exp_month: card?.exp_month ?? null,
		card_exp_year: card?.exp_year ?? null,
		card_funding: card?.funding ?? null,
		card_country: card?.country ?? null,
		card_fingerprint: card?.fingerprint ?? null,
		card_wallet_type: cardWalletType,
		bank_last4: usBank?.last4 ?? null,
		bank_routing: usBank?.routing_number ?? null,
		billing_email: billingDetails?.email ?? null,
		billing_name: billingDetails?.name ?? null,
		billing_country: billingDetails?.address?.country ?? null,
		billing_postal_code: billingDetails?.address?.postal_code ?? null,
		is_default: hints?.isDefault ?? null,
		livemode: pm.livemode ?? null,
		metadata: safeMetadata(pm.metadata),
		stripe_created_at: unixToDate(pm.created),
		stripe_updated_at: computeStripeUpdatedAt({created: pm.created}),
		mirrored_at: now,
		version: null,
	} as BillingPaymentMethodRow;
	const byCustomer: BillingPaymentMethodByCustomerRow | null =
		customerId !== null
			? ({
					customer_id: customerId,
					provider_id: pm.id,
					is_default: hints?.isDefault ?? null,
					type: pm.type ?? null,
					card_brand: card?.brand ?? null,
					card_last4: card?.last4 ?? null,
				} as BillingPaymentMethodByCustomerRow)
			: null;
	return {primary, byCustomer};
}

export interface MapStripeSubscriptionHints {
	knownUserId?: bigint;
	snapshotCapturedAt?: Date;
}

export function mapStripeSubscriptionToRow(
	s: StripeSubscriptionPayload,
	hints?: MapStripeSubscriptionHints,
): {
	primary: BillingSubscriptionRow;
	byCustomer: BillingSubscriptionByCustomerRow;
	byUser: BillingSubscriptionByUserRow | null;
} {
	if (!s.id) {
		throw new BillingMappingError('Subscription is missing id');
	}
	const customerId = idOf(s.customer);
	if (customerId === null) {
		throw new BillingMappingError('Subscription is missing customer', s.id);
	}
	const now = new Date();
	const items = s.items?.data ?? [];
	const firstItem = items[0] ?? null;
	const periodStart = firstItem?.current_period_start ?? null;
	const periodEnd = firstItem?.current_period_end ?? null;
	const itemTuples: Array<[string, string, number, bigint]> = items.map((item) => {
		const priceId = item.price?.id ?? '';
		const unitAmount = typeof item.price?.unit_amount === 'number' ? BigInt(item.price.unit_amount) : 0n;
		return createBillingSubscriptionItemValue(item.id, priceId, item.quantity ?? 1, unitAmount);
	});
	const primaryPriceId = firstItem?.price?.id ?? null;
	const primaryProductId = idOf(firstItem?.price?.product);
	const userIdFromMetadata = parseUserIdFromMetadata(s.metadata);
	const userId = hints?.knownUserId ?? userIdFromMetadata;
	const cancellationDetails = s.cancellation_details ?? null;
	const defaultPaymentMethod = idOf(s.default_payment_method);
	const latestInvoiceId = idOf(s.latest_invoice);
	const pendingUpdate = s.pending_update ?? null;
	const stripeUpdatedAt = computeStripeUpdatedAt(
		{
			created: s.created,
			canceled_at: s.canceled_at ?? null,
		},
		{floor: hints?.snapshotCapturedAt ?? null},
	);
	const primary: BillingSubscriptionRow = {
		provider_id: s.id,
		customer_id: customerId,
		user_id: userId,
		status: s.status ?? null,
		cancel_at_period_end: s.cancel_at_period_end ?? null,
		cancel_at: unixToDate(s.cancel_at),
		canceled_at: unixToDate(s.canceled_at),
		cancellation_reason: cancellationDetails?.reason ?? null,
		cancellation_comment: cancellationDetails?.comment ?? null,
		current_period_start: unixToDate(periodStart),
		current_period_end: unixToDate(periodEnd),
		trial_start: unixToDate(s.trial_start),
		trial_end: unixToDate(s.trial_end),
		started_at: unixToDate(s.started_at ?? s.start_date ?? null),
		ended_at: unixToDate(s.ended_at ?? null),
		primary_price_id: primaryPriceId,
		primary_product_id: primaryProductId,
		quantity: firstItem?.quantity ?? null,
		item_count: items.length,
		items: itemTuples,
		default_payment_method: defaultPaymentMethod,
		latest_invoice_id: latestInvoiceId,
		pending_update_expires_at: unixToDate(pendingUpdate?.expires_at ?? null),
		pending_update_subtotal:
			typeof pendingUpdate?.subscription_proration_subtotal === 'number'
				? BigInt(pendingUpdate.subscription_proration_subtotal)
				: null,
		collection_method: s.collection_method ?? null,
		currency: s.currency ?? null,
		livemode: s.livemode ?? null,
		metadata: safeMetadata(s.metadata),
		stripe_created_at: unixToDate(s.created),
		stripe_updated_at: stripeUpdatedAt,
		mirrored_at: now,
		version: null,
	} as BillingSubscriptionRow;
	const byCustomer: BillingSubscriptionByCustomerRow = {
		customer_id: customerId,
		provider_id: s.id,
		status: s.status ?? null,
		current_period_end: unixToDate(periodEnd),
	} as BillingSubscriptionByCustomerRow;
	const byUser: BillingSubscriptionByUserRow | null =
		userId !== null && userId !== undefined
			? ({
					user_id: userId,
					provider_id: s.id,
					customer_id: customerId,
					status: s.status ?? null,
					current_period_end: unixToDate(periodEnd),
				} as BillingSubscriptionByUserRow)
			: null;
	return {primary, byCustomer, byUser};
}

export function mapStripeInvoiceToRow(
	inv: Stripe.Invoice,
	hints?: {
		knownUserId?: bigint;
	},
): {
	primary: BillingInvoiceRow;
	byCustomer: BillingInvoiceByCustomerRow;
	bySubscription: BillingInvoiceBySubscriptionRow | null;
	payments: Array<{
		primary: BillingPaymentRow;
		byInvoice: BillingPaymentByInvoiceRow;
	}>;
} {
	if (!inv.id) {
		throw new BillingMappingError('Invoice is missing id');
	}
	const customerId = idOf(
		inv.customer as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	if (customerId === null) {
		throw new BillingMappingError('Invoice is missing customer', inv.id);
	}
	const now = new Date();
	const subscriptionId = idOf(inv.parent?.subscription_details?.subscription ?? null);
	const userIdFromMetadata = parseUserIdFromMetadata(inv.metadata);
	const userId = hints?.knownUserId ?? userIdFromMetadata;
	const transitions = inv.status_transitions ?? null;
	const stripeUpdatedAt = computeStripeUpdatedAt({
		created: inv.created,
		voided_at: transitions?.voided_at ?? null,
		paid_at: transitions?.paid_at ?? null,
		finalized_at: transitions?.finalized_at ?? null,
		status_transitions: transitions,
	});
	const paymentsRaw = inv.payments?.data ?? [];
	const paymentIds: Array<string> = paymentsRaw
		.map((payment) => payment.id)
		.filter((id): id is string => typeof id === 'string');
	const primary: BillingInvoiceRow = {
		provider_id: inv.id,
		customer_id: customerId,
		subscription_id: subscriptionId,
		user_id: userId,
		status: inv.status ?? null,
		number: inv.number ?? null,
		currency: inv.currency ?? null,
		amount_due: typeof inv.amount_due === 'number' ? BigInt(inv.amount_due) : null,
		amount_paid: typeof inv.amount_paid === 'number' ? BigInt(inv.amount_paid) : null,
		amount_remaining: typeof inv.amount_remaining === 'number' ? BigInt(inv.amount_remaining) : null,
		subtotal: typeof inv.subtotal === 'number' ? BigInt(inv.subtotal) : null,
		tax: sumInvoiceTotalTaxes(inv),
		total: typeof inv.total === 'number' ? BigInt(inv.total) : null,
		starting_balance: typeof inv.starting_balance === 'number' ? BigInt(inv.starting_balance) : null,
		ending_balance: typeof inv.ending_balance === 'number' ? BigInt(inv.ending_balance) : null,
		application_fee_amount: null,
		attempt_count: inv.attempt_count ?? null,
		attempted: inv.attempted ?? null,
		auto_advance: inv.auto_advance ?? null,
		billing_reason: inv.billing_reason ?? null,
		collection_method: inv.collection_method ?? null,
		description: inv.description ?? null,
		hosted_invoice_url: inv.hosted_invoice_url ?? null,
		invoice_pdf: inv.invoice_pdf ?? null,
		receipt_number: inv.receipt_number ?? null,
		statement_descriptor: inv.statement_descriptor ?? null,
		period_start: unixToDate(inv.period_start),
		period_end: unixToDate(inv.period_end),
		due_date: unixToDate(inv.due_date),
		finalized_at: unixToDate(transitions?.finalized_at ?? null),
		paid_at: unixToDate(transitions?.paid_at ?? null),
		voided_at: unixToDate(transitions?.voided_at ?? null),
		marked_uncollectible_at: unixToDate(transitions?.marked_uncollectible_at ?? null),
		next_payment_attempt: unixToDate(inv.next_payment_attempt),
		payment_ids: paymentIds.length > 0 ? paymentIds : null,
		livemode: inv.livemode ?? null,
		metadata: safeMetadata(inv.metadata),
		stripe_created_at: unixToDate(inv.created),
		stripe_updated_at: stripeUpdatedAt,
		mirrored_at: now,
		version: null,
	} as BillingInvoiceRow;
	const byCustomer: BillingInvoiceByCustomerRow = {
		customer_id: customerId,
		stripe_created_at: unixToDate(inv.created) ?? now,
		provider_id: inv.id,
		status: inv.status ?? null,
		total: typeof inv.total === 'number' ? BigInt(inv.total) : null,
		currency: inv.currency ?? null,
	} as BillingInvoiceByCustomerRow;
	const bySubscription: BillingInvoiceBySubscriptionRow | null =
		subscriptionId !== null
			? ({
					subscription_id: subscriptionId,
					stripe_created_at: unixToDate(inv.created) ?? now,
					provider_id: inv.id,
					status: inv.status ?? null,
					total: typeof inv.total === 'number' ? BigInt(inv.total) : null,
				} as BillingInvoiceBySubscriptionRow)
			: null;
	const payments = paymentsRaw.map((rawPayment) => {
		const p = rawPayment;
		const paymentId = p.id;
		const paymentInner = p.payment ?? null;
		const paymentIntentId = idOf(paymentInner?.payment_intent ?? null);
		const chargeId = idOf(paymentInner?.charge ?? null);
		const pStatusTransitions = p.status_transitions ?? null;
		const primaryPayment: BillingPaymentRow = {
			provider_id: paymentId,
			invoice_id: inv.id ?? null,
			customer_id: customerId,
			payment_intent_id: paymentIntentId,
			charge_id: chargeId,
			status: p.status ?? null,
			is_default: p.is_default ?? null,
			amount_paid: typeof p.amount_paid === 'number' ? BigInt(p.amount_paid) : null,
			amount_requested: typeof p.amount_requested === 'number' ? BigInt(p.amount_requested) : null,
			currency: p.currency ?? inv.currency ?? null,
			paid_at: unixToDate(pStatusTransitions?.paid_at ?? null),
			canceled_at: unixToDate(pStatusTransitions?.canceled_at ?? null),
			livemode: p.livemode ?? inv.livemode ?? null,
			stripe_created_at: unixToDate(p.created ?? null) ?? unixToDate(inv.created),
			stripe_updated_at: computeStripeUpdatedAt({
				created: p.created ?? inv.created,
				paid_at: pStatusTransitions?.paid_at ?? null,
				canceled_at: pStatusTransitions?.canceled_at ?? null,
			}),
			mirrored_at: now,
		} as BillingPaymentRow;
		const byInvoice: BillingPaymentByInvoiceRow = {
			invoice_id: inv.id ?? '',
			stripe_created_at: primaryPayment.stripe_created_at ?? now,
			provider_id: paymentId,
			payment_intent_id: paymentIntentId,
			charge_id: chargeId,
			status: p.status ?? null,
		} as BillingPaymentByInvoiceRow;
		return {primary: primaryPayment, byInvoice};
	});
	return {primary, byCustomer, bySubscription, payments};
}

export function mapStripePaymentIntentToRow(pi: Stripe.PaymentIntent): {
	primary: BillingPaymentIntentRow;
	byCustomer: BillingPaymentIntentByCustomerRow | null;
} {
	if (!pi.id) {
		throw new BillingMappingError('PaymentIntent is missing id');
	}
	const now = new Date();
	const customerId = idOf(
		pi.customer as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const invoiceId = null;
	const paymentMethodId = idOf(
		pi.payment_method as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const lastChargeId = idOf(pi.latest_charge ?? null);
	const lastPaymentError = pi.last_payment_error ?? null;
	const primary: BillingPaymentIntentRow = {
		provider_id: pi.id,
		customer_id: customerId,
		invoice_id: invoiceId,
		status: pi.status ?? null,
		amount: typeof pi.amount === 'number' ? BigInt(pi.amount) : null,
		amount_received: typeof pi.amount_received === 'number' ? BigInt(pi.amount_received) : null,
		amount_capturable: typeof pi.amount_capturable === 'number' ? BigInt(pi.amount_capturable) : null,
		currency: pi.currency ?? null,
		capture_method: pi.capture_method ?? null,
		confirmation_method: pi.confirmation_method ?? null,
		payment_method_id: paymentMethodId,
		payment_method_types: Array.isArray(pi.payment_method_types) ? pi.payment_method_types.slice() : null,
		setup_future_usage: pi.setup_future_usage ?? null,
		description: pi.description ?? null,
		receipt_email: pi.receipt_email ?? null,
		statement_descriptor: pi.statement_descriptor ?? null,
		canceled_at: unixToDate(pi.canceled_at),
		cancellation_reason: pi.cancellation_reason ?? null,
		last_charge_id: lastChargeId,
		last_payment_error_code: lastPaymentError?.code ?? null,
		last_payment_error_message: lastPaymentError?.message ?? null,
		livemode: pi.livemode ?? null,
		metadata: safeMetadata(pi.metadata),
		stripe_created_at: unixToDate(pi.created),
		stripe_updated_at: computeStripeUpdatedAt({
			created: pi.created,
			canceled_at: pi.canceled_at ?? null,
		}),
		mirrored_at: now,
	} as BillingPaymentIntentRow;
	const byCustomer: BillingPaymentIntentByCustomerRow | null =
		customerId !== null
			? ({
					customer_id: customerId,
					stripe_created_at: unixToDate(pi.created) ?? now,
					provider_id: pi.id,
					status: pi.status ?? null,
					amount: typeof pi.amount === 'number' ? BigInt(pi.amount) : null,
				} as BillingPaymentIntentByCustomerRow)
			: null;
	return {primary, byCustomer};
}

export function mapStripeChargeToRow(c: Stripe.Charge): {
	primary: BillingChargeRow;
	byCustomer: BillingChargeByCustomerRow | null;
} {
	if (!c.id) {
		throw new BillingMappingError('Charge is missing id');
	}
	const now = new Date();
	const customerId = idOf(
		c.customer as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const paymentIntentId = idOf(
		c.payment_intent as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const invoiceId = null;
	const paymentMethodId = c.payment_method ?? null;
	const card = c.payment_method_details?.card ?? null;
	const billingDetails = c.billing_details ?? null;
	const outcome = c.outcome ?? null;
	const primary: BillingChargeRow = {
		provider_id: c.id,
		customer_id: customerId,
		payment_intent_id: paymentIntentId,
		invoice_id: invoiceId,
		payment_id: null,
		status: c.status ?? null,
		amount: typeof c.amount === 'number' ? BigInt(c.amount) : null,
		amount_captured: typeof c.amount_captured === 'number' ? BigInt(c.amount_captured) : null,
		amount_refunded: typeof c.amount_refunded === 'number' ? BigInt(c.amount_refunded) : null,
		currency: c.currency ?? null,
		captured: c.captured ?? null,
		paid: c.paid ?? null,
		refunded: c.refunded ?? null,
		disputed: c.disputed ?? null,
		payment_method_id: paymentMethodId,
		payment_method_type: c.payment_method_details?.type ?? null,
		card_brand: card?.brand ?? null,
		card_last4: card?.last4 ?? null,
		card_country: card?.country ?? billingDetails?.address?.country ?? null,
		receipt_url: c.receipt_url ?? null,
		receipt_email: c.receipt_email ?? null,
		receipt_number: c.receipt_number ?? null,
		description: c.description ?? null,
		failure_code: c.failure_code ?? null,
		failure_message: c.failure_message ?? null,
		outcome_type: outcome?.type ?? null,
		outcome_risk_level: outcome?.risk_level ?? null,
		outcome_seller_message: outcome?.seller_message ?? null,
		livemode: c.livemode ?? null,
		metadata: safeMetadata(c.metadata),
		stripe_created_at: unixToDate(c.created),
		stripe_updated_at: computeStripeUpdatedAt({
			created: c.created,
			refunded: c.refunded ?? null,
		}),
		mirrored_at: now,
	} as BillingChargeRow;
	const byCustomer: BillingChargeByCustomerRow | null =
		customerId !== null
			? ({
					customer_id: customerId,
					stripe_created_at: unixToDate(c.created) ?? now,
					provider_id: c.id,
					status: c.status ?? null,
					amount: typeof c.amount === 'number' ? BigInt(c.amount) : null,
					currency: c.currency ?? null,
				} as BillingChargeByCustomerRow)
			: null;
	return {primary, byCustomer};
}

export function mapStripeRefundToRow(
	r: Stripe.Refund,
	hints?: {
		invoiceId?: string;
		customerId?: string;
		userId?: bigint;
		livemode?: boolean;
	},
): {
	primary: BillingRefundRow;
	byCharge: BillingRefundByChargeRow | null;
	byPaymentIntent: BillingRefundByPaymentIntentRow | null;
	byInvoice: BillingRefundByInvoiceRow | null;
} {
	if (!r.id) {
		throw new BillingMappingError('Refund is missing id');
	}
	const now = new Date();
	const chargeId = typeof r.charge === 'string' ? r.charge : (r.charge?.id ?? null);
	const paymentIntentId = typeof r.payment_intent === 'string' ? r.payment_intent : (r.payment_intent?.id ?? null);
	const invoiceId = hints?.invoiceId ?? null;
	const customerId = hints?.customerId ?? null;
	const userId = hints?.userId ?? null;
	const primary: BillingRefundRow = {
		provider_id: r.id,
		charge_id: chargeId,
		payment_intent_id: paymentIntentId,
		invoice_id: invoiceId,
		customer_id: customerId,
		user_id: userId,
		status: r.status ?? null,
		amount: typeof r.amount === 'number' ? BigInt(r.amount) : null,
		currency: r.currency ?? null,
		reason: r.reason ?? null,
		receipt_number: r.receipt_number ?? null,
		failure_reason: r.failure_reason ?? null,
		description: r.description ?? null,
		livemode: hints?.livemode ?? null,
		metadata: safeMetadata(r.metadata),
		stripe_created_at: unixToDate(r.created),
		stripe_updated_at: computeStripeUpdatedAt({created: r.created}),
		mirrored_at: now,
	} as BillingRefundRow;
	const createdAtForLookup = unixToDate(r.created) ?? now;
	const byCharge: BillingRefundByChargeRow | null =
		chargeId !== null
			? ({
					charge_id: chargeId,
					stripe_created_at: createdAtForLookup,
					provider_id: r.id,
					status: r.status ?? null,
					amount: typeof r.amount === 'number' ? BigInt(r.amount) : null,
				} as BillingRefundByChargeRow)
			: null;
	const byPaymentIntent: BillingRefundByPaymentIntentRow | null =
		paymentIntentId !== null
			? ({
					payment_intent_id: paymentIntentId,
					stripe_created_at: createdAtForLookup,
					provider_id: r.id,
					status: r.status ?? null,
					amount: typeof r.amount === 'number' ? BigInt(r.amount) : null,
				} as BillingRefundByPaymentIntentRow)
			: null;
	const byInvoice: BillingRefundByInvoiceRow | null =
		invoiceId !== null
			? ({
					invoice_id: invoiceId,
					stripe_created_at: createdAtForLookup,
					provider_id: r.id,
					status: r.status ?? null,
					amount: typeof r.amount === 'number' ? BigInt(r.amount) : null,
				} as BillingRefundByInvoiceRow)
			: null;
	return {primary, byCharge, byPaymentIntent, byInvoice};
}

export function mapStripeCheckoutSessionToRow(
	cs: Stripe.Checkout.Session,
	hints?: {
		knownUserId?: bigint;
		eventCreated?: number;
	},
): {
	primary: BillingCheckoutSessionRow;
	byCustomer: BillingCheckoutSessionByCustomerRow | null;
} {
	const completedAt = cs.status === 'complete' ? (hints?.eventCreated ?? null) : null;
	if (!cs.id) {
		throw new BillingMappingError('Checkout session is missing id');
	}
	const now = new Date();
	const customerId = idOf(
		cs.customer as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const subscriptionId = idOf(
		cs.subscription as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const paymentIntentId = idOf(
		cs.payment_intent as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const setupIntentId = idOf(
		cs.setup_intent as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const invoiceId = idOf(
		cs.invoice as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const userIdFromMetadata = parseUserIdFromMetadata(cs.metadata);
	const userId = hints?.knownUserId ?? userIdFromMetadata;
	const primary: BillingCheckoutSessionRow = {
		provider_id: cs.id,
		customer_id: customerId,
		user_id: userId,
		mode: cs.mode ?? null,
		status: cs.status ?? null,
		payment_status: cs.payment_status ?? null,
		subscription_id: subscriptionId,
		payment_intent_id: paymentIntentId,
		setup_intent_id: setupIntentId,
		invoice_id: invoiceId,
		success_url: cs.success_url ?? null,
		cancel_url: cs.cancel_url ?? null,
		customer_email: cs.customer_email ?? cs.customer_details?.email ?? null,
		amount_subtotal: typeof cs.amount_subtotal === 'number' ? BigInt(cs.amount_subtotal) : null,
		amount_total: typeof cs.amount_total === 'number' ? BigInt(cs.amount_total) : null,
		currency: cs.currency ?? null,
		expires_at: unixToDate(cs.expires_at),
		completed_at: completedAt === null ? null : unixToDate(completedAt),
		livemode: cs.livemode ?? null,
		client_reference_id: cs.client_reference_id ?? null,
		metadata: safeMetadata(cs.metadata),
		stripe_created_at: unixToDate(cs.created),
		stripe_updated_at: computeStripeUpdatedAt({
			created: cs.created,
			completed_at: completedAt,
		}),
		mirrored_at: now,
	} as BillingCheckoutSessionRow;
	const byCustomer: BillingCheckoutSessionByCustomerRow | null =
		customerId !== null
			? ({
					customer_id: customerId,
					stripe_created_at: unixToDate(cs.created) ?? now,
					provider_id: cs.id,
					status: cs.status ?? null,
					mode: cs.mode ?? null,
				} as BillingCheckoutSessionByCustomerRow)
			: null;
	return {primary, byCustomer};
}

export function mapStripeDisputeToRow(
	d: Stripe.Dispute,
	hints?: {
		customerId?: string;
		userId?: bigint;
	},
): {
	primary: BillingDisputeRow;
	byCharge: BillingDisputeByChargeRow | null;
} {
	if (!d.id) {
		throw new BillingMappingError('Dispute is missing id');
	}
	const now = new Date();
	const chargeId = idOf(
		d.charge as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const paymentIntentId = idOf(
		d.payment_intent as
			| string
			| {
					id: string;
			  }
			| null
			| undefined,
	);
	const customerId = hints?.customerId ?? null;
	const userId = hints?.userId ?? null;
	const evidenceDetails = d.evidence_details ?? null;
	const primary: BillingDisputeRow = {
		provider_id: d.id,
		charge_id: chargeId,
		payment_intent_id: paymentIntentId,
		customer_id: customerId,
		user_id: userId,
		status: d.status ?? null,
		reason: d.reason ?? null,
		amount: typeof d.amount === 'number' ? BigInt(d.amount) : null,
		currency: d.currency ?? null,
		is_charge_refundable: d.is_charge_refundable ?? null,
		evidence_due_by: unixToDate(evidenceDetails?.due_by ?? null),
		evidence_submission_count: evidenceDetails?.submission_count ?? null,
		livemode: d.livemode ?? null,
		metadata: safeMetadata(d.metadata),
		stripe_created_at: unixToDate(d.created),
		stripe_updated_at: computeStripeUpdatedAt({created: d.created}),
		mirrored_at: now,
	} as BillingDisputeRow;
	const byCharge: BillingDisputeByChargeRow | null =
		chargeId !== null
			? ({
					charge_id: chargeId,
					provider_id: d.id,
					status: d.status ?? null,
					amount: typeof d.amount === 'number' ? BigInt(d.amount) : null,
				} as BillingDisputeByChargeRow)
			: null;
	return {primary, byCharge};
}
