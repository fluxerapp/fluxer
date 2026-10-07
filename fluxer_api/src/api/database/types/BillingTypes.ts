// SPDX-License-Identifier: AGPL-3.0-or-later

type BillingSubscriptionStatus =
	| 'active'
	| 'trialing'
	| 'past_due'
	| 'canceled'
	| 'unpaid'
	| 'incomplete'
	| 'incomplete_expired'
	| 'paused';
type BillingInvoiceStatus = 'draft' | 'open' | 'paid' | 'void' | 'uncollectible';
type BillingRefundStatus = 'pending' | 'succeeded' | 'failed' | 'canceled' | 'requires_action';
type BillingPaymentIntentStatus =
	| 'requires_payment_method'
	| 'requires_confirmation'
	| 'requires_action'
	| 'processing'
	| 'requires_capture'
	| 'canceled'
	| 'succeeded';
type BillingDisputeStatus =
	| 'warning_needs_response'
	| 'warning_under_review'
	| 'warning_closed'
	| 'needs_response'
	| 'under_review'
	| 'won'
	| 'lost';
type BillingCheckoutSessionStatus = 'open' | 'complete' | 'expired';
export type BillingActionIntentStatus = 'pending' | 'sub_canceled' | 'refund_created' | 'complete' | 'failed';
type BillingPaymentStatus = 'open' | 'paid' | 'canceled' | 'abandoned';
type BillingChargeStatus = 'succeeded' | 'pending' | 'failed';
export type BillingActionType = 'cancel_and_refund' | 'cancel_immediate' | 'refund_only';

export interface BillingCustomerRow {
	provider_id: string;
	user_id: bigint | null;
	email: string | null;
	name: string | null;
	description: string | null;
	default_payment_method: string | null;
	invoice_prefix: string | null;
	currency: string | null;
	delinquent: boolean | null;
	balance: bigint | null;
	livemode: boolean | null;
	metadata: Map<string, string> | null;
	deleted: boolean | null;
	stripe_created_at: Date | null;
	stripe_updated_at: Date | null;
	mirrored_at: Date | null;
	version: bigint | null;
}

export const BILLING_CUSTOMER_COLUMNS = [
	'provider_id',
	'user_id',
	'email',
	'name',
	'description',
	'default_payment_method',
	'invoice_prefix',
	'currency',
	'delinquent',
	'balance',
	'livemode',
	'metadata',
	'deleted',
	'stripe_created_at',
	'stripe_updated_at',
	'mirrored_at',
	'version',
] as const satisfies ReadonlyArray<keyof BillingCustomerRow>;

export interface BillingCustomerByUserIdRow {
	user_id: bigint;
	provider_id: string;
	livemode: boolean | null;
}

export const BILLING_CUSTOMER_BY_USER_ID_COLUMNS = [
	'user_id',
	'provider_id',
	'livemode',
] as const satisfies ReadonlyArray<keyof BillingCustomerByUserIdRow>;

export interface BillingProductRow {
	provider_id: string;
	name: string | null;
	description: string | null;
	active: boolean | null;
	livemode: boolean | null;
	metadata: Map<string, string> | null;
	statement_descriptor: string | null;
	unit_label: string | null;
	tax_code: string | null;
	images: Array<string> | null;
	stripe_created_at: Date | null;
	stripe_updated_at: Date | null;
	mirrored_at: Date | null;
	version: bigint | null;
}

export const BILLING_PRODUCT_COLUMNS = [
	'provider_id',
	'name',
	'description',
	'active',
	'livemode',
	'metadata',
	'statement_descriptor',
	'unit_label',
	'tax_code',
	'images',
	'stripe_created_at',
	'stripe_updated_at',
	'mirrored_at',
	'version',
] as const satisfies ReadonlyArray<keyof BillingProductRow>;

export interface BillingPriceRow {
	provider_id: string;
	product_id: string | null;
	nickname: string | null;
	active: boolean | null;
	currency: string | null;
	unit_amount: bigint | null;
	billing_scheme: string | null;
	type: string | null;
	interval: string | null;
	interval_count: number | null;
	usage_type: string | null;
	livemode: boolean | null;
	lookup_key: string | null;
	metadata: Map<string, string> | null;
	tax_behavior: string | null;
	stripe_created_at: Date | null;
	stripe_updated_at: Date | null;
	mirrored_at: Date | null;
	version: bigint | null;
}

export const BILLING_PRICE_COLUMNS = [
	'provider_id',
	'product_id',
	'nickname',
	'active',
	'currency',
	'unit_amount',
	'billing_scheme',
	'type',
	'interval',
	'interval_count',
	'usage_type',
	'livemode',
	'lookup_key',
	'metadata',
	'tax_behavior',
	'stripe_created_at',
	'stripe_updated_at',
	'mirrored_at',
	'version',
] as const satisfies ReadonlyArray<keyof BillingPriceRow>;

export interface BillingPaymentMethodRow {
	provider_id: string;
	customer_id: string | null;
	type: string | null;
	card_brand: string | null;
	card_last4: string | null;
	card_exp_month: number | null;
	card_exp_year: number | null;
	card_funding: string | null;
	card_country: string | null;
	card_fingerprint: string | null;
	card_wallet_type: string | null;
	bank_last4: string | null;
	bank_routing: string | null;
	billing_email: string | null;
	billing_name: string | null;
	billing_country: string | null;
	billing_postal_code: string | null;
	is_default: boolean | null;
	livemode: boolean | null;
	metadata: Map<string, string> | null;
	stripe_created_at: Date | null;
	stripe_updated_at: Date | null;
	mirrored_at: Date | null;
	version: bigint | null;
}

export const BILLING_PAYMENT_METHOD_COLUMNS = [
	'provider_id',
	'customer_id',
	'type',
	'card_brand',
	'card_last4',
	'card_exp_month',
	'card_exp_year',
	'card_funding',
	'card_country',
	'card_fingerprint',
	'card_wallet_type',
	'bank_last4',
	'bank_routing',
	'billing_email',
	'billing_name',
	'billing_country',
	'billing_postal_code',
	'is_default',
	'livemode',
	'metadata',
	'stripe_created_at',
	'stripe_updated_at',
	'mirrored_at',
	'version',
] as const satisfies ReadonlyArray<keyof BillingPaymentMethodRow>;

export interface BillingPaymentMethodByCustomerRow {
	customer_id: string;
	provider_id: string;
	is_default: boolean | null;
	type: string | null;
	card_brand: string | null;
	card_last4: string | null;
}

export const BILLING_PAYMENT_METHOD_BY_CUSTOMER_COLUMNS = [
	'customer_id',
	'provider_id',
	'is_default',
	'type',
	'card_brand',
	'card_last4',
] as const satisfies ReadonlyArray<keyof BillingPaymentMethodByCustomerRow>;

export interface BillingSubscriptionRow {
	provider_id: string;
	customer_id: string | null;
	user_id: bigint | null;
	status: BillingSubscriptionStatus | null;
	cancel_at_period_end: boolean | null;
	cancel_at: Date | null;
	canceled_at: Date | null;
	cancellation_reason: string | null;
	cancellation_comment: string | null;
	current_period_start: Date | null;
	current_period_end: Date | null;
	trial_start: Date | null;
	trial_end: Date | null;
	started_at: Date | null;
	ended_at: Date | null;
	primary_price_id: string | null;
	primary_product_id: string | null;
	quantity: number | null;
	item_count: number | null;
	items: Array<[string, string, number, bigint]> | null;
	default_payment_method: string | null;
	latest_invoice_id: string | null;
	pending_update_expires_at: Date | null;
	pending_update_subtotal: bigint | null;
	collection_method: string | null;
	currency: string | null;
	livemode: boolean | null;
	metadata: Map<string, string> | null;
	stripe_created_at: Date | null;
	stripe_updated_at: Date | null;
	mirrored_at: Date | null;
	version: bigint | null;
}

export const BILLING_SUBSCRIPTION_COLUMNS = [
	'provider_id',
	'customer_id',
	'user_id',
	'status',
	'cancel_at_period_end',
	'cancel_at',
	'canceled_at',
	'cancellation_reason',
	'cancellation_comment',
	'current_period_start',
	'current_period_end',
	'trial_start',
	'trial_end',
	'started_at',
	'ended_at',
	'primary_price_id',
	'primary_product_id',
	'quantity',
	'item_count',
	'items',
	'default_payment_method',
	'latest_invoice_id',
	'pending_update_expires_at',
	'pending_update_subtotal',
	'collection_method',
	'currency',
	'livemode',
	'metadata',
	'stripe_created_at',
	'stripe_updated_at',
	'mirrored_at',
	'version',
] as const satisfies ReadonlyArray<keyof BillingSubscriptionRow>;

export interface BillingSubscriptionByCustomerRow {
	customer_id: string;
	provider_id: string;
	status: BillingSubscriptionStatus | null;
	current_period_end: Date | null;
}

export const BILLING_SUBSCRIPTION_BY_CUSTOMER_COLUMNS = [
	'customer_id',
	'provider_id',
	'status',
	'current_period_end',
] as const satisfies ReadonlyArray<keyof BillingSubscriptionByCustomerRow>;

export interface BillingSubscriptionByUserRow {
	user_id: bigint;
	provider_id: string;
	customer_id: string | null;
	status: BillingSubscriptionStatus | null;
	current_period_end: Date | null;
}

export const BILLING_SUBSCRIPTION_BY_USER_COLUMNS = [
	'user_id',
	'provider_id',
	'customer_id',
	'status',
	'current_period_end',
] as const satisfies ReadonlyArray<keyof BillingSubscriptionByUserRow>;

export interface BillingInvoiceRow {
	provider_id: string;
	customer_id: string | null;
	subscription_id: string | null;
	user_id: bigint | null;
	status: BillingInvoiceStatus | null;
	number: string | null;
	currency: string | null;
	amount_due: bigint | null;
	amount_paid: bigint | null;
	amount_remaining: bigint | null;
	subtotal: bigint | null;
	tax: bigint | null;
	total: bigint | null;
	starting_balance: bigint | null;
	ending_balance: bigint | null;
	application_fee_amount: bigint | null;
	attempt_count: number | null;
	attempted: boolean | null;
	auto_advance: boolean | null;
	billing_reason: string | null;
	collection_method: string | null;
	description: string | null;
	hosted_invoice_url: string | null;
	invoice_pdf: string | null;
	receipt_number: string | null;
	statement_descriptor: string | null;
	period_start: Date | null;
	period_end: Date | null;
	due_date: Date | null;
	finalized_at: Date | null;
	paid_at: Date | null;
	voided_at: Date | null;
	marked_uncollectible_at: Date | null;
	next_payment_attempt: Date | null;
	payment_ids: Array<string> | null;
	livemode: boolean | null;
	metadata: Map<string, string> | null;
	stripe_created_at: Date | null;
	stripe_updated_at: Date | null;
	mirrored_at: Date | null;
	version: bigint | null;
}

export const BILLING_INVOICE_COLUMNS = [
	'provider_id',
	'customer_id',
	'subscription_id',
	'user_id',
	'status',
	'number',
	'currency',
	'amount_due',
	'amount_paid',
	'amount_remaining',
	'subtotal',
	'tax',
	'total',
	'starting_balance',
	'ending_balance',
	'application_fee_amount',
	'attempt_count',
	'attempted',
	'auto_advance',
	'billing_reason',
	'collection_method',
	'description',
	'hosted_invoice_url',
	'invoice_pdf',
	'receipt_number',
	'statement_descriptor',
	'period_start',
	'period_end',
	'due_date',
	'finalized_at',
	'paid_at',
	'voided_at',
	'marked_uncollectible_at',
	'next_payment_attempt',
	'payment_ids',
	'livemode',
	'metadata',
	'stripe_created_at',
	'stripe_updated_at',
	'mirrored_at',
	'version',
] as const satisfies ReadonlyArray<keyof BillingInvoiceRow>;

export interface BillingInvoiceByCustomerRow {
	customer_id: string;
	stripe_created_at: Date;
	provider_id: string;
	status: BillingInvoiceStatus | null;
	total: bigint | null;
	currency: string | null;
}

export const BILLING_INVOICE_BY_CUSTOMER_COLUMNS = [
	'customer_id',
	'stripe_created_at',
	'provider_id',
	'status',
	'total',
	'currency',
] as const satisfies ReadonlyArray<keyof BillingInvoiceByCustomerRow>;

export interface BillingInvoiceBySubscriptionRow {
	subscription_id: string;
	stripe_created_at: Date;
	provider_id: string;
	status: BillingInvoiceStatus | null;
	total: bigint | null;
}

export const BILLING_INVOICE_BY_SUBSCRIPTION_COLUMNS = [
	'subscription_id',
	'stripe_created_at',
	'provider_id',
	'status',
	'total',
] as const satisfies ReadonlyArray<keyof BillingInvoiceBySubscriptionRow>;

export interface BillingPaymentIntentRow {
	provider_id: string;
	customer_id: string | null;
	invoice_id: string | null;
	status: BillingPaymentIntentStatus | null;
	amount: bigint | null;
	amount_received: bigint | null;
	amount_capturable: bigint | null;
	currency: string | null;
	capture_method: string | null;
	confirmation_method: string | null;
	payment_method_id: string | null;
	payment_method_types: Array<string> | null;
	setup_future_usage: string | null;
	description: string | null;
	receipt_email: string | null;
	statement_descriptor: string | null;
	canceled_at: Date | null;
	cancellation_reason: string | null;
	last_charge_id: string | null;
	last_payment_error_code: string | null;
	last_payment_error_message: string | null;
	livemode: boolean | null;
	metadata: Map<string, string> | null;
	stripe_created_at: Date | null;
	stripe_updated_at: Date | null;
	mirrored_at: Date | null;
}

export const BILLING_PAYMENT_INTENT_COLUMNS = [
	'provider_id',
	'customer_id',
	'invoice_id',
	'status',
	'amount',
	'amount_received',
	'amount_capturable',
	'currency',
	'capture_method',
	'confirmation_method',
	'payment_method_id',
	'payment_method_types',
	'setup_future_usage',
	'description',
	'receipt_email',
	'statement_descriptor',
	'canceled_at',
	'cancellation_reason',
	'last_charge_id',
	'last_payment_error_code',
	'last_payment_error_message',
	'livemode',
	'metadata',
	'stripe_created_at',
	'stripe_updated_at',
	'mirrored_at',
] as const satisfies ReadonlyArray<keyof BillingPaymentIntentRow>;

export interface BillingPaymentIntentByCustomerRow {
	customer_id: string;
	stripe_created_at: Date;
	provider_id: string;
	status: BillingPaymentIntentStatus | null;
	amount: bigint | null;
}

export const BILLING_PAYMENT_INTENT_BY_CUSTOMER_COLUMNS = [
	'customer_id',
	'stripe_created_at',
	'provider_id',
	'status',
	'amount',
] as const satisfies ReadonlyArray<keyof BillingPaymentIntentByCustomerRow>;

export interface BillingChargeRow {
	provider_id: string;
	customer_id: string | null;
	payment_intent_id: string | null;
	invoice_id: string | null;
	payment_id: string | null;
	status: BillingChargeStatus | null;
	amount: bigint | null;
	amount_captured: bigint | null;
	amount_refunded: bigint | null;
	currency: string | null;
	captured: boolean | null;
	paid: boolean | null;
	refunded: boolean | null;
	disputed: boolean | null;
	payment_method_id: string | null;
	payment_method_type: string | null;
	card_brand: string | null;
	card_last4: string | null;
	card_country: string | null;
	receipt_url: string | null;
	receipt_email: string | null;
	receipt_number: string | null;
	description: string | null;
	failure_code: string | null;
	failure_message: string | null;
	outcome_type: string | null;
	outcome_risk_level: string | null;
	outcome_seller_message: string | null;
	livemode: boolean | null;
	metadata: Map<string, string> | null;
	stripe_created_at: Date | null;
	stripe_updated_at: Date | null;
	mirrored_at: Date | null;
}

export const BILLING_CHARGE_COLUMNS = [
	'provider_id',
	'customer_id',
	'payment_intent_id',
	'invoice_id',
	'payment_id',
	'status',
	'amount',
	'amount_captured',
	'amount_refunded',
	'currency',
	'captured',
	'paid',
	'refunded',
	'disputed',
	'payment_method_id',
	'payment_method_type',
	'card_brand',
	'card_last4',
	'card_country',
	'receipt_url',
	'receipt_email',
	'receipt_number',
	'description',
	'failure_code',
	'failure_message',
	'outcome_type',
	'outcome_risk_level',
	'outcome_seller_message',
	'livemode',
	'metadata',
	'stripe_created_at',
	'stripe_updated_at',
	'mirrored_at',
] as const satisfies ReadonlyArray<keyof BillingChargeRow>;

export interface BillingChargeByCustomerRow {
	customer_id: string;
	stripe_created_at: Date;
	provider_id: string;
	status: BillingChargeStatus | null;
	amount: bigint | null;
	currency: string | null;
}

export const BILLING_CHARGE_BY_CUSTOMER_COLUMNS = [
	'customer_id',
	'stripe_created_at',
	'provider_id',
	'status',
	'amount',
	'currency',
] as const satisfies ReadonlyArray<keyof BillingChargeByCustomerRow>;

export interface BillingPaymentRow {
	provider_id: string;
	invoice_id: string | null;
	customer_id: string | null;
	payment_intent_id: string | null;
	charge_id: string | null;
	status: BillingPaymentStatus | null;
	is_default: boolean | null;
	amount_paid: bigint | null;
	amount_requested: bigint | null;
	currency: string | null;
	paid_at: Date | null;
	canceled_at: Date | null;
	livemode: boolean | null;
	stripe_created_at: Date | null;
	stripe_updated_at: Date | null;
	mirrored_at: Date | null;
}

export const BILLING_PAYMENT_COLUMNS = [
	'provider_id',
	'invoice_id',
	'customer_id',
	'payment_intent_id',
	'charge_id',
	'status',
	'is_default',
	'amount_paid',
	'amount_requested',
	'currency',
	'paid_at',
	'canceled_at',
	'livemode',
	'stripe_created_at',
	'stripe_updated_at',
	'mirrored_at',
] as const satisfies ReadonlyArray<keyof BillingPaymentRow>;

export interface BillingPaymentByInvoiceRow {
	invoice_id: string;
	stripe_created_at: Date;
	provider_id: string;
	payment_intent_id: string | null;
	charge_id: string | null;
	status: BillingPaymentStatus | null;
}

export const BILLING_PAYMENT_BY_INVOICE_COLUMNS = [
	'invoice_id',
	'stripe_created_at',
	'provider_id',
	'payment_intent_id',
	'charge_id',
	'status',
] as const satisfies ReadonlyArray<keyof BillingPaymentByInvoiceRow>;

export interface BillingRefundRow {
	provider_id: string;
	charge_id: string | null;
	payment_intent_id: string | null;
	invoice_id: string | null;
	customer_id: string | null;
	user_id: bigint | null;
	status: BillingRefundStatus | null;
	amount: bigint | null;
	currency: string | null;
	reason: string | null;
	receipt_number: string | null;
	failure_reason: string | null;
	description: string | null;
	livemode: boolean | null;
	metadata: Map<string, string> | null;
	stripe_created_at: Date | null;
	stripe_updated_at: Date | null;
	mirrored_at: Date | null;
}

export const BILLING_REFUND_COLUMNS = [
	'provider_id',
	'charge_id',
	'payment_intent_id',
	'invoice_id',
	'customer_id',
	'user_id',
	'status',
	'amount',
	'currency',
	'reason',
	'receipt_number',
	'failure_reason',
	'description',
	'livemode',
	'metadata',
	'stripe_created_at',
	'stripe_updated_at',
	'mirrored_at',
] as const satisfies ReadonlyArray<keyof BillingRefundRow>;

export interface BillingRefundByChargeRow {
	charge_id: string;
	stripe_created_at: Date;
	provider_id: string;
	status: BillingRefundStatus | null;
	amount: bigint | null;
}

export const BILLING_REFUND_BY_CHARGE_COLUMNS = [
	'charge_id',
	'stripe_created_at',
	'provider_id',
	'status',
	'amount',
] as const satisfies ReadonlyArray<keyof BillingRefundByChargeRow>;

export interface BillingRefundByPaymentIntentRow {
	payment_intent_id: string;
	stripe_created_at: Date;
	provider_id: string;
	status: BillingRefundStatus | null;
	amount: bigint | null;
}

export const BILLING_REFUND_BY_PAYMENT_INTENT_COLUMNS = [
	'payment_intent_id',
	'stripe_created_at',
	'provider_id',
	'status',
	'amount',
] as const satisfies ReadonlyArray<keyof BillingRefundByPaymentIntentRow>;

export interface BillingRefundByInvoiceRow {
	invoice_id: string;
	stripe_created_at: Date;
	provider_id: string;
	status: BillingRefundStatus | null;
	amount: bigint | null;
}

export const BILLING_REFUND_BY_INVOICE_COLUMNS = [
	'invoice_id',
	'stripe_created_at',
	'provider_id',
	'status',
	'amount',
] as const satisfies ReadonlyArray<keyof BillingRefundByInvoiceRow>;

export interface BillingCheckoutSessionRow {
	provider_id: string;
	customer_id: string | null;
	user_id: bigint | null;
	mode: string | null;
	status: BillingCheckoutSessionStatus | null;
	payment_status: string | null;
	subscription_id: string | null;
	payment_intent_id: string | null;
	setup_intent_id: string | null;
	invoice_id: string | null;
	success_url: string | null;
	cancel_url: string | null;
	customer_email: string | null;
	amount_subtotal: bigint | null;
	amount_total: bigint | null;
	currency: string | null;
	expires_at: Date | null;
	completed_at: Date | null;
	livemode: boolean | null;
	client_reference_id: string | null;
	metadata: Map<string, string> | null;
	stripe_created_at: Date | null;
	stripe_updated_at: Date | null;
	mirrored_at: Date | null;
}

export const BILLING_CHECKOUT_SESSION_COLUMNS = [
	'provider_id',
	'customer_id',
	'user_id',
	'mode',
	'status',
	'payment_status',
	'subscription_id',
	'payment_intent_id',
	'setup_intent_id',
	'invoice_id',
	'success_url',
	'cancel_url',
	'customer_email',
	'amount_subtotal',
	'amount_total',
	'currency',
	'expires_at',
	'completed_at',
	'livemode',
	'client_reference_id',
	'metadata',
	'stripe_created_at',
	'stripe_updated_at',
	'mirrored_at',
] as const satisfies ReadonlyArray<keyof BillingCheckoutSessionRow>;

export interface BillingCheckoutSessionByCustomerRow {
	customer_id: string;
	stripe_created_at: Date;
	provider_id: string;
	status: BillingCheckoutSessionStatus | null;
	mode: string | null;
}

export const BILLING_CHECKOUT_SESSION_BY_CUSTOMER_COLUMNS = [
	'customer_id',
	'stripe_created_at',
	'provider_id',
	'status',
	'mode',
] as const satisfies ReadonlyArray<keyof BillingCheckoutSessionByCustomerRow>;

export interface BillingDisputeRow {
	provider_id: string;
	charge_id: string | null;
	payment_intent_id: string | null;
	customer_id: string | null;
	user_id: bigint | null;
	status: BillingDisputeStatus | null;
	reason: string | null;
	amount: bigint | null;
	currency: string | null;
	is_charge_refundable: boolean | null;
	evidence_due_by: Date | null;
	evidence_submission_count: number | null;
	livemode: boolean | null;
	metadata: Map<string, string> | null;
	stripe_created_at: Date | null;
	stripe_updated_at: Date | null;
	mirrored_at: Date | null;
}

export const BILLING_DISPUTE_COLUMNS = [
	'provider_id',
	'charge_id',
	'payment_intent_id',
	'customer_id',
	'user_id',
	'status',
	'reason',
	'amount',
	'currency',
	'is_charge_refundable',
	'evidence_due_by',
	'evidence_submission_count',
	'livemode',
	'metadata',
	'stripe_created_at',
	'stripe_updated_at',
	'mirrored_at',
] as const satisfies ReadonlyArray<keyof BillingDisputeRow>;

export interface BillingDisputeByChargeRow {
	charge_id: string;
	provider_id: string;
	status: BillingDisputeStatus | null;
	amount: bigint | null;
}

export const BILLING_DISPUTE_BY_CHARGE_COLUMNS = [
	'charge_id',
	'provider_id',
	'status',
	'amount',
] as const satisfies ReadonlyArray<keyof BillingDisputeByChargeRow>;

export interface BillingActionIntentRow {
	intent_id: bigint;
	user_id: bigint | null;
	actor_admin_id: bigint | null;
	action_type: BillingActionType | null;
	subscription_id: string | null;
	invoice_id: string | null;
	payment_intent_id: string | null;
	refund_amount: bigint | null;
	refund_reason: string | null;
	status: BillingActionIntentStatus | null;
	error_message: string | null;
	started_at: Date | null;
	sub_canceled_at: Date | null;
	refund_created_at: Date | null;
	completed_at: Date | null;
	refund_id: string | null;
}

export const BILLING_ACTION_INTENT_COLUMNS = [
	'intent_id',
	'user_id',
	'actor_admin_id',
	'action_type',
	'subscription_id',
	'invoice_id',
	'payment_intent_id',
	'refund_amount',
	'refund_reason',
	'status',
	'error_message',
	'started_at',
	'sub_canceled_at',
	'refund_created_at',
	'completed_at',
	'refund_id',
] as const satisfies ReadonlyArray<keyof BillingActionIntentRow>;
