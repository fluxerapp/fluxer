// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';

export type GiftCodeDurationType = 'days' | 'weeks' | 'months' | 'years';

export interface GiftCodeRow {
	code: string;
	duration_months: number | null;
	duration_type?: GiftCodeDurationType | null;
	duration_quantity?: number | null;
	created_at: Date;
	created_by_user_id: UserID;
	redeemed_at: Date | null;
	redeemed_by_user_id: UserID | null;
	stripe_payment_intent_id: string | null;
	visionary_sequence_number: number | null;
	checkout_session_id: string | null;
	revoked_at?: Date | null;
	premium_reversed_seconds?: number | null;
	version: number;
}

export interface PaymentRow {
	checkout_session_id: string;
	user_id: UserID;
	stripe_customer_id: string | null;
	payment_intent_id: string | null;
	subscription_id: string | null;
	invoice_id: string | null;
	price_id: string | null;
	product_type: string | null;
	amount_cents: number;
	currency: string;
	status: string;
	is_gift: boolean;
	gift_code: string | null;
	purchase_geoip_country_code: string | null;
	purchase_client_country_code: string | null;
	eu_withdrawal_waiver_required: boolean;
	eu_withdrawal_waiver_accepted: boolean;
	eu_withdrawal_waiver_accepted_at: Date | null;
	eu_withdrawal_waiver_text_version: string | null;
	created_at: Date;
	completed_at: Date | null;
	version: number;
}

export interface PaymentBySubscriptionRow {
	subscription_id: string;
	checkout_session_id: string;
	user_id: UserID;
	price_id: string;
	product_type: string;
}

export interface VisionarySlotRow {
	slot_index: number;
	user_id: UserID | null;
}

export const PAYMENT_COLUMNS = [
	'checkout_session_id',
	'user_id',
	'stripe_customer_id',
	'payment_intent_id',
	'subscription_id',
	'invoice_id',
	'price_id',
	'product_type',
	'amount_cents',
	'currency',
	'status',
	'is_gift',
	'gift_code',
	'purchase_geoip_country_code',
	'purchase_client_country_code',
	'eu_withdrawal_waiver_required',
	'eu_withdrawal_waiver_accepted',
	'eu_withdrawal_waiver_accepted_at',
	'eu_withdrawal_waiver_text_version',
	'created_at',
	'completed_at',
	'version',
] as const;
export const PAYMENT_BY_SUBSCRIPTION_COLUMNS = [
	'subscription_id',
	'checkout_session_id',
	'user_id',
	'price_id',
	'product_type',
] as const;
export const PAYMENT_BY_PAYMENT_INTENT_COLUMNS = ['payment_intent_id', 'checkout_session_id'] as const;
export const PAYMENT_BY_USER_COLUMNS = ['user_id', 'created_at', 'checkout_session_id'] as const;
export const GIFT_CODE_COLUMNS = [
	'code',
	'duration_months',
	'duration_type',
	'duration_quantity',
	'created_at',
	'created_by_user_id',
	'redeemed_at',
	'redeemed_by_user_id',
	'stripe_payment_intent_id',
	'visionary_sequence_number',
	'checkout_session_id',
	'revoked_at',
	'premium_reversed_seconds',
	'version',
] as const;
export const GIFT_CODE_BY_CREATOR_COLUMNS = ['created_by_user_id', 'code'] as const;
export const GIFT_CODE_BY_PAYMENT_INTENT_COLUMNS = ['stripe_payment_intent_id', 'code'] as const;
export const GIFT_CODE_BY_REDEEMER_COLUMNS = ['redeemed_by_user_id', 'code'] as const;
export const VISIONARY_SLOT_COLUMNS = ['slot_index', 'user_id'] as const;

export interface PaymentByPaymentIntentRow {
	payment_intent_id: string;
	checkout_session_id: string;
}

export interface PaymentByUserRow {
	user_id: UserID;
	created_at: Date;
	checkout_session_id: string;
}

export interface GiftCodeByCreatorRow {
	created_by_user_id: UserID;
	code: string;
}

export interface GiftCodeByPaymentIntentRow {
	stripe_payment_intent_id: string;
	code: string;
}

export interface GiftCodeByRedeemerRow {
	redeemed_by_user_id: UserID;
	code: string;
}
