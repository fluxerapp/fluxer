/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {UserID} from '~/BrandedTypes';

type Nullish<T> = T | null;

export interface GiftCodeRow {
	code: string;
	duration_months: number;
	created_at: Date;
	created_by_user_id: UserID;
	redeemed_at: Nullish<Date>;
	redeemed_by_user_id: Nullish<UserID>;
	stripe_payment_intent_id: Nullish<string>;
	visionary_sequence_number: Nullish<number>;
	checkout_session_id: Nullish<string>;
	version: number;
}

export interface PaymentRow {
	checkout_session_id: string;
	user_id: UserID;
	stripe_customer_id: Nullish<string>;
	payment_intent_id: Nullish<string>;
	subscription_id: Nullish<string>;
	invoice_id: Nullish<string>;
	price_id: Nullish<string>;
	product_type: Nullish<string>;
	amount_cents: number;
	currency: string;
	status: string;
	is_gift: boolean;
	gift_code: Nullish<string>;
	created_at: Date;
	completed_at: Nullish<Date>;
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
	'created_at',
	'created_by_user_id',
	'redeemed_at',
	'redeemed_by_user_id',
	'stripe_payment_intent_id',
	'visionary_sequence_number',
	'checkout_session_id',
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
