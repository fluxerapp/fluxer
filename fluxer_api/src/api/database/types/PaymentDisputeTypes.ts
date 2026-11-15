// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';

type Nullish<T> = T | null;

export type PaymentDisputeProduct = 'operator_pass' | 'plutonium';

type PaymentDisputeReviewStatus = 'open' | 'justified' | 'unjustified';

export interface PaymentDisputeReviewRow {
	dispute_id: string;
	product: PaymentDisputeProduct;
	order_id: string;
	invoice_number: Nullish<string>;
	purchaser_id: UserID;
	is_gift: boolean;
	reason: string;
	payment_intent_id: Nullish<string>;
	charge_id: Nullish<string>;
	amount_minor: number;
	currency: string;
	evidence_submitted: boolean;
	evidence_due_by: Nullish<Date>;
	disputed_at: Nullish<Date>;
	closed_at: Nullish<Date>;
	outcome: Nullish<string>;
	purchased_at: Nullish<Date>;
	perks_started_at: Nullish<Date>;
	withdrawal_ends_at: Nullish<Date>;
	withdrawn_at: Nullish<Date>;
	refunded_at: Nullish<Date>;
	perks_removed_at: Nullish<Date>;
	status: PaymentDisputeReviewStatus;
	created_at: Date;
	decided_at: Nullish<Date>;
	decided_by: Nullish<UserID>;
	decision_note: Nullish<string>;
	warning_deadline: Nullish<Date>;
	warned_at: Nullish<Date>;
	warning_email_sent_at: Nullish<Date>;
	warning_dm_sent_at: Nullish<Date>;
	enforcement_decision_id: Nullish<bigint>;
	reversed_at: Nullish<Date>;
	reversed_by: Nullish<UserID>;
	reversal_note: Nullish<string>;
	dispute_purchase_at: Nullish<Date>;
	version: number;
}

export interface PaymentDisputeReviewByPurchaserRow {
	purchaser_id: UserID;
	dispute_id: string;
}

export const PAYMENT_DISPUTE_REVIEW_COLUMNS = [
	'dispute_id',
	'product',
	'order_id',
	'invoice_number',
	'purchaser_id',
	'is_gift',
	'reason',
	'payment_intent_id',
	'charge_id',
	'amount_minor',
	'currency',
	'evidence_submitted',
	'evidence_due_by',
	'disputed_at',
	'closed_at',
	'outcome',
	'purchased_at',
	'perks_started_at',
	'withdrawal_ends_at',
	'withdrawn_at',
	'refunded_at',
	'perks_removed_at',
	'status',
	'created_at',
	'decided_at',
	'decided_by',
	'decision_note',
	'warning_deadline',
	'warned_at',
	'warning_email_sent_at',
	'warning_dm_sent_at',
	'enforcement_decision_id',
	'reversed_at',
	'reversed_by',
	'reversal_note',
	'dispute_purchase_at',
	'version',
] as const satisfies ReadonlyArray<keyof PaymentDisputeReviewRow>;

export const PAYMENT_DISPUTE_REVIEW_BY_PURCHASER_COLUMNS = [
	'purchaser_id',
	'dispute_id',
] as const satisfies ReadonlyArray<keyof PaymentDisputeReviewByPurchaserRow>;
