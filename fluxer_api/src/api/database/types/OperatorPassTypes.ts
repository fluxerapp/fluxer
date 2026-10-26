// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';

type Nullish<T> = T | null;

type OperatorPassWaitlistStatus = 'pending' | 'confirmed' | 'unsubscribed' | 'bounced';

type OperatorPassPurchaseStatus =
	| 'pending'
	| 'completed'
	| 'withdrawn'
	| 'refunded'
	| 'disputed'
	| 'dispute_lost'
	| 'duplicate_refunded';

type OperatorPassRemovalState = 'notice' | 'removed';

export type OperatorPassRemovalEndReason = 'expired' | 'restored' | 'replaced';

export type OperatorPassWithdrawalChannel = 'button' | 'form' | 'email' | 'support' | 'other';

export type OperatorPassWallRevisionSource = 'checkout' | 'profile' | 'settings' | 'admin';

export type OperatorPassWallRevisionStatus = 'pending' | 'approved' | 'declined' | 'superseded' | 'reverted';

export type OperatorPassWallDecisionKind = 'approve' | 'decline' | 'revert' | 'force_fallback' | 'hide' | 'unhide';

export type OperatorPassWallDeclineReason = 'guidelines' | 'impersonation' | 'rights' | 'age_suitability';

export type OperatorPassWallEventKind =
	| 'delivered'
	| 'listed'
	| 'changed'
	| 'opted_out'
	| 'taken_down'
	| 'shown_again'
	| 'reviewed'
	| 'approved'
	| 'declined'
	| 'fallback'
	| 'display_adjusted'
	| 'legal_hold_set'
	| 'legal_hold_cleared'
	| 'release_published'
	| 'erased'
	| 'count_changed';

export type OperatorPassWallTakeDownCause =
	| 'withdrawal'
	| 'refund'
	| 'dispute'
	| 'removal'
	| 'hidden'
	| 'deletion'
	| 'holder_request'
	| 'pause'
	| 'passed_on';

export type OperatorPassUnitSource = 'purchase' | 'transfer' | 'admin';

type OperatorPassUnitStatus = 'held' | 'withdrawn' | 'revoked';

export type OperatorPassOfferStatus = 'pending' | 'accepted' | 'completed' | 'withdrawn' | 'declined' | 'cancelled';

export type OperatorPassUnitEventKind =
	| 'purchased'
	| 'delivered'
	| 'country_rule'
	| 'offer_created'
	| 'offer_withdrawn'
	| 'offer_declined'
	| 'offer_accepted'
	| 'offer_cancelled'
	| 'transferred'
	| 'withdrawn'
	| 'revoked'
	| 'paused'
	| 'resumed';

export interface OperatorPassWaitlistRow {
	email: string;
	entry_id: string;
	status: OperatorPassWaitlistStatus;
	locale: Nullish<string>;
	country_code: Nullish<string>;
	created_at: Date;
	updated_at: Date;
	confirmed_at: Nullish<Date>;
	unsubscribed_at: Nullish<Date>;
	consent_ip: Nullish<string>;
	verify_token_hash: Nullish<string>;
	verify_expires_at: Nullish<Date>;
	sale_email_sent_at: Nullish<Date>;
	price_feedback: Nullish<string>;
	currency: Nullish<string>;
	followup_picks: Set<string> | null;
	followup_about: Set<string> | null;
	followup_other_text: Nullish<string>;
	followup_answered_at: Nullish<Date>;
	version: number;
}

export interface OperatorPassWaitlistByEntryIdRow {
	entry_id: string;
	email: string;
}

export interface OperatorPassRow {
	user_id: UserID;
	granted_at: Date;
	source: string;
	pass_count: Nullish<number>;
	checkout_session_id: Nullish<string>;
	payment_intent_id: Nullish<string>;
	amount_minor: Nullish<number>;
	currency: Nullish<string>;
	paused_at: Nullish<Date>;
	terms_version: Nullish<string>;
	terms_accepted_at: Nullish<Date>;
	waiver_consent_at: Nullish<Date>;
	waiver_consent_version: Nullish<string>;
	waiver_consent_ip: Nullish<string>;
	waiver_consent_user_agent: Nullish<string>;
	version: number;
}

export interface OperatorPassPurchaseRow {
	checkout_session_id: string;
	purchaser_id: UserID;
	quantity: Nullish<number>;
	status: OperatorPassPurchaseStatus;
	phase: string;
	amount_minor: number;
	currency: string;
	quote_honored: boolean;
	payment_intent_id: Nullish<string>;
	invoice_id: Nullish<string>;
	subscription_id: Nullish<string>;
	created_at: Date;
	completed_at: Nullish<Date>;
	refunded_at: Nullish<Date>;
	waiver_consent_at: Nullish<Date>;
	waiver_consent_version: Nullish<string>;
	waiver_consent_text: Nullish<string>;
	waiver_consent_ip: Nullish<string>;
	waiver_consent_user_agent: Nullish<string>;
	wall_name: Nullish<string>;
	wall_opt_out: Nullish<boolean>;
	withdrawal_country: Nullish<string>;
	withdrawal_days: Nullish<number>;
	withdrawal_rule_version: Nullish<string>;
	fraud_warning_at: Nullish<Date>;
	stripe_terms_consent: Nullish<string>;
	operator_pass_terms_version: Nullish<string>;
	purchased_at: Nullish<Date>;
	withdrawal_ends_at: Nullish<Date>;
	perks_granted_at: Nullish<Date>;
	confirmation_sent_at: Nullish<Date>;
	withdrawn_at: Nullish<Date>;
	withdrawal_name: Nullish<string>;
	withdrawal_refund_id: Nullish<string>;
	withdrawal_channel: Nullish<string>;
	invoice_number: Nullish<string>;
	dispute_id: Nullish<string>;
	dispute_reason: Nullish<string>;
	dispute_status: Nullish<string>;
	disputed_at: Nullish<Date>;
	dispute_closed_at: Nullish<Date>;
	version: number;
}

export interface OperatorPassRemovalRow {
	user_id: UserID;
	removal_state: OperatorPassRemovalState;
	rule: string;
	facts: string;
	automated: boolean;
	serious: Nullish<boolean>;
	removed_until: Nullish<Date>;
	notice_sent_at: Nullish<Date>;
	removed_at: Nullish<Date>;
	admin_user_id: UserID;
	created_at: Date;
	updated_at: Date;
	version: number;
}

export interface OperatorPassRemovalHistoryRow {
	user_id: UserID;
	removed_at: Date;
	removed_until: Nullish<Date>;
	rule: string;
	facts: string;
	automated: boolean;
	serious: boolean;
	notice_sent_at: Nullish<Date>;
	admin_user_id: UserID;
	ended_at: Nullish<Date>;
	end_reason: Nullish<OperatorPassRemovalEndReason>;
}

export interface OperatorPassWallEntryRow {
	user_id: UserID;
	name: Nullish<string>;
	opted_out: boolean;
	keep_as_is: boolean;
	release_list_opt_in: boolean;
	hidden: boolean;
	legal_hold: boolean;
	left_wall_at: Nullish<Date>;
	username_declined: boolean;
	live_revision_id: Nullish<bigint>;
	live_name: Nullish<string>;
	live_avatar: Nullish<string>;
	live_business: Nullish<boolean>;
	pending_revision_id: Nullish<bigint>;
	pending_name: Nullish<string>;
	pending_avatar: Nullish<string>;
	pending_source: Nullish<OperatorPassWallRevisionSource>;
	pending_since: Nullish<Date>;
	declined_revision_id: Nullish<bigint>;
	declined_name: Nullish<string>;
	declined_avatar: Nullish<string>;
	decline_reason: Nullish<OperatorPassWallDeclineReason>;
	decline_note: Nullish<string>;
	decline_statement_id: Nullish<string>;
	reviewed_at: Nullish<Date>;
	reviewed_by: Nullish<UserID>;
	listed_at: Nullish<Date>;
	delivered_at: Nullish<Date>;
	release_published_year: Nullish<number>;
	release_published_at: Nullish<Date>;
	release_erased_at: Nullish<Date>;
	updated_at: Date;
	version: number;
}

export interface OperatorPassWallEventRow {
	user_id: UserID;
	occurred_at: Date;
	event_id: string;
	kind: OperatorPassWallEventKind;
	name: Nullish<string>;
	reason: Nullish<string>;
	cause: Nullish<OperatorPassWallTakeDownCause>;
	purchase_id: Nullish<string>;
	release_year: Nullish<number>;
	release_tag: Nullish<string>;
	actor_id: Nullish<UserID>;
	pass_count: Nullish<number>;
	first_pass_at?: Date | null;
}

export interface OperatorPassWallRevisionRow {
	user_id: UserID;
	revision_id: bigint;
	name: string;
	avatar: Nullish<string>;
	business: Nullish<boolean>;
	source: OperatorPassWallRevisionSource;
	created_at: Date;
	created_by: Nullish<UserID>;
}

export interface OperatorPassWallDecisionRow {
	user_id: UserID;
	decision_id: bigint;
	revision_id: Nullish<bigint>;
	kind: OperatorPassWallDecisionKind;
	decided_by: Nullish<UserID>;
	decided_at: Date;
	reason: Nullish<OperatorPassWallDeclineReason>;
	note: Nullish<string>;
	statement_id: Nullish<string>;
	username_declined: Nullish<boolean>;
}

export interface OperatorPassWallAvatarRefRow {
	avatar: string;
	user_id: UserID;
}

export interface OperatorPassWallPendingRow {
	bucket: string;
	user_id: UserID;
}

export interface OperatorPassWallAuditLogRow {
	user_id: UserID;
	log_id: bigint;
}

export interface OperatorPassWallTombstoneRow {
	token_: bigint;
	left_at: Date;
	purged_at: Date;
}

export interface OperatorPassWallStateRow {
	id: string;
	wall_version: bigint;
	frozen: boolean;
	frozen_at: Nullish<Date>;
	frozen_by: Nullish<UserID>;
	frozen_snapshot: Nullish<string>;
	published_at: Nullish<Date>;
	version: number;
}

export interface OperatorPassReleaseRow {
	year: number;
	tag: string;
	released_at: Date;
	published_at: Date;
	published_by: UserID;
	cutoff_at: Date;
	names: Array<string>;
	erased_names: Nullish<Array<string>>;
}

export interface OperatorPassUnitRow {
	pass_id: string;
	purchase_id: Nullish<string>;
	buyer_id: Nullish<UserID>;
	holder_id: UserID;
	source: OperatorPassUnitSource;
	status: OperatorPassUnitStatus;
	transferable: boolean;
	delivered_at: Date;
	first_delivered_at: Date;
	paused_at: Nullish<Date>;
	ended_at: Nullish<Date>;
	end_reason: Nullish<string>;
	withdrawal_country: Nullish<string>;
	withdrawal_days: Nullish<number>;
	version: number;
}

export interface OperatorPassUnitByHolderRow {
	holder_id: UserID;
	pass_id: string;
}

export interface OperatorPassUnitByPurchaseRow {
	purchase_id: string;
	pass_id: string;
}

export interface OperatorPassOfferRow {
	offer_id: string;
	pass_id: string;
	sender_id: UserID;
	recipient_id: UserID;
	status: OperatorPassOfferStatus;
	created_at: Date;
	accepted_at: Nullish<Date>;
	completes_at: Nullish<Date>;
	ended_at: Nullish<Date>;
	wall_name: Nullish<string>;
	wall_opt_out: Nullish<boolean>;
	terms_version: Nullish<string>;
	version: number;
}

export interface OperatorPassOfferByUserRow {
	user_id: UserID;
	offer_id: string;
}

export interface OperatorPassUnitEventRow {
	pass_id: string;
	occurred_at: Date;
	event_id: string;
	kind: OperatorPassUnitEventKind;
	payment_id: Nullish<string>;
	purchase_id: Nullish<string>;
	sender_id: Nullish<UserID>;
	recipient_id: Nullish<UserID>;
	offer_id: Nullish<string>;
	country: Nullish<string>;
	days: Nullish<number>;
	rule_version: Nullish<string>;
	reason: Nullish<string>;
}

export interface OperatorPassPurchaseByPaymentIntentRow {
	payment_intent_id: string;
	checkout_session_id: string;
}

export interface OperatorPassPurchaseByPurchaserRow {
	purchaser_id: UserID;
	checkout_session_id: string;
}

export const OPERATOR_PASS_WAITLIST_COLUMNS = [
	'email',
	'entry_id',
	'status',
	'locale',
	'country_code',
	'created_at',
	'updated_at',
	'confirmed_at',
	'unsubscribed_at',
	'consent_ip',
	'verify_token_hash',
	'verify_expires_at',
	'sale_email_sent_at',
	'price_feedback',
	'currency',
	'followup_picks',
	'followup_about',
	'followup_other_text',
	'followup_answered_at',
	'version',
] as const satisfies ReadonlyArray<keyof OperatorPassWaitlistRow>;

export const OPERATOR_PASS_WAITLIST_BY_ENTRY_ID_COLUMNS = ['entry_id', 'email'] as const satisfies ReadonlyArray<
	keyof OperatorPassWaitlistByEntryIdRow
>;

export const OPERATOR_PASS_COLUMNS = [
	'user_id',
	'granted_at',
	'source',
	'pass_count',
	'checkout_session_id',
	'payment_intent_id',
	'amount_minor',
	'currency',
	'paused_at',
	'terms_version',
	'terms_accepted_at',
	'waiver_consent_at',
	'waiver_consent_version',
	'waiver_consent_ip',
	'waiver_consent_user_agent',
	'version',
] as const satisfies ReadonlyArray<keyof OperatorPassRow>;

export const OPERATOR_PASS_PURCHASE_COLUMNS = [
	'checkout_session_id',
	'purchaser_id',
	'quantity',
	'status',
	'phase',
	'amount_minor',
	'currency',
	'quote_honored',
	'payment_intent_id',
	'invoice_id',
	'subscription_id',
	'created_at',
	'completed_at',
	'refunded_at',
	'waiver_consent_at',
	'waiver_consent_version',
	'waiver_consent_text',
	'waiver_consent_ip',
	'waiver_consent_user_agent',
	'wall_name',
	'wall_opt_out',
	'withdrawal_country',
	'withdrawal_days',
	'withdrawal_rule_version',
	'fraud_warning_at',
	'stripe_terms_consent',
	'operator_pass_terms_version',
	'purchased_at',
	'withdrawal_ends_at',
	'perks_granted_at',
	'confirmation_sent_at',
	'withdrawn_at',
	'withdrawal_name',
	'withdrawal_refund_id',
	'withdrawal_channel',
	'invoice_number',
	'dispute_id',
	'dispute_reason',
	'dispute_status',
	'disputed_at',
	'dispute_closed_at',
	'version',
] as const satisfies ReadonlyArray<keyof OperatorPassPurchaseRow>;

export const OPERATOR_PASS_PURCHASE_BY_PAYMENT_INTENT_COLUMNS = [
	'payment_intent_id',
	'checkout_session_id',
] as const satisfies ReadonlyArray<keyof OperatorPassPurchaseByPaymentIntentRow>;

export const OPERATOR_PASS_PURCHASE_BY_PURCHASER_COLUMNS = [
	'purchaser_id',
	'checkout_session_id',
] as const satisfies ReadonlyArray<keyof OperatorPassPurchaseByPurchaserRow>;

export const OPERATOR_PASS_REMOVAL_COLUMNS = [
	'user_id',
	'removal_state',
	'rule',
	'facts',
	'automated',
	'serious',
	'removed_until',
	'notice_sent_at',
	'removed_at',
	'admin_user_id',
	'created_at',
	'updated_at',
	'version',
] as const satisfies ReadonlyArray<keyof OperatorPassRemovalRow>;

export const OPERATOR_PASS_REMOVAL_HISTORY_COLUMNS = [
	'user_id',
	'removed_at',
	'removed_until',
	'rule',
	'facts',
	'automated',
	'serious',
	'notice_sent_at',
	'admin_user_id',
	'ended_at',
	'end_reason',
] as const satisfies ReadonlyArray<keyof OperatorPassRemovalHistoryRow>;

export const OPERATOR_PASS_WALL_ENTRY_COLUMNS = [
	'user_id',
	'name',
	'opted_out',
	'keep_as_is',
	'release_list_opt_in',
	'hidden',
	'legal_hold',
	'left_wall_at',
	'username_declined',
	'live_revision_id',
	'live_name',
	'live_avatar',
	'live_business',
	'pending_revision_id',
	'pending_name',
	'pending_avatar',
	'pending_source',
	'pending_since',
	'declined_revision_id',
	'declined_name',
	'declined_avatar',
	'decline_reason',
	'decline_note',
	'decline_statement_id',
	'reviewed_at',
	'reviewed_by',
	'listed_at',
	'delivered_at',
	'release_published_year',
	'release_published_at',
	'release_erased_at',
	'updated_at',
	'version',
] as const satisfies ReadonlyArray<keyof OperatorPassWallEntryRow>;

export const OPERATOR_PASS_WALL_EVENT_COLUMNS = [
	'user_id',
	'occurred_at',
	'event_id',
	'kind',
	'name',
	'reason',
	'cause',
	'purchase_id',
	'release_year',
	'release_tag',
	'actor_id',
	'pass_count',
	'first_pass_at',
] as const satisfies ReadonlyArray<keyof OperatorPassWallEventRow>;

export const OPERATOR_PASS_WALL_REVISION_COLUMNS = [
	'user_id',
	'revision_id',
	'name',
	'avatar',
	'business',
	'source',
	'created_at',
	'created_by',
] as const satisfies ReadonlyArray<keyof OperatorPassWallRevisionRow>;

export const OPERATOR_PASS_WALL_DECISION_COLUMNS = [
	'user_id',
	'decision_id',
	'revision_id',
	'kind',
	'decided_by',
	'decided_at',
	'reason',
	'note',
	'statement_id',
	'username_declined',
] as const satisfies ReadonlyArray<keyof OperatorPassWallDecisionRow>;

export const OPERATOR_PASS_WALL_AVATAR_REF_COLUMNS = ['avatar', 'user_id'] as const satisfies ReadonlyArray<
	keyof OperatorPassWallAvatarRefRow
>;

export const OPERATOR_PASS_WALL_PENDING_COLUMNS = ['bucket', 'user_id'] as const satisfies ReadonlyArray<
	keyof OperatorPassWallPendingRow
>;

export const OPERATOR_PASS_WALL_AUDIT_LOG_COLUMNS = ['user_id', 'log_id'] as const satisfies ReadonlyArray<
	keyof OperatorPassWallAuditLogRow
>;

export const OPERATOR_PASS_WALL_TOMBSTONE_COLUMNS = ['token_', 'left_at', 'purged_at'] as const satisfies ReadonlyArray<
	keyof OperatorPassWallTombstoneRow
>;

export const OPERATOR_PASS_WALL_STATE_COLUMNS = [
	'id',
	'wall_version',
	'frozen',
	'frozen_at',
	'frozen_by',
	'frozen_snapshot',
	'published_at',
	'version',
] as const satisfies ReadonlyArray<keyof OperatorPassWallStateRow>;

export const OPERATOR_PASS_RELEASE_COLUMNS = [
	'year',
	'tag',
	'released_at',
	'published_at',
	'published_by',
	'cutoff_at',
	'names',
	'erased_names',
] as const satisfies ReadonlyArray<keyof OperatorPassReleaseRow>;

export const OPERATOR_PASS_UNIT_COLUMNS = [
	'pass_id',
	'purchase_id',
	'buyer_id',
	'holder_id',
	'source',
	'status',
	'transferable',
	'delivered_at',
	'first_delivered_at',
	'paused_at',
	'ended_at',
	'end_reason',
	'withdrawal_country',
	'withdrawal_days',
	'version',
] as const satisfies ReadonlyArray<keyof OperatorPassUnitRow>;

export const OPERATOR_PASS_UNIT_BY_HOLDER_COLUMNS = ['holder_id', 'pass_id'] as const satisfies ReadonlyArray<
	keyof OperatorPassUnitByHolderRow
>;

export const OPERATOR_PASS_UNIT_BY_PURCHASE_COLUMNS = ['purchase_id', 'pass_id'] as const satisfies ReadonlyArray<
	keyof OperatorPassUnitByPurchaseRow
>;

export const OPERATOR_PASS_OFFER_COLUMNS = [
	'offer_id',
	'pass_id',
	'sender_id',
	'recipient_id',
	'status',
	'created_at',
	'accepted_at',
	'completes_at',
	'ended_at',
	'wall_name',
	'wall_opt_out',
	'terms_version',
	'version',
] as const satisfies ReadonlyArray<keyof OperatorPassOfferRow>;

export const OPERATOR_PASS_OFFER_BY_USER_COLUMNS = ['user_id', 'offer_id'] as const satisfies ReadonlyArray<
	keyof OperatorPassOfferByUserRow
>;

export const OPERATOR_PASS_UNIT_EVENT_COLUMNS = [
	'pass_id',
	'occurred_at',
	'event_id',
	'kind',
	'payment_id',
	'purchase_id',
	'sender_id',
	'recipient_id',
	'offer_id',
	'country',
	'days',
	'rule_version',
	'reason',
] as const satisfies ReadonlyArray<keyof OperatorPassUnitEventRow>;
