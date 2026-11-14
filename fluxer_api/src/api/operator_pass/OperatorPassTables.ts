// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineTable} from '@app/api/database/CassandraTableDsl';
import {
	OPERATOR_PASS_CERTIFICATE_BY_PURCHASE_COLUMNS,
	OPERATOR_PASS_CERTIFICATE_COLUMNS,
	OPERATOR_PASS_CERTIFICATE_LINK_COLUMNS,
	OPERATOR_PASS_CERTIFICATE_NUMBER_COLUMNS,
	OPERATOR_PASS_COLUMNS,
	OPERATOR_PASS_LABS_GREETING_COLUMNS,
	OPERATOR_PASS_LABS_MEMBER_COLUMNS,
	OPERATOR_PASS_OFFER_BY_USER_COLUMNS,
	OPERATOR_PASS_OFFER_COLUMNS,
	OPERATOR_PASS_PURCHASE_BY_PAYMENT_INTENT_COLUMNS,
	OPERATOR_PASS_PURCHASE_BY_PURCHASER_COLUMNS,
	OPERATOR_PASS_PURCHASE_COLUMNS,
	OPERATOR_PASS_RELEASE_COLUMNS,
	OPERATOR_PASS_REMOVAL_COLUMNS,
	OPERATOR_PASS_REMOVAL_HISTORY_COLUMNS,
	OPERATOR_PASS_UNIT_BY_HOLDER_COLUMNS,
	OPERATOR_PASS_UNIT_BY_PURCHASE_COLUMNS,
	OPERATOR_PASS_UNIT_COLUMNS,
	OPERATOR_PASS_UNIT_EVENT_COLUMNS,
	OPERATOR_PASS_VISIONARY_ACTIVATION_COLUMNS,
	OPERATOR_PASS_WAITLIST_BY_ENTRY_ID_COLUMNS,
	OPERATOR_PASS_WAITLIST_COLUMNS,
	OPERATOR_PASS_WALL_AUDIT_LOG_COLUMNS,
	OPERATOR_PASS_WALL_AVATAR_REF_COLUMNS,
	OPERATOR_PASS_WALL_DECISION_COLUMNS,
	OPERATOR_PASS_WALL_ENTRY_COLUMNS,
	OPERATOR_PASS_WALL_EVENT_COLUMNS,
	OPERATOR_PASS_WALL_REVISION_COLUMNS,
	OPERATOR_PASS_WALL_STATE_COLUMNS,
	OPERATOR_PASS_WALL_TOMBSTONE_COLUMNS,
	type OperatorPassCertificateByPurchaseRow,
	type OperatorPassCertificateLinkRow,
	type OperatorPassCertificateNumberRow,
	type OperatorPassCertificateRow,
	type OperatorPassLabsGreetingRow,
	type OperatorPassLabsMemberRow,
	type OperatorPassOfferByUserRow,
	type OperatorPassOfferRow,
	type OperatorPassPurchaseByPaymentIntentRow,
	type OperatorPassPurchaseByPurchaserRow,
	type OperatorPassPurchaseRow,
	type OperatorPassReleaseRow,
	type OperatorPassRemovalHistoryRow,
	type OperatorPassRemovalRow,
	type OperatorPassRow,
	type OperatorPassUnitByHolderRow,
	type OperatorPassUnitByPurchaseRow,
	type OperatorPassUnitEventRow,
	type OperatorPassUnitRow,
	type OperatorPassVisionaryActivationRow,
	type OperatorPassWaitlistByEntryIdRow,
	type OperatorPassWaitlistRow,
	type OperatorPassWallAuditLogRow,
	type OperatorPassWallAvatarRefRow,
	type OperatorPassWallDecisionRow,
	type OperatorPassWallEntryRow,
	type OperatorPassWallEventRow,
	type OperatorPassWallRevisionRow,
	type OperatorPassWallStateRow,
	type OperatorPassWallTombstoneRow,
} from '@app/api/database/types/OperatorPassTypes';

export const OperatorPassWaitlist = defineTable<OperatorPassWaitlistRow, 'email'>({
	name: 'operator_pass_waitlist',
	columns: OPERATOR_PASS_WAITLIST_COLUMNS,
	primaryKey: ['email'],
});

export const OperatorPassWaitlistByEntryId = defineTable<OperatorPassWaitlistByEntryIdRow, 'entry_id'>({
	name: 'operator_pass_waitlist_by_entry_id',
	columns: OPERATOR_PASS_WAITLIST_BY_ENTRY_ID_COLUMNS,
	primaryKey: ['entry_id'],
});

export const OperatorPasses = defineTable<OperatorPassRow, 'user_id'>({
	name: 'operator_passes',
	columns: OPERATOR_PASS_COLUMNS,
	primaryKey: ['user_id'],
});

export const OperatorPassPurchases = defineTable<OperatorPassPurchaseRow, 'checkout_session_id'>({
	name: 'operator_pass_purchases',
	columns: OPERATOR_PASS_PURCHASE_COLUMNS,
	primaryKey: ['checkout_session_id'],
});

export const OperatorPassCertificates = defineTable<OperatorPassCertificateRow, 'pass_id' | 'version', 'pass_id'>({
	name: 'operator_pass_certificates',
	columns: OPERATOR_PASS_CERTIFICATE_COLUMNS,
	primaryKey: ['pass_id', 'version'],
	partitionKey: ['pass_id'],
});

export const OperatorPassCertificatesByPurchase = defineTable<
	OperatorPassCertificateByPurchaseRow,
	'purchase_id' | 'pass_id',
	'purchase_id'
>({
	name: 'operator_pass_certificates_by_purchase',
	columns: OPERATOR_PASS_CERTIFICATE_BY_PURCHASE_COLUMNS,
	primaryKey: ['purchase_id', 'pass_id'],
	partitionKey: ['purchase_id'],
});

export const OperatorPassCertificateNumbers = defineTable<OperatorPassCertificateNumberRow, 'number'>({
	name: 'operator_pass_certificate_numbers',
	columns: OPERATOR_PASS_CERTIFICATE_NUMBER_COLUMNS,
	primaryKey: ['number'],
});

export const OperatorPassCertificateLinks = defineTable<OperatorPassCertificateLinkRow, 'token_'>({
	name: 'operator_pass_certificate_links',
	columns: OPERATOR_PASS_CERTIFICATE_LINK_COLUMNS,
	primaryKey: ['token_'],
});

export const OperatorPassPurchasesByPaymentIntent = defineTable<
	OperatorPassPurchaseByPaymentIntentRow,
	'payment_intent_id'
>({
	name: 'operator_pass_purchases_by_payment_intent',
	columns: OPERATOR_PASS_PURCHASE_BY_PAYMENT_INTENT_COLUMNS,
	primaryKey: ['payment_intent_id'],
});

export const OperatorPassPurchasesByPurchaser = defineTable<
	OperatorPassPurchaseByPurchaserRow,
	'purchaser_id' | 'checkout_session_id',
	'purchaser_id'
>({
	name: 'operator_pass_purchases_by_purchaser',
	columns: OPERATOR_PASS_PURCHASE_BY_PURCHASER_COLUMNS,
	primaryKey: ['purchaser_id', 'checkout_session_id'],
	partitionKey: ['purchaser_id'],
});

export const OperatorPassRemovals = defineTable<OperatorPassRemovalRow, 'user_id'>({
	name: 'operator_pass_removals',
	columns: OPERATOR_PASS_REMOVAL_COLUMNS,
	primaryKey: ['user_id'],
});

export const OperatorPassRemovalHistory = defineTable<
	OperatorPassRemovalHistoryRow,
	'user_id' | 'removed_at',
	'user_id'
>({
	name: 'operator_pass_removal_history',
	columns: OPERATOR_PASS_REMOVAL_HISTORY_COLUMNS,
	primaryKey: ['user_id', 'removed_at'],
	partitionKey: ['user_id'],
});

export const OperatorPassWallEntries = defineTable<OperatorPassWallEntryRow, 'user_id'>({
	name: 'operator_pass_wall_entries',
	columns: OPERATOR_PASS_WALL_ENTRY_COLUMNS,
	primaryKey: ['user_id'],
});

export const OperatorPassWallEvents = defineTable<
	OperatorPassWallEventRow,
	'user_id' | 'occurred_at' | 'event_id',
	'user_id'
>({
	name: 'operator_pass_wall_events',
	columns: OPERATOR_PASS_WALL_EVENT_COLUMNS,
	primaryKey: ['user_id', 'occurred_at', 'event_id'],
	partitionKey: ['user_id'],
});

export const OperatorPassReleases = defineTable<OperatorPassReleaseRow, 'year'>({
	name: 'operator_pass_releases',
	columns: OPERATOR_PASS_RELEASE_COLUMNS,
	primaryKey: ['year'],
});

export const OperatorPassUnits = defineTable<OperatorPassUnitRow, 'pass_id'>({
	name: 'operator_pass_units',
	columns: OPERATOR_PASS_UNIT_COLUMNS,
	primaryKey: ['pass_id'],
});

export const OperatorPassUnitsByHolder = defineTable<OperatorPassUnitByHolderRow, 'holder_id' | 'pass_id', 'holder_id'>(
	{
		name: 'operator_pass_units_by_holder',
		columns: OPERATOR_PASS_UNIT_BY_HOLDER_COLUMNS,
		primaryKey: ['holder_id', 'pass_id'],
		partitionKey: ['holder_id'],
	},
);

export const OperatorPassUnitsByPurchase = defineTable<
	OperatorPassUnitByPurchaseRow,
	'purchase_id' | 'pass_id',
	'purchase_id'
>({
	name: 'operator_pass_units_by_purchase',
	columns: OPERATOR_PASS_UNIT_BY_PURCHASE_COLUMNS,
	primaryKey: ['purchase_id', 'pass_id'],
	partitionKey: ['purchase_id'],
});

export const OperatorPassOffers = defineTable<OperatorPassOfferRow, 'offer_id'>({
	name: 'operator_pass_offers',
	columns: OPERATOR_PASS_OFFER_COLUMNS,
	primaryKey: ['offer_id'],
});

export const OperatorPassOffersByUser = defineTable<OperatorPassOfferByUserRow, 'user_id' | 'offer_id', 'user_id'>({
	name: 'operator_pass_offers_by_user',
	columns: OPERATOR_PASS_OFFER_BY_USER_COLUMNS,
	primaryKey: ['user_id', 'offer_id'],
	partitionKey: ['user_id'],
});

export const OperatorPassUnitEvents = defineTable<
	OperatorPassUnitEventRow,
	'pass_id' | 'occurred_at' | 'event_id',
	'pass_id'
>({
	name: 'operator_pass_unit_events',
	columns: OPERATOR_PASS_UNIT_EVENT_COLUMNS,
	primaryKey: ['pass_id', 'occurred_at', 'event_id'],
	partitionKey: ['pass_id'],
});

export const OperatorPassWallRevisions = defineTable<OperatorPassWallRevisionRow, 'user_id' | 'revision_id', 'user_id'>(
	{
		name: 'operator_pass_wall_revisions',
		columns: OPERATOR_PASS_WALL_REVISION_COLUMNS,
		primaryKey: ['user_id', 'revision_id'],
		partitionKey: ['user_id'],
	},
);

export const OperatorPassWallDecisions = defineTable<OperatorPassWallDecisionRow, 'user_id' | 'decision_id', 'user_id'>(
	{
		name: 'operator_pass_wall_decisions',
		columns: OPERATOR_PASS_WALL_DECISION_COLUMNS,
		primaryKey: ['user_id', 'decision_id'],
		partitionKey: ['user_id'],
	},
);

export const OperatorPassWallAvatarRefs = defineTable<OperatorPassWallAvatarRefRow, 'avatar' | 'user_id', 'avatar'>({
	name: 'operator_pass_wall_avatar_refs',
	columns: OPERATOR_PASS_WALL_AVATAR_REF_COLUMNS,
	primaryKey: ['avatar', 'user_id'],
	partitionKey: ['avatar'],
});

export const OperatorPassWallAuditLogs = defineTable<OperatorPassWallAuditLogRow, 'user_id' | 'log_id', 'user_id'>({
	name: 'operator_pass_wall_audit_logs',
	columns: OPERATOR_PASS_WALL_AUDIT_LOG_COLUMNS,
	primaryKey: ['user_id', 'log_id'],
	partitionKey: ['user_id'],
});

export const OperatorPassWallState = defineTable<OperatorPassWallStateRow, 'id'>({
	name: 'operator_pass_wall_state',
	columns: OPERATOR_PASS_WALL_STATE_COLUMNS,
	primaryKey: ['id'],
});

export const OperatorPassWallTombstones = defineTable<OperatorPassWallTombstoneRow, 'token_'>({
	name: 'operator_pass_wall_tombstones',
	columns: OPERATOR_PASS_WALL_TOMBSTONE_COLUMNS,
	primaryKey: ['token_'],
});

export const OperatorPassVisionaryActivations = defineTable<OperatorPassVisionaryActivationRow, 'user_id'>({
	name: 'operator_pass_visionary_activations',
	columns: OPERATOR_PASS_VISIONARY_ACTIVATION_COLUMNS,
	primaryKey: ['user_id'],
});

export const OperatorPassLabsMembers = defineTable<OperatorPassLabsMemberRow, 'user_id'>({
	name: 'operator_pass_labs_members',
	columns: OPERATOR_PASS_LABS_MEMBER_COLUMNS,
	primaryKey: ['user_id'],
});

export const OperatorPassLabsGreetings = defineTable<OperatorPassLabsGreetingRow, 'bucket' | 'user_id', 'bucket'>({
	name: 'operator_pass_labs_greetings',
	columns: OPERATOR_PASS_LABS_GREETING_COLUMNS,
	primaryKey: ['bucket', 'user_id'],
	partitionKey: ['bucket'],
});
