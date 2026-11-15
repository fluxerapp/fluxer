// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IARMessageContextRow} from '@app/api/database/types/ReportTypes';
import type {EnforcementKind, EnforcementLimit, EnforcementReasonCode} from '@fluxer/constants/src/EnforcementReasons';

export type EnforcementSubjectType = 'user' | 'guild';
type EnforcementSource = 'report' | 'own_review';
type EnforcementAutomation = 'none' | 'flagged' | 'automated';
type EnforcementStatus = 'active' | 'ended';
export type EnforcementEndOutcome = 'lifted' | 'reversed' | 'expired';
export type EnforcementNoticeStatus = 'pending' | 'sent' | 'undelivered';
export type EnforcementNoticeChannel = 'email' | 'system_dm' | 'sign_in';
export type EvidenceChannelKind = 'direct_message' | 'group_message' | 'channel';

export interface EnforcementDecisionRow {
	decision_id: bigint;
	subject_type: EnforcementSubjectType;
	subject_id: bigint;
	subject_name: string | null;
	recipient_user_id: bigint | null;
	kind: EnforcementKind;
	reason_code: EnforcementReasonCode;
	source: EnforcementSource;
	automation: EnforcementAutomation;
	statement: string | null;
	rights_holder: string | null;
	ends_at: Date | null;
	deletion_at: Date | null;
	feature_limit: EnforcementLimit | null;
	profile_fields: Set<string> | null;
	removed_message_count: number | null;
	removed_all_messages: boolean | null;
	payment_at: Date | null;
	report_ids: Set<bigint> | null;
	decided_by: bigint;
	created_at: Date;
	status: EnforcementStatus;
	end_outcome: EnforcementEndOutcome | null;
	ended_at: Date | null;
	ended_by: bigint | null;
	end_statement: string | null;
	notice_status: EnforcementNoticeStatus;
	notice_channels: Set<EnforcementNoticeChannel> | null;
	notice_locale: string | null;
	notice_text: string | null;
	notice_sent_at: Date | null;
}

export const ENFORCEMENT_DECISION_COLUMNS = [
	'decision_id',
	'subject_type',
	'subject_id',
	'subject_name',
	'recipient_user_id',
	'kind',
	'reason_code',
	'source',
	'automation',
	'statement',
	'rights_holder',
	'ends_at',
	'deletion_at',
	'feature_limit',
	'profile_fields',
	'removed_message_count',
	'removed_all_messages',
	'payment_at',
	'report_ids',
	'decided_by',
	'created_at',
	'status',
	'end_outcome',
	'ended_at',
	'ended_by',
	'end_statement',
	'notice_status',
	'notice_channels',
	'notice_locale',
	'notice_text',
	'notice_sent_at',
] as const satisfies ReadonlyArray<keyof EnforcementDecisionRow>;

export interface EnforcementDecisionBySubjectRow {
	subject_type: EnforcementSubjectType;
	subject_id: bigint;
	decision_id: bigint;
}

export const ENFORCEMENT_DECISION_BY_SUBJECT_COLUMNS = [
	'subject_type',
	'subject_id',
	'decision_id',
] as const satisfies ReadonlyArray<keyof EnforcementDecisionBySubjectRow>;

export interface EnforcementDecisionEvidenceRow {
	decision_id: bigint;
	messages: Array<IARMessageContextRow> | null;
	message_locations: Map<bigint, EvidenceChannelKind> | null;
	profile_values: Map<string, string> | null;
}

export const ENFORCEMENT_DECISION_EVIDENCE_COLUMNS = [
	'decision_id',
	'messages',
	'message_locations',
	'profile_values',
] as const satisfies ReadonlyArray<keyof EnforcementDecisionEvidenceRow>;

export interface EnforcementDecisionNoteRow {
	decision_id: bigint;
	note_id: bigint;
	author_id: bigint;
	body: string;
	created_at: Date;
}

export const ENFORCEMENT_DECISION_NOTE_COLUMNS = [
	'decision_id',
	'note_id',
	'author_id',
	'body',
	'created_at',
] as const satisfies ReadonlyArray<keyof EnforcementDecisionNoteRow>;

export interface EnforcementReviewQueueRow {
	bucket: number;
	decision_id: bigint;
}

export const ENFORCEMENT_REVIEW_QUEUE_COLUMNS = ['bucket', 'decision_id'] as const satisfies ReadonlyArray<
	keyof EnforcementReviewQueueRow
>;
