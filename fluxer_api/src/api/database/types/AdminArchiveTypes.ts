// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ArchiveSubjectType} from '@fluxer/schema/src/domains/admin/AdminArchiveSchemas';

export interface AdminArchiveRow {
	subject_type: ArchiveSubjectType;
	subject_id: bigint;
	archive_id: bigint;
	requested_by: bigint;
	requested_at: Date;
	started_at: Date | null;
	completed_at: Date | null;
	failed_at: Date | null;
	storage_key: string | null;
	file_size: bigint | null;
	progress_percent: number;
	progress_step: string | null;
	error_message: string | null;
	download_url_expires_at: Date | null;
	expires_at: Date | null;
}

export const ADMIN_ARCHIVE_COLUMNS = [
	'subject_type',
	'subject_id',
	'archive_id',
	'requested_by',
	'requested_at',
	'started_at',
	'completed_at',
	'failed_at',
	'storage_key',
	'file_size',
	'progress_percent',
	'progress_step',
	'error_message',
	'download_url_expires_at',
	'expires_at',
] as const satisfies ReadonlyArray<keyof AdminArchiveRow>;

export interface AdminAuditLogRow {
	log_id: bigint;
	admin_user_id: bigint;
	target_type: string;
	target_id: bigint;
	action: string;
	audit_log_reason: string | null;
	metadata: Map<string, string>;
	created_at: Date;
}

export const ADMIN_AUDIT_LOG_COLUMNS = [
	'log_id',
	'admin_user_id',
	'target_type',
	'target_id',
	'action',
	'audit_log_reason',
	'metadata',
	'created_at',
] as const satisfies ReadonlyArray<keyof AdminAuditLogRow>;

export interface BannedIpRow {
	ip: string;
	ban_kind?: string | null;
	reason?: string | null;
	expires_at?: Date | null;
	created_at?: Date | null;
}

export const BANNED_IP_COLUMNS = [
	'ip',
	'ban_kind',
	'reason',
	'expires_at',
	'created_at',
] as const satisfies ReadonlyArray<keyof BannedIpRow>;

export interface BannedEmailRow {
	email_lower: string;
}

export const BANNED_EMAIL_COLUMNS = ['email_lower'] as const satisfies ReadonlyArray<keyof BannedEmailRow>;

export interface BannedPhonePrefixRow {
	prefix: string;
}

export const BANNED_PHONE_PREFIX_COLUMNS = ['prefix'] as const satisfies ReadonlyArray<keyof BannedPhonePrefixRow>;

export interface SuspiciousEmailDomainRow {
	domain: string;
}

export const SUSPICIOUS_EMAIL_DOMAIN_COLUMNS = ['domain'] as const satisfies ReadonlyArray<
	keyof SuspiciousEmailDomainRow
>;

export interface DisposableEmailDomainRow {
	domain: string;
}

export const DISPOSABLE_EMAIL_DOMAIN_COLUMNS = ['domain'] as const satisfies ReadonlyArray<
	keyof DisposableEmailDomainRow
>;

export interface BannedPhraseRow {
	phrase: string;
}

export const BANNED_PHRASE_COLUMNS = ['phrase'] as const satisfies ReadonlyArray<keyof BannedPhraseRow>;

export interface BannedUrlRow {
	url_canonical: string;
	category: string | null;
	severity: number | null;
	source_url: string | null;
	added_at: Date | null;
	added_by: bigint | null;
	notes: string | null;
}

export const BANNED_URL_COLUMNS = [
	'url_canonical',
	'category',
	'severity',
	'source_url',
	'added_at',
	'added_by',
	'notes',
] as const satisfies ReadonlyArray<keyof BannedUrlRow>;

export interface BannedUrlDomainRow {
	domain: string;
	match_subdomains: boolean | null;
	category: string | null;
	severity: number | null;
	source_url: string | null;
	added_at: Date | null;
	added_by: bigint | null;
	notes: string | null;
}

export const BANNED_URL_DOMAIN_COLUMNS = [
	'domain',
	'match_subdomains',
	'category',
	'severity',
	'source_url',
	'added_at',
	'added_by',
	'notes',
] as const satisfies ReadonlyArray<keyof BannedUrlDomainRow>;

export interface BannedFileShaRow {
	sha256_hex: string;
	category: string | null;
	severity: number | null;
	content_type: string | null;
	source_url: string | null;
	added_at: Date | null;
	added_by: bigint | null;
	notes: string | null;
}

export const BANNED_FILE_SHA_COLUMNS = [
	'sha256_hex',
	'category',
	'severity',
	'content_type',
	'source_url',
	'added_at',
	'added_by',
	'notes',
] as const satisfies ReadonlyArray<keyof BannedFileShaRow>;

export interface BannedAvatarHashRow {
	hash_short: string;
	category: string | null;
	severity: number | null;
	source_url: string | null;
	added_at: Date | null;
	added_by: bigint | null;
	notes: string | null;
}

export const BANNED_AVATAR_HASH_COLUMNS = [
	'hash_short',
	'category',
	'severity',
	'source_url',
	'added_at',
	'added_by',
	'notes',
] as const satisfies ReadonlyArray<keyof BannedAvatarHashRow>;

export type BannedProfileSubstringScope = 'username' | 'global_name' | 'nickname' | 'bio' | 'pronouns';

export interface BannedProfileSubstringRow {
	scope: BannedProfileSubstringScope;
	substring: string;
	added_at: Date | null;
	added_by: bigint | null;
	notes: string | null;
}

export const BANNED_PROFILE_SUBSTRING_COLUMNS = [
	'scope',
	'substring',
	'added_at',
	'added_by',
	'notes',
] as const satisfies ReadonlyArray<keyof BannedProfileSubstringRow>;
