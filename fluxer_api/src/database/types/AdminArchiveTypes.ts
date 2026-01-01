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

export interface AdminArchiveRow {
	subject_type: 'user' | 'guild';
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
}

export const BANNED_IP_COLUMNS = ['ip'] as const satisfies ReadonlyArray<keyof BannedIpRow>;

export interface BannedEmailRow {
	email_lower: string;
}

export const BANNED_EMAIL_COLUMNS = ['email_lower'] as const satisfies ReadonlyArray<keyof BannedEmailRow>;

export interface BannedPhoneRow {
	phone: string;
}

export const BANNED_PHONE_COLUMNS = ['phone'] as const satisfies ReadonlyArray<keyof BannedPhoneRow>;

export interface PendingVerificationRow {
	user_id: bigint;
	created_at: Date;
	version: number;
	metadata: Map<string, string> | null;
}

export const PENDING_VERIFICATION_COLUMNS = [
	'user_id',
	'created_at',
	'version',
	'metadata',
] as const satisfies ReadonlyArray<keyof PendingVerificationRow>;
