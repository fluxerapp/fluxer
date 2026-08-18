// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	EmailRevertToken,
	EmailVerificationToken,
	IpAuthorizationToken,
	MfaBackupCode,
	PasswordResetToken,
	UserID,
} from '../../BrandedTypes';

type Nullish<T> = T | null;

export interface AuthSessionRow {
	user_id: UserID;
	session_id_hash: Buffer;
	created_at: Date;
	approx_last_used_at: Date;
	client_ip: string;
	client_user_agent: Nullish<string>;
	client_os: Nullish<string>;
	client_country: Nullish<string>;
	version: number;
}

export interface AuthSessionTombstoneRow {
	user_id: UserID;
	session_id_hash: Buffer;
	created_at: Date;
	approx_last_used_at: Date;
	client_ip: string;
	client_user_agent: Nullish<string>;
	client_os: Nullish<string>;
	client_country: Nullish<string>;
	deleted_at: Date;
	version: number;
}

export interface UserCountryHistoryRow {
	user_id: UserID;
	country: string;
	first_seen_at: Date;
	last_seen_at: Date;
}

export interface MfaBackupCodeRow {
	user_id: UserID;
	code: MfaBackupCode;
	consumed: boolean;
}

export interface EmailVerificationTokenRow {
	token_: EmailVerificationToken;
	user_id: UserID;
	email: string;
}

export interface PasswordResetTokenRow {
	token_: PasswordResetToken;
	user_id: UserID;
	email: string;
}

export interface EmailRevertTokenRow {
	token_: EmailRevertToken;
	user_id: UserID;
	email: string;
}

export interface IpAuthorizationTokenRow {
	token_: IpAuthorizationToken;
	user_id: UserID;
	email: string;
}

export interface UserSsoIdentityRow {
	provider_id: string;
	subject: string;
	user_id: UserID;
	claimed_at: Date;
}

export interface AuthorizedIpRow {
	user_id: UserID;
	ip: string;
}

export interface AuthorizedIpTrustKeyRow {
	user_id: UserID;
	trust_key: string;
}

export interface WebAuthnCredentialRow {
	user_id: UserID;
	credential_id: string;
	public_key: Buffer;
	counter: bigint;
	transports: Nullish<Set<string>>;
	name: string;
	created_at: Date;
	last_used_at: Nullish<Date>;
	version: number;
}

export interface EmailChangeTicketRow {
	ticket: string;
	user_id: UserID;
	require_original: boolean;
	original_email: Nullish<string>;
	original_verified: boolean;
	original_proof: Nullish<string>;
	original_code: Nullish<string>;
	original_code_sent_at: Nullish<Date>;
	original_code_expires_at: Nullish<Date>;
	new_email: Nullish<string>;
	new_code: Nullish<string>;
	new_code_sent_at: Nullish<Date>;
	new_code_expires_at: Nullish<Date>;
	status: string;
	created_at: Date;
	updated_at: Date;
}

export interface EmailChangeTokenRow {
	token_: string;
	user_id: UserID;
	new_email: string;
	expires_at: Date;
	created_at: Date;
}

export const AUTH_SESSION_COLUMNS = [
	'user_id',
	'session_id_hash',
	'created_at',
	'approx_last_used_at',
	'client_ip',
	'client_user_agent',
	'client_os',
	'client_country',
	'version',
] as const satisfies ReadonlyArray<keyof AuthSessionRow>;
export const AUTH_SESSION_TOMBSTONE_COLUMNS = [
	'user_id',
	'session_id_hash',
	'created_at',
	'approx_last_used_at',
	'client_ip',
	'client_user_agent',
	'client_os',
	'client_country',
	'deleted_at',
	'version',
] as const satisfies ReadonlyArray<keyof AuthSessionTombstoneRow>;
export const USER_COUNTRY_HISTORY_COLUMNS = [
	'user_id',
	'country',
	'first_seen_at',
	'last_seen_at',
] as const satisfies ReadonlyArray<keyof UserCountryHistoryRow>;
export const MFA_BACKUP_CODE_COLUMNS = ['user_id', 'code', 'consumed'] as const satisfies ReadonlyArray<
	keyof MfaBackupCodeRow
>;
export const EMAIL_VERIFICATION_TOKEN_COLUMNS = ['token_', 'user_id', 'email'] as const satisfies ReadonlyArray<
	keyof EmailVerificationTokenRow
>;
export const PASSWORD_RESET_TOKEN_COLUMNS = ['token_', 'user_id', 'email'] as const satisfies ReadonlyArray<
	keyof PasswordResetTokenRow
>;
export const EMAIL_REVERT_TOKEN_COLUMNS = ['token_', 'user_id', 'email'] as const satisfies ReadonlyArray<
	keyof EmailRevertTokenRow
>;
export const IP_AUTHORIZATION_TOKEN_COLUMNS = ['token_', 'user_id', 'email'] as const satisfies ReadonlyArray<
	keyof IpAuthorizationTokenRow
>;
export const USER_SSO_IDENTITY_COLUMNS = [
	'provider_id',
	'subject',
	'user_id',
	'claimed_at',
] as const satisfies ReadonlyArray<keyof UserSsoIdentityRow>;
export const AUTHORIZED_IP_COLUMNS = ['user_id', 'ip'] as const satisfies ReadonlyArray<keyof AuthorizedIpRow>;
export const AUTHORIZED_IP_TRUST_KEY_COLUMNS = ['user_id', 'trust_key'] as const satisfies ReadonlyArray<
	keyof AuthorizedIpTrustKeyRow
>;
export const WEBAUTHN_CREDENTIAL_COLUMNS = [
	'user_id',
	'credential_id',
	'public_key',
	'counter',
	'transports',
	'name',
	'created_at',
	'last_used_at',
	'version',
] as const satisfies ReadonlyArray<keyof WebAuthnCredentialRow>;

export interface PhoneTokenRow {
	token_: string;
	phone: string;
	user_id: Nullish<UserID>;
}

export const PHONE_TOKEN_COLUMNS = ['token_', 'phone', 'user_id'] as const satisfies ReadonlyArray<keyof PhoneTokenRow>;

export interface PasswordChangeTicketRow {
	ticket: string;
	user_id: UserID;
	code: Nullish<string>;
	code_sent_at: Nullish<Date>;
	code_expires_at: Nullish<Date>;
	verified: boolean;
	verification_proof: Nullish<string>;
	status: string;
	created_at: Date;
	updated_at: Date;
}

export const PASSWORD_CHANGE_TICKET_COLUMNS = [
	'ticket',
	'user_id',
	'code',
	'code_sent_at',
	'code_expires_at',
	'verified',
	'verification_proof',
	'status',
	'created_at',
	'updated_at',
] as const satisfies ReadonlyArray<keyof PasswordChangeTicketRow>;
export const EMAIL_CHANGE_TICKET_COLUMNS = [
	'ticket',
	'user_id',
	'require_original',
	'original_email',
	'original_verified',
	'original_proof',
	'original_code',
	'original_code_sent_at',
	'original_code_expires_at',
	'new_email',
	'new_code',
	'new_code_sent_at',
	'new_code_expires_at',
	'status',
	'created_at',
	'updated_at',
] as const satisfies ReadonlyArray<keyof EmailChangeTicketRow>;
export const EMAIL_CHANGE_TOKEN_COLUMNS = [
	'token_',
	'user_id',
	'new_email',
	'expires_at',
	'created_at',
] as const satisfies ReadonlyArray<keyof EmailChangeTokenRow>;
