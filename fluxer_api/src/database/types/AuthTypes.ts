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

import type {
	BetaCode,
	EmailRevertToken,
	EmailVerificationToken,
	IpAuthorizationToken,
	MfaBackupCode,
	PasswordResetToken,
	UserID,
} from '~/BrandedTypes';

type Nullish<T> = T | null;

export interface AuthSessionRow {
	user_id: UserID;
	session_id_hash: Buffer;
	created_at: Date;
	approx_last_used_at: Date;
	client_ip: string;
	client_os: string;
	client_platform: string;
	client_country: Nullish<string>;
	client_location: Nullish<string>;
	version: number;
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

export interface BetaCodeRow {
	code: BetaCode;
	creator_id: UserID;
	created_at: Date;
	redeemer_id: Nullish<UserID>;
	redeemed_at: Nullish<Date>;
	version: number;
}

export interface BetaCodeByCodeRow {
	code: BetaCode;
	creator_id: UserID;
}

export interface AuthorizedIpRow {
	user_id: UserID;
	ip: string;
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

export interface PhoneTokenRow {
	token_: string;
	phone: string;
	user_id: Nullish<UserID>;
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
	'client_os',
	'client_platform',
	'client_country',
	'client_location',
	'version',
] as const satisfies ReadonlyArray<keyof AuthSessionRow>;

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

export const AUTHORIZED_IP_COLUMNS = ['user_id', 'ip'] as const satisfies ReadonlyArray<keyof AuthorizedIpRow>;

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

export const PHONE_TOKEN_COLUMNS = ['token_', 'phone', 'user_id'] as const satisfies ReadonlyArray<keyof PhoneTokenRow>;

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

export const BETA_CODE_COLUMNS = [
	'creator_id',
	'code',
	'created_at',
	'redeemer_id',
	'redeemed_at',
	'version',
] as const satisfies ReadonlyArray<keyof BetaCodeRow>;

export const BETA_CODE_BY_CODE_COLUMNS = ['code', 'creator_id'] as const satisfies ReadonlyArray<
	keyof BetaCodeByCodeRow
>;
