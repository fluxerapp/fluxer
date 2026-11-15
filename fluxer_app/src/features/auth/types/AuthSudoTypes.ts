// SPDX-License-Identifier: AGPL-3.0-or-later

export interface SudoVerificationPayload extends Record<string, unknown> {
	password?: string;
	mfa_method?: 'totp' | 'webauthn' | 'email_code';
	mfa_code?: string;
	email_code_ticket?: string;
	email_code?: string;
	webauthn_response?: unknown;
	webauthn_challenge?: string;
}
