// SPDX-License-Identifier: AGPL-3.0-or-later

import {WebAuthnAuthenticationOptionsResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {WebAuthnAuthenticationResponse} from '@fluxer/schema/src/domains/auth/WebAuthnSchemas';
import {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const Base64UrlPattern = /^[A-Za-z0-9_-]+$/u;
const PasskeyBridgeSecretPattern = /^[A-Za-z0-9_-]{43}$/u;

export const PasskeyBridgeRunner = z
	.enum(['page', 'native'])
	.describe('Where the passkey ceremony runs: a page on the paired origin or the native desktop client');

export type PasskeyBridgeRunner = z.infer<typeof PasskeyBridgeRunner>;

const PasskeyBridgeNonceHash = z
	.string()
	.regex(/^[0-9a-f]{64}$/u)
	.describe('Lowercase hex SHA-256 digest of the nonce the starting page keeps');

export const PasskeyBridgeLoginStartRequest = z
	.object({
		purpose: z.enum(['login', 'login_mfa']).describe('Whether the passkey signs in or completes two-factor sign in'),
		runner: PasskeyBridgeRunner,
		ticket: z.string().min(1).max(256).optional().describe('The MFA ticket from the login response, for login_mfa'),
		nonce_hash: PasskeyBridgeNonceHash,
	})
	.superRefine((value, ctx) => {
		if ((value.purpose === 'login_mfa') !== (value.ticket !== undefined)) {
			ctx.addIssue({code: 'custom', path: ['ticket'], message: 'A ticket is required only for login_mfa'});
		}
	});

export type PasskeyBridgeLoginStartRequest = z.infer<typeof PasskeyBridgeLoginStartRequest>;

export const PasskeyBridgeSudoStartRequest = z.object({
	runner: PasskeyBridgeRunner,
	nonce_hash: PasskeyBridgeNonceHash,
});

export type PasskeyBridgeSudoStartRequest = z.infer<typeof PasskeyBridgeSudoStartRequest>;

export const PasskeyBridgeStartResponse = z.object({
	ceremony_id: z.string().describe('Identifier of the passkey ceremony'),
	bridge_url: z.string().nullable().describe('Page that runs the ceremony, or null for the native runner'),
});

export type PasskeyBridgeStartResponse = z.infer<typeof PasskeyBridgeStartResponse>;

export const PasskeyBridgeCeremonyIdParam = z.object({
	ceremony_id: z.string().regex(PasskeyBridgeSecretPattern).describe('Identifier of the passkey ceremony'),
});

export type PasskeyBridgeCeremonyIdParam = z.infer<typeof PasskeyBridgeCeremonyIdParam>;

export const PasskeyBridgeOptionsResponse = z.object({
	options: WebAuthnAuthenticationOptionsResponse.describe('WebAuthn authentication options for the ceremony'),
});

export type PasskeyBridgeOptionsResponse = z.infer<typeof PasskeyBridgeOptionsResponse>;

export const PasskeyBridgeCompleteRequest = z.object({
	response: WebAuthnAuthenticationResponse.describe('WebAuthn authentication response'),
});

export type PasskeyBridgeCompleteRequest = z.infer<typeof PasskeyBridgeCompleteRequest>;

export const PasskeyBridgeFinishResponse = z.object({
	return_url: z.string().nullable().describe('Where the page runner goes next, or null for the native runner'),
	completion_code: z.string().nullable().describe('Code the native runner redeems, or null for the page runner'),
});

export type PasskeyBridgeFinishResponse = z.infer<typeof PasskeyBridgeFinishResponse>;

export const PasskeyBridgeRedeemRequest = z.object({
	nonce: z
		.string()
		.min(16)
		.max(256)
		.regex(Base64UrlPattern)
		.describe('Nonce whose SHA-256 digest was sent when the ceremony started'),
	completion_code: z.string().regex(PasskeyBridgeSecretPattern).describe('Code handed back when the ceremony finished'),
});

export type PasskeyBridgeRedeemRequest = z.infer<typeof PasskeyBridgeRedeemRequest>;

const PasskeyBridgeCancelledResponse = z.object({
	status: z.literal('cancelled').describe('The ceremony was cancelled'),
});

export const PasskeyBridgeLoginRedeemResponse = z.discriminatedUnion('status', [
	PasskeyBridgeCancelledResponse,
	z.object({
		status: z.literal('completed').describe('The ceremony finished'),
		token: z.string().describe('Authentication token for API requests'),
		user_id: SnowflakeStringType.describe('ID of the authenticated user'),
		user: UserPartialResponse.describe('Partial user data for the authenticated account'),
	}),
]);

export type PasskeyBridgeLoginRedeemResponse = z.infer<typeof PasskeyBridgeLoginRedeemResponse>;

export const PasskeyBridgeSudoRedeemResponse = z.discriminatedUnion('status', [
	PasskeyBridgeCancelledResponse,
	z.object({
		status: z.literal('completed').describe('The ceremony finished'),
		sudo_token: z.string().describe('Sudo mode token'),
	}),
]);

export type PasskeyBridgeSudoRedeemResponse = z.infer<typeof PasskeyBridgeSudoRedeemResponse>;
