// SPDX-License-Identifier: AGPL-3.0-or-later

import {ThemeTypes} from '@fluxer/constants/src/UserConstants';
import {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {
	createNamedStringLiteralUnion,
	createStringType,
	SnowflakeStringType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {
	EmailType,
	GlobalNameType,
	PasswordType,
	PhoneNumberType,
	UsernameType,
} from '@fluxer/schema/src/primitives/UserValidators';
import type {
	AuthenticationResponseJSON,
	PublicKeyCredentialCreationOptionsJSON,
	RegistrationResponseJSON,
} from '@simplewebauthn/server';
import {z} from 'zod';

const RegisterThemeType = createNamedStringLiteralUnion(
	[
		[ThemeTypes.DARK, 'DARK', 'Dark colour theme'],
		[ThemeTypes.DARK_LEGACY, 'DARK_LEGACY', 'Legacy dark colour theme (original neutral grey palette)'],
		[ThemeTypes.COAL, 'COAL', 'Coal/darker colour theme'],
		[ThemeTypes.LIGHT, 'LIGHT', 'Light colour theme'],
		[ThemeTypes.SYSTEM, 'SYSTEM', 'Follow system colour preference'],
	] as const,
	'UI theme preference',
);

export const RegisterRequest = z.object({
	email: EmailType.optional().describe('Email address for the new account'),
	username: UsernameType.optional().describe('Username for the new account (1-32 characters)'),
	global_name: GlobalNameType.optional().describe('Display name shown to other users'),
	password: PasswordType.optional().describe('Password for the new account'),
	date_of_birth: createStringType(10, 10)
		.refine((value) => /^\d{4}-\d{2}-\d{2}$/.test(value), 'Invalid date format')
		.optional()
		.describe('Date of birth in YYYY-MM-DD format'),
	consent: z.boolean().default(false).describe('Whether user consents to terms of service'),
	invite_code: createStringType(0, 256).nullish().describe('Guild invite code to join after registration'),
	registration_url_code: createStringType(1, 256)
		.nullish()
		.describe('Admin-issued registration URL code to use for this registration'),
	theme: RegisterThemeType.optional().describe('Initial UI theme preference for the new account'),
});

export type RegisterRequest = z.infer<typeof RegisterRequest>;

export const UsernameSuggestionsRequest = z.object({
	global_name: GlobalNameType.describe('Display name to generate username suggestions from'),
});

export type UsernameSuggestionsRequest = z.infer<typeof UsernameSuggestionsRequest>;

export const LoginRequest = z.object({
	email: EmailType.describe('Email address for authentication'),
	password: PasswordType.describe('Account password'),
	invite_code: createStringType(0, 256).nullish().describe('Guild invite code to join after login'),
});

export type LoginRequest = z.infer<typeof LoginRequest>;

export const LogoutAuthSessionsRequest = z.object({
	session_id_hashes: z.array(createStringType()).max(100).describe('Array of session ID hashes to log out (max 100)'),
	password: PasswordType.optional().describe('Account password for verification'),
});

export type LogoutAuthSessionsRequest = z.infer<typeof LogoutAuthSessionsRequest>;

export const ForgotPasswordRequest = z.object({
	email: EmailType.describe('Email address to send password reset link'),
});

export type ForgotPasswordRequest = z.infer<typeof ForgotPasswordRequest>;

export const ResetPasswordRequest = z.object({
	token: createStringType(64, 64).describe('Password reset token from email'),
	password: PasswordType.describe('New password to set'),
});

export type ResetPasswordRequest = z.infer<typeof ResetPasswordRequest>;

export const ResetPasswordTokenParam = z.object({
	token: createStringType(64, 64).describe('Password reset token from email'),
});

export type ResetPasswordTokenParam = z.infer<typeof ResetPasswordTokenParam>;

export const ValidateResetPasswordTokenResponse = z.object({
	valid: z.boolean().describe('Whether the password reset token is valid and unexpired'),
});

export type ValidateResetPasswordTokenResponse = z.infer<typeof ValidateResetPasswordTokenResponse>;

export const EmailRevertRequest = z.object({
	token: createStringType(64, 64).describe('Email revert token from email'),
	password: PasswordType.describe('Account password for verification'),
});

export type EmailRevertRequest = z.infer<typeof EmailRevertRequest>;

export const VerifyEmailRequest = z.object({
	token: createStringType(64, 64).describe('Email verification token from email'),
});

export type VerifyEmailRequest = z.infer<typeof VerifyEmailRequest>;

export const SudoVerificationSchema = z.object({
	password: PasswordType.optional().describe('Account password for sudo verification'),
	mfa_method: createNamedStringLiteralUnion(
		[
			['totp', 'TOTP', 'Time-based one-time password authentication via authenticator app'],
			['webauthn', 'WebAuthn', 'Security key or biometric authentication'],
		],
		'MFA method to use for verification',
	).optional(),
	mfa_code: createStringType(1, 32).optional().describe('MFA verification code from an authenticator app'),
	webauthn_response: z.custom<AuthenticationResponseJSON>().optional().describe('WebAuthn authentication response'),
	webauthn_challenge: createStringType().optional().describe('WebAuthn challenge string'),
});
export const SsoStatusResponse = z.object({
	enabled: z.boolean().describe('Whether SSO is enabled for this instance'),
	enforced: z.boolean().describe('Whether SSO is required for all users'),
	display_name: z.string().nullable().describe('Display name of the SSO provider'),
	redirect_uri: z.string().describe('OAuth redirect URI for SSO'),
});

export type SsoStatusResponse = z.infer<typeof SsoStatusResponse>;

export const SsoStartResponse = z.object({
	authorization_url: z.string().describe('URL to redirect user to for SSO authentication'),
	state: z.string().describe('State parameter for CSRF protection'),
	redirect_uri: z.string().describe('OAuth redirect URI used for the SSO provider callback'),
});

export type SsoStartResponse = z.infer<typeof SsoStartResponse>;

export const SsoCompleteResponse = z.object({
	token: z.string().describe('Authentication token for the session'),
	user_id: SnowflakeStringType.describe('ID of the authenticated user'),
	user: UserPartialResponse.describe('Partial user data for the authenticated account'),
	redirect_to: z.string().describe('URL to redirect the user to after completion'),
});

export type SsoCompleteResponse = z.infer<typeof SsoCompleteResponse>;

export const AuthTokenWithUserIdResponse = z.object({
	token: z.string().describe('Authentication token for API requests'),
	user_id: SnowflakeStringType.describe('ID of the authenticated user'),
	user: UserPartialResponse.describe('Partial user data for the authenticated account'),
});

export type AuthTokenWithUserIdResponse = z.infer<typeof AuthTokenWithUserIdResponse>;

export const AuthRegistrationPendingApprovalResponse = z.object({
	registration_pending_approval: z.literal(true).describe('Registration succeeded and is waiting for admin approval'),
	user_id: SnowflakeStringType.describe('ID of the registered account waiting for approval'),
});

export type AuthRegistrationPendingApprovalResponse = z.infer<typeof AuthRegistrationPendingApprovalResponse>;

const AuthMfaRequiredResponse = z.object({
	mfa: z.literal(true).describe('Indicates MFA is required to complete authentication'),
	ticket: z.string().describe('MFA ticket to use when completing MFA verification'),
	allowed_methods: z.array(z.string()).max(10).describe('List of allowed MFA methods'),
	totp: z.boolean().describe('Whether TOTP authenticator MFA is available'),
	webauthn: z.boolean().describe('Whether WebAuthn security key MFA is available'),
});

export const AuthLoginResponse = z.union([AuthTokenWithUserIdResponse, AuthMfaRequiredResponse]);

export type AuthLoginResponse = z.infer<typeof AuthLoginResponse>;

export const AuthRegisterResponse = z.union([
	AuthTokenWithUserIdResponse,
	AuthMfaRequiredResponse,
	AuthRegistrationPendingApprovalResponse,
]);

export type AuthRegisterResponse = z.infer<typeof AuthRegisterResponse>;

export const AuthSessionLocation = z.object({
	city: z.string().nullish().describe('The city name reported by the client'),
	region: z.string().nullish().describe('The region reported by the client'),
	country: z.string().nullish().describe('The country reported by the client'),
});

export type AuthSessionLocation = z.infer<typeof AuthSessionLocation>;

const AuthSessionClientInfo = z.object({
	platform: z.string().nullish().describe('The platform reported by the client'),
	os: z.string().nullish().describe('The operating system reported by the client'),
	browser: z.string().nullish().describe('The browser reported by the client'),
	location: AuthSessionLocation.nullish().describe('The geolocation data sent by the client'),
});

export const AuthSessionResponse = z.object({
	id_hash: z.string().describe('The base64url-encoded session id hash'),
	client_info: AuthSessionClientInfo.nullish().describe('Client metadata recorded for this session'),
	masked_ip: z.string().nullable().describe('Semi-redacted IP address recorded for this session'),
	approx_last_used_at: z.iso.datetime().nullish().describe('Approximate timestamp of the last session activity'),
	current: z.boolean().describe('Whether this is the current session making the request'),
});

export type AuthSessionResponse = z.infer<typeof AuthSessionResponse>;

export const AuthSessionsResponse = z.array(AuthSessionResponse);

export type AuthSessionsResponse = z.infer<typeof AuthSessionsResponse>;

export const WebAuthnAuthenticationOptionsResponse = z.custom<PublicKeyCredentialCreationOptionsJSON>();

export type WebAuthnAuthenticationOptionsResponse = z.infer<typeof WebAuthnAuthenticationOptionsResponse>;

export const UsernameSuggestionsResponse = z.object({
	suggestions: z.array(z.string()).max(20).describe('List of suggested usernames'),
});

export type UsernameSuggestionsResponse = z.infer<typeof UsernameSuggestionsResponse>;

export const HandoffInitiateResponse = z.object({
	code: z.string().describe('Handoff code to share with the receiving device'),
	expires_at: z.iso.datetime().describe('ISO 8601 timestamp when the handoff code expires'),
});

export type HandoffInitiateResponse = z.infer<typeof HandoffInitiateResponse>;

const HandoffInfoClientInfo = z.object({
	platform: z.string().nullish().describe('The platform of the requesting device'),
	os: z.string().nullish().describe('The operating system of the requesting device'),
	location: AuthSessionLocation.nullish().describe('The approximate location of the requesting device'),
});

export const HandoffInfoResponse = z.object({
	status: z.string().describe('Current status of the handoff (pending, expired)'),
	client_info: HandoffInfoClientInfo.nullish().describe('Client information of the initiating device'),
});

export type HandoffInfoResponse = z.infer<typeof HandoffInfoResponse>;

export const HandoffStatusResponse = z.object({
	status: z.string().describe('Current status of the handoff (pending, completed, expired)'),
	token: z.string().nullish().describe('Authentication token if handoff is complete'),
	user_id: SnowflakeStringType.nullish().describe('User ID if handoff is complete'),
	user: UserPartialResponse.nullish().describe('Partial user data if handoff is complete'),
});

export type HandoffStatusResponse = z.infer<typeof HandoffStatusResponse>;

export const SsoStartRequest = z.object({
	redirect_to: createStringType(0, 2048).nullish().describe('URL to redirect to after SSO completion'),
	redirect_uri: createStringType(0, 2048).nullish().describe('OAuth redirect URI to use for the SSO provider callback'),
});

export type SsoStartRequest = z.infer<typeof SsoStartRequest>;

export const SsoCompleteRequest = z.object({
	code: createStringType().describe('Authorization code from the SSO provider'),
	state: createStringType().describe('State parameter for CSRF protection'),
});

export type SsoCompleteRequest = z.infer<typeof SsoCompleteRequest>;

export const MfaTotpRequest = z.object({
	code: createStringType().describe('The TOTP code from the authenticator app'),
	ticket: createStringType().describe('The MFA ticket from the login response'),
});

export type MfaTotpRequest = z.infer<typeof MfaTotpRequest>;

export const MfaTicketRequest = z.object({
	ticket: createStringType().describe('The MFA ticket from the login response'),
});

export type MfaTicketRequest = z.infer<typeof MfaTicketRequest>;

export const AuthorizeIpRequest = z.object({
	token: createStringType().describe('The IP authorization token from email'),
});

export type AuthorizeIpRequest = z.infer<typeof AuthorizeIpRequest>;

export const IpAuthorizationPollQuery = z.object({
	ticket: createStringType().describe('The IP authorization ticket'),
});

export type IpAuthorizationPollQuery = z.infer<typeof IpAuthorizationPollQuery>;

export const IpAuthorizationPollResponse = z.object({
	completed: z.boolean().describe('Whether the IP authorization has been completed'),
	token: z.string().nullish().describe('Authentication token if authorization is complete'),
	user_id: SnowflakeStringType.nullish().describe('User ID if authorization is complete'),
	user: UserPartialResponse.nullish().describe('Partial user data if authorization is complete'),
});

export type IpAuthorizationPollResponse = z.infer<typeof IpAuthorizationPollResponse>;

export const WebAuthnAuthenticateRequest = z.object({
	response: z.custom<AuthenticationResponseJSON>().describe('WebAuthn authentication response'),
	challenge: createStringType().describe('The challenge string from authentication options'),
});

export type WebAuthnAuthenticateRequest = z.infer<typeof WebAuthnAuthenticateRequest>;

export const WebAuthnMfaRequest = z.object({
	response: z.custom<AuthenticationResponseJSON>().describe('WebAuthn authentication response'),
	challenge: createStringType().describe('The challenge string from authentication options'),
	ticket: createStringType().describe('The MFA ticket from the login response'),
});

export type WebAuthnMfaRequest = z.infer<typeof WebAuthnMfaRequest>;

export const HandoffCompleteRequest = z.object({
	code: createStringType().describe('The handoff code from the initiating session'),
	token: createStringType().optional().describe('The authentication token to transfer'),
	user_id: createStringType().describe('The user ID associated with the authenticated session'),
});

export type HandoffCompleteRequest = z.infer<typeof HandoffCompleteRequest>;

export const HandoffCodeParam = z.object({
	code: createStringType().describe('The handoff code'),
});

export type HandoffCodeParam = z.infer<typeof HandoffCodeParam>;

export const EnableMfaTotpRequest = z
	.object({
		secret: createStringType(1, 256).describe('The TOTP secret key'),
		code: createStringType(1, 32).describe('The TOTP verification code'),
	})
	.merge(SudoVerificationSchema);

export type EnableMfaTotpRequest = z.infer<typeof EnableMfaTotpRequest>;

export const DisableTotpRequest = z
	.object({
		code: createStringType(1, 32).describe('The TOTP code to verify'),
		password: PasswordType.optional().describe('Account password for verification'),
	})
	.merge(SudoVerificationSchema);

export type DisableTotpRequest = z.infer<typeof DisableTotpRequest>;

export const MfaBackupCodesRequest = z
	.object({
		regenerate: z.boolean().describe('Whether to regenerate backup codes'),
		password: PasswordType.optional().describe('Account password for verification'),
	})
	.merge(SudoVerificationSchema);

export type MfaBackupCodesRequest = z.infer<typeof MfaBackupCodesRequest>;

const MfaBackupCodeResponse = z.object({
	code: z.string().describe('The backup code'),
	consumed: z.boolean().describe('Whether the code has been used'),
});

export const MfaBackupCodesResponse = z.object({
	backup_codes: z.array(MfaBackupCodeResponse).describe('List of backup codes'),
});

export type MfaBackupCodesResponse = z.infer<typeof MfaBackupCodesResponse>;

export const PhoneSendVerificationRequest = z.object({
	phone: PhoneNumberType.describe('Phone number to send verification code'),
	channel: z
		.enum(['sms', 'inbound_challenge'])
		.optional()
		.describe(
			'Channel to deliver the OTP on. Defaults to the first available channel from server policy. Server may override to an available fallback when the requested channel is disabled.',
		),
});

export type PhoneSendVerificationRequest = z.infer<typeof PhoneSendVerificationRequest>;

const PhoneSendVerificationDeliveredResponse = z.object({
	channel: z
		.literal('sms')
		.describe('Channel actually used for delivery (may differ from request when server adjusts)'),
});

const PhoneSendVerificationInboundChallengeResponse = z.object({
	channel: z.literal('inbound_challenge').describe('The user must send Fluxer an SMS instead of receiving one'),
	challenge_code: createStringType(4, 12).describe('The numeric code the user must text to our number'),
	our_number: createStringType(4, 32).describe('The Twilio number the user must text the code to (E.164)'),
	expires_at: z.iso.datetime().describe('ISO 8601 timestamp when this inbound challenge expires'),
	reason: z
		.enum(['voip', 'canadian', 'unknown_line_type', 'expensive_destination', 'account_forced', 'behavioural_risk'])
		.describe('Why inbound verification is required'),
});

export const PhoneSendVerificationResponse = z.union([
	PhoneSendVerificationDeliveredResponse,
	PhoneSendVerificationInboundChallengeResponse,
]);

export type PhoneSendVerificationResponse = z.infer<typeof PhoneSendVerificationResponse>;

export const PhoneVerifyRequest = z.object({
	phone: PhoneNumberType.describe('Phone number being verified'),
	code: createStringType(1, 32).describe('The verification code'),
});

export type PhoneVerifyRequest = z.infer<typeof PhoneVerifyRequest>;

export const PhoneVerifyResponse = z.object({
	verified: z.literal(true).describe('Indicates the phone number was verified successfully'),
});

export type PhoneVerifyResponse = z.infer<typeof PhoneVerifyResponse>;

export const WebAuthnCredentialResponse = z.object({
	id: z.string().describe('The credential ID'),
	name: z.string().describe('User-assigned name for the credential'),
	created_at: z.string().describe('When the credential was registered'),
	last_used_at: z.string().nullable().describe('When the credential was last used'),
});

export type WebAuthnCredentialResponse = z.infer<typeof WebAuthnCredentialResponse>;

export const WebAuthnCredentialListResponse = z.array(WebAuthnCredentialResponse);

export type WebAuthnCredentialListResponse = z.infer<typeof WebAuthnCredentialListResponse>;

export const WebAuthnChallengeResponse = z
	.object({
		challenge: z.string().describe('The WebAuthn challenge'),
	})
	.passthrough();

export type WebAuthnChallengeResponse = z.infer<typeof WebAuthnChallengeResponse>;

export const WebAuthnRegisterRequest = z.object({
	response: z.custom<RegistrationResponseJSON>().describe('WebAuthn registration response'),
	challenge: createStringType(1, 1024).describe('The challenge from registration options'),
	name: createStringType(1, 100).describe('User-assigned name for the credential'),
});

export type WebAuthnRegisterRequest = z.infer<typeof WebAuthnRegisterRequest>;

export const WebAuthnCredentialUpdateRequest = z
	.object({
		name: createStringType(1, 100).describe('New name for the credential'),
	})
	.merge(SudoVerificationSchema);

export type WebAuthnCredentialUpdateRequest = z.infer<typeof WebAuthnCredentialUpdateRequest>;

export const SudoMfaMethodsResponse = z.object({
	totp: z.boolean().describe('Whether TOTP is enabled'),
	webauthn: z.boolean().describe('Whether WebAuthn is enabled'),
	has_mfa: z.boolean().describe('Whether any MFA method is enabled'),
});

export type SudoMfaMethodsResponse = z.infer<typeof SudoMfaMethodsResponse>;

export const InboundSmsChallengeStartResponse = z.object({
	challenge_code: createStringType(4, 12).describe('The numeric code the user must text to our number'),
	our_number: createStringType(4, 32).describe('The Twilio number the user must text the code to (E.164)'),
	expires_at: z.string().describe('ISO timestamp at which the challenge becomes invalid'),
});

export type InboundSmsChallengeStartResponse = z.infer<typeof InboundSmsChallengeStartResponse>;
