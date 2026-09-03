// SPDX-License-Identifier: AGPL-3.0-or-later

import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {
	DisableTotpRequest,
	EnableMfaTotpRequest,
	InboundSmsChallengeStartResponse,
	MfaBackupCodesChallengeRegenerateRequest,
	MfaBackupCodesChallengeResendRequest,
	MfaBackupCodesChallengeStartResponse,
	MfaBackupCodesChallengeVerifyRequest,
	MfaBackupCodesChallengeVerifyResponse,
	MfaBackupCodesRequest,
	MfaBackupCodesResponse,
	PhoneSendVerificationRequest,
	PhoneSendVerificationResponse,
	PhoneVerifyRequest,
	PhoneVerifyResponse,
	SudoMfaMethodsResponse,
	SudoVerificationSchema,
	WebAuthnChallengeResponse,
	WebAuthnCredentialListResponse,
	WebAuthnCredentialUpdateRequest,
	WebAuthnRegisterRequest,
} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {CredentialIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {EmptyBodyRequest} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {requireSudoMode} from '../../auth/services/SudoVerificationService';
import {Config} from '../../Config';
import {DefaultUserOnly, LoginRequired, LoginRequiredAllowSuspicious} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {SudoModeMiddleware} from '../../middleware/SudoModeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function UserAuthController(app: HonoApp) {
	app.post(
		'/users/@me/mfa/totp/enable',
		RateLimitMiddleware(RateLimitConfigs.USER_MFA_TOTP_ENABLE),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', EnableMfaTotpRequest),
		OpenAPI({
			operationId: 'enable_totp_mfa',
			summary: 'Enable TOTP multi-factor authentication',
			responseSchema: MfaBackupCodesResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Enable time-based one-time password (TOTP) MFA on the current account. Returns backup codes for account recovery. Requires sudo mode verification.',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const user = ctx.get('user');
			const sudoResult = await requireSudoMode(ctx, user, body);
			return ctx.json(
				await ctx.get('userAuthRequestService').enableTotp({
					user,
					data: body,
					sudoContext: sudoResult,
				}),
			);
		},
	);
	app.post(
		'/users/@me/mfa/totp/disable',
		RateLimitMiddleware(RateLimitConfigs.USER_MFA_TOTP_DISABLE),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', DisableTotpRequest),
		OpenAPI({
			operationId: 'disable_totp_mfa',
			summary: 'Disable TOTP multi-factor authentication',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Disable TOTP multi-factor authentication on the current account. Requires sudo mode verification for security.',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const user = ctx.get('user');
			const sudoResult = await requireSudoMode(ctx, user, body);
			await ctx.get('userAuthRequestService').disableTotp({user, data: body, sudoContext: sudoResult});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/users/@me/mfa/backup-codes',
		RateLimitMiddleware(RateLimitConfigs.USER_MFA_BACKUP_CODES),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', MfaBackupCodesRequest),
		OpenAPI({
			operationId: 'get_backup_codes_mfa',
			summary: 'Get backup codes for multi-factor authentication',
			responseSchema: MfaBackupCodesResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Generate and retrieve new backup codes for account recovery. Requires sudo mode verification. Old codes are invalidated.',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const user = ctx.get('user');
			const sudoResult = await requireSudoMode(ctx, user, body);
			return ctx.json(
				await ctx.get('userAuthRequestService').getBackupCodes({user, data: body, sudoContext: sudoResult}),
			);
		},
	);
	app.post(
		'/users/@me/mfa/backup-codes/challenge',
		RateLimitMiddleware(RateLimitConfigs.USER_MFA_BACKUP_CODES_CHALLENGE_START),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', EmptyBodyRequest),
		OpenAPI({
			operationId: 'start_backup_codes_challenge',
			summary: 'Start backup codes challenge',
			responseSchema: MfaBackupCodesChallengeStartResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				"Initiates the challenge required to view existing backup codes. Sends a verification code to the user's email address. Returns a ticket for use in the remaining challenge steps.",
		}),
		async (ctx) => {
			const user = ctx.get('user');
			return ctx.json(await ctx.get('mfaBackupCodesChallengeService').start(user));
		},
	);
	app.post(
		'/users/@me/mfa/backup-codes/challenge/resend',
		RateLimitMiddleware(RateLimitConfigs.USER_MFA_BACKUP_CODES_CHALLENGE_RESEND),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', MfaBackupCodesChallengeResendRequest),
		OpenAPI({
			operationId: 'resend_backup_codes_challenge',
			summary: 'Resend backup codes challenge code',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Resends the verification code for a backup codes challenge. Use if the original code was not received. Requires a valid backup codes challenge ticket.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await ctx.get('mfaBackupCodesChallengeService').resend(user, body.ticket);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/users/@me/mfa/backup-codes/challenge/verify',
		RateLimitMiddleware(RateLimitConfigs.USER_MFA_BACKUP_CODES_CHALLENGE_VERIFY),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', MfaBackupCodesChallengeVerifyRequest),
		OpenAPI({
			operationId: 'verify_backup_codes_challenge',
			summary: 'Verify backup codes challenge code',
			responseSchema: MfaBackupCodesChallengeVerifyResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Verifies the email code sent during a backup codes challenge and returns the existing backup codes along with a proof token. The code is consumed on success and the proof token authorizes regeneration on the same ticket.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			return ctx.json(await ctx.get('mfaBackupCodesChallengeService').verify(user, body.ticket, body.code));
		},
	);
	app.post(
		'/users/@me/mfa/backup-codes/challenge/regenerate',
		RateLimitMiddleware(RateLimitConfigs.USER_MFA_BACKUP_CODES_CHALLENGE_REGENERATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', MfaBackupCodesChallengeRegenerateRequest),
		OpenAPI({
			operationId: 'regenerate_backup_codes_challenge',
			summary: 'Regenerate backup codes with a verified challenge',
			responseSchema: MfaBackupCodesResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Replaces the account backup codes using the proof token from a verified backup codes challenge. Old codes are invalidated.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			return ctx.json(
				await ctx.get('mfaBackupCodesChallengeService').regenerate(user, body.ticket, body.verification_proof),
			);
		},
	);
	app.post(
		'/users/@me/phone/send-verification',
		RateLimitMiddleware(RateLimitConfigs.PHONE_SEND_VERIFICATION),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator('json', PhoneSendVerificationRequest),
		OpenAPI({
			operationId: 'send_phone_verification_code',
			summary: 'Send phone verification code',
			responseSchema: PhoneSendVerificationResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Send a one-time code on the requested channel. Defaults to the first available channel from server policy. Pass channel="sms" to request SMS (only honoured for SMS-allowlisted destinations) or channel="inbound_challenge" to receive challenge details to text in. Expensive outbound destinations always downgrade to an inbound challenge.',
		}),
		async (ctx) => {
			return ctx.json(
				await ctx.get('userAuthRequestService').sendPhoneVerificationCode({
					user: ctx.get('user'),
					data: ctx.req.valid('json'),
					clientIp: requireClientIp(ctx.req.raw, {
						trustClientIpHeader: Config.proxy.trust_client_ip_header,
						clientIpHeaderName: Config.proxy.client_ip_header,
					}),
				}),
			);
		},
	);
	app.post(
		'/users/@me/phone/inbound-challenge',
		RateLimitMiddleware(RateLimitConfigs.PHONE_SEND_VERIFICATION),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'start_inbound_phone_challenge',
			summary: 'Start an inbound SMS challenge',
			responseSchema: InboundSmsChallengeStartResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				"For very-high-risk registrations the platform requires the user to text a one-time code to the platform's number, instead of receiving a code from the platform. This endpoint generates the code and the destination number to display.",
		}),
		async (ctx) => {
			return ctx.json(await ctx.get('userAuthRequestService').startInboundPhoneChallenge(ctx.get('user')));
		},
	);
	app.post(
		'/users/@me/phone/verify',
		RateLimitMiddleware(RateLimitConfigs.PHONE_VERIFY_CODE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator('json', PhoneVerifyRequest),
		OpenAPI({
			operationId: 'verify_phone_code',
			summary: 'Verify phone code',
			responseSchema: PhoneVerifyResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description: 'Verify a phone number by confirming the SMS verification code. Returns phone verification status.',
		}),
		async (ctx) => {
			return ctx.json(
				await ctx.get('userAuthRequestService').verifyPhoneCode({user: ctx.get('user'), data: ctx.req.valid('json')}),
			);
		},
	);
	app.delete(
		'/users/@me/authorized-ips',
		RateLimitMiddleware(RateLimitConfigs.USER_AUTHORIZED_IPS_FORGET),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		OpenAPI({
			operationId: 'forget_authorized_ips',
			summary: 'Forget authorized IPs for current user',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Clears all authorized IP addresses for the current user. After calling this endpoint, the user will be required to re-authorize any new IP addresses they log in from. Requires sudo mode verification.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body);
			await ctx.get('userAuthRequestService').forgetAuthorizedIps(user);
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/users/@me/mfa/webauthn/credentials',
		RateLimitMiddleware(RateLimitConfigs.MFA_WEBAUTHN_LIST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_webauthn_credentials',
			summary: 'List WebAuthn credentials',
			responseSchema: WebAuthnCredentialListResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Retrieve all registered WebAuthn credentials (security keys, biometric devices) for the current user. Requires authentication.',
		}),
		async (ctx) => {
			return ctx.json(await ctx.get('userAuthRequestService').listWebAuthnCredentials(ctx.get('user')));
		},
	);
	app.post(
		'/users/@me/mfa/webauthn/credentials/registration-options',
		RateLimitMiddleware(RateLimitConfigs.MFA_WEBAUTHN_REGISTRATION_OPTIONS),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		OpenAPI({
			operationId: 'get_webauthn_registration_options',
			summary: 'Get WebAuthn registration options',
			responseSchema: WebAuthnChallengeResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Generate challenge and options to register a new WebAuthn credential. Requires sudo mode verification.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, {
				issueSudoToken: false,
			});
			return ctx.json(await ctx.get('userAuthRequestService').generateWebAuthnRegistrationOptions(user));
		},
	);
	app.post(
		'/users/@me/mfa/webauthn/credentials',
		RateLimitMiddleware(RateLimitConfigs.MFA_WEBAUTHN_REGISTER),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', WebAuthnRegisterRequest),
		OpenAPI({
			operationId: 'register_webauthn_credential',
			summary: 'Register WebAuthn credential',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Complete registration of a new WebAuthn credential (security key or biometric device) using a challenge created after sudo mode verification.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await ctx.get('userAuthRequestService').registerWebAuthnCredential({
				user,
				data: body,
			});
			return ctx.body(null, 204);
		},
	);
	app.patch(
		'/users/@me/mfa/webauthn/credentials/:credential_id',
		RateLimitMiddleware(RateLimitConfigs.MFA_WEBAUTHN_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', CredentialIdParam),
		Validator('json', WebAuthnCredentialUpdateRequest),
		SudoModeMiddleware,
		OpenAPI({
			operationId: 'update_webauthn_credential',
			summary: 'Update WebAuthn credential',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description: 'Update the name or settings of a registered WebAuthn credential. Requires sudo mode verification.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const {credential_id} = ctx.req.valid('param');
			const {name, ...sudoBody} = ctx.req.valid('json');
			await requireSudoMode(ctx, user, sudoBody);
			await ctx.get('userAuthRequestService').renameWebAuthnCredential({
				user,
				credentialId: credential_id,
				data: {name},
			});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/users/@me/mfa/webauthn/credentials/:credential_id',
		RateLimitMiddleware(RateLimitConfigs.MFA_WEBAUTHN_DELETE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', CredentialIdParam),
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		OpenAPI({
			operationId: 'delete_webauthn_credential',
			summary: 'Delete WebAuthn credential',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Remove a registered WebAuthn credential from the current account. Requires sudo mode verification for security.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const {credential_id} = ctx.req.valid('param');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body);
			await ctx.get('userAuthRequestService').deleteWebAuthnCredential({user, credentialId: credential_id});
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/users/@me/sudo/mfa-methods',
		RateLimitMiddleware(RateLimitConfigs.SUDO_MFA_METHODS),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_sudo_mfa_methods',
			summary: 'List sudo multi-factor authentication methods',
			responseSchema: SudoMfaMethodsResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Retrieve all available MFA methods for sudo mode verification (TOTP, WebAuthn). Requires authentication.',
		}),
		async (ctx) => {
			return ctx.json(await ctx.get('userAuthRequestService').listSudoMfaMethods(ctx.get('user')));
		},
	);
	app.post(
		'/users/@me/sudo/webauthn/authentication-options',
		RateLimitMiddleware(RateLimitConfigs.SUDO_WEBAUTHN_OPTIONS),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'get_sudo_webauthn_authentication_options',
			summary: 'Get sudo WebAuthn authentication options',
			responseSchema: WebAuthnChallengeResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Generate WebAuthn challenge for sudo mode verification using a registered security key or biometric device.',
		}),
		async (ctx) => {
			return ctx.json(await ctx.get('userAuthRequestService').getSudoWebAuthnOptions(ctx.get('user')));
		},
	);
}
