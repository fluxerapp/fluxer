// SPDX-License-Identifier: AGPL-3.0-or-later

import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {
	AuthLoginResponse,
	AuthorizeIpRequest,
	AuthRegisterResponse,
	AuthSessionsResponse,
	AuthTokenWithUserIdResponse,
	EmailRevertRequest,
	ForgotPasswordRequest,
	HandoffCodeParam,
	HandoffCompleteRequest,
	HandoffInfoResponse,
	HandoffInitiateResponse,
	HandoffStatusResponse,
	IpAuthorizationPollQuery,
	IpAuthorizationPollResponse,
	LoginRequest,
	LogoutAuthSessionsRequest,
	MfaTicketRequest,
	MfaTotpRequest,
	RegisterRequest,
	ResetPasswordRequest,
	ResetPasswordTokenParam,
	SsoCompleteRequest,
	SsoCompleteResponse,
	SsoStartRequest,
	SsoStartResponse,
	SsoStatusResponse,
	SudoVerificationSchema,
	UsernameSuggestionsRequest,
	UsernameSuggestionsResponse,
	ValidateResetPasswordTokenResponse,
	VerifyEmailRequest,
	WebAuthnAuthenticateRequest,
	WebAuthnAuthenticationOptionsResponse,
	WebAuthnMfaRequest,
} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {Config} from '../Config';
import {DefaultUserOnly, LoginRequiredAllowSuspicious} from '../middleware/AuthMiddleware';
import {CaptchaMiddleware} from '../middleware/CaptchaMiddleware';
import {LocalAuthMiddleware} from '../middleware/LocalAuthMiddleware';
import {RateLimitMiddleware} from '../middleware/RateLimitMiddleware';
import {OpenAPI} from '../middleware/ResponseTypeMiddleware';
import {SudoModeMiddleware} from '../middleware/SudoModeMiddleware';
import {RateLimitConfigs} from '../RateLimitConfig';
import type {HonoApp} from '../types/HonoEnv';
import {Validator} from '../Validator';
import {requireSudoMode} from './services/SudoVerificationService';

export function AuthController(app: HonoApp) {
	app.get(
		'/auth/sso/status',
		RateLimitMiddleware(RateLimitConfigs.AUTH_SSO_START),
		OpenAPI({
			operationId: 'get_sso_status',
			summary: 'Get SSO status',
			responseSchema: SsoStatusResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description: 'Retrieve the current status of the SSO authentication session without authentication required.',
		}),
		async (ctx) => {
			const status = await ctx.get('authRequestService').getSsoStatus();
			return ctx.json(status);
		},
	);
	app.post(
		'/auth/sso/start',
		RateLimitMiddleware(RateLimitConfigs.AUTH_SSO_START),
		Validator('json', SsoStartRequest),
		OpenAPI({
			operationId: 'start_sso',
			summary: 'Start SSO',
			responseSchema: SsoStartResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Initiate a new Single Sign-On (SSO) session. Returns a session URL to be completed with SSO provider credentials.',
		}),
		async (ctx) => {
			const result = await ctx.get('authRequestService').startSso(ctx.req.valid('json'));
			return ctx.json(result);
		},
	);
	app.post(
		'/auth/sso/complete',
		RateLimitMiddleware(RateLimitConfigs.AUTH_SSO_COMPLETE),
		Validator('json', SsoCompleteRequest),
		OpenAPI({
			operationId: 'complete_sso',
			summary: 'Complete SSO',
			responseSchema: SsoCompleteResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Complete the SSO authentication flow with the authorization code from the SSO provider. Returns authentication token and user information.',
		}),
		async (ctx) => {
			const result = await ctx.get('authRequestService').completeSso(ctx.req.valid('json'), ctx.req.raw);
			return ctx.json(result);
		},
	);
	app.post(
		'/auth/register',
		LocalAuthMiddleware,
		CaptchaMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_REGISTER),
		Validator('json', RegisterRequest),
		OpenAPI({
			operationId: 'register_account',
			summary: 'Register account',
			responseSchema: AuthRegisterResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Create a new user account with email and password. Requires CAPTCHA verification. User account is created but must verify email before logging in.',
		}),
		async (ctx) => {
			const result = await ctx.get('authRequestService').register({
				data: ctx.req.valid('json'),
				request: ctx.req.raw,
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(result);
		},
	);
	app.post(
		'/auth/login',
		LocalAuthMiddleware,
		CaptchaMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_LOGIN),
		Validator('json', LoginRequest),
		OpenAPI({
			operationId: 'login_user',
			summary: 'Login account',
			responseSchema: AuthLoginResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Authenticate with email and password. Returns authentication token if credentials are valid and MFA is not required. If MFA is enabled, returns a ticket for MFA verification.',
		}),
		async (ctx) => {
			const result = await ctx.get('authRequestService').login({
				data: ctx.req.valid('json'),
				request: ctx.req.raw,
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(result);
		},
	);
	app.post(
		'/auth/login/mfa/totp',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_LOGIN_MFA),
		Validator('json', MfaTotpRequest),
		OpenAPI({
			operationId: 'login_with_totp',
			summary: 'Login with TOTP',
			responseSchema: AuthTokenWithUserIdResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Complete login by verifying TOTP code during multi-factor authentication. Requires the MFA ticket from initial login attempt.',
		}),
		async (ctx) => {
			const {code, ticket} = ctx.req.valid('json');
			const result = await ctx.get('authRequestService').loginMfaTotp({code, ticket, request: ctx.req.raw});
			return ctx.json(result);
		},
	);
	app.post(
		'/auth/logout',
		RateLimitMiddleware(RateLimitConfigs.AUTH_LOGOUT),
		OpenAPI({
			operationId: 'logout_user',
			summary: 'Logout account',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Auth'],
			description:
				'Invalidate the current authentication token and end the session. The auth token in the Authorization header will no longer be valid.',
		}),
		async (ctx) => {
			await ctx.get('authRequestService').logout({
				authorizationHeader: ctx.req.header('Authorization') ?? undefined,
				authToken: ctx.get('authToken') ?? undefined,
			});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/auth/verify',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_VERIFY_EMAIL),
		Validator('json', VerifyEmailRequest),
		OpenAPI({
			operationId: 'verify_email',
			summary: 'Verify email',
			responseSchema: null,
			statusCode: 204,
			security: [],
			tags: ['Auth'],
			description:
				'Verify user email address using the code sent during registration. Email verification is required before the account becomes fully usable.',
		}),
		async (ctx) => {
			await ctx.get('authRequestService').verifyEmail(ctx.req.valid('json'));
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/auth/verify/resend',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_RESEND_VERIFICATION),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'resend_verification_email',
			summary: 'Resend verification email',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Auth'],
			description:
				'Request a new email verification code to be sent. Requires authentication. Use this if the original verification email was lost or expired.',
		}),
		async (ctx) => {
			await ctx.get('authRequestService').resendVerificationEmail(ctx.get('user'));
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/auth/forgot',
		LocalAuthMiddleware,
		CaptchaMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_FORGOT_PASSWORD),
		Validator('json', ForgotPasswordRequest),
		OpenAPI({
			operationId: 'forgot_password',
			summary: 'Forgot password',
			responseSchema: null,
			statusCode: 204,
			security: [],
			tags: ['Auth'],
			description:
				"Initiate password reset process by email. A password reset link will be sent to the user's email address. Requires CAPTCHA verification.",
		}),
		async (ctx) => {
			await ctx.get('authRequestService').forgotPassword({
				data: ctx.req.valid('json'),
				request: ctx.req.raw,
			});
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/auth/reset/:token',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_VALIDATE_RESET_TOKEN),
		Validator('param', ResetPasswordTokenParam),
		OpenAPI({
			operationId: 'validate_reset_password_token',
			summary: 'Validate reset password token',
			responseSchema: ValidateResetPasswordTokenResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Check whether a password reset token is valid and unexpired before allowing the user to submit a new password. Does not consume the token.',
		}),
		async (ctx) => {
			const result = await ctx.get('authRequestService').validateResetPasswordToken(ctx.req.valid('param').token);
			return ctx.json(result);
		},
	);
	app.post(
		'/auth/reset',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_RESET_PASSWORD),
		Validator('json', ResetPasswordRequest),
		OpenAPI({
			operationId: 'reset_password',
			summary: 'Reset password',
			responseSchema: AuthLoginResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Complete the password reset flow using the token from the reset email. Returns authentication token after successful password reset.',
		}),
		async (ctx) => {
			const result = await ctx.get('authRequestService').resetPassword({
				data: ctx.req.valid('json'),
				request: ctx.req.raw,
			});
			return ctx.json(result);
		},
	);
	app.post(
		'/auth/email-revert',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_EMAIL_REVERT),
		Validator('json', EmailRevertRequest),
		OpenAPI({
			operationId: 'revert_email_change',
			summary: 'Revert email change',
			responseSchema: AuthLoginResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Revert a pending email change using the verification token sent to the old email. Returns authentication token after successful revert.',
		}),
		async (ctx) => {
			const result = await ctx.get('authRequestService').revertEmailChange({
				data: ctx.req.valid('json'),
				request: ctx.req.raw,
			});
			return ctx.json(result);
		},
	);
	app.get(
		'/auth/sessions',
		RateLimitMiddleware(RateLimitConfigs.AUTH_SESSIONS_GET),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_auth_sessions',
			summary: 'List auth sessions',
			responseSchema: AuthSessionsResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Auth'],
			description: 'Retrieve all active authentication sessions for the current user. Requires authentication.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			return ctx.json(await ctx.get('authRequestService').getAuthSessions(userId));
		},
	);
	app.post(
		'/auth/sessions/logout',
		RateLimitMiddleware(RateLimitConfigs.AUTH_SESSIONS_LOGOUT),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', LogoutAuthSessionsRequest.merge(SudoVerificationSchema)),
		OpenAPI({
			operationId: 'logout_all_sessions',
			summary: 'Logout all sessions',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Auth'],
			description:
				'Invalidate all active authentication sessions for the current user. Requires sudo mode verification for security.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body);
			await ctx.get('authRequestService').logoutAuthSessions({user, data: body});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/auth/authorize-ip',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_AUTHORIZE_IP),
		Validator('json', AuthorizeIpRequest),
		OpenAPI({
			operationId: 'authorize_ip_address',
			summary: 'Authorize IP address',
			responseSchema: null,
			statusCode: 204,
			security: [],
			tags: ['Auth'],
			description:
				'Verify and authorize a new IP address using the confirmation code sent via email. Completes IP authorization flow.',
		}),
		async (ctx) => {
			await ctx.get('authRequestService').completeIpAuthorization({data: ctx.req.valid('json')});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/auth/ip-authorization/resend',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_IP_AUTHORIZATION_RESEND),
		Validator('json', MfaTicketRequest),
		OpenAPI({
			operationId: 'resend_ip_authorization',
			summary: 'Resend IP authorization',
			responseSchema: null,
			statusCode: 204,
			security: [],
			tags: ['Auth'],
			description:
				'Request a new IP authorization verification code to be sent via email. Use if the original code was lost or expired.',
		}),
		async (ctx) => {
			await ctx.get('authRequestService').resendIpAuthorization(ctx.req.valid('json'));
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/auth/ip-authorization/poll',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_IP_AUTHORIZATION_POLL),
		Validator('query', IpAuthorizationPollQuery),
		OpenAPI({
			operationId: 'poll_ip_authorization',
			summary: 'Poll IP authorization',
			responseSchema: IpAuthorizationPollResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Poll the status of an IP authorization request. Use the ticket parameter to check if verification has been completed.',
		}),
		async (ctx) => {
			const {ticket} = ctx.req.valid('query');
			return ctx.json(await ctx.get('authRequestService').pollIpAuthorization({ticket}));
		},
	);
	app.post(
		'/auth/webauthn/authentication-options',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_WEBAUTHN_OPTIONS),
		OpenAPI({
			operationId: 'get_webauthn_authentication_options',
			summary: 'Get WebAuthn authentication options',
			responseSchema: WebAuthnAuthenticationOptionsResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Retrieve WebAuthn authentication challenge and options for passwordless login with biometrics or security keys.',
		}),
		async (ctx) => {
			return ctx.json(await ctx.get('authRequestService').getWebAuthnAuthenticationOptions());
		},
	);
	app.post(
		'/auth/webauthn/authenticate',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_WEBAUTHN_AUTHENTICATE),
		Validator('json', WebAuthnAuthenticateRequest),
		OpenAPI({
			operationId: 'authenticate_with_webauthn',
			summary: 'Authenticate with WebAuthn',
			responseSchema: AuthTokenWithUserIdResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Complete passwordless login using WebAuthn (biometrics or security key). Returns authentication token on success.',
		}),
		async (ctx) => {
			return ctx.json(
				await ctx.get('authRequestService').authenticateWebAuthnDiscoverable({
					data: ctx.req.valid('json'),
					request: ctx.req.raw,
				}),
			);
		},
	);
	app.post(
		'/auth/login/mfa/webauthn/authentication-options',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_LOGIN_MFA),
		Validator('json', MfaTicketRequest),
		OpenAPI({
			operationId: 'get_webauthn_mfa_options',
			summary: 'Get WebAuthn MFA options',
			responseSchema: WebAuthnAuthenticationOptionsResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Retrieve WebAuthn challenge and options for multi-factor authentication. Requires the MFA ticket from initial login.',
		}),
		async (ctx) => {
			return ctx.json(await ctx.get('authRequestService').getWebAuthnMfaOptions(ctx.req.valid('json')));
		},
	);
	app.post(
		'/auth/login/mfa/webauthn',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_LOGIN_MFA),
		Validator('json', WebAuthnMfaRequest),
		OpenAPI({
			operationId: 'login_with_webauthn_mfa',
			summary: 'Login with WebAuthn MFA',
			responseSchema: AuthTokenWithUserIdResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Complete login by verifying WebAuthn response during MFA. Requires the MFA ticket from initial login attempt.',
		}),
		async (ctx) => {
			const result = await ctx.get('authRequestService').loginMfaWebAuthn({
				data: ctx.req.valid('json'),
				request: ctx.req.raw,
			});
			return ctx.json(result);
		},
	);
	app.post(
		'/auth/username-suggestions',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_REGISTER),
		Validator('json', UsernameSuggestionsRequest),
		OpenAPI({
			operationId: 'get_username_suggestions',
			summary: 'Get username suggestions',
			responseSchema: UsernameSuggestionsResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description: 'Generate username suggestions based on a provided global name for new account registration.',
		}),
		async (ctx) => {
			const response = ctx.get('authRequestService').getUsernameSuggestions({
				globalName: ctx.req.valid('json').global_name,
			});
			return ctx.json(response);
		},
	);
	app.post(
		'/auth/handoff/initiate',
		RateLimitMiddleware(RateLimitConfigs.AUTH_HANDOFF_INITIATE),
		OpenAPI({
			operationId: 'initiate_handoff',
			summary: 'Initiate handoff',
			responseSchema: HandoffInitiateResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Start a handoff session to transfer authentication between devices. Returns a handoff code for device linking.',
		}),
		async (ctx) => {
			const clientIp = requireClientIp(ctx.req.raw, {
				trustClientIpHeader: Config.proxy.trust_client_ip_header,
				clientIpHeaderName: Config.proxy.client_ip_header,
			});
			const clientPlatform = ctx.req.header('x-fluxer-platform')?.trim().toLowerCase() ?? undefined;
			return ctx.json(
				await ctx.get('authRequestService').initiateHandoff({
					userAgent: ctx.req.header('User-Agent'),
					clientIp,
					clientPlatform,
				}),
			);
		},
	);
	app.get(
		'/auth/handoff/:code/info',
		RateLimitMiddleware(RateLimitConfigs.AUTH_HANDOFF_INFO),
		Validator('param', HandoffCodeParam),
		OpenAPI({
			operationId: 'get_handoff_info',
			summary: 'Get handoff info',
			responseSchema: HandoffInfoResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Retrieve device and location information about a pending handoff request. Non-destructive – the code remains valid after this call.',
		}),
		async (ctx) => {
			const clientIp = requireClientIp(ctx.req.raw, {
				trustClientIpHeader: Config.proxy.trust_client_ip_header,
				clientIpHeaderName: Config.proxy.client_ip_header,
			});
			const response = await ctx.get('authRequestService').getHandoffInfo({
				code: ctx.req.valid('param').code,
				clientIp,
			});
			return ctx.json(response);
		},
	);
	app.post(
		'/auth/handoff/complete',
		RateLimitMiddleware(RateLimitConfigs.AUTH_HANDOFF_COMPLETE),
		Validator('json', HandoffCompleteRequest),
		OpenAPI({
			operationId: 'complete_handoff',
			summary: 'Complete handoff',
			responseSchema: null,
			statusCode: 204,
			security: [],
			tags: ['Auth'],
			description: 'Complete the handoff process and authenticate on the target device using the handoff code.',
		}),
		async (ctx) => {
			const clientIp = requireClientIp(ctx.req.raw, {
				trustClientIpHeader: Config.proxy.trust_client_ip_header,
				clientIpHeaderName: Config.proxy.client_ip_header,
			});
			await ctx.get('authRequestService').completeHandoff({
				data: ctx.req.valid('json'),
				request: ctx.req.raw,
				clientIp,
				authToken: ctx.get('authToken') ?? undefined,
			});
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/auth/handoff/:code/status',
		RateLimitMiddleware(RateLimitConfigs.AUTH_HANDOFF_STATUS),
		Validator('param', HandoffCodeParam),
		OpenAPI({
			operationId: 'get_handoff_status',
			summary: 'Get handoff status',
			responseSchema: HandoffStatusResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Check the status of a handoff session. Returns whether the handoff has been completed or is still pending.',
		}),
		async (ctx) => {
			const clientIp = requireClientIp(ctx.req.raw, {
				trustClientIpHeader: Config.proxy.trust_client_ip_header,
				clientIpHeaderName: Config.proxy.client_ip_header,
			});
			const response = await ctx.get('authRequestService').getHandoffStatus({
				code: ctx.req.valid('param').code,
				clientIp,
			});
			return ctx.json(response);
		},
	);
	app.delete(
		'/auth/handoff/:code',
		RateLimitMiddleware(RateLimitConfigs.AUTH_HANDOFF_CANCEL),
		Validator('param', HandoffCodeParam),
		OpenAPI({
			operationId: 'cancel_handoff',
			summary: 'Cancel handoff',
			responseSchema: null,
			statusCode: 204,
			security: [],
			tags: ['Auth'],
			description: 'Cancel an ongoing handoff session. The handoff code will no longer be valid for authentication.',
		}),
		async (ctx) => {
			await ctx.get('authRequestService').cancelHandoff({code: ctx.req.valid('param').code});
			return ctx.body(null, 204);
		},
	);
}
