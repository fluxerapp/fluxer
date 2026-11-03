// SPDX-License-Identifier: AGPL-3.0-or-later

import {addressLookupCaptchaSteps, budgetEnvFor} from '@app/api/auth/AccountBudgets';
import {normalizeEmailKey} from '@app/api/auth/AccountCodes';
import * as AuthEmail from '@app/api/auth/AuthEmail';
import * as AuthLogin from '@app/api/auth/AuthLogin';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import * as AuthPassword from '@app/api/auth/AuthPassword';
import {
	assertAccountCreationPastSetup,
	instantAccountsAvailable,
	isInstantAccountRequest,
} from '@app/api/auth/AuthRegistration';
import * as AuthSession from '@app/api/auth/AuthSession';
import {redeemSignInLink} from '@app/api/auth/SignInLinks';
import {requireSudoMode} from '@app/api/auth/services/SudoVerificationService';
import {usesUsernameSignIn} from '@app/api/instance/AccountIdentityModeCache';
import {
	onUsernameInstance,
	RequireEmailAccountIdentity,
	RequireUsernameAccountIdentity,
	RequireUsernameLookup,
} from '@app/api/middleware/AccountIdentityMiddleware';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {CaptchaMiddleware, EscalatingCaptchaMiddleware} from '@app/api/middleware/CaptchaMiddleware';
import {LocalAuthMiddleware, MfaTicketAuthMiddleware} from '@app/api/middleware/LocalAuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {SudoModeMiddleware} from '@app/api/middleware/SudoModeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {scheduleAbandonedOnLastLogout} from '@app/api/user/AccountCleanup';
import {tierContextFrom} from '@app/api/user/AccountTier';
import {isJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {requireRequestClientIp} from '@app/api/utils/RequestClientIp';
import {Validator} from '@app/api/Validator';
import {
	AuthLoginResponse,
	AuthorizeIpRequest,
	AuthRegisterResponse,
	AuthSessionsResponse,
	AuthTokenWithUserIdResponse,
	EmailRevertRequest,
	ForgotPasswordRequest,
	HandoffCancelRequest,
	HandoffCodeParam,
	HandoffCompleteRequest,
	HandoffCompleteResponse,
	HandoffInfoResponse,
	HandoffInitiateRequest,
	HandoffInitiateResponse,
	HandoffStatusRequest,
	HandoffStatusResponse,
	IpAuthorizationPollQuery,
	IpAuthorizationPollResponse,
	LoginRequest,
	LogoutAuthSessionsWithVerificationRequest,
	MfaTicketRequest,
	MfaTotpRequest,
	RecoverAccountRequest,
	RecoverAccountResponse,
	RegisterRequest,
	ResetPasswordRequest,
	ResetPasswordTokenParam,
	SignInLinkRequest,
	SsoCompleteRequest,
	SsoCompleteResponse,
	SsoStartRequest,
	SsoStartResponse,
	SsoStatusResponse,
	UsernameAvailabilityQuery,
	UsernameAvailabilityResponse,
	UsernameInstanceLoginRequest,
	UsernameInstanceRegisterRequest,
	UsernameSuggestionsRequest,
	UsernameSuggestionsResponse,
	ValidateResetPasswordTokenResponse,
	VerifyEmailRequest,
	WebAuthnAuthenticateRequest,
	WebAuthnAuthenticationOptionsResponse,
	WebAuthnMfaRequest,
} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {Context} from 'hono';

function optionalString(value: unknown): string | null {
	return typeof value === 'string' && value.length > 0 ? value : null;
}

async function registrationCaptchaSteps(ctx: Context<HonoEnv>): Promise<number> {
	const body: unknown = await ctx.req.json();
	const fields = isJsonRecord(body) ? body : {};
	const data = {email: optionalString(fields.email), password: optionalString(fields.password)};
	const clientIp = requireRequestClientIp(ctx);
	if (isInstantAccountRequest(data, {usernameMode: usesUsernameSignIn()})) {
		const registrationUrlCode = optionalString(fields.registration_url_code);
		if (!(await instantAccountsAvailable(ctx.get('instanceConfigRepository'), registrationUrlCode))) return 0;
		if (optionalString(fields.setup_token) === null) {
			await assertAccountCreationPastSetup(ctx.get('apiContext'), ctx.get('instanceConfigRepository'), clientIp);
		}
		return await ctx.get('authRequestService').instantAccountChallengeSteps(ctx.req.raw, clientIp, registrationUrlCode);
	}
	if (!data.email || usesUsernameSignIn()) return 0;
	const lookupSteps = await addressLookupCaptchaSteps(budgetEnvFor(ctx.get('apiContext').services, ctx.req.raw), {
		clientIp,
		emailKey: normalizeEmailKey(data.email),
		requester: 'unknown',
	});
	return lookupSteps ?? 0;
}

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
			const status = await ctx.get('ssoService').getPublicStatus();
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
				'Complete the SSO authentication flow with the authorization code from the SSO provider. Returns authentication token and user information, or linked when the flow was started from account settings.',
		}),
		async (ctx) => {
			const result = await ctx
				.get('authRequestService')
				.completeSso(ctx.req.valid('json'), ctx.req.raw, ctx.get('requestCache'), requireRequestClientIp(ctx));
			return ctx.json(result);
		},
	);
	app.post(
		'/auth/register',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_REGISTER),
		Validator('json', RegisterRequest, {schemaFor: onUsernameInstance(UsernameInstanceRegisterRequest)}),
		EscalatingCaptchaMiddleware(registrationCaptchaSteps),
		OpenAPI({
			operationId: 'register_account',
			summary: 'Register account',
			responseSchema: AuthRegisterResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Create a new user account. A body without an email and a password creates an account without an email from global_name: the username is derived from the display name and given a tag, and the account signs in on this device only until it is claimed (an email on email instances, a password on username instances). On email instances an email sent here is stored on the account and waits for confirmation. An address stored on another account, confirmed or not, answers EMAIL_ALREADY_IN_USE. Username instances take a username and password, and an email sent by an older client is discarded. The date of birth can be left out where the instance collects it, and the app then asks for it before anything else. Requires a solved captcha challenge (X-Captcha-Token), which costs more after recent sign-ups without an email from the same network.',
		}),
		async (ctx) => {
			const result = await ctx.get('authRequestService').register({
				data: ctx.req.valid('json'),
				request: ctx.req.raw,
				requestCache: ctx.get('requestCache'),
				tierContext: tierContextFrom(ctx),
			});
			return ctx.json(result);
		},
	);
	app.post(
		'/auth/login',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_LOGIN),
		CaptchaMiddleware,
		Validator('json', LoginRequest, {schemaFor: onUsernameInstance(UsernameInstanceLoginRequest)}),
		OpenAPI({
			operationId: 'login_user',
			summary: 'Login account',
			responseSchema: AuthLoginResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Authenticate with a password and either email (or login on email instances) or login (a username on username instances). Returns authentication token if credentials are valid and MFA is not required. If MFA is enabled, returns a ticket for MFA verification. Requires a solved captcha challenge (X-Captcha-Token).',
		}),
		async (ctx) => {
			const result = await ctx.get('authRequestService').login({
				data: ctx.req.valid('json'),
				request: ctx.req.raw,
				captchaVerified: ctx.get('captchaVerified') === true,
			});
			return ctx.json(result);
		},
	);
	app.post(
		'/auth/login/mfa/totp',
		RateLimitMiddleware(RateLimitConfigs.AUTH_LOGIN_MFA),
		Validator('json', MfaTotpRequest),
		MfaTicketAuthMiddleware,
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
		LoginRequired,
		OpenAPI({
			operationId: 'logout_user',
			summary: 'Logout account',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Auth'],
			description:
				'Invalidate the current authentication token and end the session. The auth token in the Authorization header will no longer be valid. A bot token has no session to end, so the call answers 204 and the token stays valid. Ending the last session of an account that cannot sign in again, because it has no email, password or passkey, schedules that account for deletion after 24 hours.',
		}),
		async (ctx) => {
			const authToken = ctx.get('authToken');
			if (authToken) {
				await AuthSession.revokeToken(ctx.get('apiContext'), authToken);
				await scheduleAbandonedOnLastLogout(ctx.get('apiContext'), ctx.get('user'));
			}
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/auth/verify',
		LocalAuthMiddleware,
		RequireEmailAccountIdentity,
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
		RequireEmailAccountIdentity,
		RateLimitMiddleware(RateLimitConfigs.AUTH_RESEND_VERIFICATION),
		LoginRequired,
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
			await AuthEmail.resendVerificationEmail(ctx.get('apiContext'), ctx.get('user'));
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/auth/forgot',
		LocalAuthMiddleware,
		RequireEmailAccountIdentity,
		RateLimitMiddleware(RateLimitConfigs.AUTH_FORGOT_PASSWORD),
		CaptchaMiddleware,
		Validator('json', ForgotPasswordRequest),
		OpenAPI({
			operationId: 'forgot_password',
			summary: 'Forgot password',
			responseSchema: null,
			statusCode: 204,
			security: [],
			tags: ['Auth'],
			description:
				"Initiate password reset process by email. A password reset link will be sent to the user's email address. Requires a solved captcha challenge (X-Captcha-Token).",
		}),
		async (ctx) => {
			await AuthPassword.forgotPassword(ctx.get('apiContext'), {
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
		'/auth/recover',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_RECOVER_ACCOUNT),
		RequireUsernameAccountIdentity,
		CaptchaMiddleware,
		Validator('json', RecoverAccountRequest),
		OpenAPI({
			operationId: 'recover_account',
			summary: 'Recover account with recovery kit',
			responseSchema: RecoverAccountResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Set a new password using the recovery key from a recovery kit. Only available on instances where people sign in with a username. Ends every session, replaces the recovery kit and returns the new recovery key. Returns an MFA ticket instead of a token when the account has two-factor authentication. Requires a solved captcha challenge (X-Captcha-Token).',
		}),
		async (ctx) => {
			const result = await ctx.get('authRequestService').recoverAccount({
				data: ctx.req.valid('json'),
				request: ctx.req.raw,
			});
			return ctx.json(result);
		},
	);
	app.post(
		'/auth/email-revert',
		LocalAuthMiddleware,
		RequireEmailAccountIdentity,
		RateLimitMiddleware(RateLimitConfigs.AUTH_EMAIL_REVERT),
		Validator('json', EmailRevertRequest),
		OpenAPI({
			operationId: 'revert_email_change',
			summary: 'Revert email change',
			responseSchema: AuthTokenWithUserIdResponse,
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
		LoginRequired,
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
			const currentSessionIdHash = ctx.get('authSession')?.sessionIdHash;
			return ctx.json(await AuthSession.getAuthSessions(ctx.get('apiContext'), userId, currentSessionIdHash));
		},
	);
	app.post(
		'/auth/sessions/logout',
		RateLimitMiddleware(RateLimitConfigs.AUTH_SESSIONS_LOGOUT),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', LogoutAuthSessionsWithVerificationRequest),
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
		RequireEmailAccountIdentity,
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
		'/auth/sign-in-link',
		RateLimitMiddleware(RateLimitConfigs.AUTH_AUTHORIZE_IP),
		Validator('json', SignInLinkRequest),
		OpenAPI({
			operationId: 'redeem_sign_in_link',
			summary: 'Redeem sign-in link',
			responseSchema: AuthTokenWithUserIdResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Sign in with the token from a one-time sign-in link that the instance operator created for an admin account. Authorizes the IP address of the request and returns an authentication token. The token works once and expires after 15 minutes.',
		}),
		async (ctx) => {
			const result = await redeemSignInLink(ctx.get('apiContext'), ctx.get('adminService').auditService, {
				token: ctx.req.valid('json').token,
				request: ctx.req.raw,
			});
			return ctx.json(await ctx.get('authRequestService').toAuthTokenResponse(result));
		},
	);
	app.post(
		'/auth/ip-authorization/resend',
		LocalAuthMiddleware,
		RequireEmailAccountIdentity,
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
			await AuthLogin.resendIpAuthorization(ctx.get('apiContext'), ctx.req.valid('json').ticket);
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
			return ctx.json(
				await AuthMfa.generateWebAuthnAuthenticationOptionsDiscoverable(
					ctx.get('apiContext'),
					ctx.req.header('origin'),
				),
			);
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
		RateLimitMiddleware(RateLimitConfigs.AUTH_LOGIN_MFA),
		Validator('json', MfaTicketRequest),
		MfaTicketAuthMiddleware,
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
			return ctx.json(
				await AuthMfa.generateWebAuthnAuthenticationOptionsForMfa(
					ctx.get('apiContext'),
					ctx.req.valid('json').ticket,
					ctx.req.header('origin'),
				),
			);
		},
	);
	app.post(
		'/auth/login/mfa/webauthn',
		RateLimitMiddleware(RateLimitConfigs.AUTH_LOGIN_MFA),
		Validator('json', WebAuthnMfaRequest),
		MfaTicketAuthMiddleware,
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
	app.get(
		'/auth/username-availability',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_USERNAME_AVAILABILITY),
		RequireUsernameLookup,
		Validator('query', UsernameAvailabilityQuery),
		OpenAPI({
			operationId: 'get_username_availability',
			summary: 'Check username availability',
			responseSchema: UsernameAvailabilityResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Check whether a username is free for a new account. Only available on instances where people sign in with a username or where usernames are unique. Usernames are compared without regard to case, and bots do not hold names. An invalid or reserved username returns a validation error.',
		}),
		async (ctx) => {
			const {username} = ctx.req.valid('query');
			return ctx.json(await ctx.get('authRequestService').getUsernameAvailability(username));
		},
	);
	app.post(
		'/auth/handoff/initiate',
		RateLimitMiddleware(RateLimitConfigs.AUTH_HANDOFF_INITIATE),
		Validator('json', HandoffInitiateRequest),
		OpenAPI({
			operationId: 'initiate_handoff',
			summary: 'Initiate handoff',
			responseSchema: HandoffInitiateResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Start a handoff session to transfer authentication between devices. Returns a handoff code for device linking. A desktop app that registers a return deep link can be signed in without the user typing the code.',
		}),
		async (ctx) => {
			return ctx.json(
				await ctx.get('authRequestService').initiateHandoff({
					request: ctx.req.raw,
					returnUri: ctx.req.valid('json')?.return_uri ?? null,
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
			const clientIp = requireRequestClientIp(ctx);
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
			responseSchema: HandoffCompleteResponse,
			statusCode: [200, 204],
			security: [],
			tags: ['Auth'],
			description:
				'Complete the handoff process and authenticate on the target device using the handoff code. With the deep_link return method, responds with the deep link that hands the one-time grant back to the initiating app. Otherwise responds with no content.',
		}),
		async (ctx) => {
			const clientIp = requireRequestClientIp(ctx);
			const response = await ctx.get('authRequestService').completeHandoff({
				data: ctx.req.valid('json'),
				clientIp,
				authToken: ctx.get('authToken') ?? undefined,
				approverOrigin: ctx.req.header('origin'),
			});
			return response ? ctx.json(response) : ctx.body(null, 204);
		},
	);
	app.post(
		'/auth/handoff/:code/deny',
		RateLimitMiddleware(RateLimitConfigs.AUTH_HANDOFF_DENY),
		Validator('param', HandoffCodeParam),
		OpenAPI({
			operationId: 'deny_handoff',
			summary: 'Deny handoff',
			responseSchema: null,
			statusCode: 204,
			security: [],
			tags: ['Auth'],
			description:
				'Decline a handoff request after looking it up. The initiating device sees the denied status and the code can no longer be approved.',
		}),
		async (ctx) => {
			const clientIp = requireRequestClientIp(ctx);
			await ctx.get('desktopHandoffService').denyHandoff(ctx.req.valid('param').code, clientIp);
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
			const clientIp = requireRequestClientIp(ctx);
			const response = await ctx.get('authRequestService').getHandoffStatus({
				code: ctx.req.valid('param').code,
				clientIp,
			});
			return ctx.json(response);
		},
	);
	app.post(
		'/auth/handoff/:code/status',
		RateLimitMiddleware(RateLimitConfigs.AUTH_HANDOFF_STATUS),
		Validator('param', HandoffCodeParam),
		Validator('json', HandoffStatusRequest),
		OpenAPI({
			operationId: 'get_handoff_status_with_secret',
			summary: 'Get handoff status with secret',
			responseSchema: HandoffStatusResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Check the status of a handoff session using the poll secret from initiation. Returns the authentication token once the handoff is complete and the presented secret matches.',
		}),
		async (ctx) => {
			const clientIp = requireRequestClientIp(ctx);
			const response = await ctx.get('authRequestService').getHandoffStatus({
				code: ctx.req.valid('param').code,
				clientIp,
				pollSecret: ctx.req.valid('json').poll_secret,
				grant: ctx.req.valid('json').grant,
			});
			return ctx.json(response);
		},
	);
	app.delete(
		'/auth/handoff/:code',
		RateLimitMiddleware(RateLimitConfigs.AUTH_HANDOFF_CANCEL),
		Validator('param', HandoffCodeParam),
		Validator('json', HandoffCancelRequest),
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
			await ctx
				.get('desktopHandoffService')
				.cancelHandoff(ctx.req.valid('param').code, ctx.req.valid('json').poll_secret);
			return ctx.body(null, 204);
		},
	);
}
