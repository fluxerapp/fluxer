// SPDX-License-Identifier: AGPL-3.0-or-later

import {usesUsernameSignIn} from '@app/api/instance/AccountIdentityModeCache';
import {
	createCaptchaChallenge,
	EscalatingCaptchaMiddleware,
	verifyCaptchaToken,
} from '@app/api/middleware/CaptchaMiddleware';
import {LocalAuthMiddleware} from '@app/api/middleware/LocalAuthMiddleware';
import {getTrustedForwardedClientIp, RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {tierContextFrom} from '@app/api/user/AccountTier';
import {isJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {requireRequestClientIp} from '@app/api/utils/RequestClientIp';
import {Validator} from '@app/api/Validator';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {
	AccountHandoffRedeemRequest,
	AccountHandoffRequest,
	AccountHandoffResponse,
	AuthRegisterSuccessResponse,
	CaptchaChallengeResponse,
	EmailCodeSendRequest,
	EmailCodeSendResponse,
	EmailCodeVerifyRequest,
	EmailCodeVerifyResponse,
	IdentifyRequest,
	IdentifyResponse,
} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {Context} from 'hono';

function captchaGate(ctx: Context<HonoEnv>) {
	return async (escalationSteps: number): Promise<void> => {
		await verifyCaptchaToken(ctx, escalationSteps);
	};
}

function requireTrustedClientIp(ctx: Context<HonoEnv>, bucket: string): string {
	const clientIp = getTrustedForwardedClientIp(ctx, bucket);
	if (!clientIp) throw new MissingAccessError();
	return clientIp;
}

async function handoffCaptchaSteps(ctx: Context<HonoEnv>): Promise<number> {
	const clientIp = requireTrustedClientIp(ctx, RateLimitConfigs.AUTH_REGISTER_HANDOFF.bucket);
	const body: unknown = await ctx.req.json();
	const code = isJsonRecord(body) && typeof body.registration_url_code === 'string' ? body.registration_url_code : null;
	return await ctx.get('authRequestService').instantAccountChallengeSteps(ctx.req.raw, clientIp, code);
}

export function AccountEntryController(app: HonoApp) {
	app.post(
		'/auth/identify',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_IDENTIFY),
		Validator('json', IdentifyRequest),
		OpenAPI({
			operationId: 'identify_account',
			summary: 'Start signing in or signing up',
			responseSchema: IdentifyResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Looks up whether an account uses this email address (or username on username instances) and returns a ticket for the next step. It says whether the account exists and whether it signs in with its password or with an email code. Answers take at least 300 ms. Heavy use from one network or for one address asks for a solved captcha challenge (X-Captcha-Token) whose cost grows with use.',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const field = usesUsernameSignIn() ? 'login' : 'email';
			const identifier = body[field];
			if (!identifier) {
				throw InputValidationError.fromCode(
					field,
					field === 'login' ? ValidationErrorCodes.USERNAME_LENGTH_INVALID : ValidationErrorCodes.EMAIL_IS_REQUIRED,
				);
			}
			return ctx.json(
				await ctx.get('authRequestService').identify({
					identifier,
					deviceProof: body.device_proof,
					registrationUrlCode: body.registration_url_code,
					request: ctx.req.raw,
					clientIp: requireRequestClientIp(ctx),
					captcha: captchaGate(ctx),
				}),
			);
		},
	);
	app.post(
		'/auth/email-code/send',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_EMAIL_CODE_SEND),
		Validator('json', EmailCodeSendRequest),
		OpenAPI({
			operationId: 'send_email_code',
			summary: 'Send a sign-in or sign-up code',
			responseSchema: EmailCodeSendResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Emails a 6-digit code for the address behind an identify ticket. The code signs in to an existing account without a password or creates a new one. An account with a password answers PASSWORD_SIGN_IN_REQUIRED and gets no email. Sign-ups always need a solved captcha challenge (X-Captcha-Token), and sign-ins need one after several codes for the same address. A bounced address answers the same and gets no email. On a new self-hosted instance it fails with SETUP_TOKEN_REQUIRED until the first admin account exists.',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			return ctx.json(
				await ctx.get('authRequestService').sendEmailCode({
					ticket: body.ticket,
					registrationUrlCode: body.registration_url_code,
					request: ctx.req.raw,
					clientIp: requireRequestClientIp(ctx),
					captcha: captchaGate(ctx),
				}),
			);
		},
	);
	app.post(
		'/auth/email-code/verify',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_EMAIL_CODE_VERIFY),
		Validator('json', EmailCodeVerifyRequest),
		OpenAPI({
			operationId: 'verify_email_code',
			summary: 'Finish signing in or signing up with a code',
			responseSchema: EmailCodeVerifyResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Checks the emailed code. For an existing account without a password this signs in, or returns an MFA ticket when the account has two-factor authentication. An account with a password answers PASSWORD_SIGN_IN_REQUIRED. When the code confirms an address that was not confirmed yet, it also ends every other session of the account, forgets its authorized IPs and revokes its OAuth2 grants. For a new address this creates the account with a confirmed email and signs in. Each code allows 5 tries. After many wrong codes for one address, or across the instance, it needs a solved captcha challenge (X-Captcha-Token).',
		}),
		async (ctx) => {
			return ctx.json(
				await ctx.get('authRequestService').verifyEmailCode({
					data: ctx.req.valid('json'),
					request: ctx.req.raw,
					clientIp: requireRequestClientIp(ctx),
					requestCache: ctx.get('requestCache'),
					tierContext: tierContextFrom(ctx),
					captcha: captchaGate(ctx),
				}),
			);
		},
	);
	app.get(
		'/auth/register/handoff/challenge',
		RateLimitMiddleware(RateLimitConfigs.AUTH_REGISTER_HANDOFF_CHALLENGE),
		OpenAPI({
			operationId: 'get_account_handoff_challenge',
			summary: 'Get the captcha challenge for creating an account from the website',
			responseSchema: CaptchaChallengeResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Returns the captcha challenge that creating an account without an email from the website needs right now for this visitor. Only the website server may call it.',
		}),
		async (ctx) => {
			const bucket = RateLimitConfigs.AUTH_REGISTER_HANDOFF_CHALLENGE.bucket;
			const clientIp = requireTrustedClientIp(ctx, bucket);
			const steps = await ctx.get('authRequestService').instantAccountChallengeSteps(ctx.req.raw, clientIp);
			ctx.header('Cache-Control', 'no-store');
			return ctx.json(await createCaptchaChallenge(ctx, steps));
		},
	);
	app.post(
		'/auth/register/handoff',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_REGISTER_HANDOFF),
		Validator('json', AccountHandoffRequest),
		EscalatingCaptchaMiddleware(handoffCaptchaSteps),
		OpenAPI({
			operationId: 'create_account_handoff',
			summary: 'Start an account from the website',
			responseSchema: AccountHandoffResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Checks the display name and returns a single-use code that the web app redeems within 15 minutes. No account exists yet: the web app creates it on redeem, when the person submits the step whose notice says that creating the account accepts the terms of service. Only the website server may call it, and it passes the visitor IP. Requires a solved captcha challenge (X-Captcha-Token) whose cost grows with recent sign-ups. On a new self-hosted instance it fails with SETUP_TOKEN_REQUIRED until the first admin account exists.',
		}),
		async (ctx) => {
			const clientIp = requireTrustedClientIp(ctx, RateLimitConfigs.AUTH_REGISTER_HANDOFF.bucket);
			return ctx.json(
				await ctx.get('authRequestService').createAccountHandoff({data: ctx.req.valid('json'), clientIp}),
			);
		},
	);
	app.post(
		'/auth/register/handoff/redeem',
		LocalAuthMiddleware,
		RateLimitMiddleware(RateLimitConfigs.AUTH_ACCOUNT_HANDOFF_REDEEM),
		Validator('json', AccountHandoffRedeemRequest),
		OpenAPI({
			operationId: 'redeem_account_handoff',
			summary: 'Create the account started on the website',
			responseSchema: AuthRegisterSuccessResponse,
			statusCode: 200,
			security: [],
			tags: ['Auth'],
			description:
				'Redeems the code from create account handoff together with the nonce whose hash was sent then, creates the account with the agreement and display name given here (the display name sent with the handoff when none is given), and starts a session for the client that redeems it. The web app and the native apps can redeem it. Where the instance collects dates of birth, the account then sets one through the date of birth step. The code works once, and a wrong nonce also uses it up. A missing agreement or a display name that is not allowed answers before the code is used. It never ends or replaces another session.',
		}),
		async (ctx) => {
			return ctx.json(
				await ctx.get('authRequestService').redeemHandoff({
					data: ctx.req.valid('json'),
					request: ctx.req.raw,
					requestCache: ctx.get('requestCache'),
					tierContext: tierContextFrom(ctx),
				}),
			);
		},
	);
}
