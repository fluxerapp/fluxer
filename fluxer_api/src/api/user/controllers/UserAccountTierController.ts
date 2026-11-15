// SPDX-License-Identifier: AGPL-3.0-or-later

import {sendSudoEmailCode} from '@app/api/auth/services/SudoEmailCode';
import {emailCodeSudoAvailable, userHasUsableMfa} from '@app/api/auth/services/SudoMethods';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {claimAccount, confirmEmailVerification, sendEmailVerificationCode} from '@app/api/user/AccountClaim';
import {tierContextFrom} from '@app/api/user/AccountTier';
import {setDateOfBirth} from '@app/api/user/DateOfBirth';
import {requireRequestClientIp} from '@app/api/utils/RequestClientIp';
import {Validator} from '@app/api/Validator';
import {EmailCodesUnavailableError} from '@fluxer/errors/src/domains/auth/EmailCodesUnavailableError';
import {SudoSecondFactorRequiredError} from '@fluxer/errors/src/domains/auth/SudoSecondFactorRequiredError';
import {
	ClaimAccountRequest,
	ClaimAccountResponse,
	DateOfBirthUpdateRequest,
	DateOfBirthUpdateResponse,
	EmailVerificationCodeResponse,
	EmailVerificationConfirmRequest,
	SudoEmailCodeResponse,
} from '@fluxer/schema/src/domains/user/AccountSchemas';
import {UserPrivateResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export function UserAccountTierController(app: HonoApp) {
	app.put(
		'/users/@me/date-of-birth',
		RateLimitMiddleware(RateLimitConfigs.USER_DATE_OF_BIRTH),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', DateOfBirthUpdateRequest),
		OpenAPI({
			operationId: 'set_user_date_of_birth',
			summary: 'Set date of birth',
			responseSchema: DateOfBirthUpdateResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Sets the date of birth once, for accounts that do not have one yet. It cannot be changed afterwards. The minimum age depends on the country the request comes from. Below it, the account is closed and deleted after 72 hours. Returns the updated account and the community to open next, when an invite was used at sign-up.',
		}),
		async (ctx) => {
			return ctx.json(
				await setDateOfBirth(
					ctx.get('apiContext'),
					{
						inviteService: ctx.get('inviteService'),
						singleCommunityService: ctx.get('singleCommunityService'),
						guildVisitService: ctx.get('guildVisitService'),
					},
					{
						user: ctx.get('user'),
						dateOfBirth: ctx.req.valid('json').date_of_birth,
						clientIp: requireRequestClientIp(ctx),
						tierContext: tierContextFrom(ctx),
						requestCache: ctx.get('requestCache'),
					},
				),
			);
		},
	);
	app.post(
		'/users/@me/claim',
		RateLimitMiddleware(RateLimitConfigs.USER_CLAIM),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', ClaimAccountRequest),
		OpenAPI({
			operationId: 'claim_account',
			summary: 'Claim an account created without credentials',
			responseSchema: ClaimAccountResponse,
			statusCode: 200,
			security: ['sessionToken'],
			tags: ['Users'],
			description:
				'Claims an account created without credentials. On email instances it adds an email address, and optionally a password. When the instance sends email, the address is stored at once and waits for confirmation with the emailed code, and next_step is confirm_email. When it does not, the account needs a password or an existing passkey, and next_step is none. An address stored on another account, confirmed or not, answers EMAIL_ALREADY_IN_USE. On username instances it sets a password, and next_step is none.',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			return ctx.json(
				await claimAccount(ctx.get('apiContext'), {
					user: ctx.get('user'),
					email: body.email,
					password: body.password,
					request: ctx.req.raw,
					clientIp: requireRequestClientIp(ctx),
				}),
			);
		},
	);
	app.post(
		'/users/@me/email-verification/send',
		RateLimitMiddleware(RateLimitConfigs.USER_EMAIL_VERIFICATION),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'send_email_verification_code',
			summary: 'Send an email confirmation code',
			responseSchema: EmailVerificationCodeResponse,
			statusCode: 200,
			security: ['sessionToken'],
			tags: ['Users'],
			description: 'Emails a 6-digit code to confirm the email address on the account.',
		}),
		async (ctx) => {
			return ctx.json(
				await sendEmailVerificationCode(ctx.get('apiContext'), {
					user: ctx.get('user'),
					request: ctx.req.raw,
					clientIp: requireRequestClientIp(ctx),
				}),
			);
		},
	);
	app.post(
		'/users/@me/email-verification/confirm',
		RateLimitMiddleware(RateLimitConfigs.USER_EMAIL_VERIFICATION_CONFIRM),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', EmailVerificationConfirmRequest),
		OpenAPI({
			operationId: 'confirm_email_verification',
			summary: 'Confirm the email address with a code',
			responseSchema: UserPrivateResponse,
			statusCode: 200,
			security: ['sessionToken'],
			tags: ['Users'],
			description: 'Confirms the email address with the emailed code.',
		}),
		async (ctx) => {
			return ctx.json(
				await confirmEmailVerification(ctx.get('apiContext'), {
					user: ctx.get('user'),
					code: ctx.req.valid('json').code,
					request: ctx.req.raw,
				}),
			);
		},
	);
	app.post(
		'/users/@me/sudo/email-code',
		RateLimitMiddleware(RateLimitConfigs.USER_SUDO_EMAIL_CODE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'send_sudo_email_code',
			summary: 'Send a code to confirm it is you',
			responseSchema: SudoEmailCodeResponse,
			statusCode: 200,
			security: ['sessionToken'],
			tags: ['Users'],
			description:
				'Emails a 6-digit code to the confirmed address, for accounts without two-factor authentication. Accounts with two-factor authentication get 400 SUDO_SECOND_FACTOR_REQUIRED and confirm with that factor instead. Send it back with mfa_method email_code, email_code_ticket and email_code to enter sudo mode.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const apiContext = ctx.get('apiContext');
			const credentials = await apiContext.services.users.listWebAuthnCredentials(user.id);
			if (userHasUsableMfa(user, credentials.length > 0)) throw new SudoSecondFactorRequiredError();
			if (!emailCodeSudoAvailable(user, credentials.length > 0)) throw new EmailCodesUnavailableError();
			return ctx.json(await sendSudoEmailCode(apiContext, {user, request: ctx.req.raw}));
		},
	);
}
