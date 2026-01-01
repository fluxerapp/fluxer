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

import type {RegistrationResponseJSON} from '@simplewebauthn/server';
import type {HonoApp} from '~/App';
import {requireSudoMode} from '~/auth/services/SudoVerificationService';
import {DefaultUserOnly, LoginRequired, LoginRequiredAllowSuspicious} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {SudoModeMiddleware} from '~/middleware/SudoModeMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {createStringType, PasswordType, PhoneNumberType, SudoVerificationSchema, z} from '~/Schema';
import {Validator} from '~/Validator';

const DisableTotpSchema = z
	.object({code: createStringType(), password: PasswordType.optional()})
	.merge(SudoVerificationSchema);

const MfaBackupCodesSchema = z
	.object({regenerate: z.boolean(), password: PasswordType.optional()})
	.merge(SudoVerificationSchema);

export const UserAuthController = (app: HonoApp) => {
	app.post(
		'/users/@me/mfa/totp/enable',
		RateLimitMiddleware(RateLimitConfigs.USER_MFA_TOTP_ENABLE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', z.object({secret: createStringType(), code: createStringType()})),
		async (ctx) => {
			const {secret, code} = ctx.req.valid('json');
			const backupCodes = await ctx.get('userService').enableMfaTotp({
				user: ctx.get('user'),
				secret,
				code,
			});
			return ctx.json({
				backup_codes: backupCodes.map((bc) => ({
					code: bc.code,
					consumed: bc.consumed,
				})),
			});
		},
	);

	app.post(
		'/users/@me/mfa/totp/disable',
		RateLimitMiddleware(RateLimitConfigs.USER_MFA_TOTP_DISABLE),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', DisableTotpSchema),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const user = ctx.get('user');
			const sudoResult = await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));
			await ctx.get('userService').disableMfaTotp({
				user,
				code: body.code,
				sudoContext: sudoResult,
				password: body.password,
			});
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/users/@me/mfa/backup-codes',
		RateLimitMiddleware(RateLimitConfigs.USER_MFA_BACKUP_CODES),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', MfaBackupCodesSchema),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const user = ctx.get('user');
			const sudoResult = await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));
			const backupCodes = await ctx.get('userService').getMfaBackupCodes({
				user,
				regenerate: body.regenerate,
				sudoContext: sudoResult,
				password: body.password,
			});
			return ctx.json({
				backup_codes: backupCodes.map((bc) => ({
					code: bc.code,
					consumed: bc.consumed,
				})),
			});
		},
	);

	app.post(
		'/users/@me/phone/send-verification',
		RateLimitMiddleware(RateLimitConfigs.PHONE_SEND_VERIFICATION),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator('json', z.object({phone: PhoneNumberType})),
		async (ctx) => {
			const {phone} = ctx.req.valid('json');
			await ctx.get('authService').sendPhoneVerificationCode(phone, ctx.get('user').id);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/users/@me/phone/verify',
		RateLimitMiddleware(RateLimitConfigs.PHONE_VERIFY_CODE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator('json', z.object({phone: PhoneNumberType, code: createStringType()})),
		async (ctx) => {
			const {phone, code} = ctx.req.valid('json');
			const phoneToken = await ctx.get('authService').verifyPhoneCode(phone, code, ctx.get('user').id);
			return ctx.json({phone_token: phoneToken});
		},
	);

	app.post(
		'/users/@me/phone',
		RateLimitMiddleware(RateLimitConfigs.PHONE_ADD),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', z.object({phone_token: createStringType()}).merge(SudoVerificationSchema)),
		async (ctx) => {
			const user = ctx.get('user');
			const {phone_token, ...sudoBody} = ctx.req.valid('json');
			await requireSudoMode(ctx, user, sudoBody, ctx.get('authService'), ctx.get('authMfaService'));
			await ctx.get('authService').addPhoneToAccount(user.id, phone_token);
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/users/@me/phone',
		RateLimitMiddleware(RateLimitConfigs.PHONE_REMOVE),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));
			await ctx.get('authService').removePhoneFromAccount(user.id);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/users/@me/mfa/sms/enable',
		RateLimitMiddleware(RateLimitConfigs.MFA_SMS_ENABLE),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'), {
				issueSudoToken: false,
			});
			await ctx.get('authService').enableSmsMfa(user.id);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/users/@me/mfa/sms/disable',
		RateLimitMiddleware(RateLimitConfigs.MFA_SMS_DISABLE),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));
			await ctx.get('authService').disableSmsMfa(user.id);
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/users/@me/mfa/webauthn/credentials',
		RateLimitMiddleware(RateLimitConfigs.MFA_WEBAUTHN_LIST),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const credentials = await ctx.get('userRepository').listWebAuthnCredentials(ctx.get('user').id);
			return ctx.json(
				credentials.map((cred) => ({
					id: cred.credentialId,
					name: cred.name,
					created_at: cred.createdAt.toISOString(),
					last_used_at: cred.lastUsedAt?.toISOString() ?? null,
				})),
			);
		},
	);

	app.post(
		'/users/@me/mfa/webauthn/credentials/registration-options',
		RateLimitMiddleware(RateLimitConfigs.MFA_WEBAUTHN_REGISTRATION_OPTIONS),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'), {
				issueSudoToken: false,
			});
			const options = await ctx.get('authService').generateWebAuthnRegistrationOptions(user.id);
			return ctx.json(options);
		},
	);

	app.post(
		'/users/@me/mfa/webauthn/credentials',
		RateLimitMiddleware(RateLimitConfigs.MFA_WEBAUTHN_REGISTER),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator(
			'json',
			z
				.object({
					response: z.custom<RegistrationResponseJSON>(),
					challenge: createStringType(),
					name: createStringType(1, 100),
				})
				.merge(SudoVerificationSchema),
		),
		async (ctx) => {
			const user = ctx.get('user');
			const {response, challenge, name, ...sudoBody} = ctx.req.valid('json');
			await requireSudoMode(ctx, user, sudoBody, ctx.get('authService'), ctx.get('authMfaService'), {
				issueSudoToken: false,
			});
			await ctx.get('authService').verifyWebAuthnRegistration(user.id, response, challenge, name);
			return ctx.body(null, 204);
		},
	);

	app.patch(
		'/users/@me/mfa/webauthn/credentials/:credential_id',
		RateLimitMiddleware(RateLimitConfigs.MFA_WEBAUTHN_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({credential_id: createStringType()})),
		Validator('json', z.object({name: createStringType(1, 100)}).merge(SudoVerificationSchema)),
		SudoModeMiddleware,
		async (ctx) => {
			const user = ctx.get('user');
			const {credential_id} = ctx.req.valid('param');
			const {name, ...sudoBody} = ctx.req.valid('json');
			await requireSudoMode(ctx, user, sudoBody, ctx.get('authService'), ctx.get('authMfaService'));
			await ctx.get('authService').renameWebAuthnCredential(user.id, credential_id, name);
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/users/@me/mfa/webauthn/credentials/:credential_id',
		RateLimitMiddleware(RateLimitConfigs.MFA_WEBAUTHN_DELETE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({credential_id: createStringType()})),
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const {credential_id} = ctx.req.valid('param');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));
			await ctx.get('authService').deleteWebAuthnCredential(user.id, credential_id);
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/users/@me/sudo/mfa-methods',
		RateLimitMiddleware(RateLimitConfigs.SUDO_MFA_METHODS),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const methods = await ctx.get('authMfaService').getAvailableMfaMethods(ctx.get('user').id);
			return ctx.json(methods);
		},
	);

	app.post(
		'/users/@me/sudo/mfa/sms/send',
		RateLimitMiddleware(RateLimitConfigs.SUDO_SMS_SEND),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			await ctx.get('authService').sendSmsMfaCode(ctx.get('user').id);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/users/@me/sudo/webauthn/authentication-options',
		RateLimitMiddleware(RateLimitConfigs.SUDO_WEBAUTHN_OPTIONS),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const options = await ctx.get('authMfaService').generateWebAuthnOptionsForSudo(ctx.get('user').id);
			return ctx.json(options);
		},
	);
};
