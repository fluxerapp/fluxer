// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {type SudoModeMethods, SudoModeRequiredError} from '@fluxer/errors/src/domains/auth/SudoModeRequiredError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {AuthenticationResponseJSON} from '@simplewebauthn/server';
import type {Context} from 'hono';
import * as AuthMfa from '../../auth/AuthMfa';
import * as AuthPassword from '../../auth/AuthPassword';
import {getInstanceConfigRepository} from '../../middleware/ServiceSingletons';
import {SUDO_MODE_HEADER} from '../../middleware/SudoModeMiddleware';
import type {User} from '../../models/User';
import type {HonoEnv} from '../../types/HonoEnv';
import {setSudoCookie} from '../../utils/SudoCookieUtils';
import {getSudoModeService} from './SudoModeService';

export interface SudoVerificationBody {
	password?: string;
	mfa_method?: 'totp' | 'webauthn';
	mfa_code?: string;
	webauthn_response?: AuthenticationResponseJSON;
	webauthn_challenge?: string;
}

type SudoVerificationMethod = 'password' | 'mfa' | 'sudo_token';

export function userHasMfa(user: {authenticatorTypes?: Set<number> | null}): boolean {
	return (
		(user.authenticatorTypes?.has(UserAuthenticatorTypes.TOTP) ?? false) ||
		(user.authenticatorTypes?.has(UserAuthenticatorTypes.WEBAUTHN) ?? false)
	);
}

interface DeriveSudoMethodsOptions {
	includeLocal?: boolean;
	sso?: boolean;
}

export function deriveSudoMethods(
	user: {
		totpSecret?: string | null;
		authenticatorTypes?: Set<number> | null;
	},
	options: DeriveSudoMethodsOptions = {},
): SudoModeMethods {
	const includeLocal = options.includeLocal ?? true;
	const authenticatorTypes = user.authenticatorTypes ?? null;
	const methods: SudoModeMethods = {
		totp:
			includeLocal &&
			(user.totpSecret ?? null) !== null &&
			(authenticatorTypes?.has(UserAuthenticatorTypes.TOTP) ?? false),
		webauthn: includeLocal && (authenticatorTypes?.has(UserAuthenticatorTypes.WEBAUTHN) ?? false),
	};
	if (options.sso) {
		methods.sso = true;
	}
	return methods;
}

export interface SudoVerificationResult {
	verified: boolean;
	method: SudoVerificationMethod;
	sudoToken?: string;
}

interface SudoVerificationOptions {
	issueSudoToken?: boolean;
}

async function verifySudoMode(
	ctx: Context<HonoEnv>,
	user: User,
	body: SudoVerificationBody,
	options: SudoVerificationOptions = {},
): Promise<SudoVerificationResult> {
	if (user.isBot) {
		return {verified: true, method: 'sudo_token'};
	}
	const ssoConfig = await getInstanceConfigRepository().getSsoConfig();
	const ssoSudoRequired =
		user.traits.has('sso') && ((ssoConfig.enabled && ssoConfig.enforced) || ssoConfig.disableAdditionalAuth);
	const hasSsoOption = user.traits.has('sso') && ssoConfig.enabled;
	const hasMfa = userHasMfa(user);
	const issueSudoToken = options.issueSudoToken ?? (hasMfa || ssoSudoRequired);
	if (hasMfa && ctx.get('sudoModeValid')) {
		const sudoToken = ctx.get('sudoModeToken') ?? ctx.req.header(SUDO_MODE_HEADER) ?? undefined;
		return {verified: true, method: 'sudo_token', sudoToken: issueSudoToken ? sudoToken : undefined};
	}
	const incomingToken = ctx.req.header(SUDO_MODE_HEADER);
	if (!hasMfa && ctx.get('sudoModeValid') && (incomingToken || ssoSudoRequired)) {
		const sudoToken = ctx.get('sudoModeToken') ?? incomingToken ?? undefined;
		return {verified: true, method: 'sudo_token', sudoToken: issueSudoToken ? sudoToken : undefined};
	}
	if (ssoSudoRequired) {
		throw new SudoModeRequiredError(true, deriveSudoMethods(user, {includeLocal: false, sso: true}));
	}
	if (hasMfa && body.mfa_method) {
		const result = await AuthMfa.verifySudoMfa(ctx.get('apiContext'), {
			userId: user.id,
			method: body.mfa_method,
			code: body.mfa_code,
			webauthnResponse: body.webauthn_response,
			webauthnChallenge: body.webauthn_challenge,
		});
		if (!result.success) {
			throw InputValidationError.fromCode('mfa_code', ValidationErrorCodes.INVALID_MFA_CODE);
		}
		const sudoModeService = getSudoModeService();
		const sudoToken = issueSudoToken ? await sudoModeService.generateSudoToken(user.id) : undefined;
		return {verified: true, sudoToken, method: 'mfa'};
	}
	const isUnclaimedAccount = user.isUnclaimedAccount();
	if (isUnclaimedAccount && !hasMfa) {
		return {verified: true, method: 'password'};
	}
	if (body.password && !hasMfa) {
		if (!user.passwordHash) {
			throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_NOT_SET);
		}
		const passwordValid = await AuthPassword.verifyPassword(ctx.get('apiContext'), {
			password: body.password,
			passwordHash: user.passwordHash,
		});
		if (!passwordValid) {
			throw InputValidationError.fromCode('password', ValidationErrorCodes.INVALID_PASSWORD);
		}
		return {verified: true, method: 'password'};
	}
	throw new SudoModeRequiredError(hasMfa, deriveSudoMethods(user, {sso: hasSsoOption}));
}

function setSudoTokenHeader(
	ctx: Context<HonoEnv>,
	result: SudoVerificationResult,
	options: SudoVerificationOptions = {},
): void {
	const issueSudoToken = options.issueSudoToken ?? true;
	if (!issueSudoToken) {
		return;
	}
	const tokenToSet = result.sudoToken ?? ctx.req.header(SUDO_MODE_HEADER);
	if (tokenToSet) {
		ctx.header(SUDO_MODE_HEADER, tokenToSet);
		const user = ctx.get('user');
		if (user) {
			setSudoCookie(ctx, tokenToSet, user.id.toString());
		}
	}
}

export async function requireSudoMode(
	ctx: Context<HonoEnv>,
	user: User,
	body: SudoVerificationBody,
	options: SudoVerificationOptions = {},
): Promise<SudoVerificationResult> {
	const sudoResult = await verifySudoMode(ctx, user, body, options);
	setSudoTokenHeader(ctx, sudoResult, options);
	return sudoResult;
}
