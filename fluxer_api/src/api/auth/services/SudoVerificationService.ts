// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthMfa from '@app/api/auth/AuthMfa';
import * as AuthPassword from '@app/api/auth/AuthPassword';
import {deriveSudoMethods, userHasMfa, userHasSudoCapability} from '@app/api/auth/services/SudoMethods';
import {getSudoModeService} from '@app/api/auth/services/SudoModeService';
import {SUDO_MODE_HEADER} from '@app/api/middleware/SudoModeMiddleware';
import type {User} from '@app/api/models/User';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {SudoModeRequiredError} from '@fluxer/errors/src/domains/auth/SudoModeRequiredError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {AuthenticationResponseJSON} from '@simplewebauthn/server';
import type {Context} from 'hono';

export interface SudoVerificationBody {
	password?: string;
	mfa_method?: 'totp' | 'webauthn';
	mfa_code?: string;
	webauthn_response?: AuthenticationResponseJSON;
	webauthn_challenge?: string;
}

type SudoVerificationMethod = 'password' | 'mfa' | 'sudo_token';

export function hasNoVerifiableCredential(
	user: {passwordHash: string | null; isBot: boolean},
	hasMfa: boolean,
	hasPasskeyCredentials: boolean,
): boolean {
	if (user.isBot || hasMfa || hasPasskeyCredentials) {
		return false;
	}
	return user.passwordHash === null;
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
	const apiContext = ctx.get('apiContext');
	const credentials = await apiContext.services.users.listWebAuthnCredentials(user.id);
	const hasPasskeyCredentials = credentials.length > 0;
	const hasMfa = userHasMfa(user);
	const hasSudoCapability = userHasSudoCapability(user, hasPasskeyCredentials);
	const issueSudoToken = options.issueSudoToken ?? hasSudoCapability;
	if (hasSudoCapability && ctx.get('sudoModeValid')) {
		const sudoToken = ctx.get('sudoModeToken') ?? ctx.req.header(SUDO_MODE_HEADER) ?? undefined;
		return {verified: true, method: 'sudo_token', sudoToken: issueSudoToken ? sudoToken : undefined};
	}
	const incomingToken = ctx.req.header(SUDO_MODE_HEADER);
	if (!hasSudoCapability && incomingToken && ctx.get('sudoModeValid')) {
		return {verified: true, method: 'sudo_token', sudoToken: issueSudoToken ? incomingToken : undefined};
	}
	if (hasSudoCapability && body.mfa_method) {
		const result = await AuthMfa.verifySudoMfa(apiContext, {
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
	if (hasNoVerifiableCredential(user, hasMfa, hasPasskeyCredentials)) {
		return {verified: true, method: 'password'};
	}
	if (body.password && !hasMfa) {
		if (!user.passwordHash) {
			throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_NOT_SET);
		}
		const passwordValid = await AuthPassword.verifyPassword(apiContext, {
			password: body.password,
			passwordHash: user.passwordHash,
		});
		if (!passwordValid) {
			throw InputValidationError.fromCode('password', ValidationErrorCodes.INVALID_PASSWORD);
		}
		return {verified: true, method: 'password'};
	}
	const hasBackupCodes = await AuthMfa.hasUnconsumedBackupCodes(apiContext, user.id);
	throw new SudoModeRequiredError(hasSudoCapability, deriveSudoMethods(user, hasPasskeyCredentials, hasBackupCodes));
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
