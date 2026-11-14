// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthMfa from '@app/api/auth/AuthMfa';
import {verifySudoEmailCode} from '@app/api/auth/services/SudoEmailCode';
import {
	createSudoModeRequiredError,
	emailCodeSudoAvailable,
	hasNoVerifiableCredential,
	userHasMfa,
	userHasSudoCapability,
} from '@app/api/auth/services/SudoMethods';
import {getSudoModeService} from '@app/api/auth/services/SudoModeService';
import {SUDO_MODE_HEADER} from '@app/api/middleware/SudoModeMiddleware';
import type {AuthSession} from '@app/api/models/AuthSession';
import type {User} from '@app/api/models/User';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {isSsoLinkedUser} from '@app/api/user/UserHelpers';
import {verifyPassword} from '@app/api/utils/PasswordUtils';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {AuthenticationResponseJSON} from '@simplewebauthn/server';
import type {Context} from 'hono';

export interface SudoVerificationBody {
	password?: string;
	mfa_method?: 'totp' | 'webauthn' | 'email_code';
	mfa_code?: string;
	email_code_ticket?: string;
	email_code?: string;
	webauthn_response?: AuthenticationResponseJSON;
	webauthn_challenge?: string;
}

type SudoVerificationMethod = 'password' | 'mfa' | 'sudo_token' | 'email_code';

const SSO_SIGN_IN_SUDO_WINDOW_MS = 10 * 60 * 1000;

function isRecentSsoSignIn(session: AuthSession | undefined): boolean {
	return session?.authMethod === 'sso' && Date.now() - session.createdAt.getTime() < SSO_SIGN_IN_SUDO_WINDOW_MS;
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
	const hasSudoCapability = userHasSudoCapability(user, hasPasskeyCredentials);
	const hasUsableMfa = userHasMfa(user) && hasSudoCapability;
	const emailCodeAvailable = emailCodeSudoAvailable(user, hasPasskeyCredentials);
	const issueSudoToken = options.issueSudoToken ?? (hasSudoCapability || emailCodeAvailable);
	if (hasSudoCapability && ctx.get('sudoModeValid')) {
		const sudoToken = ctx.get('sudoModeToken') ?? ctx.req.header(SUDO_MODE_HEADER) ?? undefined;
		return {verified: true, method: 'sudo_token', sudoToken: issueSudoToken ? sudoToken : undefined};
	}
	const incomingToken = ctx.req.header(SUDO_MODE_HEADER);
	if (!hasSudoCapability && incomingToken && ctx.get('sudoModeValid')) {
		return {verified: true, method: 'sudo_token', sudoToken: issueSudoToken ? incomingToken : undefined};
	}
	if (body.mfa_method === 'email_code' && emailCodeAvailable) {
		await verifySudoEmailCode(apiContext, {
			user,
			ticket: body.email_code_ticket,
			code: body.email_code,
			request: ctx.req.raw,
		});
		const sudoToken = issueSudoToken ? await getSudoModeService().generateSudoToken(user.id) : undefined;
		return {verified: true, sudoToken, method: 'email_code'};
	}
	if (hasSudoCapability && (body.mfa_method === 'totp' || body.mfa_method === 'webauthn')) {
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
	if (
		hasNoVerifiableCredential(user, hasUsableMfa, hasPasskeyCredentials) &&
		(!isSsoLinkedUser(user) || isRecentSsoSignIn(ctx.get('authSession')))
	) {
		return {verified: true, method: 'password'};
	}
	if (body.password && !hasUsableMfa) {
		if (!user.passwordHash) {
			throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_NOT_SET);
		}
		const passwordValid = await verifyPassword({
			password: body.password,
			passwordHash: user.passwordHash,
		});
		if (!passwordValid) {
			throw InputValidationError.fromCode('password', ValidationErrorCodes.INVALID_PASSWORD);
		}
		return {verified: true, method: 'password'};
	}
	throw await createSudoModeRequiredError(apiContext, user, hasPasskeyCredentials);
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
