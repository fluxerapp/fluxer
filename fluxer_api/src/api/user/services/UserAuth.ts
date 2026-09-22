// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {deriveSudoMethods, userHasMfa, userHasSudoCapability} from '@app/api/auth/services/SudoMethods';
import type {SudoVerificationResult} from '@app/api/auth/services/SudoVerificationService';
import type {MfaBackupCode} from '@app/api/models/MfaBackupCode';
import type {User} from '@app/api/models/User';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {MfaNotDisabledError} from '@fluxer/errors/src/domains/auth/MfaNotDisabledError';
import {MfaNotEnabledError} from '@fluxer/errors/src/domains/auth/MfaNotEnabledError';
import {SudoModeRequiredError} from '@fluxer/errors/src/domains/auth/SudoModeRequiredError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';

const LEGACY_PHONE_AUTHENTICATOR_TYPE = 1;

interface EnableMfaTotpParams {
	user: User;
	secret: string;
	code: string;
	sudoContext: SudoVerificationResult;
}

interface DisableMfaTotpParams {
	user: User;
	code: string;
	sudoContext: SudoVerificationResult;
}

interface GetMfaBackupCodesParams {
	user: User;
	regenerate: boolean;
	sudoContext: SudoVerificationResult;
}

async function assertSudoVerifiedForMfa(
	ctx: ApiContext,
	user: User,
	sudoContext: SudoVerificationResult,
): Promise<void> {
	const identityVerifiedViaSudo = sudoContext.method === 'mfa' || sudoContext.method === 'sudo_token';
	const identityVerifiedViaPassword = sudoContext.method === 'password';
	if (identityVerifiedViaSudo || identityVerifiedViaPassword) {
		return;
	}
	const credentials = await ctx.services.users.listWebAuthnCredentials(user.id);
	const hasPasskeyCredentials = credentials.length > 0;
	const hasBackupCodes = await AuthMfa.hasUnconsumedBackupCodes(ctx, user.id);
	throw new SudoModeRequiredError(
		userHasSudoCapability(user, hasPasskeyCredentials),
		deriveSudoMethods(user, hasPasskeyCredentials, hasBackupCodes),
	);
}

export async function enableMfaTotp(
	ctx: ApiContext,
	{user, secret, code, sudoContext}: EnableMfaTotpParams,
): Promise<Array<MfaBackupCode>> {
	const {users, botMfaMirror} = ctx.services;
	await assertSudoVerifiedForMfa(ctx, user, sudoContext);
	if (user.totpSecret) throw new MfaNotDisabledError();
	const userId = user.id;
	if (!(await AuthMfa.verifyMfaCode(ctx, {userId: user.id, mfaSecret: secret, code}))) {
		throw InputValidationError.fromCode('code', ValidationErrorCodes.INVALID_CODE);
	}
	const authenticatorTypes = new Set<number>(user.authenticatorTypes ?? []);
	authenticatorTypes.add(UserAuthenticatorTypes.TOTP);
	const updatedUser = await users.patchUpsert(
		userId,
		{
			totp_secret: secret,
			authenticator_types: authenticatorTypes,
		},
		user.toRow(),
	);
	const newBackupCodes = AuthUtility.generateBackupCodes(ctx);
	const mfaBackupCodes = await users.createMfaBackupCodes(userId, newBackupCodes);
	await dispatchUserUpdate(ctx, updatedUser);
	await botMfaMirror.syncAuthenticatorTypesForOwner(updatedUser);
	return mfaBackupCodes;
}

export async function disableMfaTotp(ctx: ApiContext, {user, code, sudoContext}: DisableMfaTotpParams): Promise<void> {
	const {users, botMfaMirror} = ctx.services;
	if (!user.totpSecret) throw new MfaNotEnabledError();
	await assertSudoVerifiedForMfa(ctx, user, sudoContext);
	if (
		sudoContext.method !== 'mfa' &&
		!(await AuthMfa.verifyMfaCode(ctx, {
			userId: user.id,
			mfaSecret: user.totpSecret,
			code,
			allowBackup: true,
		}))
	) {
		throw InputValidationError.fromCode('code', ValidationErrorCodes.INVALID_CODE);
	}
	const userId = user.id;
	const authenticatorTypes = new Set<number>(user.authenticatorTypes ?? []);
	authenticatorTypes.delete(UserAuthenticatorTypes.TOTP);
	const hasLegacyPhoneAuthenticator = authenticatorTypes.has(LEGACY_PHONE_AUTHENTICATOR_TYPE);
	if (hasLegacyPhoneAuthenticator) {
		authenticatorTypes.delete(LEGACY_PHONE_AUTHENTICATOR_TYPE);
	}
	const updatedUser = await users.patchUpsert(
		userId,
		{
			totp_secret: null,
			authenticator_types: authenticatorTypes,
		},
		user.toRow(),
	);
	if (!userHasMfa(updatedUser)) {
		await users.clearMfaBackupCodes(userId);
	}
	await dispatchUserUpdate(ctx, updatedUser);
	await botMfaMirror.syncAuthenticatorTypesForOwner(updatedUser);
}

export async function getMfaBackupCodes(
	ctx: ApiContext,
	{user, regenerate, sudoContext}: GetMfaBackupCodesParams,
): Promise<Array<MfaBackupCode>> {
	const {users} = ctx.services;
	await assertSudoVerifiedForMfa(ctx, user, sudoContext);
	if (regenerate) {
		return regenerateMfaBackupCodes(ctx, user);
	}
	return await users.listMfaBackupCodes(user.id);
}

export async function regenerateMfaBackupCodes(ctx: ApiContext, user: User): Promise<Array<MfaBackupCode>> {
	const {users} = ctx.services;
	const userId = user.id;
	const newBackupCodes = AuthUtility.generateBackupCodes(ctx);
	await users.clearMfaBackupCodes(userId);
	return await users.createMfaBackupCodes(userId, newBackupCodes);
}

async function dispatchUserUpdate(ctx: ApiContext, user: User): Promise<void> {
	const {gateway} = ctx.services;
	await gateway.dispatchPresence({
		userId: user.id,
		event: 'USER_UPDATE',
		data: mapUserToPrivateResponse(user),
	});
}
