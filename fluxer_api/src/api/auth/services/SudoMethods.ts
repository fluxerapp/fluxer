// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import {hasOwnedEmail} from '@app/api/auth/services/SudoEmailCode';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {isSsoLinkedUser} from '@app/api/user/UserHelpers';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {type SudoModeMethods, SudoModeRequiredError} from '@fluxer/errors/src/domains/auth/SudoModeRequiredError';

interface SudoMethodsUser {
	totpSecret?: string | null;
	authenticatorTypes?: Set<number> | null;
	passwordHash?: string | null;
	email?: string | null;
	emailVerified?: boolean;
}

function hasTotpEnrolled(user: SudoMethodsUser): boolean {
	return (user.totpSecret ?? null) !== null && (user.authenticatorTypes?.has(UserAuthenticatorTypes.TOTP) ?? false);
}

export function userHasMfa(user: {authenticatorTypes?: Set<number> | null}): boolean {
	return (
		(user.authenticatorTypes?.has(UserAuthenticatorTypes.TOTP) ?? false) ||
		(user.authenticatorTypes?.has(UserAuthenticatorTypes.WEBAUTHN) ?? false)
	);
}

export function userHasSudoCapability(user: SudoMethodsUser, hasPasskeyCredentials: boolean): boolean {
	return hasTotpEnrolled(user) || hasPasskeyCredentials;
}

export function userHasUsableMfa(user: SudoMethodsUser, hasPasskeyCredentials: boolean): boolean {
	return userHasMfa(user) && userHasSudoCapability(user, hasPasskeyCredentials);
}

export function emailCodeSudoAvailable(user: SudoMethodsUser, hasPasskeyCredentials: boolean): boolean {
	return (
		!userHasUsableMfa(user, hasPasskeyCredentials) &&
		(user.passwordHash ?? null) === null &&
		hasOwnedEmail({email: user.email ?? null, emailVerified: user.emailVerified === true})
	);
}

export function hasNoVerifiableCredential(
	user: {passwordHash: string | null; isBot: boolean; email: string | null; emailVerified: boolean},
	hasMfa: boolean,
	hasPasskeyCredentials: boolean,
): boolean {
	if (user.isBot || hasMfa || hasPasskeyCredentials || hasOwnedEmail(user)) {
		return false;
	}
	return user.passwordHash === null;
}

export function deriveSudoMethods(
	user: User,
	hasPasskeyCredentials: boolean,
	hasBackupCodes: boolean,
	ssoEnabled: boolean,
): SudoModeMethods {
	return {
		totp: hasTotpEnrolled(user),
		webauthn: hasPasskeyCredentials,
		backup_codes: hasBackupCodes,
		email_code: emailCodeSudoAvailable(user, hasPasskeyCredentials),
		password: (user.passwordHash ?? null) !== null && !userHasUsableMfa(user, hasPasskeyCredentials),
		sso_reauth:
			ssoEnabled &&
			isSsoLinkedUser(user) &&
			hasNoVerifiableCredential(user, userHasUsableMfa(user, hasPasskeyCredentials), hasPasskeyCredentials),
	};
}

export async function createSudoModeRequiredError(
	ctx: ApiContext,
	user: User,
	hasPasskeyCredentials?: boolean,
): Promise<SudoModeRequiredError> {
	const hasPasskeys = hasPasskeyCredentials ?? (await ctx.services.users.listWebAuthnCredentials(user.id)).length > 0;
	const hasBackupCodes = await AuthMfa.hasUnconsumedBackupCodes(ctx, user.id);
	const ssoEnabled = (await getInstanceConfigRepository().getSsoConfig()).enabled;
	return new SudoModeRequiredError(
		userHasSudoCapability(user, hasPasskeys),
		deriveSudoMethods(user, hasPasskeys, hasBackupCodes, ssoEnabled),
	);
}
