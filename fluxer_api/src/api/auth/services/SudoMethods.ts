// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import {isLinkedToSsoProvider, ssoProviderIdOf} from '@app/api/auth/services/SsoIdentityLinks';
import {hasOwnedEmail} from '@app/api/auth/services/SudoEmailCode';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
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
	ssoProviderId: string | null,
): SudoModeMethods {
	return {
		totp: hasTotpEnrolled(user),
		webauthn: hasPasskeyCredentials,
		backup_codes: hasBackupCodes,
		email_code: emailCodeSudoAvailable(user, hasPasskeyCredentials),
		password: (user.passwordHash ?? null) !== null && !userHasUsableMfa(user, hasPasskeyCredentials),
		sso_reauth:
			ssoProviderId !== null &&
			isLinkedToSsoProvider(user, ssoProviderId) &&
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
	const ssoConfig = await getInstanceConfigRepository().getSsoConfig();
	const ssoProviderId = ssoConfig.enabled ? ssoProviderIdOf(ssoConfig) : null;
	return new SudoModeRequiredError(
		userHasSudoCapability(user, hasPasskeys),
		deriveSudoMethods(user, hasPasskeys, hasBackupCodes, ssoProviderId),
	);
}
