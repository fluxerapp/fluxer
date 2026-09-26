// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import type {SudoModeMethods} from '@fluxer/errors/src/domains/auth/SudoModeRequiredError';

interface SudoMethodsUser {
	totpSecret?: string | null;
	authenticatorTypes?: Set<number> | null;
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

export function deriveSudoMethods(
	user: SudoMethodsUser,
	hasPasskeyCredentials: boolean,
	hasBackupCodes: boolean,
): SudoModeMethods {
	return {
		totp: hasTotpEnrolled(user),
		webauthn: hasPasskeyCredentials,
		backup_codes: hasBackupCodes,
	};
}
