// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import type {SudoModeMethods} from '@fluxer/errors/src/domains/auth/SudoModeRequiredError';

export function userHasMfa(user: {authenticatorTypes?: Set<number> | null}): boolean {
	return (
		(user.authenticatorTypes?.has(UserAuthenticatorTypes.TOTP) ?? false) ||
		(user.authenticatorTypes?.has(UserAuthenticatorTypes.WEBAUTHN) ?? false)
	);
}

export function deriveSudoMethods(user: {
	totpSecret?: string | null;
	authenticatorTypes?: Set<number> | null;
}): SudoModeMethods {
	const authenticatorTypes = user.authenticatorTypes ?? null;
	return {
		totp: (user.totpSecret ?? null) !== null && (authenticatorTypes?.has(UserAuthenticatorTypes.TOTP) ?? false),
		webauthn: authenticatorTypes?.has(UserAuthenticatorTypes.WEBAUTHN) ?? false,
	};
}
