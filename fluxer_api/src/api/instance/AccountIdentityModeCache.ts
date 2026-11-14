// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {
	type AccountIdentity,
	type AccountIdentityMode,
	AccountIdentityModes,
	DEFAULT_ACCOUNT_IDENTITY_MODE,
	normalizeAccountIdentity,
	type TagStyle,
	TagStyles,
} from '@fluxer/constants/src/AccountIdentityConstants';

let cachedStored: AccountIdentity | null = null;

export function resolveAccountIdentity(stored: AccountIdentity | null): AccountIdentity {
	if (!Config.instance.selfHosted || !stored) {
		return {mode: DEFAULT_ACCOUNT_IDENTITY_MODE, tagStyle: TagStyles.RANDOM};
	}
	return normalizeAccountIdentity(stored.mode, stored.tagStyle);
}

function getCachedAccountIdentity(): AccountIdentity {
	return resolveAccountIdentity(cachedStored);
}

export function getCachedAccountIdentityMode(): AccountIdentityMode {
	return getCachedAccountIdentity().mode;
}

export function getCachedTagStyle(): TagStyle {
	return getCachedAccountIdentity().tagStyle;
}

export function usesUsernameSignIn(): boolean {
	return getCachedAccountIdentityMode() === AccountIdentityModes.USERNAME;
}

export function usesUniqueUsernames(): boolean {
	return getCachedTagStyle() === TagStyles.NONE;
}

export function setCachedAccountIdentity(stored: AccountIdentity | null): void {
	cachedStored = stored;
}
