// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {
	type AccountIdentityMode,
	AccountIdentityModes,
	DEFAULT_ACCOUNT_IDENTITY_MODE,
	type TagStyle,
	TagStyles,
} from '@fluxer/constants/src/AccountIdentityConstants';

export interface StoredAccountIdentity {
	mode: AccountIdentityMode;
	tagStyle: TagStyle;
}

export interface AccountIdentity {
	mode: AccountIdentityMode;
	tagStyle: TagStyle;
}

let cachedStored: StoredAccountIdentity | null = null;

export function resolveAccountIdentity(stored: StoredAccountIdentity | null): AccountIdentity {
	if (!Config.instance.selfHosted || !stored) {
		return {mode: DEFAULT_ACCOUNT_IDENTITY_MODE, tagStyle: TagStyles.RANDOM};
	}
	return {
		mode: stored.mode,
		tagStyle: stored.mode === AccountIdentityModes.USERNAME ? TagStyles.NONE : stored.tagStyle,
	};
}

export function getCachedAccountIdentity(): AccountIdentity {
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

export function setCachedAccountIdentity(stored: StoredAccountIdentity | null): void {
	cachedStored = stored;
}
