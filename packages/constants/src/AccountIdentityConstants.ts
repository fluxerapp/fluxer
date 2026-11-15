// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const AccountIdentityModes = {
	EMAIL: 'email',
	USERNAME: 'username',
} as const;

export type AccountIdentityMode = ValueOf<typeof AccountIdentityModes>;

export const DEFAULT_ACCOUNT_IDENTITY_MODE: AccountIdentityMode = AccountIdentityModes.EMAIL;

export const TagStyles = {
	NONE: 'none',
	RANDOM: 'random',
} as const;

export type TagStyle = ValueOf<typeof TagStyles>;

export const AccountIdentityLockReasons = {
	SETUP_CONFIGURED: 'setup_configured',
	EXISTING_INSTANCE: 'existing_instance',
	ADMIN_EXISTS: 'admin_exists',
	ACCOUNTS_EXIST: 'accounts_exist',
} as const;

export type AccountIdentityLockReason = ValueOf<typeof AccountIdentityLockReasons>;

export const AccountIdentityDecisionSources = {
	NEW_INSTANCE: 'new_instance',
	EXISTING_INSTANCE: 'existing_instance',
	SETUP: 'setup',
} as const;

export type AccountIdentityDecisionSource = ValueOf<typeof AccountIdentityDecisionSources>;

export interface AccountIdentity {
	mode: AccountIdentityMode;
	tagStyle: TagStyle;
}

export function normalizeAccountIdentity(mode: AccountIdentityMode, tagStyle: TagStyle): AccountIdentity {
	return {mode, tagStyle: mode === AccountIdentityModes.USERNAME ? TagStyles.NONE : tagStyle};
}
