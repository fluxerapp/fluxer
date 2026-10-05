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
