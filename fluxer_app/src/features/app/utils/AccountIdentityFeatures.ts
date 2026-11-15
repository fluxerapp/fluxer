// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AccountIdentityMode,
	AccountIdentityModes,
	type TagStyle,
	TagStyles,
} from '@fluxer/constants/src/AccountIdentityConstants';
import type {InstanceFeatures} from '@fluxer/schema/src/domains/instance/InstanceSchemas';

type AccountIdentityFeatures = Pick<InstanceFeatures, 'account_identity' | 'tag_style'>;

export function accountIdentityOf(features: AccountIdentityFeatures): AccountIdentityMode {
	return features.account_identity ?? AccountIdentityModes.EMAIL;
}

export function usesUsernameSignIn(features: AccountIdentityFeatures): boolean {
	return accountIdentityOf(features) === AccountIdentityModes.USERNAME;
}

export function tagStyleOf(features: AccountIdentityFeatures): TagStyle {
	if (usesUsernameSignIn(features)) return TagStyles.NONE;
	return features.tag_style ?? TagStyles.RANDOM;
}

export function usesUniqueUsernames(features: AccountIdentityFeatures): boolean {
	return tagStyleOf(features) === TagStyles.NONE;
}
