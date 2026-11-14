// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const UserTraits = {
	SSO: 'sso',
	PASSKEY: 'passkey',
	DOB_DEFERRED: 'dob_deferred',
	BELOW_MINIMUM_AGE: 'below_minimum_age',
} as const;

export type UserTrait = ValueOf<typeof UserTraits>;

export type UserTraitSet = ReadonlySet<string> | ReadonlyArray<string>;

export function hasUserTrait(traits: UserTraitSet, trait: UserTrait): boolean {
	return 'has' in traits ? traits.has(trait) : traits.includes(trait);
}
