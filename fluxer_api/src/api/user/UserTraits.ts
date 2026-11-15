// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {UserTraits} from '@fluxer/constants/src/UserTraitConstants';

export {UserTraits};

export const DOB_DEFERRED_TRAIT = UserTraits.DOB_DEFERRED;
export const BELOW_MINIMUM_AGE_TRAIT = UserTraits.BELOW_MINIMUM_AGE;

const DERIVED_TRAITS = new Set(['premium']);

const SERVER_MANAGED_TRAITS = new Set<string>([
	UserTraits.SSO,
	UserTraits.PASSKEY,
	'registration_pending_approval',
	'registration_rejected',
	UserTraits.DOB_DEFERRED,
	UserTraits.BELOW_MINIMUM_AGE,
]);

const SERVER_MANAGED_TRAIT_PREFIXES = ['sso:', 'sso_provider:', 'sso_identity:'];

export function isDerivedTrait(trait: string): boolean {
	return !Config.instance.selfHosted && DERIVED_TRAITS.has(trait);
}

export function isServerManagedTrait(trait: string): boolean {
	return SERVER_MANAGED_TRAITS.has(trait) || SERVER_MANAGED_TRAIT_PREFIXES.some((prefix) => trait.startsWith(prefix));
}

export function resolveAssignedTraits(current: Iterable<string>, requested: Iterable<string>): Set<string> {
	const next = new Set<string>();
	for (const trait of requested) {
		if (!trait || isDerivedTrait(trait) || isServerManagedTrait(trait)) {
			continue;
		}
		next.add(trait);
	}
	for (const trait of current) {
		if (isServerManagedTrait(trait)) {
			next.add(trait);
		}
	}
	return next;
}
