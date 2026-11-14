// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';

const PREMIUM_TRAIT = 'premium';

export function resolveEffectiveUserTraits(
	storedTraits: Iterable<string> | null | undefined,
	isPremium: boolean,
): Set<string> {
	const traits = new Set<string>();
	for (const trait of storedTraits ?? []) {
		const normalized = trait.trim().toLowerCase();
		if (!normalized || (normalized === PREMIUM_TRAIT && !Config.instance.selfHosted)) continue;
		traits.add(normalized);
	}
	if (isPremium) traits.add(PREMIUM_TRAIT);
	return traits;
}
