// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveEffectiveUserTraits} from '@app/api/limits/EffectiveUserTraits';
import type {User} from '@app/api/models/User';
import {checkIsPremium} from '@app/api/user/UserHelpers';
import type {LimitMatchContext} from '@fluxer/limits/src/LimitTypes';

export function createLimitMatchContext({
	user,
	guildFeatures,
}: {
	user?: User | null;
	guildFeatures?: Iterable<string> | null;
}): LimitMatchContext {
	const traits = resolveEffectiveUserTraits(user?.traits, user != null && checkIsPremium(user));
	const guildFeatureSet = new Set<string>();
	if (guildFeatures) {
		for (const feature of guildFeatures) {
			if (feature) guildFeatureSet.add(feature);
		}
	}
	return {traits, guildFeatures: guildFeatureSet};
}
