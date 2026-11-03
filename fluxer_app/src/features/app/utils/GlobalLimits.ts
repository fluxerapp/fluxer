// SPDX-License-Identifier: AGPL-3.0-or-later

import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {AVATAR_MAX_SIZE} from '@fluxer/constants/src/LimitConstants';

class GlobalLimitsClass {
	getAvatarMaxSize(): number {
		return LimitResolver.resolve({
			key: 'avatar_max_size',
			fallback: AVATAR_MAX_SIZE,
			context: {traits: [], guildFeatures: []},
		});
	}
}

export const GlobalLimits = new GlobalLimitsClass();
