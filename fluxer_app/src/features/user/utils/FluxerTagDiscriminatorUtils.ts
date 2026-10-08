// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';

export function isVisionaryDiscriminator0000Blocked(options: {
	showPremium: boolean;
	isVisionary: boolean;
	discriminator: string;
}): boolean {
	const {showPremium, isVisionary, discriminator} = options;
	if (RuntimeConfig.tagStyle !== TagStyles.RANDOM) return false;
	return showPremium && !isVisionary && discriminator === '0000';
}
