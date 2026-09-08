// SPDX-License-Identifier: AGPL-3.0-or-later

import {FAVORITES_GUILD_ID} from '@fluxer/constants/src/AppConstants';

export function selectGuildActivationTarget(input: {
	selectedGuildId: string | null;
	openChannelGuildId: string | null;
}): string | null {
	if (input.selectedGuildId && input.selectedGuildId !== FAVORITES_GUILD_ID) {
		return input.selectedGuildId;
	}
	return input.openChannelGuildId;
}
