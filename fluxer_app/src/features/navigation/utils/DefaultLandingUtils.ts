// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import GuildListState from '@app/features/guild/state/GuildList';

function getFirstAvailableGuildId(): string | null {
	const guilds = GuildListState.guilds;
	for (const guild of guilds) {
		if (!guild.unavailable) {
			return guild.id;
		}
	}
	return null;
}

export function getDirectMessagesFallbackPath(): string {
	const firstGuildId = getFirstAvailableGuildId();
	if (firstGuildId) {
		return Routes.guildChannel(firstGuildId);
	}
	if (Accessibility.showFavorites) {
		return Routes.FAVORITES;
	}
	if (!RuntimeConfig.singleCommunityEnabled) {
		return Routes.DISCOVER;
	}
	return Routes.ME;
}

export function getDefaultLandingPath(): string {
	if (RuntimeConfig.directMessagesDisabled) {
		return getDirectMessagesFallbackPath();
	}
	return Routes.ME;
}
