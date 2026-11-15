// SPDX-License-Identifier: AGPL-3.0-or-later

import {ME} from '@fluxer/constants/src/AppConstants';

export function isChannelP2p(guildId: string | null, channelId: string): boolean {
	const channelStates = globalThis.window?._mediaEngine?.getAllVoiceStatesInGuild(guildId ?? ME)?.[channelId];
	if (!channelStates) return false;
	const states = Object.values(channelStates);
	return states.length > 0 && states.every((state) => state.p2p === true);
}

export function isActiveVoiceChannelP2p(): boolean {
	const mediaEngine = globalThis.window?._mediaEngine;
	const channelId = mediaEngine?.channelId;
	if (!mediaEngine || !channelId) return false;
	return isChannelP2p(mediaEngine.guildId, channelId);
}
