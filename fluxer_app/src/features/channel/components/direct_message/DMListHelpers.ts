// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {Guild} from '@app/features/guild/models/Guild';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';

export interface InviteCandidate {
	guild: Guild;
	channelId: string;
}

export function getDmRouteChannelId(pathname: string): string | null {
	if (!pathname.startsWith(`${Routes.ME}/`)) {
		return null;
	}
	const [, , , channelId] = pathname.split('/');
	return channelId ?? null;
}

export function canInviteInChannel(channel?: Channel | null): channel is Channel {
	if (!channel?.guildId) {
		return false;
	}
	return InviteUtils.canInviteToChannel(channel.id, channel.guildId);
}

export function getDefaultInviteChannelId(guildId: string): string | null {
	const selectedChannelId = SelectedChannel.selectedChannelIds.get(guildId);
	if (selectedChannelId) {
		const selectedChannel = Channels.getChannel(selectedChannelId);
		if (canInviteInChannel(selectedChannel)) {
			return selectedChannel.id;
		}
	}
	const guildChannels = Channels.getGuildChannels(guildId);
	for (const guildChannel of guildChannels) {
		if (canInviteInChannel(guildChannel)) {
			return guildChannel.id;
		}
	}
	return null;
}
