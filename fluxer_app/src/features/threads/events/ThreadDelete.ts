// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Channels from '@app/features/channel/state/Channels';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import Navigation from '@app/features/navigation/state/Navigation';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import ReadStates from '@app/features/read_state/state/ReadStates';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import ChannelThreads, {type ThreadDeletePayload} from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';

export function handleThreadDelete(data: ThreadDeletePayload, _context: GatewayHandlerContext): void {
	if (!ThreadGuilds.isActive(data.guild_id)) return;
	const panelOpen = Navigation.threadId === data.id && Navigation.guildId != null && Navigation.channelId != null;
	ChannelThreads.handleThreadDelete(data);
	const parent = data.parent_id ? Channels.getChannel(data.parent_id) : undefined;
	if (parent?.isThreadOnly() && parent.lastMessageId !== data.id && ReadStates.lastMessageId(parent.id) === data.id) {
		ReadStates.handleForumPostDiscarded(parent.id, parent.lastMessageId);
		GuildReadState.handleGenericUpdate(parent.id);
	}
	if (panelOpen && Navigation.guildId && Navigation.channelId) {
		RouterUtils.replaceWith(Routes.guildChannel(Navigation.guildId, Navigation.channelId));
	}
	QuickSwitcher.recomputeIfOpen();
}
