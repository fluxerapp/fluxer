// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelWire} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import ReadStates from '@app/features/read_state/state/ReadStates';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';

export function handleThreadCreate(data: ChannelWire, _context: GatewayHandlerContext): void {
	if (!ThreadGuilds.isActive(data.guild_id)) return;
	ChannelThreads.upsert(data);
	const parent = data.parent_id ? Channels.getChannel(data.parent_id) : undefined;
	if (
		data.newly_created &&
		parent?.isThreadOnly() &&
		(parent.lastMessageId == null || SnowflakeUtils.compare(data.id, parent.lastMessageId) > 0)
	) {
		const bumped = parent.withUpdates({last_message_id: data.id});
		Channels.handleChannelCreate({channel: bumped});
		ReadStates.handleChannelCreate({channel: bumped.toJSON()});
		GuildReadState.handleGenericUpdate(parent.id);
	}
	QuickSwitcher.recomputeIfOpen();
}
