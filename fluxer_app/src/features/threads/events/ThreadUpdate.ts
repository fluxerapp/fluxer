// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelWire} from '@app/features/channel/models/Channel';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';

export function handleThreadUpdate(data: ChannelWire, _context: GatewayHandlerContext): void {
	if (!ThreadGuilds.isActive(data.guild_id)) return;
	ChannelThreads.upsert(data);
	QuickSwitcher.recomputeIfOpen();
}
