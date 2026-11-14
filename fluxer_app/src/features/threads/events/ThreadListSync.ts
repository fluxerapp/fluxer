// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import ChannelThreads, {type ThreadListSyncPayload} from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import {getOpenThreadIds} from '@app/features/threads/utils/ThreadViewUtils';

export function handleThreadListSync(data: ThreadListSyncPayload, _context: GatewayHandlerContext): void {
	if (!ThreadGuilds.isActive(data.guild_id)) return;
	ChannelThreads.handleListSync(data, getOpenThreadIds());
	QuickSwitcher.recomputeIfOpen();
}
