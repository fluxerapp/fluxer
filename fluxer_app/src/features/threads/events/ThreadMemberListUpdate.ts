// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadRoster, {type ThreadMemberListPayload} from '@app/features/threads/state/ThreadRoster';

export function handleThreadMemberListUpdate(data: ThreadMemberListPayload, _context: GatewayHandlerContext): void {
	if (!ThreadGuilds.isActive(data.guild_id)) return;
	ThreadRoster.handleListUpdate(data);
}
