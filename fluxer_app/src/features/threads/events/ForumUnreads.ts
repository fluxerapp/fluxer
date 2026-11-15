// SPDX-License-Identifier: AGPL-3.0-or-later

import ForumReadState, {type ForumUnreadsPayload} from '@app/features/forum/state/ForumReadState';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';

export function handleForumUnreads(data: ForumUnreadsPayload, _context: GatewayHandlerContext): void {
	if (!ThreadGuilds.isActive(data.guild_id)) return;
	ForumReadState.handleForumUnreads(data);
}
