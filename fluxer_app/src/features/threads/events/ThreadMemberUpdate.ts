// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';

type ThreadMemberUpdatePayload = ThreadMemberResponse & {guild_id: string};

export function handleThreadMemberUpdate(data: ThreadMemberUpdatePayload, _context: GatewayHandlerContext): void {
	if (!ThreadGuilds.isActive(data.guild_id) || !data.id || !ChannelThreads.isCurrentUser(data.user_id)) return;
	ThreadMemberships.set(data.id, data);
	GuildReadState.handleGenericUpdate(data.id);
}
