// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Channels from '@app/features/channel/state/Channels';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import Navigation from '@app/features/navigation/state/Navigation';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import {isThreadModeratorFor} from '@app/features/threads/utils/ThreadActionRules';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';

interface ThreadMembersUpdatePayload {
	id: string;
	guild_id: string;
	member_count: number;
	added_members?: ReadonlyArray<ThreadMemberResponse>;
	removed_member_ids?: ReadonlyArray<string>;
}

export function handleThreadMembersUpdate(data: ThreadMembersUpdatePayload, _context: GatewayHandlerContext): void {
	if (!ThreadGuilds.isActive(data.guild_id)) return;
	const thread = Channels.getChannel(data.id);
	if (thread?.isThread() && thread.memberCount !== data.member_count) {
		Channels.upsertThread(thread.withUpdates({member_count: data.member_count}).toJSON());
	}
	for (const member of data.added_members ?? []) {
		if (ChannelThreads.isCurrentUser(member.user_id)) {
			ThreadMemberships.set(data.id, member);
			GuildReadState.handleGenericUpdate(data.id);
		}
	}
	const removedSelf = data.removed_member_ids?.some((userId) => ChannelThreads.isCurrentUser(userId)) ?? false;
	if (!removedSelf) return;
	ThreadMemberships.remove(data.id);
	GuildReadState.handleGenericUpdate(data.id);
	if (thread?.isPrivateThread() && !isThreadModeratorFor(thread)) {
		const panelOpen = Navigation.threadId === data.id;
		ChannelThreads.deleteThread(data.id);
		if (panelOpen && Navigation.guildId && Navigation.channelId) {
			RouterUtils.replaceWith(Routes.guildChannel(Navigation.guildId, Navigation.channelId));
		}
	}
}
