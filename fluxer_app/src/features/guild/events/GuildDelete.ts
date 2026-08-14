// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Emoji from '@app/features/emoji/state/Emoji';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import ChannelMemberCount from '@app/features/guild/state/ChannelMemberCount';
import GuildAvailability from '@app/features/guild/state/GuildAvailability';
import GuildCount from '@app/features/guild/state/GuildCount';
import GuildList from '@app/features/guild/state/GuildList';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import Guilds from '@app/features/guild/state/Guilds';
import GuildVerification from '@app/features/guild/state/GuildVerification';
import Invites from '@app/features/invite/state/Invites';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch from '@app/features/member/state/MemberSearch';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import Messages from '@app/features/messaging/state/MessagingMessages';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import Permission from '@app/features/permissions/state/Permission';
import Presence from '@app/features/presence/state/Presence';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import Webhooks from '@app/features/webhook/state/Webhooks';
import type {Guild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

interface GuildDeletePayload {
	id: string;
	unavailable?: boolean;
	unavailable_hidden?: boolean;
}

export function handleGuildDelete(data: GuildDeletePayload, _context: GatewayHandlerContext): void {
	GuildAvailability.handleGuildAvailability(data.id, data.unavailable, data.unavailable_hidden);
	Guilds.handleGuildDelete({guildId: data.id, unavailable: data.unavailable});
	GuildList.handleGuildDelete(data.id, data.unavailable);
	MemberSearch.handleGuildDelete(data.id);
	GuildMembers.handleGuildDelete(data.id);
	GuildCount.handleGuildDelete(data.id);
	ChannelMemberCount.handleGuildDelete(data.id);
	GuildReadState.handleGuildDelete({guild: data as Guild});
	GuildVerification.handleGuildDelete(data.id);
	Channels.handleGuildDelete({guildId: data.id});
	Sticker.handleGuildDelete(data.id);
	Emoji.handleGuildDelete({guildId: data.id});
	Permission.handleGuild();
	Invites.handleGuildDelete(data.id);
	Presence.handleGuildDelete(data.id);
	Webhooks.handleGuildDelete(data.id);
	MediaEngine.handleGuildDelete(data.id);
	MemberSidebar.handleGuildDelete(data.id);
	Messages.handleGuildUnavailable(data.id, data.unavailable ?? false);
	Messages.handleCleanup();
	MentionFeed.handleGuildDelete(data.id);
	QuickSwitcher.recomputeIfOpen();
}
