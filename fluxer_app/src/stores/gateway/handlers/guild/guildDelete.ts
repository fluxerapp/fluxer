/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {Guild} from '~/records/GuildRecord';
import ChannelStore from '~/stores/ChannelStore';
import EmojiStore from '~/stores/EmojiStore';
import GuildAvailabilityStore from '~/stores/GuildAvailabilityStore';
import GuildListStore from '~/stores/GuildListStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildReadStateStore from '~/stores/GuildReadStateStore';
import GuildStore from '~/stores/GuildStore';
import GuildVerificationStore from '~/stores/GuildVerificationStore';
import InviteStore from '~/stores/InviteStore';
import MemberSearchStore from '~/stores/MemberSearchStore';
import MemberSidebarStore from '~/stores/MemberSidebarStore';
import MessageStore from '~/stores/MessageStore';
import PermissionStore from '~/stores/PermissionStore';
import PresenceStore from '~/stores/PresenceStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';
import RecentMentionsStore from '~/stores/RecentMentionsStore';
import StickerStore from '~/stores/StickerStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import WebhookStore from '~/stores/WebhookStore';
import type {GatewayHandlerContext} from '../index';

interface GuildDeletePayload {
	id: string;
	unavailable?: boolean;
}

export function handleGuildDelete(data: GuildDeletePayload, _context: GatewayHandlerContext): void {
	GuildAvailabilityStore.handleGuildAvailability(data.id, data.unavailable);
	GuildStore.handleGuildDelete({guildId: data.id, unavailable: data.unavailable});
	GuildListStore.handleGuildDelete(data.id, data.unavailable);
	GuildMemberStore.handleGuildDelete(data.id);
	GuildReadStateStore.handleGuildDelete({guild: data as Guild});
	GuildVerificationStore.handleGuildDelete(data.id);
	ChannelStore.handleGuildDelete({guildId: data.id});
	StickerStore.handleGuildDelete(data.id);
	EmojiStore.handleGuildDelete({guildId: data.id});
	PermissionStore.handleGuild();
	InviteStore.handleGuildDelete(data.id);
	PresenceStore.handleGuildDelete(data.id);
	WebhookStore.handleGuildDelete(data.id);
	MediaEngineStore.handleGuildDelete(data.id);
	MemberSidebarStore.handleGuildDelete(data.id);
	MessageStore.handleGuildUnavailable(data.id, data.unavailable ?? false);
	MessageStore.handleCleanup();
	RecentMentionsStore.handleGuildDelete(data.id);
	MemberSearchStore.handleGuildDelete(data.id);

	QuickSwitcherStore.recomputeIfOpen();
}
