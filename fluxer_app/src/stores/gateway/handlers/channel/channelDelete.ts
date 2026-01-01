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

import type {Channel} from '~/records/ChannelRecord';
import ChannelPinsStore from '~/stores/ChannelPinsStore';
import ChannelStore from '~/stores/ChannelStore';
import DraftStore from '~/stores/DraftStore';
import GuildReadStateStore from '~/stores/GuildReadStateStore';
import InviteStore from '~/stores/InviteStore';
import MessageStore from '~/stores/MessageStore';
import PermissionStore from '~/stores/PermissionStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';
import ReadStateStore from '~/stores/ReadStateStore';
import RecentMentionsStore from '~/stores/RecentMentionsStore';
import SavedMessagesStore from '~/stores/SavedMessagesStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import SlowmodeStore from '~/stores/SlowmodeStore';
import WebhookStore from '~/stores/WebhookStore';
import type {GatewayHandlerContext} from '../index';

interface ChannelDeletePayload {
	id: string;
	type: number;
	guild_id?: string;
}

export function handleChannelDelete(data: ChannelDeletePayload, _context: GatewayHandlerContext): void {
	const channel = data as Channel;
	const guildId = data.guild_id;

	SlowmodeStore.deleteChannel(data.id);
	DraftStore.deleteChannelDraft(data.id);
	SavedMessagesStore.handleChannelDelete(channel);
	ChannelPinsStore.handleChannelDelete(channel);
	ChannelStore.handleChannelDelete({channel});
	PermissionStore.handleChannelDelete(data.id, guildId);
	GuildReadStateStore.handleChannelDelete(data.id);
	InviteStore.handleChannelDelete(data.id);
	WebhookStore.handleChannelDelete(data.id);
	ReadStateStore.handleChannelDelete({channel});
	SelectedChannelStore.handleChannelDelete(channel);
	MessageStore.handleCleanup();
	RecentMentionsStore.handleChannelDelete(channel);
	QuickSwitcherStore.recomputeIfOpen();
}
