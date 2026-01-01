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

import {FAVORITES_GUILD_ID} from '~/Constants';
import type {GuildReadyData} from '~/records/GuildRecord';
import ChannelStore from '~/stores/ChannelStore';
import EmojiStore from '~/stores/EmojiStore';
import GuildAvailabilityStore from '~/stores/GuildAvailabilityStore';
import GuildListStore from '~/stores/GuildListStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildReadStateStore from '~/stores/GuildReadStateStore';
import GuildStore from '~/stores/GuildStore';
import GuildVerificationStore from '~/stores/GuildVerificationStore';
import MemberSearchStore from '~/stores/MemberSearchStore';
import MemberSidebarStore from '~/stores/MemberSidebarStore';
import MessageStore from '~/stores/MessageStore';
import NagbarStore from '~/stores/NagbarStore';
import PermissionStore from '~/stores/PermissionStore';
import PresenceStore from '~/stores/PresenceStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';
import ReadStateStore from '~/stores/ReadStateStore';
import SelectedGuildStore from '~/stores/SelectedGuildStore';
import StickerStore from '~/stores/StickerStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import type {GatewayHandlerContext} from '../index';

export function handleGuildCreate(data: GuildReadyData, _context: GatewayHandlerContext): void {
	GuildAvailabilityStore.setGuildAvailable(data.id);
	GuildStore.handleGuildCreate(data);
	MemberSidebarStore.handleGuildCreate(data.id);

	if (data.channels.length > 0 && !data.unavailable) {
		ChannelStore.handleGuildCreate(data);
	}

	GuildMemberStore.handleGuildCreate(data);
	GuildReadStateStore.handleGuildCreate({guild: data});
	PresenceStore.handleGuildCreate(data);
	MediaEngineStore.handleGuildCreate(data);
	MessageStore.handleGuildCreate({guild: data});
	ReadStateStore.handleGuildCreate({guild: data});

	if (data.emojis.length > 0) {
		EmojiStore.handleGuildEmojiUpdated({guildId: data.id, emojis: data.emojis});
	}
	if (data.stickers && data.stickers.length > 0) {
		StickerStore.handleGuildStickersUpdate(data.id, data.stickers);
	}

	GuildListStore.handleGuild(data);
	StickerStore.handleGuildUpdate(data);
	NagbarStore.handleGuildUpdate({guild: data});
	EmojiStore.handleGuildUpdate({guild: data});
	PermissionStore.handleGuild();

	UserGuildSettingsStore.handleGuildCreate({id: data.id});
	GuildVerificationStore.handleGuildCreate({id: data.id});

	MemberSearchStore.handleGuildCreate(data.id);

	QuickSwitcherStore.recomputeIfOpen();

	const isSync = (_context as any)._isSync;
	const selectedId = SelectedGuildStore.selectedGuildId;

	if (!isSync && selectedId === data.id && selectedId !== FAVORITES_GUILD_ID) {
		_context.socket?.updateGuildSubscriptions({
			subscriptions: {
				[data.id]: {
					active: true,
					sync: true,
				},
			},
		});
	}
}
