// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Emoji from '@app/features/emoji/state/Emoji';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import {handleGuildDelete} from '@app/features/guild/events/GuildDelete';
import GuildAvailability from '@app/features/guild/state/GuildAvailability';
import GuildCount from '@app/features/guild/state/GuildCount';
import GuildList from '@app/features/guild/state/GuildList';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import Guilds from '@app/features/guild/state/Guilds';
import GuildVerification from '@app/features/guild/state/GuildVerification';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch from '@app/features/member/state/MemberSearch';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import Permission from '@app/features/permissions/state/Permission';
import Presence from '@app/features/presence/state/Presence';
import ReadStates from '@app/features/read_state/state/ReadStates';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import Nagbar from '@app/features/ui/state/Nagbar';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {FAVORITES_GUILD_ID} from '@fluxer/constants/src/AppConstants';

function shouldTreatGuildCreateAsUnavailable(data: GuildReadyData): boolean {
	return (
		data.unavailable === true ||
		!Array.isArray(data.channels) ||
		!Array.isArray(data.members) ||
		!Array.isArray(data.roles) ||
		!Array.isArray(data.emojis)
	);
}

export function handleGuildCreate(data: GuildReadyData, _context: GatewayHandlerContext): void {
	const isSync = (
		_context as {
			_isSync?: boolean;
		}
	)._isSync;
	if (shouldTreatGuildCreateAsUnavailable(data)) {
		handleGuildDelete({id: data.id, unavailable: true}, _context);
		return;
	}
	GuildAvailability.setGuildAvailable(data.id);
	Guilds.handleGuildCreate(data);
	GuildCount.handleGuildCreate(data);
	if (!isSync) {
		MemberSidebar.handleGuildCreate(data.id);
	}
	if (!data.unavailable) {
		Channels.handleGuildCreate(data);
	}
	GuildMembers.handleGuildCreate(data, {synced: isSync});
	GuildReadState.handleGuildCreate({guild: data});
	Presence.handleGuildCreate(data);
	MediaEngine.handleGuildCreate(data);
	Messages.handleGuildCreate({guild: data});
	ReadStates.handleGuildCreate({guild: data});
	if (data.emojis.length > 0) {
		Emoji.handleGuildEmojiUpdated({guildId: data.id, emojis: data.emojis});
	}
	if (data.stickers && data.stickers.length > 0) {
		Sticker.handleGuildStickersUpdate(data.id, data.stickers);
	}
	GuildList.handleGuild(data);
	Sticker.handleGuildUpdate(data);
	Nagbar.handleGuildUpdate({guild: data});
	Emoji.handleGuildUpdate({guild: data});
	Permission.handleGuild();
	UserGuildSettings.handleGuildCreate({id: data.id});
	GuildVerification.handleGuildCreate({id: data.id});
	MemberSearch.handleGuildCreate(data.id);
	QuickSwitcher.recomputeIfOpen();
	const selectedId = SelectedGuild.selectedGuildId;
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
