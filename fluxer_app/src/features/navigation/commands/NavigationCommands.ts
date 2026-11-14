// SPDX-License-Identifier: AGPL-3.0-or-later

import * as LinkChannelCommands from '@app/features/channel/commands/LinkChannelCommands';
import Channels from '@app/features/channel/state/Channels';
import Navigation from '@app/features/navigation/state/Navigation';
import {FAVORITES_GUILD_ID, ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

type NavigationMode = 'push' | 'replace';

function navigateToChannelNow(
	guildId?: string,
	channelId?: string | null,
	messageId?: string,
	mode: NavigationMode = 'push',
): void {
	if (!guildId || guildId === ME) {
		Navigation.navigateToDM(channelId ?? undefined, messageId, mode);
	} else if (guildId === FAVORITES_GUILD_ID) {
		Navigation.navigateToFavorites(channelId ?? undefined, messageId, mode);
	} else {
		Navigation.navigateToGuild(guildId, channelId ?? undefined, messageId, mode);
	}
}

function shouldInterceptGuildChannelNavigation(channelId: string | null | undefined): boolean {
	if (!channelId) return false;
	const channel = Channels.getChannel(channelId);
	if (!channel || channel.isPrivate()) return false;
	if (channel.type === ChannelTypes.GUILD_CATEGORY) return false;
	if (channel.type === ChannelTypes.GUILD_LINK) {
		return LinkChannelCommands.openLinkChannel(channel);
	}
	return false;
}

function getGuildSelectionChannelId(guildId: string, channelId: string | null | undefined): string | undefined {
	if (!channelId) return undefined;
	const channel = Channels.getChannel(channelId);
	if (!channel) return channelId;
	if (channel.type === ChannelTypes.GUILD_CATEGORY || channel.type === ChannelTypes.GUILD_LINK) return undefined;
	if (guildId === ME) return channel.isPrivate() ? channelId : undefined;
	if (guildId === FAVORITES_GUILD_ID && channel.isPrivate()) return undefined;
	if (guildId !== FAVORITES_GUILD_ID && channel.guildId !== guildId) return undefined;
	return channelId;
}

export function selectChannel(
	guildId?: string,
	channelId?: string | null,
	messageId?: string,
	mode: NavigationMode = 'push',
): void {
	if (shouldInterceptGuildChannelNavigation(channelId)) {
		return;
	}
	navigateToChannelNow(guildId, channelId, messageId, mode);
}

export function selectGuild(guildId: string, channelId?: string, mode: NavigationMode = 'push'): void {
	const selectionChannelId = getGuildSelectionChannelId(guildId, channelId);
	if (shouldInterceptGuildChannelNavigation(selectionChannelId)) {
		return;
	}
	navigateToChannelNow(guildId, selectionChannelId, undefined, mode);
}

export function deselectGuild(): void {
	Navigation.navigateToDM();
}

export function navigateToMessage(
	guildId: string | null | undefined,
	channelId: string,
	messageId: string,
	mode: NavigationMode = 'push',
): void {
	selectChannel(guildId ?? undefined, channelId, messageId, mode);
}

export function clearMessageIdForChannel(channelId: string, mode: NavigationMode = 'replace'): void {
	Navigation.clearMessageIdForChannel(channelId, mode);
}
