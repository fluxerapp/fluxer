// SPDX-License-Identifier: AGPL-3.0-or-later

import {useChannelHoverPreload} from '@app/features/app/hooks/useChannelHoverPreload';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildCount from '@app/features/guild/state/GuildCount';
import {filterViewableChannels} from '@app/features/messaging/utils/ChannelShared';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {useEffect, useMemo} from 'react';

interface UseGuildListItemPreloadOptions {
	readonly guild: Guild;
	readonly isHovering: boolean;
	readonly isMobileExperience: boolean;
	readonly isSortingList: boolean;
}

interface UseGuildListItemPreloadResult {
	readonly preloadChannelNow: () => void;
	readonly selectedChannelId: string | null;
}

function resolveSelectedChannel(selectedChannelId: string | null): Channel | null {
	if (selectedChannelId == null) {
		return null;
	}
	return Channels.getChannel(selectedChannelId) ?? null;
}

function resolveDefaultHiddenForChannel(channel: Channel | null): boolean {
	if (channel == null) {
		return false;
	}
	return channel.type === ChannelTypes.GUILD_VOICE;
}

export function useGuildListItemPreload({
	guild,
	isHovering,
	isMobileExperience,
	isSortingList,
}: UseGuildListItemPreloadOptions): UseGuildListItemPreloadResult {
	const storedChannelId = SelectedChannel.selectedChannelIds.get(guild.id);
	let selectedChannelId: string | null = null;
	if (storedChannelId != null) {
		selectedChannelId = storedChannelId;
	}
	const selectedChannel = resolveSelectedChannel(selectedChannelId);
	const guildChannels = Channels.getGuildChannels(guild.id);
	const preloadTargetChannel = useMemo(() => {
		if (
			selectedChannel != null &&
			selectedChannel.guildId === guild.id &&
			selectedChannel.type !== ChannelTypes.GUILD_CATEGORY &&
			selectedChannel.type !== ChannelTypes.GUILD_LINK
		) {
			return selectedChannel;
		}
		const firstTextChannel = filterViewableChannels(guildChannels)[0];
		if (firstTextChannel == null) {
			return null;
		}
		return firstTextChannel;
	}, [guild.id, guildChannels, selectedChannel]);
	const {scheduleChannelPreload, cancelChannelPreload, preloadChannelNow} = useChannelHoverPreload({
		channel: preloadTargetChannel,
		guild,
		defaultHiddenForChannel: resolveDefaultHiddenForChannel(preloadTargetChannel),
		enabled: !guild.unavailable && !isSortingList,
	});
	useEffect(() => {
		if (isMobileExperience || isSortingList || !isHovering) {
			cancelChannelPreload();
			return;
		}
		scheduleChannelPreload();
		return cancelChannelPreload;
	}, [cancelChannelPreload, isHovering, isMobileExperience, isSortingList, scheduleChannelPreload]);
	useEffect(() => {
		if (isMobileExperience || isSortingList || !isHovering) return;
		const timeoutId = window.setTimeout(() => GuildCount.requestCounts(guild.id, {force: false}), 250);
		return () => window.clearTimeout(timeoutId);
	}, [guild.id, isHovering, isMobileExperience, isSortingList]);
	return {preloadChannelNow, selectedChannelId};
}
