// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {Guild} from '@app/features/guild/models/Guild';
import {ensureMembersForMessages} from '@app/features/messaging/commands/MessageCommands';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {useCallback, useEffect, useRef} from 'react';

const CHANNEL_HOVER_PRELOAD_DELAY_MS = 220;

interface UseChannelHoverPreloadOptions {
	channel: Channel | null | undefined;
	guild?: Guild | null;
	defaultHiddenForChannel?: boolean;
	enabled?: boolean;
	preloadMemberList?: boolean;
	preloadMessages?: boolean;
}

export function ensureMembersForCachedChannelMessages(channelId: string): void {
	const messages = Messages.getCachedMessages(channelId);
	if (!messages || messages.length === 0) {
		return;
	}
	void ensureMembersForMessages(messages.toArray());
}

export function useChannelHoverPreload({
	channel,
	guild = null,
	enabled = true,
	preloadMessages = true,
}: UseChannelHoverPreloadOptions): {
	scheduleChannelPreload: () => void;
	cancelChannelPreload: () => void;
	preloadChannelNow: () => void;
} {
	const timerRef = useRef<number | null>(null);
	const cancelChannelPreload = useCallback(() => {
		if (timerRef.current == null || typeof window === 'undefined') {
			timerRef.current = null;
			return;
		}
		window.clearTimeout(timerRef.current);
		timerRef.current = null;
	}, []);
	const preloadChannelNow = useCallback(() => {
		cancelChannelPreload();
		if (
			!enabled ||
			!channel ||
			channel.type === ChannelTypes.GUILD_CATEGORY ||
			channel.type === ChannelTypes.GUILD_LINK
		) {
			return;
		}
		if (preloadMessages) {
			if (!Messages.preloadLatestPage(channel.id, guild?.id ?? channel.guildId ?? null)) {
				ensureMembersForCachedChannelMessages(channel.id);
			}
		}
	}, [cancelChannelPreload, channel, enabled, guild, preloadMessages]);
	const scheduleChannelPreload = useCallback(() => {
		if (!enabled || !channel || typeof window === 'undefined') {
			return;
		}
		cancelChannelPreload();
		timerRef.current = window.setTimeout(preloadChannelNow, CHANNEL_HOVER_PRELOAD_DELAY_MS);
	}, [cancelChannelPreload, channel, enabled, preloadChannelNow]);
	useEffect(() => cancelChannelPreload, [cancelChannelPreload]);
	return {scheduleChannelPreload, cancelChannelPreload, preloadChannelNow};
}
