// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {GuildLayout} from '@app/features/app/components/layout/GuildLayout';
import Channels from '@app/features/channel/state/Channels';
import {pickDefaultGuildChannelId} from '@app/features/messaging/utils/ChannelShared';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {ME} from '@fluxer/constants/src/AppConstants';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';

function isGuildRootPath(pathname: string, guildId: string): boolean {
	if (guildId === ME || pathname === Routes.ME || pathname.startsWith(Routes.ME)) {
		return false;
	}
	if (!pathname.startsWith('/channels/')) {
		return false;
	}
	const segments = pathname.split('/');
	return segments.length === 3 && segments[2] === guildId;
}

export const GuildChannelRouter = observer<{guildId: string; children: React.ReactNode}>(({guildId, children}) => {
	const location = useLocation();
	const needsDefaultChannel = !MobileLayout.enabled && isGuildRootPath(location.pathname, guildId);
	const defaultChannelId = needsDefaultChannel
		? pickDefaultGuildChannelId({
				guildId,
				channels: Channels.getGuildChannels(guildId),
				selectedChannelId: SelectedChannel.selectedChannelIds.get(guildId),
			})
		: null;
	useEffect(() => {
		if (!defaultChannelId) {
			return;
		}
		NavigationCommands.selectChannel(guildId, defaultChannelId, undefined, 'replace');
	}, [guildId, defaultChannelId]);
	if (guildId === ME || location.pathname === Routes.ME) {
		return null;
	}
	return <GuildLayout data-flx="app.router.guild-channel-router.guild-layout">{children}</GuildLayout>;
});
