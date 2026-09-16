// SPDX-License-Identifier: AGPL-3.0-or-later

import {TopNagbarContext} from '@app/features/app/components/layout/app_layout/TopNagbarContext';
import {ChannelListContent} from '@app/features/app/components/layout/ChannelListContent';
import {GuildHeader} from '@app/features/app/components/layout/GuildHeader';
import {GuildSidebar} from '@app/features/app/components/layout/GuildSidebar';
import {useGuildBannerPresentation} from '@app/features/app/hooks/useGuildBannerPresentation';
import {useNativePlatform} from '@app/features/app/hooks/useNativePlatform';
import KeybindManager from '@app/features/app/keybindings/KeybindManager';
import Channels from '@app/features/channel/state/Channels';
import type {Guild} from '@app/features/guild/models/Guild';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {useMotionValue} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useContext, useMemo} from 'react';
import {useHotkeys} from 'react-hotkeys-hook';

export const GuildNavbar = observer(({guild}: {guild: Guild}) => {
	const scrollY = useMotionValue(0);
	const banner = useGuildBannerPresentation({guild, scrollY});
	const {isNative, isWindows, isLinux} = useNativePlatform();
	const topNagbarCount = useContext(TopNagbarContext);
	const hasTopNagbar = topNagbarCount > 0;
	const shouldRoundTopLeft = isNative && (isWindows || isLinux) && !hasTopNagbar;
	const channels = Channels.getGuildChannels(guild.id);
	const categoryIds = useMemo(() => {
		return channels.filter((ch) => ch.type === ChannelTypes.GUILD_CATEGORY).map((ch) => ch.id);
	}, [channels]);
	useHotkeys(
		'mod+shift+a',
		() => {
			if (KeybindManager.isSuspended()) return;
			if (categoryIds.length > 0) {
				UserGuildSettingsCommands.toggleAllCategoriesCollapsed(guild.id, categoryIds);
			}
		},
		{
			enableOnFormTags: true,
			enableOnContentEditable: true,
			preventDefault: true,
		},
		[guild.id, categoryIds],
	);
	return (
		<GuildSidebar
			roundTopLeft={shouldRoundTopLeft}
			header={<GuildHeader guild={guild} banner={banner} data-flx="app.guild-navbar.guild-header" />}
			content={
				<ChannelListContent
					guild={guild}
					scrollY={scrollY}
					banner={banner}
					data-flx="app.guild-navbar.channel-list-content"
				/>
			}
			data-flx="app.guild-navbar.guild-sidebar"
		/>
	);
});
