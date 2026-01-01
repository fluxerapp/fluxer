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

import {useMotionValue} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {useHotkeys} from 'react-hotkeys-hook';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import {ChannelTypes} from '~/Constants';
import {useNativePlatform} from '~/hooks/useNativePlatform';
import type {GuildRecord} from '~/records/GuildRecord';
import ChannelStore from '~/stores/ChannelStore';
import {TopNagbarContext} from './app-layout/TopNagbarContext';
import {ChannelListContent} from './ChannelListContent';
import {GuildHeader} from './GuildHeader';
import {GuildSidebar} from './GuildSidebar';

export const GuildNavbar = observer(({guild}: {guild: GuildRecord}) => {
	const scrollY = useMotionValue(0);
	const {isNative, isWindows, isLinux} = useNativePlatform();
	const hasTopNagbar = React.useContext(TopNagbarContext);
	const shouldRoundTopLeft = isNative && (isWindows || isLinux) && !hasTopNagbar;

	React.useEffect(() => {
		scrollY.set(0);
	}, [guild.id, scrollY]);

	const channels = ChannelStore.getGuildChannels(guild.id);

	const categoryIds = React.useMemo(() => {
		return channels.filter((ch) => ch.type === ChannelTypes.GUILD_CATEGORY).map((ch) => ch.id);
	}, [channels]);

	useHotkeys(
		'mod+shift+a',
		() => {
			if (categoryIds.length > 0) {
				UserGuildSettingsActionCreators.toggleAllCategoriesCollapsed(guild.id, categoryIds);
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
			header={<GuildHeader guild={guild} />}
			content={<ChannelListContent guild={guild} scrollY={scrollY} />}
		/>
	);
});
