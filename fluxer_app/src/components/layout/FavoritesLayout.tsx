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

import {observer} from 'mobx-react-lite';
import React from 'react';
import * as NavigationActionCreators from '~/actions/NavigationActionCreators';
import {FAVORITES_GUILD_ID} from '~/Constants';
import {FavoritesWelcomeSection} from '~/components/favorites/FavoritesWelcomeSection';
import {FavoritesChannelListContent} from '~/components/layout/FavoritesChannelListContent';
import {FavoritesGuildHeader} from '~/components/layout/FavoritesGuildHeader';
import {GuildSidebar} from '~/components/layout/GuildSidebar';
import {useParams} from '~/lib/router';
import {Routes} from '~/Routes';
import FavoritesStore from '~/stores/FavoritesStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import * as RouterUtils from '~/utils/RouterUtils';
import styles from './GuildLayout.module.css';

export const FavoritesLayout = observer(({children}: {children?: React.ReactNode}) => {
	const mobileLayout = MobileLayoutStore;
	const {channelId} = useParams() as {channelId?: string};

	const hasAccessibleChannels = FavoritesStore.getFirstAccessibleChannel() !== undefined;
	const showWelcomeScreen = !channelId && !hasAccessibleChannels;
	const shouldRenderWelcomeScreen = showWelcomeScreen && !mobileLayout.enabled;

	React.useEffect(() => {
		if (channelId) {
			NavigationActionCreators.selectChannel(FAVORITES_GUILD_ID, channelId);
		}
	}, [channelId]);

	React.useEffect(() => {
		if (!channelId) return;

		const isStillFavorited = FavoritesStore.getChannel(channelId);

		if (!isStillFavorited) {
			const validChannelId = SelectedChannelStore.getValidatedFavoritesChannel();

			if (validChannelId) {
				RouterUtils.transitionTo(Routes.favoritesChannel(validChannelId));
			} else {
				RouterUtils.transitionTo(Routes.FAVORITES);
			}
		}
	}, [channelId, FavoritesStore.channels]);

	if (shouldRenderWelcomeScreen) {
		return (
			<div className={styles.guildLayoutContainer}>
				<div className={styles.guildLayoutContent}>
					<GuildSidebar header={<FavoritesGuildHeader />} content={<FavoritesChannelListContent />} />
					<div className={styles.guildMainContent}>
						<FavoritesWelcomeSection />
					</div>
				</div>
			</div>
		);
	}

	if (mobileLayout.enabled) {
		if (!channelId) {
			return <GuildSidebar header={<FavoritesGuildHeader />} content={<FavoritesChannelListContent />} />;
		}

		return (
			<div className={styles.guildLayoutContainer}>
				<div className={styles.guildMainContent}>{children}</div>
			</div>
		);
	}

	return (
		<div className={styles.guildLayoutContainer}>
			<div className={styles.guildLayoutContent}>
				<GuildSidebar header={<FavoritesGuildHeader />} content={<FavoritesChannelListContent />} />
				<div className={styles.guildMainContent}>{children}</div>
			</div>
		</div>
	);
});
