// SPDX-License-Identifier: AGPL-3.0-or-later

import {FavoritesChannelListContent} from '@app/features/app/components/layout/FavoritesChannelListContent';
import {FavoritesGuildHeader} from '@app/features/app/components/layout/FavoritesGuildHeader';
import styles from '@app/features/app/components/layout/GuildLayout.module.css';
import {GuildSidebar} from '@app/features/app/components/layout/GuildSidebar';
import {NativeDragRegion} from '@app/features/app/components/layout/NativeDragRegion';
import Channels from '@app/features/channel/state/Channels';
import {FavoritesWelcomeSection} from '@app/features/expressions/components/FavoritesWelcomeSection';
import Guilds from '@app/features/guild/state/Guilds';
import Favorites from '@app/features/messaging/state/Favorites';
import Navigation from '@app/features/navigation/state/Navigation';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {useParams} from '@app/features/platform/components/router/RouterReact';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';

export const FavoritesLayout = observer(({children}: {children: React.ReactNode}) => {
	const mobileLayout = MobileLayout;
	const {channelId} = useParams() as {channelId?: string};
	const hasAccessibleChannels = Favorites.getFirstAccessibleChannel() !== undefined;
	const showWelcomeScreen = !channelId && !hasAccessibleChannels;
	const favorite = channelId ? Favorites.getChannel(channelId) : undefined;
	const isFavoriteDeleted =
		favorite != null && Guilds.getGuild(favorite.guildId) != null && Channels.getChannel(favorite.channelId) == null;
	useEffect(() => {
		if (!channelId) return;
		if (!favorite || isFavoriteDeleted) {
			const validChannelId = SelectedChannel.getValidatedFavoritesChannel();
			if (validChannelId) {
				Navigation.navigateToFavorites(validChannelId, undefined, 'push');
			} else {
				Navigation.navigateToFavorites(undefined, undefined, 'push');
			}
		}
	}, [channelId, favorite, isFavoriteDeleted]);
	if (showWelcomeScreen && !mobileLayout.enabled) {
		return (
			<div className={styles.guildLayoutContainer} data-flx="app.favorites-layout.guild-layout-container">
				<div className={styles.guildLayoutContent} data-flx="app.favorites-layout.guild-layout-content">
					<GuildSidebar
						header={<FavoritesGuildHeader data-flx="app.favorites-layout.favorites-guild-header" />}
						content={<FavoritesChannelListContent data-flx="app.favorites-layout.favorites-channel-list-content" />}
						data-flx="app.favorites-layout.guild-sidebar"
					/>
					<div className={styles.guildMainContentWithTopDragRegion} data-flx="app.favorites-layout.guild-main-content">
						<NativeDragRegion
							className={styles.guildMainTopDragRegion}
							data-flx="app.favorites-layout.empty-favorites-drag-region"
						/>
						<FavoritesWelcomeSection data-flx="app.favorites-layout.favorites-welcome-section" />
					</div>
				</div>
			</div>
		);
	}
	if (mobileLayout.enabled) {
		if (!channelId) {
			if (showWelcomeScreen) {
				return (
					<div className={styles.guildLayoutContainer} data-flx="app.favorites-layout.guild-layout-container--2">
						<div
							className={styles.guildMainContentWithTopDragRegion}
							data-flx="app.favorites-layout.guild-main-content--2"
						>
							<NativeDragRegion
								className={styles.guildMainTopDragRegion}
								data-flx="app.favorites-layout.empty-favorites-drag-region--2"
							/>
							<FavoritesWelcomeSection data-flx="app.favorites-layout.favorites-welcome-section--2" />
						</div>
					</div>
				);
			}
			return (
				<GuildSidebar
					header={<FavoritesGuildHeader data-flx="app.favorites-layout.favorites-guild-header--2" />}
					content={<FavoritesChannelListContent data-flx="app.favorites-layout.favorites-channel-list-content--2" />}
					data-flx="app.favorites-layout.guild-sidebar--2"
				/>
			);
		}
		return (
			<div className={styles.guildLayoutContainer} data-flx="app.favorites-layout.guild-layout-container--3">
				<div className={styles.guildMainContent} data-flx="app.favorites-layout.guild-main-content--3">
					{children}
				</div>
			</div>
		);
	}
	return (
		<div className={styles.guildLayoutContainer} data-flx="app.favorites-layout.guild-layout-container--4">
			<div className={styles.guildLayoutContent} data-flx="app.favorites-layout.guild-layout-content--2">
				<GuildSidebar
					header={<FavoritesGuildHeader data-flx="app.favorites-layout.favorites-guild-header--3" />}
					content={<FavoritesChannelListContent data-flx="app.favorites-layout.favorites-channel-list-content--3" />}
					data-flx="app.favorites-layout.guild-sidebar--3"
				/>
				<div className={styles.guildMainContent} data-flx="app.favorites-layout.guild-main-content--4">
					{children}
				</div>
			</div>
		</div>
	);
});
