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
import {ME} from '~/Constants';
import {DMChannelView} from '~/components/channel/channel-view/DMChannelView';
import {DMFriendsView} from '~/components/channel/dm/DMFriendsView';
import {DMList} from '~/components/channel/dm/DMList';
import {RecentMentionsPage} from '~/components/pages/RecentMentionsPage';
import {SavedMessagesPage} from '~/components/pages/SavedMessagesPage';
import {useLocation, useParams} from '~/lib/router';
import {Routes} from '~/Routes';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import styles from './DMLayout.module.css';

export const DMLayout = observer(({children}: {children?: React.ReactNode}) => {
	const {channelId, messageId} = useParams() as {channelId?: string; messageId?: string};
	const location = useLocation();
	const mobileLayout = MobileLayoutStore;

	React.useEffect(() => {
		if (Routes.isDMRoute(location.pathname) || Routes.isFavoritesRoute(location.pathname)) {
			NavigationActionCreators.deselectGuild();
		}
	}, [location.pathname]);

	React.useEffect(() => {
		if (Routes.isDMRoute(location.pathname) && channelId) {
			NavigationActionCreators.selectChannel(ME, channelId, messageId);
		}
	}, [channelId, messageId, location.pathname]);

	const renderContent = () => {
		if (location.pathname === Routes.BOOKMARKS) {
			return <SavedMessagesPage />;
		}
		if (location.pathname === Routes.MENTIONS) {
			return <RecentMentionsPage />;
		}
		if (channelId) {
			return <DMChannelView channelId={channelId} />;
		}
		if (children) {
			return children;
		}
		return <DMFriendsView />;
	};

	if (mobileLayout.enabled) {
		if (!channelId && !children) {
			return (
				<div className={styles.dmListColumn}>
					<DMList />
				</div>
			);
		}
		return (
			<div className={styles.contentColumn}>
				<div className={styles.contentInner}>{renderContent()}</div>
			</div>
		);
	}

	return (
		<div className={styles.dmLayoutContainer}>
			<div className={styles.dmListColumn}>
				<DMList />
			</div>
			<div className={styles.contentColumn}>
				<div className={styles.contentInner}>{renderContent()}</div>
			</div>
		</div>
	);
});
