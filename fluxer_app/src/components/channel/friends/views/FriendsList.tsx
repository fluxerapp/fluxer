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

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {isOfflineStatus, RelationshipTypes} from '~/Constants';
import {Scroller} from '~/components/uikit/Scroller';
import PresenceStore from '~/stores/PresenceStore';
import RelationshipStore from '~/stores/RelationshipStore';
import UserStore from '~/stores/UserStore';
import * as NicknameUtils from '~/utils/NicknameUtils';
import {EmptyStateView} from '../EmptyStateView';
import {FriendListItem} from '../FriendListItem';
import {ListSection} from '../ListSection';
import styles from './FriendsList.module.css';

interface FriendsListProps {
	showOnlineOnly: boolean;
	openProfile: (userId: string) => void;
	searchQuery: string;
}

export const FriendsList: React.FC<FriendsListProps> = observer(({showOnlineOnly, openProfile, searchQuery}) => {
	const {t} = useLingui();
	const relationships = RelationshipStore.getRelationships();
	const friendIds = relationships
		.filter((relation) => relation.type === RelationshipTypes.FRIEND)
		.map((relation) => relation.id);

	const normalizedQuery = searchQuery.trim().toLowerCase();
	const hasSearch = normalizedQuery.length > 0;

	const matchesSearch = (userId: string) => {
		if (!hasSearch) {
			return true;
		}
		const user = UserStore.getUser(userId);
		const nickname = user ? NicknameUtils.getNickname(user) : '';
		const username = user?.username ?? '';
		return `${nickname} ${username}`.toLowerCase().includes(normalizedQuery);
	};

	const onlineFriendIds = friendIds.filter((id) => {
		const status = PresenceStore.getStatus(id);
		return !isOfflineStatus(status);
	});

	const tabFriendIds = showOnlineOnly ? onlineFriendIds : friendIds;
	const visibleFriends = hasSearch ? tabFriendIds.filter(matchesSearch) : tabFriendIds;

	if (friendIds.length === 0) {
		return (
			<EmptyStateView
				title={t`This friends list needs more power`}
				subtitle={t`Where we're going, we need more friends.`}
			/>
		);
	}

	if (showOnlineOnly && onlineFriendIds.length === 0 && !hasSearch) {
		return (
			<EmptyStateView
				title={t`Your friends are currently stuck in another timeline`}
				subtitle={t`When they hit 88mph, they'll appear right here.`}
			/>
		);
	}

	if (hasSearch && visibleFriends.length === 0) {
		return (
			<EmptyStateView title={t`No friends match your search`} subtitle={t`Try another name or check your spelling.`} />
		);
	}

	return (
		<Scroller className={styles.scroller} key="friends-list-view-scroller">
			<div className={styles.friendsListContainer}>
				<ListSection title={showOnlineOnly ? t`Online` : t`All friends`} count={visibleFriends.length}>
					{visibleFriends.map((userId) => (
						<FriendListItem
							key={userId}
							userId={userId}
							relationshipType={RelationshipTypes.FRIEND}
							openProfile={openProfile}
						/>
					))}
				</ListSection>
			</div>
		</Scroller>
	);
});
