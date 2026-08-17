// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	reportSkeletonFriendsRowCount,
	SkeletonFriendsTab,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {EmptyStateView} from '@app/features/channel/components/friends/EmptyStateView';
import {FriendListItem} from '@app/features/channel/components/friends/FriendListItem';
import {ListSection} from '@app/features/channel/components/friends/FriendsListSection';
import styles from '@app/features/channel/components/friends/views/FriendsList.module.css';
import {ONLINE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Presence from '@app/features/presence/state/Presence';
import Relationships from '@app/features/relationship/state/Relationships';
import {Scroller} from '@app/features/ui/components/Scroller';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {isOfflineStatus} from '@fluxer/constants/src/StatusConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';

const THIS_FRIENDS_LIST_NEEDS_MORE_POWER_DESCRIPTOR = msg({
	message: 'This friends list needs more power',
	comment: 'Empty state title in the friends list when the user has no friends. Tone can be friendly and playful.',
});
const WHERE_WE_RE_GOING_WE_NEED_MORE_FRIENDS_DESCRIPTOR = msg({
	message: "Where we're going, we need more friends.",
	comment: 'Empty state body in the friends list when the user has no friends. Tone can be friendly and playful.',
});
const YOUR_FRIENDS_ARE_CURRENTLY_STUCK_IN_ANOTHER_TIMELINE_DESCRIPTOR = msg({
	message: 'Your friends are stuck in another timeline',
	comment: 'Empty state title in the friends list Online tab when no friends are online. Tone can be friendly.',
});
const WHEN_THEY_COME_ONLINE_THEY_LL_APPEAR_RIGHT_DESCRIPTOR = msg({
	message: "When they come online, they'll appear right here.",
	comment: 'Empty state body in the friends list Online tab.',
});
const NO_FRIENDS_MATCH_YOUR_SEARCH_DESCRIPTOR = msg({
	message: 'No friends match that search',
	comment: 'Empty state title in the friends list when the search query returns no results.',
});
const TRY_ANOTHER_NAME_OR_CHECK_YOUR_SPELLING_DESCRIPTOR = msg({
	message: 'Try another name or check your spelling.',
	comment: 'Empty state body in the friends list when the search query returns no results.',
});
const ALL_FRIENDS_DESCRIPTOR = msg({
	message: 'All friends',
	comment: 'Tab label in the friends list showing all friends.',
});

interface FriendsListProps {
	showOnlineOnly: boolean;
	openProfile: (userId: string) => void;
	searchQuery: string;
}

export const FriendsList: React.FC<FriendsListProps> = observer(({showOnlineOnly, openProfile, searchQuery}) => {
	const {i18n} = useLingui();
	const relationships = Relationships.getRelationships();
	const friendIds = relationships
		.filter((relation) => relation.type === RelationshipTypes.FRIEND)
		.map((relation) => relation.id);
	const normalizedQuery = searchQuery.trim().toLowerCase();
	const hasSearch = normalizedQuery.length > 0;
	const matchesSearch = (userId: string) => {
		if (!hasSearch) {
			return true;
		}
		const user = Users.getUser(userId);
		const nickname = user ? NicknameUtils.getNickname(user, null) : '';
		const username = user?.username ?? '';
		return `${nickname} ${username}`.toLowerCase().includes(normalizedQuery);
	};
	const onlineFriendIds = friendIds.filter((id) => {
		const status = Presence.getStatus(id);
		return !isOfflineStatus(status);
	});
	const tabFriendIds = showOnlineOnly ? onlineFriendIds : friendIds;
	const filteredFriends = hasSearch ? tabFriendIds.filter(matchesSearch) : tabFriendIds;
	const visibleFriends = [...filteredFriends].sort((a, b) => {
		const userA = Users.getUser(a);
		const userB = Users.getUser(b);
		if (!userA || !userB) return 0;
		return NicknameUtils.getNickname(userA, null).localeCompare(NicknameUtils.getNickname(userB, null));
	});
	const reportedTab = showOnlineOnly ? SkeletonFriendsTab.ONLINE : SkeletonFriendsTab.ALL;
	useEffect(() => {
		if (hasSearch) {
			return;
		}
		reportSkeletonFriendsRowCount(reportedTab, visibleFriends.length);
	}, [hasSearch, reportedTab, visibleFriends.length]);
	if (friendIds.length === 0) {
		return (
			<EmptyStateView
				title={i18n._(THIS_FRIENDS_LIST_NEEDS_MORE_POWER_DESCRIPTOR)}
				subtitle={i18n._(WHERE_WE_RE_GOING_WE_NEED_MORE_FRIENDS_DESCRIPTOR)}
				data-flx="channel.friends.views.friends-list.empty-state-view"
			/>
		);
	}
	if (showOnlineOnly && onlineFriendIds.length === 0 && !hasSearch) {
		return (
			<EmptyStateView
				title={i18n._(YOUR_FRIENDS_ARE_CURRENTLY_STUCK_IN_ANOTHER_TIMELINE_DESCRIPTOR)}
				subtitle={i18n._(WHEN_THEY_COME_ONLINE_THEY_LL_APPEAR_RIGHT_DESCRIPTOR)}
				data-flx="channel.friends.views.friends-list.empty-state-view--2"
			/>
		);
	}
	if (hasSearch && visibleFriends.length === 0) {
		return (
			<EmptyStateView
				title={i18n._(NO_FRIENDS_MATCH_YOUR_SEARCH_DESCRIPTOR)}
				subtitle={i18n._(TRY_ANOTHER_NAME_OR_CHECK_YOUR_SPELLING_DESCRIPTOR)}
				data-flx="channel.friends.views.friends-list.empty-state-view--3"
			/>
		);
	}
	return (
		<Scroller
			className={styles.scroller}
			key="friends-list-view-scroller"
			data-flx="channel.friends.views.friends-list.scroller"
		>
			<div className={styles.friendsListContainer} data-flx="channel.friends.views.friends-list.friends-list-container">
				<ListSection
					title={showOnlineOnly ? i18n._(ONLINE_DESCRIPTOR) : i18n._(ALL_FRIENDS_DESCRIPTOR)}
					count={visibleFriends.length}
					data-flx="channel.friends.views.friends-list.list-section"
				>
					{visibleFriends.map((userId) => (
						<FriendListItem
							key={userId}
							userId={userId}
							relationshipType={RelationshipTypes.FRIEND}
							openProfile={openProfile}
							data-flx="channel.friends.views.friends-list.friend-list-item"
						/>
					))}
				</ListSection>
			</div>
		</Scroller>
	);
});
