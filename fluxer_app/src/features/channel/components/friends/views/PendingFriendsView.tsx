// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	reportSkeletonFriendsRowCount,
	SkeletonFriendsTab,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {EmptyStateView} from '@app/features/channel/components/friends/EmptyStateView';
import {FriendListItem} from '@app/features/channel/components/friends/FriendListItem';
import {ListSection} from '@app/features/channel/components/friends/FriendsListSection';
import styles from '@app/features/channel/components/friends/views/PendingFriendsView.module.css';
import type {Relationship} from '@app/features/relationship/models/Relationship';
import {Scroller} from '@app/features/ui/components/Scroller';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';

const NO_PENDING_REQUESTS_DESCRIPTOR = msg({
	message: 'No pending requests',
	comment: 'Empty-state text in the channel and chat pending friends view.',
});
const INCOMING_AND_OUTGOING_FRIEND_REQUESTS_WILL_SHOW_UP_DESCRIPTOR = msg({
	message: 'Pending friend requests show up here.',
	comment: 'Description text in the channel and chat pending friends view.',
});
const NO_PENDING_REQUESTS_MATCH_YOUR_SEARCH_DESCRIPTOR = msg({
	message: 'No pending requests match that search',
	comment: 'Empty-state text in the channel and chat pending friends view.',
});
const TRY_ANOTHER_NAME_OR_CHECK_YOUR_SPELLING_DESCRIPTOR = msg({
	message: 'Try a different name.',
	comment: 'Description text in the channel and chat pending friends view.',
});
const INCOMING_FRIEND_REQUESTS_DESCRIPTOR = msg({
	message: 'Incoming friend requests',
	comment: 'Short label in the channel and chat pending friends view. Keep it concise.',
});
const OUTGOING_FRIEND_REQUESTS_DESCRIPTOR = msg({
	message: 'Outgoing friend requests',
	comment: 'Short label in the channel and chat pending friends view. Keep it concise.',
});

interface PendingFriendsViewProps {
	relationships: Record<string, Relationship>;
	openProfile: (userId: string) => void;
	searchQuery: string;
}

export const PendingFriendsView: React.FC<PendingFriendsViewProps> = observer(
	({relationships, openProfile, searchQuery}) => {
		const {i18n} = useLingui();
		const allRelationships = Object.values(relationships);
		const incomingRequests = allRelationships.filter(
			(relation) => relation.type === RelationshipTypes.INCOMING_REQUEST,
		);
		const outgoingRequests = allRelationships.filter(
			(relation) => relation.type === RelationshipTypes.OUTGOING_REQUEST,
		);
		const pendingCount = incomingRequests.length + outgoingRequests.length;
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
		const sortByDisplayName = (requests: Array<Relationship>) =>
			[...requests].sort((a, b) => {
				const userA = Users.getUser(a.id);
				const userB = Users.getUser(b.id);
				if (!userA || !userB) return 0;
				return NicknameUtils.getNickname(userA, null).localeCompare(NicknameUtils.getNickname(userB, null));
			});
		const visibleIncoming = sortByDisplayName(
			hasSearch ? incomingRequests.filter((request) => matchesSearch(request.id)) : incomingRequests,
		);
		const visibleOutgoing = sortByDisplayName(
			hasSearch ? outgoingRequests.filter((request) => matchesSearch(request.id)) : outgoingRequests,
		);
		const visibleRowCount = visibleIncoming.length + visibleOutgoing.length;
		useEffect(() => {
			if (hasSearch) {
				return;
			}
			reportSkeletonFriendsRowCount(SkeletonFriendsTab.PENDING, visibleRowCount, visibleIncoming.length);
		}, [hasSearch, visibleRowCount, visibleIncoming.length]);
		if (pendingCount === 0) {
			return (
				<EmptyStateView
					title={i18n._(NO_PENDING_REQUESTS_DESCRIPTOR)}
					subtitle={i18n._(INCOMING_AND_OUTGOING_FRIEND_REQUESTS_WILL_SHOW_UP_DESCRIPTOR)}
					data-flx="channel.friends.views.pending-friends-view.empty-state-view"
				/>
			);
		}
		if (hasSearch && visibleIncoming.length === 0 && visibleOutgoing.length === 0) {
			return (
				<EmptyStateView
					title={i18n._(NO_PENDING_REQUESTS_MATCH_YOUR_SEARCH_DESCRIPTOR)}
					subtitle={i18n._(TRY_ANOTHER_NAME_OR_CHECK_YOUR_SPELLING_DESCRIPTOR)}
					data-flx="channel.friends.views.pending-friends-view.empty-state-view--2"
				/>
			);
		}
		return (
			<Scroller
				className={styles.scroller}
				key="pending-friends-view-scroller"
				data-flx="channel.friends.views.pending-friends-view.scroller"
			>
				<div
					className={styles.pendingViewContainer}
					data-flx="channel.friends.views.pending-friends-view.pending-view-container"
				>
					{visibleIncoming.length > 0 && (
						<ListSection
							title={i18n._(INCOMING_FRIEND_REQUESTS_DESCRIPTOR)}
							count={visibleIncoming.length}
							marginBottom={true}
							data-flx="channel.friends.views.pending-friends-view.list-section"
						>
							{visibleIncoming.map((request) => (
								<FriendListItem
									key={request.id}
									userId={request.id}
									relationshipType={RelationshipTypes.INCOMING_REQUEST}
									openProfile={openProfile}
									data-flx="channel.friends.views.pending-friends-view.friend-list-item"
								/>
							))}
						</ListSection>
					)}
					{visibleOutgoing.length > 0 && (
						<ListSection
							title={i18n._(OUTGOING_FRIEND_REQUESTS_DESCRIPTOR)}
							count={visibleOutgoing.length}
							data-flx="channel.friends.views.pending-friends-view.list-section--2"
						>
							{visibleOutgoing.map((request) => (
								<FriendListItem
									key={request.id}
									userId={request.id}
									relationshipType={RelationshipTypes.OUTGOING_REQUEST}
									openProfile={openProfile}
									data-flx="channel.friends.views.pending-friends-view.friend-list-item--2"
								/>
							))}
						</ListSection>
					)}
				</div>
			</Scroller>
		);
	},
);
