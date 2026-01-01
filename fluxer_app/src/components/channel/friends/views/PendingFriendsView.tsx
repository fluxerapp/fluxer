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
import {RelationshipTypes} from '~/Constants';
import {Scroller} from '~/components/uikit/Scroller';
import type {RelationshipRecord} from '~/records/RelationshipRecord';
import UserStore from '~/stores/UserStore';
import * as NicknameUtils from '~/utils/NicknameUtils';
import {EmptyStateView} from '../EmptyStateView';
import {FriendListItem} from '../FriendListItem';
import {ListSection} from '../ListSection';
import styles from './PendingFriendsView.module.css';

interface PendingFriendsViewProps {
	relationships: Record<string, RelationshipRecord>;
	openProfile: (userId: string) => void;
	searchQuery: string;
}

export const PendingFriendsView: React.FC<PendingFriendsViewProps> = observer(
	({relationships, openProfile, searchQuery}) => {
		const {t} = useLingui();

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
			const user = UserStore.getUser(userId);
			const nickname = user ? NicknameUtils.getNickname(user) : '';
			const username = user?.username ?? '';
			return `${nickname} ${username}`.toLowerCase().includes(normalizedQuery);
		};

		const visibleIncoming = hasSearch
			? incomingRequests.filter((request) => matchesSearch(request.id))
			: incomingRequests;
		const visibleOutgoing = hasSearch
			? outgoingRequests.filter((request) => matchesSearch(request.id))
			: outgoingRequests;

		if (pendingCount === 0) {
			return (
				<EmptyStateView title={t`No pending requests`} subtitle={t`Hello? McFly? Anybody trying to add you yet?`} />
			);
		}

		if (hasSearch && visibleIncoming.length === 0 && visibleOutgoing.length === 0) {
			return (
				<EmptyStateView
					title={t`No pending requests match your search`}
					subtitle={t`Try another name or check your spelling.`}
				/>
			);
		}

		return (
			<Scroller className={styles.scroller} key="pending-friends-view-scroller">
				<div className={styles.pendingViewContainer}>
					{visibleIncoming.length > 0 && (
						<ListSection title={t`Incoming friend requests`} count={visibleIncoming.length} marginBottom={true}>
							{visibleIncoming.map((request) => (
								<FriendListItem
									key={request.id}
									userId={request.id}
									relationshipType={RelationshipTypes.INCOMING_REQUEST}
									openProfile={openProfile}
								/>
							))}
						</ListSection>
					)}

					{visibleOutgoing.length > 0 && (
						<ListSection title={t`Outgoing friend requests`} count={visibleOutgoing.length}>
							{visibleOutgoing.map((request) => (
								<FriendListItem
									key={request.id}
									userId={request.id}
									relationshipType={RelationshipTypes.OUTGOING_REQUEST}
									openProfile={openProfile}
								/>
							))}
						</ListSection>
					)}
				</div>
			</Scroller>
		);
	},
);
