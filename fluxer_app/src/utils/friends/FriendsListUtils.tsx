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

import {msg} from '@lingui/core/macro';
import {CaretRightIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as LayoutActionCreators from '~/actions/LayoutActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as QuickSwitcherActionCreators from '~/actions/QuickSwitcherActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {getStatusTypeLabel, RelationshipTypes} from '~/Constants';
import {LongPressable} from '~/components/LongPressable';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Scroller} from '~/components/uikit/Scroller';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import i18n from '~/i18n';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PresenceStore from '~/stores/PresenceStore';
import RelationshipStore from '~/stores/RelationshipStore';
import UserStore from '~/stores/UserStore';
import * as NicknameUtils from '~/utils/NicknameUtils';
import styles from './FriendsListUtils.module.css';

interface FriendGroup {
	letter: string;
	friendIds: Array<string>;
}

interface FriendsListContentProps {
	variant?: 'sheet' | 'embedded';
	onBack?: () => void;
	className?: string;
	searchQuery?: string;
	onSearchChange?: (value: string) => void;
	showSearch?: boolean;
	showHeader?: boolean;
	onTotalCountChange?: (count: number) => void;
}

const useFriendGroups = (friendIds: Array<string>, searchQuery: string) => {
	return React.useMemo(() => {
		const filtered = friendIds.filter((userId) => {
			const user = UserStore.getUser(userId);
			if (!user) return false;
			if (!searchQuery) return true;

			const nickname = NicknameUtils.getNickname(user).toLowerCase();
			return nickname.includes(searchQuery.toLowerCase());
		});

		const groups: Record<string, Array<string>> = {};

		for (const userId of filtered) {
			const user = UserStore.getUser(userId);
			if (!user) continue;

			const firstLetter = NicknameUtils.getNickname(user)[0].toUpperCase();
			if (!groups[firstLetter]) {
				groups[firstLetter] = [];
			}
			groups[firstLetter].push(userId);
		}

		for (const letter of Object.keys(groups)) {
			groups[letter].sort((a, b) => {
				const userA = UserStore.getUser(a);
				const userB = UserStore.getUser(b);
				if (!userA || !userB) return 0;
				return NicknameUtils.getNickname(userA).localeCompare(NicknameUtils.getNickname(userB));
			});
		}

		const groupArray: Array<FriendGroup> = Object.keys(groups)
			.sort()
			.map((letter) => ({
				letter,
				friendIds: groups[letter],
			}));

		return groupArray;
	}, [friendIds, searchQuery]);
};

const FriendItem = observer(({userId}: {userId: string}) => {
	const user = UserStore.getUser(userId);
	const status = PresenceStore.getStatus(userId);

	const handleClick = React.useCallback(async () => {
		try {
			await PrivateChannelActionCreators.openDMChannel(userId);
			if (MobileLayoutStore.isMobileLayout()) {
				LayoutActionCreators.updateMobileLayoutState(false, true);
			}
			QuickSwitcherActionCreators.hide();
		} catch {}
	}, [userId]);

	const handleLongPress = React.useCallback(() => {
		UserProfileActionCreators.openUserProfile(userId);
	}, [userId]);

	if (!user) return null;

	const statusLabel = getStatusTypeLabel(i18n, status);

	return (
		<LongPressable className={styles.friendItemWrapper} onLongPress={handleLongPress}>
			<FocusRing offset={-2} enabled={false}>
				<button type="button" className={styles.friendItem} onClick={handleClick}>
					<div className={styles.friendItemContent}>
						<div className={styles.avatar}>
							<StatusAwareAvatar user={user} size={32} />
						</div>
						<div className={styles.friendItemText}>
							<div className={styles.friendItemName}>{NicknameUtils.getNickname(user)}</div>
							{statusLabel && <div className={styles.friendItemStatus}>{statusLabel}</div>}
						</div>
					</div>
					<CaretRightIcon weight="bold" className={styles.friendItemCaret} />
				</button>
			</FocusRing>
		</LongPressable>
	);
});

export const FriendsListContent: React.FC<FriendsListContentProps> = observer(
	({className, searchQuery, onTotalCountChange, variant}) => {
		const [internalSearchQuery, _setInternalSearchQuery] = React.useState('');
		const query = searchQuery ?? internalSearchQuery;
		const relationships = RelationshipStore.getRelationships();
		const friendIds = relationships
			.filter((relation) => relation.type === RelationshipTypes.FRIEND)
			.map((relation) => relation.id);

		const groupedFriends = useFriendGroups(friendIds, query);
		const totalCount = groupedFriends.reduce((sum, group) => sum + group.friendIds.length, 0);

		React.useEffect(() => {
			onTotalCountChange?.(totalCount);
		}, [onTotalCountChange, totalCount]);

		const containerClassName = clsx(styles.container, variant === 'embedded' && styles.variantEmbedded, className);

		if (friendIds.length === 0) {
			return (
				<div className={containerClassName}>
					<div className={styles.emptyState}>
						<div className={styles.emptyStateTitle}>{i18n._(msg`No friends yet`)}</div>
						<div className={styles.emptyStateHint}>{i18n._(msg`Add some friends to see them here.`)}</div>
					</div>
				</div>
			);
		}

		if (query && totalCount === 0) {
			return (
				<div className={containerClassName}>
					<div className={styles.emptyState}>
						<div className={styles.emptyStateTitle}>{i18n._(msg`No friends match your search`)}</div>
						<div className={styles.emptyStateHint}>{i18n._(msg`Try another name or check your spelling.`)}</div>
					</div>
				</div>
			);
		}

		return (
			<div className={containerClassName}>
				<Scroller className={styles.scroller} key="friends-list-content-scroller">
					<div className={styles.scrollContent}>
						{groupedFriends.map((group) => (
							<div key={group.letter} className={styles.section}>
								<div className={styles.sectionHeader}>{group.letter}</div>
								<div className={styles.sectionList}>
									{group.friendIds.map((friendId) => (
										<FriendItem key={friendId} userId={friendId} />
									))}
								</div>
							</div>
						))}
					</div>
				</Scroller>
			</div>
		);
	},
);

export type {FriendsListContentProps};
