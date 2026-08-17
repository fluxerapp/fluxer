// SPDX-License-Identifier: AGPL-3.0-or-later

import {LongPressable} from '@app/features/app/components/LongPressable';
import {getStatusTypeLabel} from '@app/features/app/constants/AppConstants';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import Presence from '@app/features/presence/state/Presence';
import Relationships from '@app/features/relationship/state/Relationships';
import styles from '@app/features/relationship/utils/FriendsListUtils.module.css';
import * as QuickSwitcherCommands from '@app/features/search/commands/QuickSwitcherCommands';
import * as LayoutCommands from '@app/features/ui/commands/LayoutCommands';
import {Scroller} from '@app/features/ui/components/Scroller';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretRightIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const NO_FRIENDS_YET_DESCRIPTOR = msg({
	message: 'No friends yet',
	comment: 'Empty-state text in the friends list utils helper.',
});
const ADD_SOME_FRIENDS_TO_SEE_THEM_HERE_DESCRIPTOR = msg({
	message: 'Add a friend to get started.',
	comment: 'Empty-state hint in the friends list when the user has no friends yet.',
});
const NO_FRIENDS_MATCH_YOUR_SEARCH_DESCRIPTOR = msg({
	message: 'No friends match that search',
	comment: 'Empty-state text in the friends list utils helper.',
});
const TRY_ANOTHER_NAME_OR_CHECK_YOUR_SPELLING_DESCRIPTOR = msg({
	message: 'Try a different name.',
	comment: 'Empty-state hint in the friends list when a search returns no results.',
});

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
	return useMemo(() => {
		const filtered = friendIds.filter((userId) => {
			const user = Users.getUser(userId);
			if (!user) return false;
			if (!searchQuery) return true;
			const nickname = NicknameUtils.getNickname(user, null).toLowerCase();
			return nickname.includes(searchQuery.toLowerCase());
		});
		const groups: Record<string, Array<string>> = {};
		for (const userId of filtered) {
			const user = Users.getUser(userId);
			if (!user) continue;
			const firstLetter = NicknameUtils.getNickname(user, null)[0].toUpperCase();
			if (!groups[firstLetter]) {
				groups[firstLetter] = [];
			}
			groups[firstLetter].push(userId);
		}
		for (const letter of Object.keys(groups)) {
			groups[letter].sort((a, b) => {
				const userA = Users.getUser(a);
				const userB = Users.getUser(b);
				if (!userA || !userB) return 0;
				return NicknameUtils.getNickname(userA, null).localeCompare(NicknameUtils.getNickname(userB, null));
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
	const {i18n} = useLingui();
	const user = Users.getUser(userId);
	const status = Presence.getStatus(userId);
	const handleClick = useCallback(async () => {
		try {
			await PrivateChannelCommands.openDMChannel(userId);
			if (MobileLayout.isMobileLayout()) {
				LayoutCommands.updateMobileLayoutState(false, true);
			}
			QuickSwitcherCommands.hide();
		} catch {}
	}, [userId]);
	const handleLongPress = useCallback(() => {
		UserProfileCommands.openUserProfile(userId);
	}, [userId]);
	if (!user) return null;
	const statusLabel = getStatusTypeLabel(i18n, status);
	return (
		<LongPressable
			className={styles.friendItemWrapper}
			onLongPress={handleLongPress}
			data-flx="relationship.friends-list-utils.friend-item.friend-item-wrapper"
		>
			<FocusRing offset={-2} enabled={false} data-flx="relationship.friends-list-utils.friend-item.focus-ring">
				<button
					type="button"
					className={styles.friendItem}
					onClick={handleClick}
					data-flx="relationship.friends-list-utils.friend-item.friend-item.click.button"
				>
					<div
						className={styles.friendItemContent}
						data-flx="relationship.friends-list-utils.friend-item.friend-item-content"
					>
						<div className={styles.avatar} data-flx="relationship.friends-list-utils.friend-item.avatar">
							<StatusAwareAvatar
								user={user}
								size={32}
								data-flx="relationship.friends-list-utils.friend-item.status-aware-avatar"
							/>
						</div>
						<div
							className={styles.friendItemText}
							data-flx="relationship.friends-list-utils.friend-item.friend-item-text"
						>
							<div
								className={styles.friendItemName}
								data-flx="relationship.friends-list-utils.friend-item.friend-item-name"
							>
								{NicknameUtils.getNickname(user, null)}
							</div>
							{statusLabel && (
								<div
									className={styles.friendItemStatus}
									data-flx="relationship.friends-list-utils.friend-item.friend-item-status"
								>
									{statusLabel}
								</div>
							)}
						</div>
					</div>
					<CaretRightIcon
						weight="bold"
						className={styles.friendItemCaret}
						data-flx="relationship.friends-list-utils.friend-item.friend-item-caret"
					/>
				</button>
			</FocusRing>
		</LongPressable>
	);
});
export const FriendsListContent: React.FC<FriendsListContentProps> = observer(
	({className, searchQuery, onTotalCountChange, variant}) => {
		const {i18n} = useLingui();
		const [internalSearchQuery, _setInternalSearchQuery] = useState('');
		const query = searchQuery ?? internalSearchQuery;
		const relationships = Relationships.getRelationships();
		const friendIds = relationships
			.filter((relation) => relation.type === RelationshipTypes.FRIEND)
			.map((relation) => relation.id);
		const groupedFriends = useFriendGroups(friendIds, query);
		const totalCount = groupedFriends.reduce((sum, group) => sum + group.friendIds.length, 0);
		useEffect(() => {
			onTotalCountChange?.(totalCount);
		}, [onTotalCountChange, totalCount]);
		const containerClassName = clsx(styles.container, variant === 'embedded' && styles.variantEmbedded, className);
		if (friendIds.length === 0) {
			return (
				<div className={containerClassName} data-flx="relationship.friends-list-utils.friends-list-content.div">
					<div
						className={styles.emptyState}
						data-flx="relationship.friends-list-utils.friends-list-content.empty-state"
					>
						<div
							className={styles.emptyStateTitle}
							data-flx="relationship.friends-list-utils.friends-list-content.empty-state-title"
						>
							{i18n._(NO_FRIENDS_YET_DESCRIPTOR)}
						</div>
						<div
							className={styles.emptyStateHint}
							data-flx="relationship.friends-list-utils.friends-list-content.empty-state-hint"
						>
							{i18n._(ADD_SOME_FRIENDS_TO_SEE_THEM_HERE_DESCRIPTOR)}
						</div>
					</div>
				</div>
			);
		}
		if (query && totalCount === 0) {
			return (
				<div className={containerClassName} data-flx="relationship.friends-list-utils.friends-list-content.div--2">
					<div
						className={styles.emptyState}
						data-flx="relationship.friends-list-utils.friends-list-content.empty-state--2"
					>
						<div
							className={styles.emptyStateTitle}
							data-flx="relationship.friends-list-utils.friends-list-content.empty-state-title--2"
						>
							{i18n._(NO_FRIENDS_MATCH_YOUR_SEARCH_DESCRIPTOR)}
						</div>
						<div
							className={styles.emptyStateHint}
							data-flx="relationship.friends-list-utils.friends-list-content.empty-state-hint--2"
						>
							{i18n._(TRY_ANOTHER_NAME_OR_CHECK_YOUR_SPELLING_DESCRIPTOR)}
						</div>
					</div>
				</div>
			);
		}
		return (
			<div className={containerClassName} data-flx="relationship.friends-list-utils.friends-list-content.div--3">
				<Scroller
					className={styles.scroller}
					key="friends-list-content-scroller"
					data-flx="relationship.friends-list-utils.friends-list-content.scroller"
				>
					<div
						className={styles.scrollContent}
						data-flx="relationship.friends-list-utils.friends-list-content.scroll-content"
					>
						{groupedFriends.map((group) => (
							<div
								key={group.letter}
								className={styles.section}
								data-flx="relationship.friends-list-utils.friends-list-content.section"
							>
								<div
									className={styles.sectionHeader}
									data-flx="relationship.friends-list-utils.friends-list-content.section-header"
								>
									{group.letter}
								</div>
								<div
									className={styles.sectionList}
									data-flx="relationship.friends-list-utils.friends-list-content.section-list"
								>
									{group.friendIds.map((friendId) => (
										<FriendItem
											key={friendId}
											userId={friendId}
											data-flx="relationship.friends-list-utils.friends-list-content.friend-item"
										/>
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
