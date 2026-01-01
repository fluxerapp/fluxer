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

import {Trans, useLingui} from '@lingui/react/macro';
import {
	CaretRightIcon,
	CircleNotchIcon,
	CrownIcon,
	DotsThreeVerticalIcon,
	MagnifyingGlassIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import debounce from 'lodash/debounce';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {Permissions} from '~/Constants';
import {Input} from '~/components/form/Input';
import {AddRoleButton} from '~/components/guild/RoleManagement';
import {LongPressable} from '~/components/LongPressable';
import {GuildMemberActionsSheet} from '~/components/modals/guildTabs/GuildMemberActionsSheet';
import {GuildMemberContextMenu} from '~/components/uikit/ContextMenu/GuildMemberContextMenu';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {usePressable} from '~/hooks/usePressable';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PermissionStore from '~/stores/PermissionStore';
import UserStore from '~/stores/UserStore';
import * as NicknameUtils from '~/utils/NicknameUtils';
import styles from './GuildMembersTab.module.css';

const MEMBERS_BATCH_SIZE = 100;
const SCROLL_LOAD_THRESHOLD = 200;
const SEARCH_DEBOUNCE_MS = 300;

function SkeletonMemberItem({showDivider}: {showDivider: boolean}) {
	return (
		<>
			<div className={styles.skeletonItem}>
				<div className={clsx(styles.skeletonAvatar, styles.skeleton)} />
				<div className={styles.skeletonInfo}>
					<div className={clsx(styles.skeletonName, styles.skeleton)} />
					<div className={clsx(styles.skeletonTag, styles.skeleton)} />
				</div>
			</div>
			{showDivider && <div className={styles.divider} />}
		</>
	);
}

interface MemberItemProps {
	member: GuildMemberRecord;
	user: UserRecord;
	guildId: string;
	isOwner: boolean;
	canManageRoles: boolean;
	isMobile: boolean;
	onContextMenu: (member: GuildMemberRecord, event: React.MouseEvent<HTMLElement>, fromButton?: boolean) => void;
	onLongPress: (member: GuildMemberRecord, user: UserRecord) => void;
	onTap: (member: GuildMemberRecord, user: UserRecord) => void;
	activeMenuMemberId: string | null;
}

const MemberItem: React.FC<MemberItemProps> = observer(
	({
		member,
		user,
		guildId,
		isOwner,
		canManageRoles,
		isMobile,
		onContextMenu,
		onLongPress,
		onTap,
		activeMenuMemberId,
	}) => {
		const {t} = useLingui();
		const isMenuActive = activeMenuMemberId === member.user.id;

		const {isPressed, pressableProps} = usePressable({disabled: !isMobile});

		const handleLongPress = React.useCallback(() => {
			onLongPress(member, user);
		}, [member, user, onLongPress]);

		const handleActivate = React.useCallback(() => {
			if (isMobile) {
				onTap(member, user);
			}
		}, [isMobile, member, onTap, user]);

		const handleContextMenu = React.useCallback(
			(e: React.MouseEvent<HTMLElement>) => {
				if (!isMobile) {
					e.preventDefault();
					onContextMenu(member, e);
				}
			},
			[isMobile, member, onContextMenu],
		);

		const handleKeyDown = React.useCallback(
			(e: React.KeyboardEvent<HTMLElement>) => {
				if (e.key === 'Enter' || e.key === ' ') {
					e.preventDefault();
					handleActivate();
				}
			},
			[handleActivate],
		);

		const sharedContent = (
			<>
				<div className={styles.memberMain}>
					<div className={styles.avatarWrapper}>
						<StatusAwareAvatar user={user} size={40} guildId={guildId} />
					</div>
					<div className={styles.memberInfo}>
						<div className={styles.nameRow}>
							<span className={styles.displayName}>{NicknameUtils.getNickname(user, guildId)}</span>
							{isOwner && (
								<Tooltip text={t`Community Owner`}>
									<CrownIcon className={styles.ownerIcon} weight="fill" />
								</Tooltip>
							)}
						</div>
						<span className={styles.tag}>{user.tag}</span>
					</div>
				</div>
				{isMobile ? (
					<CaretRightIcon className={styles.chevron} size={20} weight="bold" />
				) : (
					<div className={styles.memberActions}>
						{canManageRoles && <AddRoleButton guildId={guildId} userId={member.user.id} />}
						<button
							type="button"
							className={styles.moreButton}
							data-menu-active={isMenuActive ? 'true' : undefined}
							onClick={(event) => {
								event.stopPropagation();
								onContextMenu(member, event, true);
							}}
							onContextMenu={(event) => {
								event.preventDefault();
								event.stopPropagation();
								onContextMenu(member, event);
							}}
						>
							<DotsThreeVerticalIcon weight="bold" className={styles.moreButtonIcon} />
						</button>
					</div>
				)}
			</>
		);

		if (isMobile) {
			const content = (
				<button
					type="button"
					className={clsx(styles.memberItem, styles.memberItemInteractive, isPressed && styles.memberItemPressed)}
					onContextMenu={handleContextMenu}
					onClick={handleActivate}
					onKeyDown={handleKeyDown}
					{...pressableProps}
				>
					<div className={styles.memberMain}>
						<div className={styles.avatarWrapper}>
							<StatusAwareAvatar user={user} size={40} guildId={guildId} />
						</div>
						<div className={styles.memberInfo}>
							<div className={styles.nameRow}>
								<span className={styles.displayName}>{NicknameUtils.getNickname(user, guildId)}</span>
								{isOwner && (
									<Tooltip text={t`Community Owner`}>
										<CrownIcon className={styles.ownerIcon} weight="fill" />
									</Tooltip>
								)}
							</div>
							<span className={styles.tag}>{user.tag}</span>
						</div>
					</div>
					<CaretRightIcon className={styles.chevron} size={20} weight="bold" />
				</button>
			);

			return (
				<LongPressable onLongPress={handleLongPress} delay={500} className={styles.memberItemWrapper}>
					{content}
				</LongPressable>
			);
		}

		return (
			<div className={styles.memberItemWrapper}>
				{/* biome-ignore lint/a11y/noStaticElementInteractions: desktop row should only open context menu */}
				<div
					className={styles.memberItem}
					data-non-interactive="true"
					onContextMenu={(event) => onContextMenu(member, event, false)}
				>
					{sharedContent}
				</div>
			</div>
		);
	},
);

const GuildMembersTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {t} = useLingui();
	const guild = GuildStore.getGuild(guildId);
	const members = GuildMemberStore.getMembers(guildId);
	const isFullyLoaded = GuildMemberStore.isGuildFullyLoaded(guildId);
	const currentUserId = AuthenticationStore.currentUserId;
	const [searchQuery, setSearchQuery] = React.useState('');
	const [activeSheetMember, setActiveSheetMember] = React.useState<{
		member: GuildMemberRecord;
		user: UserRecord;
	} | null>(null);
	const [activeMenuMemberId, setActiveMenuMemberId] = React.useState<string | null>(null);
	const [isLoading, setIsLoading] = React.useState(false);
	const [isSearching, setIsSearching] = React.useState(false);
	const [hasMore, setHasMore] = React.useState(true);
	const [searchResults, setSearchResults] = React.useState<Array<GuildMemberRecord> | null>(null);
	const scrollerRef = React.useRef<ScrollerHandle>(null);
	const isLoadingRef = React.useRef(false);

	const canManageRoles = PermissionStore.can(Permissions.MANAGE_ROLES, {guildId});
	const isMobile = MobileLayoutStore.enabled;

	const needsLazyLoading = !isFullyLoaded;

	React.useEffect(() => {
		if (isFullyLoaded) {
			return;
		}

		setIsLoading(true);
		isLoadingRef.current = true;

		GuildMemberStore.fetchMembers(guildId, {limit: MEMBERS_BATCH_SIZE})
			.then((fetchedMembers) => {
				setHasMore(fetchedMembers.length === MEMBERS_BATCH_SIZE);
			})
			.catch(() => {
				setHasMore(false);
			})
			.finally(() => {
				setIsLoading(false);
				isLoadingRef.current = false;
			});
	}, [guildId, isFullyLoaded]);

	const debouncedSearch = React.useMemo(
		() =>
			debounce((query: string) => {
				if (!query || !needsLazyLoading) {
					setSearchResults(null);
					setIsSearching(false);
					return;
				}

				setIsSearching(true);
				GuildMemberStore.fetchMembers(guildId, {query, limit: MEMBERS_BATCH_SIZE})
					.then(setSearchResults)
					.catch(() => {
						setSearchResults([]);
					})
					.finally(() => {
						setIsSearching(false);
					});
			}, SEARCH_DEBOUNCE_MS),
		[guildId, needsLazyLoading],
	);

	React.useEffect(() => {
		return () => {
			debouncedSearch.cancel();
		};
	}, [debouncedSearch]);

	React.useEffect(() => {
		if (needsLazyLoading) {
			debouncedSearch(searchQuery);
		} else {
			setSearchResults(null);
		}
	}, [searchQuery, needsLazyLoading, debouncedSearch]);

	const loadMore = React.useCallback(() => {
		if (isLoadingRef.current || !hasMore || searchQuery || isFullyLoaded) {
			return;
		}

		setIsLoading(true);
		isLoadingRef.current = true;

		GuildMemberStore.fetchMembers(guildId, {limit: MEMBERS_BATCH_SIZE})
			.then((fetchedMembers) => {
				setHasMore(fetchedMembers.length === MEMBERS_BATCH_SIZE);
			})
			.catch(() => {
				setHasMore(false);
			})
			.finally(() => {
				setIsLoading(false);
				isLoadingRef.current = false;
			});
	}, [guildId, hasMore, searchQuery, isFullyLoaded]);

	const handleScroll = React.useCallback(() => {
		const scroller = scrollerRef.current;
		if (!scroller) {
			return;
		}

		const distanceFromBottom = scroller.getDistanceFromBottom();
		if (distanceFromBottom < SCROLL_LOAD_THRESHOLD) {
			loadMore();
		}
	}, [loadMore]);

	const membersWithUsers = React.useMemo(() => {
		const sourceMembers = searchResults ?? members;
		return sourceMembers
			.map((member) => {
				const user = UserStore.getUser(member.user.id);
				return user ? {member, user} : null;
			})
			.filter(
				(item): item is {member: GuildMemberRecord; user: UserRecord} => item !== null && item.user !== undefined,
			);
	}, [members, searchResults]);

	const filteredMembers = React.useMemo(() => {
		const filtered =
			searchQuery && !needsLazyLoading
				? matchSorter(membersWithUsers, searchQuery, {
						keys: [(item) => item.user.username, (item) => item.user.tag, (item) => item.member.nick ?? ''],
					})
				: membersWithUsers;

		return filtered.sort((a, b) => {
			const aIsCurrentUser = a.member.user.id === currentUserId;
			const bIsCurrentUser = b.member.user.id === currentUserId;
			if (aIsCurrentUser && !bIsCurrentUser) return -1;
			if (!aIsCurrentUser && bIsCurrentUser) return 1;
			return 0;
		});
	}, [membersWithUsers, searchQuery, currentUserId, needsLazyLoading]);

	const handleMemberContextMenu = React.useCallback(
		(member: GuildMemberRecord, event: React.MouseEvent<HTMLElement>, fromButton?: boolean) => {
			const user = UserStore.getUser(member.user.id);
			if (!user) return;
			if (fromButton) {
				setActiveMenuMemberId(member.user.id);
			} else {
				setActiveMenuMemberId(null);
			}

			ContextMenuActionCreators.openFromEvent(
				event,
				({onClose}) => (
					<GuildMemberContextMenu
						user={user}
						onClose={() => {
							onClose();
							setActiveMenuMemberId(null);
						}}
						guildId={guildId}
					/>
				),
				{
					onClose: () => setActiveMenuMemberId(null),
				},
			);
		},
		[guildId],
	);

	const handleMemberLongPress = React.useCallback((member: GuildMemberRecord, user: UserRecord) => {
		setActiveSheetMember({member, user});
	}, []);

	const handleMemberTap = React.useCallback((member: GuildMemberRecord, user: UserRecord) => {
		setActiveSheetMember({member, user});
	}, []);

	const handleCloseSheet = React.useCallback(() => {
		setActiveSheetMember(null);
	}, []);

	if (!guild) {
		return null;
	}

	const showInitialSkeleton = needsLazyLoading && members.length <= 1 && isLoading;
	const showLoadMoreIndicator = isLoading && !showInitialSkeleton && hasMore && !searchQuery;
	const showSearchIndicator = isSearching;
	const showEmptyState = filteredMembers.length === 0 && searchQuery && !isSearching;

	return (
		<div className={styles.container}>
			<div className={styles.header}>
				<h2 className={styles.title}>
					<Trans>Members</Trans>
				</h2>
				<p className={styles.subtitle}>
					<Trans>View and manage community members.</Trans>
				</p>
			</div>

			<div className={styles.searchContainer}>
				<Input
					type="text"
					placeholder={t`Search members...`}
					value={searchQuery}
					onChange={(e) => setSearchQuery(e.target.value)}
					leftIcon={<MagnifyingGlassIcon size={16} weight="bold" />}
					className={styles.searchInput}
				/>
			</div>

			{showEmptyState && (
				<div className={styles.emptyState}>
					<p className={styles.emptyStateText}>
						<Trans>No members found matching your search.</Trans>
					</p>
				</div>
			)}

			{showInitialSkeleton && (
				<div className={styles.scrollContainer}>
					<div className={styles.memberList}>
						<div className={styles.memberGroup}>
							{Array.from({length: 10}).map((_, index) => (
								<SkeletonMemberItem key={index} showDivider={index < 9} />
							))}
						</div>
					</div>
				</div>
			)}

			{!showInitialSkeleton && (filteredMembers.length > 0 || showSearchIndicator) && (
				<div className={styles.scrollContainer}>
					<Scroller
						ref={scrollerRef}
						fade={true}
						reserveScrollbarTrack={false}
						key="guild-members-scroller"
						onScroll={handleScroll}
					>
						<div className={styles.memberList}>
							<div className={styles.memberGroup}>
								{filteredMembers.map(({member, user}, index) => (
									<React.Fragment key={member.user.id}>
										<MemberItem
											member={member}
											user={user}
											guildId={guildId}
											isOwner={guild.ownerId === member.user.id}
											canManageRoles={canManageRoles}
											isMobile={isMobile}
											onContextMenu={handleMemberContextMenu}
											onLongPress={handleMemberLongPress}
											onTap={handleMemberTap}
											activeMenuMemberId={activeMenuMemberId}
										/>
										{(index < filteredMembers.length - 1 || showLoadMoreIndicator) && (
											<div className={styles.divider} />
										)}
									</React.Fragment>
								))}
								{showLoadMoreIndicator && (
									<div className={styles.loadingIndicator}>
										<CircleNotchIcon className={styles.loadingSpinner} weight="bold" />
									</div>
								)}
								{showSearchIndicator && filteredMembers.length === 0 && (
									<div className={styles.loadingIndicator}>
										<CircleNotchIcon className={styles.loadingSpinner} weight="bold" />
									</div>
								)}
							</div>
						</div>
					</Scroller>
				</div>
			)}

			{activeSheetMember && (
				<GuildMemberActionsSheet
					isOpen={true}
					onClose={handleCloseSheet}
					user={activeSheetMember.user}
					member={activeSheetMember.member}
					guildId={guildId}
				/>
			)}
		</div>
	);
});

export default GuildMembersTab;
