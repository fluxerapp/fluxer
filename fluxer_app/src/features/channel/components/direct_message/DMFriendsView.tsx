// SPDX-License-Identifier: AGPL-3.0-or-later

import {reportSkeletonFriendsHeaderLayout} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {measureSkeletonWidthPx, useSkeletonLayoutReport} from '@app/features/app/hooks/useSkeletonLayoutMemoryCapture';
import {ActiveNowSidebar} from '@app/features/channel/components/active_now/ActiveNowSidebar';
import {ChannelHeader} from '@app/features/channel/components/ChannelHeader';
import {AddFriendView} from '@app/features/channel/components/direct_message/AddFriendView';
import styles from '@app/features/channel/components/direct_message/DMFriendsView.module.css';
import type {FriendsTab} from '@app/features/channel/components/friends/FriendsTypes';
import {FriendsList} from '@app/features/channel/components/friends/views/FriendsList';
import {PendingFriendsView} from '@app/features/channel/components/friends/views/PendingFriendsView';
import {ONLINE_DESCRIPTOR, SEARCH_FRIENDS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Relationship} from '@app/features/relationship/models/Relationship';
import FriendsTabState from '@app/features/relationship/state/FriendsTab';
import Relationships from '@app/features/relationship/state/Relationships';
import {ADD_FRIEND_DESCRIPTOR} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import {Input} from '@app/features/ui/components/form/FormInput';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {getNextTabIndex, getTabNavigationDirection} from '@app/features/ui/tabs/TabKeyboardNavigation';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, UsersThreeIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const MY_FRIENDS_DESCRIPTOR = msg({
	message: 'My friends',
	comment: 'Page title for the DM-list friends view.',
});
const FRIENDS_SECTIONS_DESCRIPTOR = msg({
	message: 'Friends sections',
	comment: 'Accessible label for the friends-view section tablist.',
});
const ALL_DESCRIPTOR = msg({
	message: 'All',
	comment: 'Tab label in the DM friends view showing all friends.',
});
const PENDING_DESCRIPTOR = msg({
	message: 'Pending',
	comment: 'Tab label in the DM friends view showing pending friend requests.',
});
const SEARCH_ONLINE_FRIENDS_DESCRIPTOR = msg({
	message: 'Search online friends',
	comment: 'Placeholder text in the search input of the Online tab in the DM friends view.',
});
const SEARCH_PENDING_REQUESTS_DESCRIPTOR = msg({
	message: 'Search pending requests',
	comment: 'Placeholder text in the search input of the Pending tab in the DM friends view.',
});
const FRIENDS_TAB_ORDER: ReadonlyArray<FriendsTab> = ['online', 'all', 'pending', 'add'];

interface TabButtonProps extends Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'onClick'> {
	tab: FriendsTab;
	activeTab: FriendsTab;
	onClick: (tab: FriendsTab) => void;
	label: string;
	panelId: string;
	badge?: number;
	primary?: boolean;
}

const TabButton = observer(
	React.forwardRef<HTMLButtonElement, TabButtonProps>(
		({tab, activeTab, onClick, label, panelId, badge, primary, ...props}, ref) => {
			const isSelected = activeTab === tab;
			const isVisuallyActive = isSelected && !primary;
			return (
				<FocusRing within offset={-2} data-flx="channel.direct-message.dm-friends-view.focus-ring">
					<button
						ref={ref}
						id={`dm-friends-tab-${tab}`}
						type="button"
						role="tab"
						aria-selected={isSelected}
						aria-controls={panelId}
						tabIndex={isSelected ? 0 : -1}
						className={clsx(styles.tabButton, {
							[styles.active]: isVisuallyActive,
							[styles.primary]: primary,
						})}
						onClick={() => onClick(tab)}
						data-flx="channel.direct-message.dm-friends-view.tab-button.click"
						{...props}
					>
						<div className={styles.tabContent} data-flx="channel.direct-message.dm-friends-view.tab-content">
							{label}
							{badge !== undefined && badge > 0 && (
								<MentionBadge mentionCount={badge} data-flx="channel.direct-message.dm-friends-view.mention-badge" />
							)}
						</div>
					</button>
				</FocusRing>
			);
		},
	),
);
export const DMFriendsView: React.FC = observer(() => {
	const {i18n} = useLingui();
	const [activeTab, setActiveTab] = useState<FriendsTab>('online');
	const mobileLayout = MobileLayout;
	const relationships = Relationships.getRelationships();
	const pendingCount = relationships.filter((relation) => relation.type === RelationshipTypes.INCOMING_REQUEST).length;
	const tabRefs = useRef<Array<HTMLButtonElement | null>>([]);
	const [searchQuery, setSearchQuery] = useState('');
	useEffect(() => {
		const pendingTab = FriendsTabState.consumeTab();
		if (pendingTab) {
			setActiveTab(pendingTab);
		}
	}, []);
	const pendingBadgeVisible = pendingCount > 0;
	useSkeletonLayoutReport(() => {
		reportSkeletonFriendsHeaderLayout({
			activeTab,
			tabWidthsPx: tabRefs.current.map((tabElement) => measureSkeletonWidthPx(tabElement)),
			pendingBadgeVisible,
		});
	}, `${activeTab}:${pendingBadgeVisible}:${i18n.locale}`);
	const openProfile = useCallback((userId: string) => {
		UserProfileCommands.openUserProfile(userId);
	}, []);
	useFluxerDocumentTitle(i18n._(MY_FRIENDS_DESCRIPTOR));
	const renderTabContent = () => {
		const relationshipsRecord = relationships.reduce(
			(acc, rel) => {
				acc[rel.id] = rel;
				return acc;
			},
			{} as Record<string, Relationship>,
		);
		switch (activeTab) {
			case 'add':
				return <AddFriendView data-flx="channel.direct-message.dm-friends-view.render-tab-content.add-friend-view" />;
			case 'pending':
				return (
					<PendingFriendsView
						relationships={relationshipsRecord}
						openProfile={openProfile}
						searchQuery={searchQuery}
						data-flx="channel.direct-message.dm-friends-view.render-tab-content.pending-friends-view"
					/>
				);
			case 'online':
				return (
					<FriendsList
						showOnlineOnly={true}
						openProfile={openProfile}
						searchQuery={searchQuery}
						data-flx="channel.direct-message.dm-friends-view.render-tab-content.friends-list"
					/>
				);
			case 'all':
				return (
					<FriendsList
						showOnlineOnly={false}
						openProfile={openProfile}
						searchQuery={searchQuery}
						data-flx="channel.direct-message.dm-friends-view.render-tab-content.friends-list--2"
					/>
				);
		}
	};
	const handleKeyDown = (e: React.KeyboardEvent, index: number) => {
		const direction = getTabNavigationDirection(e.key, 'horizontal');
		if (!direction) return;
		const nextIndex = getNextTabIndex(index, FRIENDS_TAB_ORDER.length, direction);
		if (nextIndex == null) return;
		const nextTab = FRIENDS_TAB_ORDER[nextIndex] ?? null;
		if (!nextTab) return;
		e.preventDefault();
		e.stopPropagation();
		setActiveTab(nextTab);
		requestAnimationFrame(() => tabRefs.current[nextIndex]?.focus());
	};
	const FriendsHeaderContent = (
		<div className={styles.headerContent} data-flx="channel.direct-message.dm-friends-view.header-content">
			{!mobileLayout.enabled && (
				<>
					<div className={styles.titleSection} data-flx="channel.direct-message.dm-friends-view.title-section">
						<UsersThreeIcon
							weight="fill"
							className={styles.titleIcon}
							data-flx="channel.direct-message.dm-friends-view.title-icon"
						/>
						<span className={styles.titleText} data-flx="channel.direct-message.dm-friends-view.title-text">
							{i18n._(MY_FRIENDS_DESCRIPTOR)}
						</span>
					</div>
					<div className={styles.divider} data-flx="channel.direct-message.dm-friends-view.divider" />
				</>
			)}
			<div className={styles.tabsWrapper} data-flx="channel.direct-message.dm-friends-view.tabs-wrapper">
				<Scroller
					className={styles.tabsScroller}
					orientation="horizontal"
					overflow="auto"
					fade={false}
					key="dm-friends-tabs-scroller"
					data-flx="channel.direct-message.dm-friends-view.tabs-scroller"
				>
					<div
						className={styles.tabsInner}
						role="tablist"
						aria-label={i18n._(FRIENDS_SECTIONS_DESCRIPTOR)}
						data-flx="channel.direct-message.dm-friends-view.tabs-inner"
					>
						<TabButton
							ref={(el) => {
								tabRefs.current[0] = el;
							}}
							tab="online"
							activeTab={activeTab}
							onClick={setActiveTab}
							label={i18n._(ONLINE_DESCRIPTOR)}
							panelId="dm-friends-panel-online"
							onKeyDown={(e) => handleKeyDown(e, 0)}
							data-flx="channel.direct-message.dm-friends-view.tab-button.set-active-tab"
						/>
						<TabButton
							ref={(el) => {
								tabRefs.current[1] = el;
							}}
							tab="all"
							activeTab={activeTab}
							onClick={setActiveTab}
							label={i18n._(ALL_DESCRIPTOR)}
							panelId="dm-friends-panel-all"
							onKeyDown={(e) => handleKeyDown(e, 1)}
							data-flx="channel.direct-message.dm-friends-view.tab-button.set-active-tab--2"
						/>
						<TabButton
							ref={(el) => {
								tabRefs.current[2] = el;
							}}
							tab="pending"
							activeTab={activeTab}
							onClick={setActiveTab}
							label={i18n._(PENDING_DESCRIPTOR)}
							panelId="dm-friends-panel-pending"
							badge={pendingCount}
							onKeyDown={(e) => handleKeyDown(e, 2)}
							data-flx="channel.direct-message.dm-friends-view.tab-button.set-active-tab--3"
						/>
						<TabButton
							ref={(el) => {
								tabRefs.current[3] = el;
							}}
							tab="add"
							activeTab={activeTab}
							onClick={setActiveTab}
							label={i18n._(ADD_FRIEND_DESCRIPTOR)}
							panelId="dm-friends-panel-add"
							primary
							onKeyDown={(e) => handleKeyDown(e, 3)}
							data-flx="channel.direct-message.dm-friends-view.tab-button.set-active-tab--4"
						/>
					</div>
				</Scroller>
			</div>
		</div>
	);
	const searchPlaceholder = useMemo(() => {
		switch (activeTab) {
			case 'online':
				return i18n._(SEARCH_ONLINE_FRIENDS_DESCRIPTOR);
			case 'all':
				return i18n._(SEARCH_FRIENDS_DESCRIPTOR);
			case 'pending':
				return i18n._(SEARCH_PENDING_REQUESTS_DESCRIPTOR);
			default:
				return i18n._(SEARCH_FRIENDS_DESCRIPTOR);
		}
	}, [activeTab, i18n.locale]);
	const showSearchBar = activeTab !== 'add';
	const activePanelId = `dm-friends-panel-${activeTab}`;
	return (
		<div className={styles.container} data-flx="channel.direct-message.dm-friends-view.container">
			<div className={styles.mainColumn} data-flx="channel.direct-message.dm-friends-view.main-column">
				<ChannelHeader
					leftContent={FriendsHeaderContent}
					showMembersToggle={false}
					showPins={false}
					data-flx="channel.direct-message.dm-friends-view.channel-header"
				/>
				<div className={styles.content} data-flx="channel.direct-message.dm-friends-view.content">
					{showSearchBar && (
						<div className={styles.searchWrapper} data-flx="channel.direct-message.dm-friends-view.search-wrapper">
							<Input
								value={searchQuery}
								onChange={(event) => setSearchQuery(event.currentTarget.value)}
								placeholder={searchPlaceholder}
								aria-label={searchPlaceholder}
								spellCheck={false}
								autoComplete="off"
								leftIcon={
									<MagnifyingGlassIcon
										weight="bold"
										className={styles.searchIcon}
										data-flx="channel.direct-message.dm-friends-view.search-icon"
									/>
								}
								data-flx="channel.direct-message.dm-friends-view.input.set-search-query"
							/>
						</div>
					)}
					<div
						id={activePanelId}
						className={styles.tabBody}
						role="tabpanel"
						aria-labelledby={`dm-friends-tab-${activeTab}`}
						data-flx="channel.direct-message.dm-friends-view.tab-body"
					>
						{renderTabContent()}
					</div>
				</div>
			</div>
			<ActiveNowSidebar data-flx="channel.direct-message.dm-friends-view.active-now-sidebar" />
		</div>
	);
});
