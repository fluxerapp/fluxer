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
import {MagnifyingGlassIcon, UsersThreeIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {RelationshipTypes} from '~/Constants';
import {ChannelHeader} from '~/components/channel/ChannelHeader';
import {AddFriendView} from '~/components/channel/dm/AddFriendView';
import {FriendsList} from '~/components/channel/friends/views/FriendsList';
import {PendingFriendsView} from '~/components/channel/friends/views/PendingFriendsView';
import {Input} from '~/components/form/Input';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {MentionBadge} from '~/components/uikit/MentionBadge';
import {useFluxerDocumentTitle} from '~/hooks/useFluxerDocumentTitle';
import type {RelationshipRecord} from '~/records/RelationshipRecord';
import FriendsTabStore from '~/stores/FriendsTabStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import RelationshipStore from '~/stores/RelationshipStore';
import styles from './DMFriendsView.module.css';

type FriendsTab = 'online' | 'all' | 'pending' | 'add';

interface TabButtonProps extends Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'onClick'> {
	tab: FriendsTab;
	activeTab: FriendsTab;
	onClick: (tab: FriendsTab) => void;
	label: string;
	badge?: number;
	primary?: boolean;
}

const TabButton = observer(
	React.forwardRef<HTMLButtonElement, TabButtonProps>(
		({tab, activeTab, onClick, label, badge, primary, ...props}, ref) => {
			const isActive = activeTab === tab && !primary;

			return (
				<FocusRing within offset={-2}>
					<button
						ref={ref}
						type="button"
						role="tab"
						aria-selected={isActive}
						tabIndex={isActive || (primary && activeTab === tab) ? 0 : -1}
						className={clsx(styles.tabButton, {
							[styles.active]: isActive,
							[styles.primary]: primary,
						})}
						onClick={() => onClick(tab)}
						{...props}
					>
						<div className={styles.tabContent}>
							{label}
							{badge !== undefined && badge > 0 && <MentionBadge mentionCount={badge} />}
						</div>
					</button>
				</FocusRing>
			);
		},
	),
);

export const DMFriendsView: React.FC = observer(() => {
	const {t} = useLingui();
	const [activeTab, setActiveTab] = React.useState<FriendsTab>('online');
	const mobileLayout = MobileLayoutStore;
	const relationships = RelationshipStore.getRelationships();
	const pendingCount = relationships.filter((relation) => relation.type === RelationshipTypes.INCOMING_REQUEST).length;
	const tabRefs = React.useRef<Array<HTMLButtonElement | null>>([]);
	const [searchQuery, setSearchQuery] = React.useState('');

	React.useEffect(() => {
		const pendingTab = FriendsTabStore.consumeTab();
		if (pendingTab) {
			setActiveTab(pendingTab);
		}
	}, []);

	const openProfile = React.useCallback((userId: string) => {
		UserProfileActionCreators.openUserProfile(userId);
	}, []);

	useFluxerDocumentTitle(t`My Friends`);

	const renderTabContent = () => {
		const relationshipsRecord = relationships.reduce(
			(acc, rel) => {
				acc[rel.id] = rel;
				return acc;
			},
			{} as Record<string, RelationshipRecord>,
		);

		switch (activeTab) {
			case 'add':
				return <AddFriendView />;
			case 'pending':
				return (
					<PendingFriendsView relationships={relationshipsRecord} openProfile={openProfile} searchQuery={searchQuery} />
				);
			case 'online':
				return <FriendsList showOnlineOnly={true} openProfile={openProfile} searchQuery={searchQuery} />;
			case 'all':
				return <FriendsList showOnlineOnly={false} openProfile={openProfile} searchQuery={searchQuery} />;
		}
	};

	const handleKeyDown = (e: React.KeyboardEvent, index: number) => {
		const tabCount = 4;
		if (e.key === 'ArrowRight') {
			e.preventDefault();
			const nextIndex = (index + 1) % tabCount;
			tabRefs.current[nextIndex]?.focus();
		} else if (e.key === 'ArrowLeft') {
			e.preventDefault();
			const prevIndex = (index - 1 + tabCount) % tabCount;
			tabRefs.current[prevIndex]?.focus();
		} else if (e.key === 'Home') {
			e.preventDefault();
			tabRefs.current[0]?.focus();
		} else if (e.key === 'End') {
			e.preventDefault();
			tabRefs.current[tabCount - 1]?.focus();
		}
	};

	const FriendsHeaderContent = (
		<div className={styles.headerContent}>
			{!mobileLayout.enabled && (
				<>
					<div className={styles.titleSection}>
						<UsersThreeIcon weight="fill" className={styles.titleIcon} />
						<span className={styles.titleText}>{t`My Friends`}</span>
					</div>
					<div className={styles.divider} />
				</>
			)}

			<div className={styles.tabsWrapper}>
				<div className={styles.tabsInner} role="tablist">
					<TabButton
						ref={(el) => {
							tabRefs.current[0] = el;
						}}
						tab="online"
						activeTab={activeTab}
						onClick={setActiveTab}
						label={t`Online`}
						onKeyDown={(e) => handleKeyDown(e, 0)}
					/>
					<TabButton
						ref={(el) => {
							tabRefs.current[1] = el;
						}}
						tab="all"
						activeTab={activeTab}
						onClick={setActiveTab}
						label={t`All`}
						onKeyDown={(e) => handleKeyDown(e, 1)}
					/>
					<TabButton
						ref={(el) => {
							tabRefs.current[2] = el;
						}}
						tab="pending"
						activeTab={activeTab}
						onClick={setActiveTab}
						label={t`Pending`}
						badge={pendingCount}
						onKeyDown={(e) => handleKeyDown(e, 2)}
					/>
					<TabButton
						ref={(el) => {
							tabRefs.current[3] = el;
						}}
						tab="add"
						activeTab={activeTab}
						onClick={setActiveTab}
						label={t`Add Friend`}
						primary
						onKeyDown={(e) => handleKeyDown(e, 3)}
					/>
				</div>
			</div>
		</div>
	);

	const searchPlaceholder = React.useMemo(() => {
		switch (activeTab) {
			case 'online':
				return t`Search online friends`;
			case 'all':
				return t`Search friends`;
			case 'pending':
				return t`Search pending requests`;
			default:
				return t`Search friends`;
		}
	}, [activeTab]);

	const showSearchBar = activeTab !== 'add';

	return (
		<div className={styles.container}>
			<ChannelHeader leftContent={FriendsHeaderContent} showMembersToggle={false} showPins={false} />
			<div className={styles.content}>
				{showSearchBar && (
					<div className={styles.searchWrapper}>
						<Input
							value={searchQuery}
							onChange={(event) => setSearchQuery(event.currentTarget.value)}
							placeholder={searchPlaceholder}
							aria-label={searchPlaceholder}
							spellCheck={false}
							autoComplete="off"
							leftIcon={<MagnifyingGlassIcon weight="bold" className={styles.searchIcon} />}
						/>
					</div>
				)}
				<div className={styles.tabBody}>{renderTabContent()}</div>
			</div>
		</div>
	);
});
