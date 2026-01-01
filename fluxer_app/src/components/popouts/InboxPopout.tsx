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

import {AtIcon, BookmarkSimpleIcon, ClockIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as InboxActionCreators from '~/actions/InboxActionCreators';
import styles from '~/components/popouts/InboxPopout.module.css';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import FocusRingScope from '~/components/uikit/FocusRing/FocusRingScope';
import FeatureFlagStore from '~/stores/FeatureFlagStore';
import InboxStore, {type InboxTab} from '~/stores/InboxStore';
import SelectedGuildStore from '~/stores/SelectedGuildStore';
import {RecentMentionsContent} from './RecentMentionsContent';
import {SavedMessagesContent} from './SavedMessagesContent';
import {ScheduledMessagesContent} from './ScheduledMessagesContent';

interface TabConfig {
	key: InboxTab;
	label: string;
	icon: React.ReactNode;
}

export const InboxPopout = observer(({initialTab}: {initialTab?: InboxTab} = {}) => {
	const {t} = useLingui();
	const activeTab = initialTab ?? InboxStore.selectedTab;
	const [headerActions, setHeaderActions] = React.useState<React.ReactNode>(null);
	const containerRef = React.useRef<HTMLDivElement | null>(null);

	const baseTabs: Array<TabConfig> = [
		{
			key: 'bookmarks',
			label: t`Bookmarks`,
			icon: <BookmarkSimpleIcon className={styles.iconSmall} />,
		},
		{
			key: 'mentions',
			label: t`Mentions`,
			icon: <AtIcon weight="bold" className={styles.iconSmall} />,
		},
	];

	const scheduledTab: TabConfig = {
		key: 'scheduled',
		label: t`Scheduled`,
		icon: <ClockIcon className={styles.iconSmall} />,
	};

	const selectedGuildId = SelectedGuildStore.selectedGuildId;
	const showScheduledTab = FeatureFlagStore.isMessageSchedulingEnabled(selectedGuildId ?? undefined);
	const tabs = showScheduledTab ? [...baseTabs, scheduledTab] : baseTabs;

	const normalizedActiveTab = tabs.some((tab) => tab.key === activeTab) ? activeTab : tabs[0].key;

	const setActiveTab = React.useCallback((tab: InboxTab) => {
		InboxActionCreators.setTab(tab);
	}, []);

	return (
		<FocusRingScope containerRef={containerRef}>
			<div className={styles.container} ref={containerRef}>
				<div className={styles.header}>
					<div className={styles.headerContent}>
						<nav>
							<div className={styles.tabList} role="tablist" aria-label={t`Inbox tabs`}>
								{tabs.map((tab) => (
									<FocusRing key={tab.key} offset={-2}>
										<button
											id={tab.key}
											role="tab"
											type="button"
											aria-selected={normalizedActiveTab === tab.key}
											className={clsx(
												styles.tab,
												normalizedActiveTab === tab.key ? styles.tabActive : styles.tabInactive,
											)}
											onClick={() => setActiveTab(tab.key)}
										>
											{tab.icon}
											<span>{tab.label}</span>
										</button>
									</FocusRing>
								))}
							</div>
						</nav>
						<div className={styles.headerActions}>{normalizedActiveTab === 'mentions' && headerActions}</div>
					</div>
				</div>

				<div className={styles.content}>
					{normalizedActiveTab === 'bookmarks' && (
						<div className={styles.tabContent}>
							<SavedMessagesContent />
						</div>
					)}
					{normalizedActiveTab === 'mentions' && (
						<div className={styles.tabContent}>
							<RecentMentionsContent onHeaderActionsChange={setHeaderActions} />
						</div>
					)}
					{showScheduledTab && normalizedActiveTab === 'scheduled' && (
						<div className={styles.tabContent}>
							<ScheduledMessagesContent />
						</div>
					)}
				</div>
			</div>
		</FocusRingScope>
	);
});
