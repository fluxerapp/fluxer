// SPDX-License-Identifier: AGPL-3.0-or-later

import {getUnreadChannels, UnreadChannelsContent} from '@app/features/app/components/floating/UnreadChannelsContent';
import {
	reportSkeletonSimplePageLayout,
	SkeletonSimplePageBody,
	SkeletonSimplePageRoute,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {useSkeletonLayoutReport} from '@app/features/app/hooks/useSkeletonLayoutMemoryCapture';
import {MENTIONS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {RecentMentionsContent} from '@app/features/messaging/components/popouts/RecentMentionsContent';
import styles from '@app/features/notification/components/pages/NotificationsPage.module.css';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {BookmarkSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useState} from 'react';

const UNREADS_DESCRIPTOR = msg({
	message: 'Unreads',
	comment: 'Short label in the notifications page. Keep it concise.',
});
const OPEN_BOOKMARKS_DESCRIPTOR = msg({
	message: 'Open bookmarks',
	comment: 'Button or menu action label in the notifications page. Keep it concise.',
});

interface NotificationsPageProps {
	onBookmarksClick: () => void;
}

export const NotificationsPage = observer(({onBookmarksClick}: NotificationsPageProps) => {
	const {i18n} = useLingui();
	const [filter, setFilter] = useState<'unreads' | 'mentions'>('unreads');
	const [headerActions, setHeaderActions] = useState<React.ReactNode>(null);
	const filterOptions = useMemo(
		() => [
			{label: i18n._(UNREADS_DESCRIPTOR), value: 'unreads' as const},
			{label: i18n._(MENTIONS_DESCRIPTOR), value: 'mentions' as const},
		],
		[i18n.locale],
	);
	const unreadChannelCount = getUnreadChannels().length;
	const mentionCount = MentionFeed.getAccessibleMentions().length;
	let body: SkeletonSimplePageBody = SkeletonSimplePageBody.CHANNEL_LIST;
	let rowCount = unreadChannelCount;
	if (filter === 'mentions') {
		body = SkeletonSimplePageBody.MESSAGE_LIST;
		rowCount = mentionCount;
	}
	useSkeletonLayoutReport(() => {
		reportSkeletonSimplePageLayout(SkeletonSimplePageRoute.NOTIFICATIONS, body, rowCount);
	}, `${body}:${rowCount}`);
	return (
		<div className={styles.container} data-flx="notification.notifications-page.container">
			<div className={styles.header} data-flx="notification.notifications-page.header">
				<h1 className={styles.title} data-flx="notification.notifications-page.title">
					<Trans>Notifications</Trans>
				</h1>
				<div className={styles.headerActions} data-flx="notification.notifications-page.header-actions">
					<Combobox
						value={filter}
						options={filterOptions}
						onChange={(value) => setFilter(value)}
						className={styles.filterTrigger}
						data-flx="notification.notifications-page.filter-trigger.set-filter"
					/>
					{filter === 'mentions' && headerActions}
					<button
						type="button"
						onClick={onBookmarksClick}
						className={styles.bookmarkButton}
						aria-label={i18n._(OPEN_BOOKMARKS_DESCRIPTOR)}
						data-flx="notification.notifications-page.bookmark-button.bookmarks-click"
					>
						<BookmarkSimpleIcon
							weight="fill"
							className={styles.bookmarkIcon}
							data-flx="notification.notifications-page.bookmark-icon"
						/>
					</button>
				</div>
			</div>
			<div className={styles.content} data-flx="notification.notifications-page.content">
				{filter === 'unreads' && (
					<UnreadChannelsContent data-flx="notification.notifications-page.unread-channels-content" />
				)}
				{filter === 'mentions' && (
					<RecentMentionsContent
						onHeaderActionsChange={setHeaderActions}
						data-flx="notification.notifications-page.recent-mentions-content"
					/>
				)}
			</div>
		</div>
	);
});
