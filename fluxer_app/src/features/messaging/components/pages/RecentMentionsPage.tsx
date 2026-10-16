// SPDX-License-Identifier: AGPL-3.0-or-later

import previewStyles from '@app/features/app/components/shared/MessagePreview.module.css';
import {
	reportSkeletonSimplePageLayout,
	SkeletonSimplePageBody,
	SkeletonSimplePageRoute,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {RECENT_MENTIONS_RETENTION_DAYS} from '@app/features/app/config/I18nDisplayConstants';
import {useSkeletonLayoutReport} from '@app/features/app/hooks/useSkeletonLayoutMemoryCapture';
import {ensureMembersForMessages} from '@app/features/messaging/commands/MessageCommands';
import {MessageListPage} from '@app/features/messaging/components/pages/MessageListPage';
import styles from '@app/features/messaging/components/pages/RecentMentionsPage.module.css';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import * as RecentMentionCommands from '@app/features/notification/commands/RecentMentionCommands';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {AtIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

const DISMISS_MENTION_DESCRIPTOR = msg({
	message: 'Dismiss mention',
	comment: 'Short label in the recent mentions page. Keep it concise.',
});
const RECENT_MENTIONS_DESCRIPTOR = msg({
	message: 'Recent mentions',
	comment: 'Short label in the recent mentions page. Keep it concise.',
});
const NO_RECENT_MENTIONS_DESCRIPTOR = msg({
	message: 'No recent mentions',
	comment: 'Empty-state text in the recent mentions page.',
});
const ALL_MENTIONS_OF_YOU_WILL_APPEAR_HERE_FOR_DESCRIPTOR = msg({
	message: 'Mentions of you show up here for {recentMentionsRetentionDays} days.',
	comment:
		'Description text in the recent mentions page. Preserve {recentMentionsRetentionDays}; it is inserted by code.',
});
const YOU_VE_SEEN_ALL_YOUR_RECENT_MENTIONS_MORE_DESCRIPTOR = msg({
	message: "You're all caught up.",
	comment: 'End-of-list text in the recent mentions page when every mention has been seen.',
});
export const RecentMentionsPage = observer(() => {
	const {i18n} = useLingui();
	const recentMentions = MentionFeed.getAccessibleMentions();
	const fetched = MentionFeed.fetched;
	useEffect(() => {
		if (!fetched) {
			RecentMentionCommands.fetch();
		}
	}, [fetched]);
	useEffect(() => {
		if (recentMentions.length === 0) return;
		void ensureMembersForMessages(recentMentions);
	}, [recentMentions]);
	useSkeletonLayoutReport(() => {
		if (!fetched) {
			return;
		}
		reportSkeletonSimplePageLayout(
			SkeletonSimplePageRoute.MENTIONS,
			SkeletonSimplePageBody.MESSAGE_LIST,
			recentMentions.length,
		);
	}, `${fetched}|${recentMentions.length}`);
	const renderActionButtons = (message: Message) => (
		<button
			type="button"
			className={previewStyles.actionIconButton}
			onClick={() => RecentMentionCommands.remove(message.id)}
			aria-label={i18n._(DISMISS_MENTION_DESCRIPTOR)}
			data-flx="messaging.recent-mentions-page.render-action-buttons.button.remove"
		>
			<XIcon
				weight="bold"
				className={previewStyles.actionIcon}
				data-flx="messaging.recent-mentions-page.render-action-buttons.x-icon"
			/>
		</button>
	);
	return (
		<MessageListPage
			icon={<AtIcon weight="bold" className={styles.icon} data-flx="messaging.recent-mentions-page.icon" />}
			title={i18n._(RECENT_MENTIONS_DESCRIPTOR)}
			messages={recentMentions.slice()}
			emptyStateTitle={i18n._(NO_RECENT_MENTIONS_DESCRIPTOR)}
			emptyStateDescription={i18n._(ALL_MENTIONS_OF_YOU_WILL_APPEAR_HERE_FOR_DESCRIPTOR, {
				recentMentionsRetentionDays: RECENT_MENTIONS_RETENTION_DAYS,
			})}
			endStateDescription={i18n._(YOU_VE_SEEN_ALL_YOUR_RECENT_MENTIONS_MORE_DESCRIPTOR)}
			renderActionButtons={renderActionButtons}
			data-flx="messaging.recent-mentions-page.message-list-page"
		/>
	);
});
