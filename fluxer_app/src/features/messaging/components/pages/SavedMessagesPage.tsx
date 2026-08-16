// SPDX-License-Identifier: AGPL-3.0-or-later

import previewStyles from '@app/features/app/components/shared/MessagePreview.module.css';
import {SavedMessageMissingCard} from '@app/features/app/components/shared/SavedMessageMissingCard';
import {
	reportSkeletonSimplePageLayout,
	SkeletonSimplePageBody,
	SkeletonSimplePageRoute,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {useSkeletonLayoutReport} from '@app/features/app/hooks/useSkeletonLayoutMemoryCapture';
import {REMOVE_BOOKMARK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as SavedMessageCommands from '@app/features/messaging/commands/SavedMessageCommands';
import {MessageListPage} from '@app/features/messaging/components/pages/MessageListPage';
import styles from '@app/features/messaging/components/pages/SavedMessagesPage.module.css';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {BookmarkSimpleIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

const BOOKMARKS_DESCRIPTOR = msg({
	message: 'Bookmarks',
	comment: 'Short label in the saved messages page. Keep it concise.',
});
const NO_BOOKMARKS_DESCRIPTOR = msg({
	message: 'No bookmarks',
	comment: 'Empty-state text in the saved messages page.',
});
const BOOKMARK_MESSAGES_TO_SAVE_THEM_FOR_LATER_DESCRIPTOR = msg({
	message: 'Bookmark messages to save them for later.',
	comment: 'Description text in the saved messages page.',
});
const THERE_S_NOTHING_MORE_TO_SEE_HERE_DESCRIPTOR = msg({
	message: "That's all of them.",
	comment: 'End-of-list line on the saved messages page after every bookmark has been shown.',
});
export const SavedMessagesPage = observer(() => {
	const {i18n} = useLingui();
	const {savedMessages, missingSavedMessages, fetched} = SavedMessages;
	useEffect(() => {
		if (!fetched) {
			SavedMessageCommands.fetch();
		}
	}, [fetched]);
	useSkeletonLayoutReport(() => {
		if (!fetched) {
			return;
		}
		reportSkeletonSimplePageLayout(
			SkeletonSimplePageRoute.BOOKMARKS,
			SkeletonSimplePageBody.MESSAGE_LIST,
			savedMessages.length,
		);
	}, `${fetched}|${savedMessages.length}`);
	const renderActionButtons = (message: Message) => (
		<button
			type="button"
			className={previewStyles.actionIconButton}
			onClick={() => SavedMessageCommands.remove(i18n, message.id)}
			aria-label={i18n._(REMOVE_BOOKMARK_DESCRIPTOR)}
			data-flx="messaging.saved-messages-page.render-action-buttons.button.remove"
		>
			<XIcon
				weight="bold"
				className={previewStyles.actionIcon}
				data-flx="messaging.saved-messages-page.render-action-buttons.x-icon"
			/>
		</button>
	);
	return (
		<div data-flx="messaging.saved-messages-page.div">
			{missingSavedMessages.length > 0 && (
				<div className={styles.missingList} data-flx="messaging.saved-messages-page.missing-list">
					{missingSavedMessages.map((entry) => (
						<SavedMessageMissingCard
							key={entry.id}
							entryId={entry.id}
							onRemove={() => SavedMessageCommands.remove(i18n, entry.id)}
							data-flx="messaging.saved-messages-page.saved-message-missing-card"
						/>
					))}
				</div>
			)}
			<MessageListPage
				icon={<BookmarkSimpleIcon className={styles.icon} data-flx="messaging.saved-messages-page.icon" />}
				title={i18n._(BOOKMARKS_DESCRIPTOR)}
				messages={savedMessages.slice()}
				emptyStateTitle={i18n._(NO_BOOKMARKS_DESCRIPTOR)}
				emptyStateDescription={i18n._(BOOKMARK_MESSAGES_TO_SAVE_THEM_FOR_LATER_DESCRIPTOR)}
				endStateDescription={i18n._(THERE_S_NOTHING_MORE_TO_SEE_HERE_DESCRIPTOR)}
				renderActionButtons={renderActionButtons}
				data-flx="messaging.saved-messages-page.message-list-page"
			/>
		</div>
	);
});
