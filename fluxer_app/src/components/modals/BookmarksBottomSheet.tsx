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
import {ArrowSquareOutIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as SavedMessageActionCreators from '~/actions/SavedMessageActionCreators';
import {MessagePreviewContext} from '~/Constants';
import {Message} from '~/components/channel/Message';
import {LongPressable} from '~/components/LongPressable';
import styles from '~/components/modals/BookmarksBottomSheet.module.css';
import {SavedMessageMissingCard} from '~/components/shared/SavedMessageMissingCard';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import type {MenuGroupType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {MenuBottomSheet} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {Scroller} from '~/components/uikit/Scroller';
import type {MessageRecord} from '~/records/MessageRecord';
import ChannelStore from '~/stores/ChannelStore';
import SavedMessagesStore from '~/stores/SavedMessagesStore';
import {goToMessage} from '~/utils/MessageNavigator';

interface BookmarksBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
}

export const BookmarksBottomSheet = observer(({isOpen, onClose}: BookmarksBottomSheetProps) => {
	const {t, i18n} = useLingui();
	const {savedMessages, missingSavedMessages, fetched} = SavedMessagesStore;
	const hasBookmarks = savedMessages.length > 0 || missingSavedMessages.length > 0;
	const [selectedMessage, setSelectedMessage] = React.useState<MessageRecord | null>(null);
	const [menuOpen, setMenuOpen] = React.useState(false);

	React.useEffect(() => {
		if (!fetched && isOpen) {
			SavedMessageActionCreators.fetch();
		}
	}, [fetched, isOpen]);

	const handleLongPress = (message: MessageRecord) => {
		setSelectedMessage(message);
		setMenuOpen(true);
	};

	const handleJumpToMessage = (message: MessageRecord) => {
		goToMessage(message.channelId, message.id);
		onClose();
	};

	const handleMenuJump = () => {
		if (selectedMessage) {
			handleJumpToMessage(selectedMessage);
		}
		setMenuOpen(false);
		setSelectedMessage(null);
	};

	const handleRemove = () => {
		if (selectedMessage) {
			SavedMessageActionCreators.remove(i18n, selectedMessage.id);
		}
		setMenuOpen(false);
		setSelectedMessage(null);
	};

	const menuGroups: Array<MenuGroupType> = [
		{
			items: [
				{
					icon: <ArrowSquareOutIcon weight="fill" className={styles.menuIcon} />,
					label: t`Jump to Message`,
					onClick: handleMenuJump,
				},
				{
					icon: <TrashIcon weight="fill" className={styles.menuIcon} />,
					label: t`Remove Bookmark`,
					onClick: handleRemove,
					danger: true,
				},
			],
		},
	];

	return (
		<>
			<BottomSheet isOpen={isOpen} onClose={onClose} snapPoints={[0, 1]} initialSnap={1} title={t`Bookmarks`}>
				{hasBookmarks ? (
					<Scroller className={styles.messageList} key="bookmarks-bottom-sheet-scroller">
						{missingSavedMessages.length > 0 && (
							<div className={styles.missingList}>
								{missingSavedMessages.map((entry) => (
									<SavedMessageMissingCard
										key={entry.id}
										entryId={entry.id}
										onRemove={() => SavedMessageActionCreators.remove(i18n, entry.id)}
									/>
								))}
							</div>
						)}
						<div className={styles.topSpacer} />
						<div className={styles.messagesContainer}>
							{savedMessages.map((message) => (
								<MessageWithLongPress
									key={message.id}
									message={message}
									onLongPress={handleLongPress}
									onClick={handleJumpToMessage}
								/>
							))}
						</div>
					</Scroller>
				) : (
					<div className={styles.emptyState}>
						<div className={styles.emptyContent}>
							<p className={styles.emptyTitle}>
								<Trans>No Bookmarks</Trans>
							</p>
							<p className={styles.emptyDescription}>
								<Trans>Bookmark messages to save them for later.</Trans>
							</p>
						</div>
					</div>
				)}
			</BottomSheet>

			<MenuBottomSheet isOpen={menuOpen} onClose={() => setMenuOpen(false)} groups={menuGroups} />
		</>
	);
});

interface MessageWithLongPressProps {
	message: MessageRecord;
	onLongPress: (message: MessageRecord) => void;
	onClick: (message: MessageRecord) => void;
}

const MessageWithLongPress = observer(({message, onLongPress, onClick}: MessageWithLongPressProps) => {
	const channel = ChannelStore.getChannel(message.channelId);
	if (!channel) return null;

	return (
		<LongPressable
			className={styles.messagePreviewCard}
			onLongPress={() => onLongPress(message)}
			onClick={() => onClick(message)}
		>
			<Message message={message} channel={channel} previewContext={MessagePreviewContext.LIST_POPOUT} />
		</LongPressable>
	);
});
