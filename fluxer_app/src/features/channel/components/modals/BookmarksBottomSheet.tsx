// SPDX-License-Identifier: AGPL-3.0-or-later

import {LongPressable} from '@app/features/app/components/LongPressable';
import {SavedMessageMissingCard} from '@app/features/app/components/shared/SavedMessageMissingCard';
import {Message as MessageComponent} from '@app/features/channel/components/ChannelMessage';
import styles from '@app/features/channel/components/modals/BookmarksBottomSheet.module.css';
import Channels from '@app/features/channel/state/Channels';
import {REMOVE_BOOKMARK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {ensureMembersForMessages} from '@app/features/messaging/commands/MessageCommands';
import * as SavedMessageCommands from '@app/features/messaging/commands/SavedMessageCommands';
import {useMessageListKeyboardNavigation} from '@app/features/messaging/hooks/useMessageListKeyboardNavigation';
import {useMessageSelectionCopyForMessages} from '@app/features/messaging/hooks/useMessageSelectionCopy';
import {NearViewportSurfaceContext} from '@app/features/messaging/hooks/useNearViewport';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import {focusChannelTextareaAfterNavigation} from '@app/features/messaging/utils/ChannelTextareaFocusUtils';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import type {MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo, useRef, useState} from 'react';

const JUMP_TO_MESSAGE_DESCRIPTOR = msg({
	message: 'Jump to message',
	comment: 'Short label in the bookmarks bottom sheet. Keep it concise.',
});
const BOOKMARKS_DESCRIPTOR = msg({
	message: 'Bookmarks',
	comment: 'Short label in the bookmarks bottom sheet. Keep it concise.',
});

interface BookmarksBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
}

export const BookmarksBottomSheet = observer(({isOpen, onClose}: BookmarksBottomSheetProps) => {
	const {i18n} = useLingui();
	const {savedMessages, missingSavedMessages, fetched} = SavedMessages;
	const hasBookmarks = savedMessages.length > 0 || missingSavedMessages.length > 0;
	const hasMore = SavedMessages.getHasMore();
	const isLoadingMore = SavedMessages.getIsLoadingMore();
	const scrollerRef = useRef<ScrollerHandle | null>(null);
	const resolveBookmarksScrollSurface = useMemo(() => () => scrollerRef.current?.getViewportElement() ?? null, []);
	const [selectedMessage, setSelectedMessage] = useState<Message | null>(null);
	const [menuOpen, setMenuOpen] = useState(false);
	const onCopySelectedMessages = useMessageSelectionCopyForMessages<HTMLDivElement>(savedMessages);
	useEffect(() => {
		if (!fetched && isOpen) {
			SavedMessageCommands.fetch();
		}
	}, [fetched, isOpen]);
	useEffect(() => {
		if (!isOpen || savedMessages.length === 0) return;
		void ensureMembersForMessages(savedMessages);
	}, [isOpen, savedMessages]);
	useMessageListKeyboardNavigation({
		containerRef: scrollerRef,
	});
	const handleScroll = (event: React.UIEvent<HTMLDivElement>) => {
		const target = event.currentTarget;
		const scrollPercentage = (target.scrollTop + target.offsetHeight) / target.scrollHeight;
		if (scrollPercentage > 0.8 && hasMore && !isLoadingMore) {
			SavedMessageCommands.loadMore();
		}
	};
	const handleLongPress = (message: Message) => {
		setSelectedMessage(message);
		setMenuOpen(true);
	};
	const handleJumpToMessage = (message: Message) => {
		goToMessage(message.channelId, message.id);
		onClose();
		focusChannelTextareaAfterNavigation(message.channelId);
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
			SavedMessageCommands.remove(i18n, selectedMessage.id);
		}
		setMenuOpen(false);
		setSelectedMessage(null);
	};
	const menuGroups: Array<MenuGroupType> = [
		{
			items: [
				{
					icon: (
						<ArrowSquareOutIcon
							weight="fill"
							className={styles.menuIcon}
							data-flx="channel.bookmarks-bottom-sheet.menu-icon"
						/>
					),
					label: i18n._(JUMP_TO_MESSAGE_DESCRIPTOR),
					onClick: handleMenuJump,
				},
				{
					icon: (
						<TrashIcon
							weight="fill"
							className={styles.menuIcon}
							data-flx="channel.bookmarks-bottom-sheet.menu-icon--2"
						/>
					),
					label: i18n._(REMOVE_BOOKMARK_DESCRIPTOR),
					onClick: handleRemove,
					danger: true,
				},
			],
		},
	];
	return (
		<>
			<BottomSheet
				isOpen={isOpen}
				onClose={onClose}
				snapPoints={[0, 1]}
				initialSnap={1}
				title={i18n._(BOOKMARKS_DESCRIPTOR)}
				data-flx="channel.bookmarks-bottom-sheet.bottom-sheet"
			>
				{hasBookmarks ? (
					<NearViewportSurfaceContext.Provider value={resolveBookmarksScrollSurface}>
						<Scroller
							className={styles.messageList}
							onScroll={handleScroll}
							key="bookmarks-bottom-sheet-scroller"
							ref={scrollerRef}
							onCopy={onCopySelectedMessages}
							data-message-selection-root="true"
							data-flx="channel.bookmarks-bottom-sheet.message-list"
						>
							{missingSavedMessages.length > 0 && (
								<div className={styles.missingList} data-flx="channel.bookmarks-bottom-sheet.missing-list">
									{missingSavedMessages.map((entry) => (
										<SavedMessageMissingCard
											key={entry.id}
											entryId={entry.id}
											onRemove={() => SavedMessageCommands.remove(i18n, entry.id)}
											data-flx="channel.bookmarks-bottom-sheet.saved-message-missing-card"
										/>
									))}
								</div>
							)}
							<div className={styles.topSpacer} data-flx="channel.bookmarks-bottom-sheet.top-spacer" />
							<div className={styles.messagesContainer} data-flx="channel.bookmarks-bottom-sheet.messages-container">
								{savedMessages.map((message) => (
									<MessageWithLongPress
										key={message.id}
										message={message}
										onLongPress={handleLongPress}
										onClick={handleJumpToMessage}
										data-flx="channel.bookmarks-bottom-sheet.message-with-long-press.jump-to-message"
									/>
								))}
							</div>
							{isLoadingMore && (
								<div className={styles.loadingState} data-flx="channel.bookmarks-bottom-sheet.loading-state">
									<Spinner data-flx="channel.bookmarks-bottom-sheet.spinner" />
								</div>
							)}
						</Scroller>
					</NearViewportSurfaceContext.Provider>
				) : (
					<div className={styles.emptyState} data-flx="channel.bookmarks-bottom-sheet.empty-state">
						<div className={styles.emptyContent} data-flx="channel.bookmarks-bottom-sheet.empty-content">
							<p className={styles.emptyTitle} data-flx="channel.bookmarks-bottom-sheet.empty-title">
								<Trans>No bookmarks</Trans>
							</p>
							<p className={styles.emptyDescription} data-flx="channel.bookmarks-bottom-sheet.empty-description">
								<Trans>Bookmark messages to save them for later.</Trans>
							</p>
						</div>
					</div>
				)}
			</BottomSheet>
			<MenuBottomSheet
				isOpen={menuOpen}
				onClose={() => setMenuOpen(false)}
				groups={menuGroups}
				data-flx="channel.bookmarks-bottom-sheet.menu-bottom-sheet"
			/>
		</>
	);
});

interface MessageWithLongPressProps {
	message: Message;
	onLongPress: (message: Message) => void;
	onClick: (message: Message) => void;
}

const MessageWithLongPress = observer(({message, onLongPress, onClick}: MessageWithLongPressProps) => {
	const channel = Channels.getChannel(message.channelId);
	if (!channel) return null;
	return (
		<LongPressable
			className={styles.messagePreviewCard}
			data-message-id={message.id}
			data-is-group-start="true"
			onLongPress={() => onLongPress(message)}
			onClick={() => onClick(message)}
			data-flx="channel.bookmarks-bottom-sheet.message-with-long-press.message-preview-card.click"
		>
			<MessageComponent
				message={message}
				channel={channel}
				previewContext={MessagePreviewContext.LIST_POPOUT}
				data-flx="channel.bookmarks-bottom-sheet.message-with-long-press.message-component"
			/>
		</LongPressable>
	);
});
