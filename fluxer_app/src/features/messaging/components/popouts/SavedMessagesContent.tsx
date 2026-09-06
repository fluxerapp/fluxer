// SPDX-License-Identifier: AGPL-3.0-or-later

import previewStyles from '@app/features/app/components/shared/MessagePreview.module.css';
import {SavedMessageMissingCard} from '@app/features/app/components/shared/SavedMessageMissingCard';
import {Message} from '@app/features/channel/components/ChannelMessage';
import Channels from '@app/features/channel/state/Channels';
import {JUMP_DESCRIPTOR, REMOVE_BOOKMARK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InboxCommands from '@app/features/inbox/commands/InboxCommands';
import {ensureMembersForMessages} from '@app/features/messaging/commands/MessageCommands';
import * as SavedMessageCommands from '@app/features/messaging/commands/SavedMessageCommands';
import {InboxMessageHeader} from '@app/features/messaging/components/popouts/InboxMessageHeader';
import headerStyles from '@app/features/messaging/components/popouts/InboxMessageHeader.module.css';
import styles from '@app/features/messaging/components/popouts/SavedMessagesContent.module.css';
import {useMessageListKeyboardNavigation} from '@app/features/messaging/hooks/useMessageListKeyboardNavigation';
import {useMessageSelectionCopyForMessages} from '@app/features/messaging/hooks/useMessageSelectionCopy';
import {NearViewportSurfaceContext} from '@app/features/messaging/hooks/useNearViewport';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {FlagCheckeredIcon, SparkleIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useRef} from 'react';

const NO_BOOKMARKS_DESCRIPTOR = msg({
	message: 'No bookmarks',
	comment: 'Empty-state text in the saved messages content popout.',
});
const BOOKMARK_MESSAGES_TO_SAVE_THEM_FOR_LATER_DESCRIPTOR = msg({
	message: 'Bookmark messages to save them for later.',
	comment: 'Description text in the saved messages content popout.',
});
const YOU_VE_REACHED_THE_END_DESCRIPTOR = msg({
	message: "You've reached the end",
	comment: 'Label in the saved messages content popout.',
});
const THERE_S_NOTHING_MORE_TO_SEE_HERE_DESCRIPTOR = msg({
	message: "That's all of them.",
	comment: 'End-of-list line in the saved messages popout after every bookmark has been shown.',
});
const readonlyBehaviorOverrides = {
	disableContextMenu: true,
};
export const SavedMessagesContent = observer(() => {
	const {i18n} = useLingui();
	const {savedMessages, missingSavedMessages, fetched} = SavedMessages;
	const hasMore = SavedMessages.getHasMore();
	const isLoadingMore = SavedMessages.getIsLoadingMore();
	const scrollerRef = useRef<ScrollerHandle | null>(null);
	const resolveSavedScrollSurface = useMemo(() => () => scrollerRef.current?.getViewportElement() ?? null, []);
	const onCopySelectedMessages = useMessageSelectionCopyForMessages<HTMLDivElement>(savedMessages);
	const renderMissingSavedMessage = useCallback(
		(entryId: string) => (
			<SavedMessageMissingCard
				key={`lost-${entryId}`}
				entryId={entryId}
				onRemove={() => SavedMessageCommands.remove(i18n, entryId)}
				data-flx="messaging.saved-messages-content.render-missing-saved-message.saved-message-missing-card"
			/>
		),
		[i18n],
	);
	useEffect(() => {
		if (!fetched) {
			SavedMessageCommands.fetch();
		}
	}, [fetched]);
	useEffect(() => {
		if (savedMessages.length === 0) return;
		void ensureMembersForMessages(savedMessages);
	}, [savedMessages]);
	useMessageListKeyboardNavigation({
		containerRef: scrollerRef,
	});
	const handleJumpToMessage = useCallback((channelId: string, messageId: string) => {
		goToMessage(channelId, messageId);
		InboxCommands.closeInboxAndFocusChannelTextarea(channelId);
	}, []);
	const handleScroll = useCallback(
		(event: React.UIEvent<HTMLDivElement>) => {
			const target = event.currentTarget;
			const scrollPercentage = (target.scrollTop + target.offsetHeight) / target.scrollHeight;
			if (scrollPercentage > 0.8 && hasMore && !isLoadingMore) {
				SavedMessageCommands.loadMore();
			}
		},
		[hasMore, isLoadingMore],
	);
	if (!fetched) {
		return (
			<div className={previewStyles.emptyState} data-flx="messaging.saved-messages-content.div">
				<Spinner data-flx="messaging.saved-messages-content.spinner" />
			</div>
		);
	}
	if (!savedMessages.length && !missingSavedMessages.length) {
		return (
			<div className={previewStyles.emptyState} data-flx="messaging.saved-messages-content.div--2">
				<div className={previewStyles.emptyStateContent} data-flx="messaging.saved-messages-content.div--3">
					<SparkleIcon
						className={previewStyles.emptyStateIcon}
						data-flx="messaging.saved-messages-content.sparkle-icon"
					/>
					<div className={previewStyles.emptyStateTextContainer} data-flx="messaging.saved-messages-content.div--4">
						<h3 className={previewStyles.emptyStateTitle} data-flx="messaging.saved-messages-content.h3">
							{i18n._(NO_BOOKMARKS_DESCRIPTOR)}
						</h3>
						<p className={previewStyles.emptyStateDescription} data-flx="messaging.saved-messages-content.p">
							{i18n._(BOOKMARK_MESSAGES_TO_SAVE_THEM_FOR_LATER_DESCRIPTOR)}
						</p>
					</div>
				</div>
			</div>
		);
	}
	return (
		<NearViewportSurfaceContext.Provider value={resolveSavedScrollSurface}>
			<Scroller
				className={styles.scroller}
				onScroll={handleScroll}
				key="saved-messages-scroller"
				ref={scrollerRef}
				onCopy={onCopySelectedMessages}
				data-message-selection-root="true"
				data-flx="messaging.saved-messages-content.scroller"
			>
				{missingSavedMessages.map((entry) => renderMissingSavedMessage(entry.id))}
				{savedMessages.map((message) => {
					const channel = Channels.getChannel(message.channelId);
					if (!channel) {
						return renderMissingSavedMessage(message.id);
					}
					return (
						<div
							key={message.id}
							className={styles.messageCard}
							data-flx="messaging.saved-messages-content.message-card"
						>
							<InboxMessageHeader
								channel={channel}
								onClick={() => handleJumpToMessage(message.channelId, message.id)}
								rightActions={
									<Tooltip
										text={i18n._(REMOVE_BOOKMARK_DESCRIPTOR)}
										position="top"
										data-flx="messaging.saved-messages-content.tooltip"
									>
										<FocusRing offset={-2} data-flx="messaging.saved-messages-content.focus-ring">
											<button
												type="button"
												className={headerStyles.headerIconButton}
												onClick={() => SavedMessageCommands.remove(i18n, message.id)}
												aria-label={i18n._(REMOVE_BOOKMARK_DESCRIPTOR)}
												data-flx="messaging.saved-messages-content.button.remove"
											>
												<XIcon
													weight="bold"
													className={headerStyles.headerIcon}
													data-flx="messaging.saved-messages-content.x-icon"
												/>
											</button>
										</FocusRing>
									</Tooltip>
								}
								data-flx="messaging.saved-messages-content.inbox-message-header.jump-to-message"
							/>
							<div
								className={previewStyles.previewCard}
								data-message-id={message.id}
								data-is-group-start="true"
								data-flx="messaging.saved-messages-content.div--5"
							>
								<Message
									message={message}
									channel={channel}
									previewContext={MessagePreviewContext.LIST_POPOUT}
									behaviorOverrides={readonlyBehaviorOverrides}
									suppressMessageActions
									onHeadingActivate={() => handleJumpToMessage(message.channelId, message.id)}
									data-flx="messaging.saved-messages-content.message"
								/>
								<div className={previewStyles.actionButtons} data-flx="messaging.saved-messages-content.div--6">
									<FocusRing offset={-2} data-flx="messaging.saved-messages-content.focus-ring--2">
										<button
											type="button"
											className={previewStyles.actionButton}
											onClick={() => {
												handleJumpToMessage(message.channelId, message.id);
											}}
											data-flx="messaging.saved-messages-content.button.jump-to-message"
										>
											{i18n._(JUMP_DESCRIPTOR)}
										</button>
									</FocusRing>
								</div>
							</div>
						</div>
					);
				})}
				{isLoadingMore && (
					<div className={previewStyles.loadingState} data-flx="messaging.saved-messages-content.div--10">
						<Spinner data-flx="messaging.saved-messages-content.spinner--2" />
					</div>
				)}
				{!hasMore && !isLoadingMore && (
					<div className={previewStyles.endState} data-flx="messaging.saved-messages-content.div--7">
						<div className={previewStyles.endStateContent} data-flx="messaging.saved-messages-content.div--8">
							<FlagCheckeredIcon
								className={previewStyles.endStateIcon}
								data-flx="messaging.saved-messages-content.flag-checkered-icon"
							/>
							<div className={previewStyles.endStateTextContainer} data-flx="messaging.saved-messages-content.div--9">
								<h3 className={previewStyles.endStateTitle} data-flx="messaging.saved-messages-content.h3--2">
									{i18n._(YOU_VE_REACHED_THE_END_DESCRIPTOR)}
								</h3>
								<p className={previewStyles.endStateDescription} data-flx="messaging.saved-messages-content.p--2">
									{i18n._(THERE_S_NOTHING_MORE_TO_SEE_HERE_DESCRIPTOR)}
								</p>
							</div>
						</div>
					</div>
				)}
			</Scroller>
		</NearViewportSurfaceContext.Provider>
	);
});
