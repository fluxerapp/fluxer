// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {
	MessageReactionsFilters,
	MessageReactionsReactorsList,
} from '@app/features/app/components/shared/MessageReactionsContent';
import Authentication from '@app/features/auth/state/Authentication';
import styles from '@app/features/channel/components/MessageReactionsSheet.module.css';
import {REACTIONS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ReactionCommands from '@app/features/messaging/commands/ReactionCommands';
import {useMessageReactionsState} from '@app/features/messaging/hooks/useMessageReactionsState';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {getEmojiNameWithColons} from '@app/features/messaging/utils/ReactionUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {
	MenuBottomSheet,
	type MenuGroupType,
	type MenuItemType,
} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {MessageReaction} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useState} from 'react';

const REMOVE_REACTION_DESCRIPTOR = msg({
	message: 'Remove reaction',
	comment:
		'Button or menu action label in the channel and chat message reactions sheet. Keep it concise. Keep the tone plain and specific.',
});
const REMOVE_ALL_REACTIONS_FROM_THIS_MESSAGE_DESCRIPTOR = msg({
	message: 'Remove all {emojiNameWithColons} reactions from this message?',
	comment:
		'Confirmation prompt in the channel and chat message reactions sheet. Preserve {emojiNameWithColons}; it is inserted by code. Keep the tone plain and specific.',
});
const REMOVE_DESCRIPTOR = msg({
	message: 'Remove',
	comment:
		'Button or menu action label in the channel and chat message reactions sheet. Keep it concise. Keep the tone plain and specific.',
});
const REMOVE_REACTION_FROM_DESCRIPTOR = msg({
	message: 'Remove {emojiNameWithColons} reaction from {reactorName}?',
	comment:
		'Confirmation prompt in the channel and chat message reactions sheet. Preserve {emojiNameWithColons}, {reactorName}; they are inserted by code. Keep the tone plain and specific.',
});
const NO_REACTORS_FOR_THIS_REACTION_YET_DESCRIPTOR = msg({
	message: 'Nobody has reacted with this yet.',
	comment: 'Empty-state text in the channel and chat message reactions sheet.',
});
const LOADING_REACTIONS_DESCRIPTOR = msg({
	message: 'Loading reactions',
	comment: 'Short label in the channel and chat message reactions sheet. Keep it concise.',
});

interface MessageReactionsSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channelId: string;
	messageId: string;
	message?: Message | null;
	openToReaction?: MessageReaction;
}

export const MessageReactionsSheet = observer(
	({isOpen, onClose, channelId, messageId, message: messageFallback, openToReaction}: MessageReactionsSheetProps) => {
		const {i18n} = useLingui();
		const [reactionMenuTarget, setReactionMenuTarget] = useState<MessageReaction | null>(null);
		const {
			message,
			reactions,
			selectedReaction,
			setSelectedReaction,
			reactors,
			isLoading,
			hasMore,
			loadMore,
			canManageMessages,
			guildId,
			reactorScrollerKey,
		} = useMessageReactionsState({
			channelId,
			messageId,
			message: messageFallback,
			openToReaction,
			isOpen,
			onMissingMessage: onClose,
		});
		useEffect(() => {
			if (!isOpen) {
				setReactionMenuTarget(null);
			}
		}, [isOpen]);
		const closeReactionMenu = useCallback(() => {
			setReactionMenuTarget(null);
		}, []);
		const handleReactionLongPress = useCallback((reaction: MessageReaction) => {
			setReactionMenuTarget(reaction);
		}, []);
		const handleRemoveReactionEmoji = useCallback(
			(reaction: MessageReaction) => {
				ModalCommands.pushAfterBottomSheetClose(
					closeReactionMenu,
					modal(() => (
						<ConfirmModal
							title={i18n._(REMOVE_REACTION_DESCRIPTOR)}
							description={i18n._(REMOVE_ALL_REACTIONS_FROM_THIS_MESSAGE_DESCRIPTOR, {
								emojiNameWithColons: getEmojiNameWithColons(reaction.emoji),
							})}
							primaryText={i18n._(REMOVE_DESCRIPTOR)}
							onPrimary={() => ReactionCommands.removeReactionEmoji(i18n, channelId, messageId, reaction.emoji)}
							data-flx="channel.message-reactions-sheet.handle-remove-reaction-emoji.confirm-modal"
						/>
					)),
				);
			},
			[channelId, closeReactionMenu, i18n, messageId],
		);
		const handleRemoveReactor = useCallback(
			(reactor: User) => {
				if (!selectedReaction) {
					return;
				}
				const isOwnReaction = Authentication.currentUserId != null && reactor.id === Authentication.currentUserId;
				const reactorName = NicknameUtils.getNickname(reactor, guildId, channelId);
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(REMOVE_REACTION_DESCRIPTOR)}
							description={i18n._(REMOVE_REACTION_FROM_DESCRIPTOR, {
								emojiNameWithColons: getEmojiNameWithColons(selectedReaction.emoji),
								reactorName,
							})}
							primaryText={i18n._(REMOVE_DESCRIPTOR)}
							onPrimary={() =>
								ReactionCommands.removeReaction(
									i18n,
									channelId,
									messageId,
									selectedReaction.emoji,
									isOwnReaction ? undefined : reactor.id,
								)
							}
							data-flx="channel.message-reactions-sheet.handle-remove-reactor.confirm-modal"
						/>
					)),
				);
			},
			[channelId, guildId, i18n, messageId, selectedReaction],
		);
		const reactionMenuGroups = useMemo<Array<MenuGroupType>>(() => {
			if (!reactionMenuTarget || !canManageMessages) {
				return [];
			}
			const item: MenuItemType = {
				icon: (
					<TrashIcon size={remFromPx(20)} data-flx="channel.message-reactions-sheet.reaction-menu-groups.trash-icon" />
				),
				label: i18n._(REMOVE_REACTION_DESCRIPTOR),
				onClick: () => handleRemoveReactionEmoji(reactionMenuTarget),
				danger: true,
			};
			return [{items: [item]}];
		}, [canManageMessages, handleRemoveReactionEmoji, reactionMenuTarget, i18n.locale]);
		if (!message || !selectedReaction) {
			return null;
		}
		const selectedReactionName = getEmojiNameWithColons(selectedReaction.emoji);
		const reactorCountLabel = plural(
			{count: selectedReaction.count},
			{
				one: '# reactor',
				other: '# reactors',
			},
		);
		return (
			<>
				<BottomSheet
					isOpen={isOpen}
					onClose={onClose}
					title={i18n._(REACTIONS_DESCRIPTOR)}
					initialSnap={2}
					snapPoints={[0, 0.4, 0.75, 1]}
					data-flx="channel.message-reactions-sheet.bottom-sheet"
				>
					<div className={styles.sheetBody} data-flx="channel.message-reactions-sheet.sheet-body">
						<div className={styles.filterContainer} data-flx="channel.message-reactions-sheet.filter-container">
							<MessageReactionsFilters
								messageId={messageId}
								reactions={reactions}
								selectedReaction={selectedReaction}
								onSelectReaction={setSelectedReaction}
								canManageMessages={canManageMessages}
								variant="sheet"
								onReactionLongPress={canManageMessages ? handleReactionLongPress : undefined}
								data-flx="channel.message-reactions-sheet.message-reactions-filters"
							/>
						</div>
						<div className={styles.listHeader} data-flx="channel.message-reactions-sheet.list-header">
							<span data-flx="channel.message-reactions-sheet.span">{selectedReactionName}</span>
							<span className={styles.countBadge} data-flx="channel.message-reactions-sheet.count-badge">
								{reactorCountLabel}
							</span>
						</div>
						<MessageReactionsReactorsList
							channelId={channelId}
							reactors={reactors}
							isLoading={isLoading}
							hasMore={hasMore}
							onLoadMore={loadMore}
							canManageMessages={canManageMessages}
							currentUserId={Authentication.currentUserId}
							guildId={guildId}
							scrollerKey={reactorScrollerKey}
							emptyLabel={i18n._(NO_REACTORS_FOR_THIS_REACTION_YET_DESCRIPTOR)}
							loadingLabel={i18n._(LOADING_REACTIONS_DESCRIPTOR)}
							showLoadingLabel={true}
							onRemoveReactor={handleRemoveReactor}
							data-flx="channel.message-reactions-sheet.message-reactions-reactors-list"
						/>
					</div>
				</BottomSheet>
				<MenuBottomSheet
					isOpen={Boolean(reactionMenuTarget)}
					onClose={closeReactionMenu}
					groups={reactionMenuGroups}
					showCloseButton={true}
					data-flx="channel.message-reactions-sheet.menu-bottom-sheet"
				/>
			</>
		);
	},
);
