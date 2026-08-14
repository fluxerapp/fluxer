// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/MessageActionBottomSheet.module.css';
import {useMessageActionMenuData} from '@app/features/channel/components/MessageActionMenu';
import {MessageReactionsSheet} from '@app/features/channel/components/MessageReactionsSheet';
import {
	getQuickReactionEmojiSrc,
	REACT_WITH_EMOJI_DESCRIPTOR,
	renderQuickReactionEmoji,
} from '@app/features/channel/components/QuickReactionsRow';
import quickReactionStyles from '@app/features/channel/components/QuickReactionsRow.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import EmojiPicker from '@app/features/emoji/state/EmojiPicker';
import {ExpressionPickerSheet} from '@app/features/expressions/components/modals/ExpressionPickerSheet';
import {useExpressionImagesPreload} from '@app/features/expressions/utils/ExpressionImageCache';
import {COPY_LINK_DESCRIPTOR, OPEN_LINK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useMessageReactions as useMessageReactionsSnapshot} from '@app/features/messaging/hooks/useMessageReactionStore';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {getEmojiNameWithColons, toReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {CopyLinkIcon, OpenLinkIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import type {MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';

const ADD_ANOTHER_REACTION_DESCRIPTOR = msg({
	message: 'Add another reaction',
	comment: 'Button or menu action label in the channel and chat message action bottom sheet. Keep it concise.',
});

interface MessageActionBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	message: Message;
	sourceChannel?: Channel | null;
	handleDelete: (bypassConfirm?: boolean) => void;
	linkUrl?: string;
}

export const MessageActionBottomSheet: React.FC<MessageActionBottomSheetProps> = observer(
	({isOpen, onClose, message, sourceChannel, handleDelete, linkUrl}) => {
		const {i18n} = useLingui();
		const [isEmojiPickerOpen, setIsEmojiPickerOpen] = useState(false);
		const [isReactionsSheetOpen, setIsReactionsSheetOpen] = useState(false);
		const reactions = useMessageReactionsSnapshot(message.id);
		const handleOpenLink = useCallback(() => {
			if (!linkUrl) return;
			void openExternalUrl(linkUrl);
			onClose();
		}, [linkUrl, onClose]);
		const handleCopyLink = useCallback(async () => {
			if (!linkUrl) return;
			await TextCopyCommands.copy(i18n, linkUrl);
			onClose();
		}, [i18n, linkUrl, onClose]);
		const handleAddReaction = useCallback(() => {
			setIsEmojiPickerOpen(true);
		}, []);
		const handleOpenReactionsSheet = useCallback(() => {
			setIsReactionsSheetOpen(true);
		}, []);
		const handleReactionsSheetClose = useCallback(() => {
			setIsReactionsSheetOpen(false);
			onClose();
		}, [onClose]);
		const handleEmojiPickerClose = useCallback(() => {
			setIsEmojiPickerOpen(false);
			onClose();
		}, [onClose]);
		const {groups, handlers, quickReactionEmojis, quickReactionRowVisible} = useMessageActionMenuData(message, {
			onClose,
			onDelete: () => handleDelete(),
			sourceChannel,
			onOpenEmojiPicker: handleAddReaction,
			onOpenReactionsSheet: handleOpenReactionsSheet,
			quickReactionCount: 4,
		});
		const linkGroups = useMemo<Array<MenuGroupType>>(() => {
			if (!linkUrl) {
				return [];
			}
			return [
				{
					items: [
						{
							id: 'open_link',
							icon: (
								<OpenLinkIcon size={20} data-flx="channel.message-action-bottom-sheet.link-groups.open-link-icon" />
							),
							label: i18n._(OPEN_LINK_DESCRIPTOR),
							onClick: handleOpenLink,
						},
						{
							id: 'copy_link',
							icon: (
								<CopyLinkIcon size={20} data-flx="channel.message-action-bottom-sheet.link-groups.copy-link-icon" />
							),
							label: i18n._(COPY_LINK_DESCRIPTOR),
							onClick: handleCopyLink,
						},
					],
				},
			];
		}, [handleCopyLink, handleOpenLink, i18n.locale, linkUrl]);
		const quickReactionImageUrls = useMemo(
			() => quickReactionEmojis.map((emoji) => getQuickReactionEmojiSrc(emoji)),
			[quickReactionEmojis],
		);
		useExpressionImagesPreload(quickReactionImageUrls);
		const visibleGroups = useMemo(
			() => [...linkGroups, ...groups].filter((group) => group.items.length > 0),
			[groups, linkGroups],
		);
		const quickReactionRow = quickReactionRowVisible ? (
			<div
				className={styles.quickReactionWrapper}
				data-flx="channel.message-action-bottom-sheet.quick-reaction-wrapper"
			>
				<div className={quickReactionStyles.row} data-flx="channel.message-action-bottom-sheet.div">
					{quickReactionEmojis.map((emoji) => {
						const emojiShortcode = getEmojiNameWithColons(toReactionEmoji(emoji));
						return (
							<button
								key={emoji.name}
								type="button"
								onClick={() => {
									EmojiPicker.trackEmoji(emoji);
									handlers.handleEmojiSelect(emoji);
									onClose();
								}}
								aria-label={i18n._(REACT_WITH_EMOJI_DESCRIPTOR, {emojiShortcode})}
								className={quickReactionStyles.button}
								data-flx="channel.message-action-bottom-sheet.button.track-emoji"
							>
								{renderQuickReactionEmoji(emoji)}
							</button>
						);
					})}
					<button
						type="button"
						onClick={handleAddReaction}
						aria-label={i18n._(ADD_ANOTHER_REACTION_DESCRIPTOR)}
						className={quickReactionStyles.button}
						data-flx="channel.message-action-bottom-sheet.button.add-reaction"
					>
						<PlusIcon size={remFromPx(24)} weight="bold" data-flx="channel.message-action-bottom-sheet.plus-icon" />
					</button>
				</div>
			</div>
		) : null;
		return (
			<>
				<MenuBottomSheet
					isOpen={isOpen && !isEmojiPickerOpen && !isReactionsSheetOpen}
					onClose={onClose}
					groups={visibleGroups}
					headerContent={quickReactionRow}
					data-flx="channel.message-action-bottom-sheet.menu-bottom-sheet"
				/>
				<ExpressionPickerSheet
					isOpen={isEmojiPickerOpen}
					onClose={handleEmojiPickerClose}
					channelId={message.channelId}
					onEmojiSelect={handlers.handleEmojiSelect}
					visibleTabs={['emojis']}
					data-flx="channel.message-action-bottom-sheet.expression-picker-sheet"
				/>
				<MessageReactionsSheet
					isOpen={isReactionsSheetOpen}
					onClose={handleReactionsSheetClose}
					channelId={message.channelId}
					messageId={message.id}
					message={message}
					openToReaction={reactions[0]}
					data-flx="channel.message-action-bottom-sheet.message-reactions-sheet"
				/>
			</>
		);
	},
);
