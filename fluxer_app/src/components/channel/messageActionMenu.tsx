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
import {ArrowsClockwiseIcon} from '@phosphor-icons/react';
import React from 'react';
import {MessageStates} from '~/Constants';
import {
	createMessageActionHandlers,
	isEmbedsSuppressed,
	useMessagePermissions,
} from '~/components/channel/messageActionUtils';
import {
	AddReactionIcon,
	BookmarkIcon,
	CopyIdIcon,
	CopyLinkIcon,
	CopyTextIcon,
	DeleteIcon,
	EditIcon,
	ForwardIcon,
	MarkAsUnreadIcon,
	PinIcon,
	ReplyIcon,
	SuppressEmbedsIcon,
} from '~/components/uikit/ContextMenu/ContextMenuIcons';
import type {MenuGroupType, MenuItemType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import type {MessageRecord} from '~/records/MessageRecord';
import ChannelStore from '~/stores/ChannelStore';
import EmojiPickerStore from '~/stores/EmojiPickerStore';
import type {Emoji} from '~/stores/EmojiStore';
import EmojiStore from '~/stores/EmojiStore';
import SavedMessagesStore from '~/stores/SavedMessagesStore';

interface MessageActionMenuOptions {
	onOpenEmojiPicker?: () => void;
	onClose?: () => void;
	onDelete?: () => void;
	quickReactionCount?: number;
}

export interface MessageActionMenuData {
	handlers: ReturnType<typeof createMessageActionHandlers>;
	permissions: ReturnType<typeof useMessagePermissions>;
	groups: Array<MenuGroupType>;
	quickReactionEmojis: Array<Emoji>;
	quickReactionRowVisible: boolean;
	isFailed: boolean;
	isSaved: boolean;
}

export const useMessageActionMenuData = (
	message: MessageRecord,
	options: MessageActionMenuOptions = {},
): MessageActionMenuData => {
	const {t} = useLingui();
	const {onOpenEmojiPicker, onClose, onDelete, quickReactionCount = 5} = options;
	const permissions = useMessagePermissions(message);
	const handlers = React.useMemo(() => createMessageActionHandlers(message, {onClose}), [message, onClose]);
	const isSaved = React.useMemo(() => SavedMessagesStore.isSaved(message.id), [message.id]);
	const channel = React.useMemo(() => ChannelStore.getChannel(message.channelId) ?? null, [message.channelId]);
	const allEmojis = React.useMemo(() => EmojiStore.search(channel, ''), [channel]);
	const quickReactionEmojis = React.useMemo(
		() => EmojiPickerStore.getQuickReactionEmojis(allEmojis, quickReactionCount),
		[allEmojis, quickReactionCount],
	);

	const groups = React.useMemo(() => {
		const interactionActions: Array<MenuItemType> = [];
		const managementActions: Array<MenuItemType> = [];
		const utilityActions: Array<MenuItemType> = [];

		if (message.state === MessageStates.SENT) {
			if (permissions.canAddReactions && onOpenEmojiPicker) {
				interactionActions.push({
					id: 'add-reaction',
					icon: <AddReactionIcon size={20} />,
					label: t`Add Reaction`,
					onClick: onOpenEmojiPicker,
				});
			}

			interactionActions.push({
				icon: <MarkAsUnreadIcon size={20} />,
				label: t`Mark as Unread`,
				onClick: handlers.handleMarkAsUnread,
			});

			if (message.isUserMessage() && permissions.canSendMessages) {
				interactionActions.push({
					id: 'reply',
					icon: <ReplyIcon size={20} />,
					label: t`Reply`,
					onClick: handlers.handleReply,
				});
			}

			if (message.isUserMessage()) {
				interactionActions.push({
					icon: <ForwardIcon size={20} />,
					label: t`Forward`,
					onClick: handlers.handleForward,
				});
			}

			if (message.isCurrentUserAuthor() && message.isUserMessage() && !message.messageSnapshots) {
				interactionActions.push({
					id: 'edit',
					icon: <EditIcon size={20} />,
					label: t`Edit Message`,
					onClick: handlers.handleEditMessage,
				});
			}

			if (message.isUserMessage() && permissions.canPinMessage) {
				managementActions.push({
					icon: <PinIcon size={20} />,
					label: message.pinned ? t`Unpin Message` : t`Pin Message`,
					onClick: handlers.handlePinMessage,
				});
			}

			if (message.isUserMessage()) {
				managementActions.push({
					icon: <BookmarkIcon size={20} filled={isSaved} />,
					label: isSaved ? t`Remove Bookmark` : t`Bookmark Message`,
					onClick: handlers.handleSaveMessage(isSaved),
				});
			}

			if (permissions.shouldRenderSuppressEmbeds) {
				managementActions.push({
					icon: <SuppressEmbedsIcon size={20} />,
					label: isEmbedsSuppressed(message) ? t`Unsuppress Embeds` : t`Suppress Embeds`,
					onClick: handlers.handleToggleSuppressEmbeds,
				});
			}

			if (permissions.canDeleteMessage && onDelete) {
				managementActions.push({
					icon: <DeleteIcon size={20} />,
					label: t`Delete Message`,
					onClick: () => {
						onClose?.();
						onDelete();
					},
					danger: true,
				});
			}

			utilityActions.push({
				icon: <CopyLinkIcon size={20} />,
				label: t`Copy Message Link`,
				onClick: handlers.handleCopyMessageLink,
			});

			if (message.content) {
				utilityActions.push({
					icon: <CopyTextIcon size={20} />,
					label: t`Copy Message`,
					onClick: handlers.handleCopyMessage,
				});
			}

			utilityActions.push({
				icon: <CopyIdIcon size={20} />,
				label: t`Copy Message ID`,
				onClick: handlers.handleCopyMessageId,
			});
		} else if (message.state === MessageStates.FAILED) {
			interactionActions.push({
				icon: <ArrowsClockwiseIcon size={20} />,
				label: t`Retry`,
				onClick: handlers.handleRetryMessage,
			});

			managementActions.push({
				icon: <DeleteIcon size={20} />,
				label: t`Delete Message`,
				onClick: handlers.handleFailedMessageDelete,
				danger: true,
			});
		}

		const groups: Array<MenuGroupType> = [];
		if (interactionActions.length > 0) groups.push({items: interactionActions});
		if (managementActions.length > 0) groups.push({items: managementActions});
		if (utilityActions.length > 0) groups.push({items: utilityActions});

		return groups;
	}, [message, handlers, isSaved, onClose, onDelete, onOpenEmojiPicker, permissions]);

	const quickReactionRowVisible =
		permissions.canAddReactions && message.state === MessageStates.SENT && quickReactionEmojis.length > 0;

	return {
		handlers,
		permissions,
		groups,
		quickReactionEmojis,
		quickReactionRowVisible,
		isFailed: message.state === MessageStates.FAILED,
		isSaved,
	};
};
