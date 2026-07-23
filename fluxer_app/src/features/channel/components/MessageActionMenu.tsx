// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	canReportMessage,
	createMessageActionHandlers,
	getCopyableMessageText,
	getEffectiveContent,
	isClientSystemMessage,
	isEmbedsSuppressed,
	type MessageActionHandlers,
	type MessagePermissions,
	requestSpeakMessage,
	useMessagePermissions,
} from '@app/features/channel/components/MessageActionUtils';
import {useQuickReactionEmojis} from '@app/features/channel/state/QuickReactionStore';
import {MessageDebugModal} from '@app/features/devtools/components/debug/MessageDebugModal';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {
	ADD_REACTION_DESCRIPTOR,
	BOOKMARK_MESSAGE_DESCRIPTOR,
	COPY_MESSAGE_ID_DESCRIPTOR,
	COPY_MESSAGE_LINK_DESCRIPTOR,
	DELETE_MESSAGE_DESCRIPTOR,
	EDIT_MESSAGE_DESCRIPTOR,
	MARK_AS_UNREAD_DESCRIPTOR,
	PIN_MESSAGE_DESCRIPTOR,
	REMOVE_BOOKMARK_DESCRIPTOR,
	REPLY_DESCRIPTOR,
	REPORT_MESSAGE_DESCRIPTOR,
	SUPPRESS_EMBEDS_DESCRIPTOR,
	TRY_AGAIN_DESCRIPTOR,
	UNPIN_MESSAGE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useMessageReactions as useMessageReactionsSnapshot} from '@app/features/messaging/hooks/useMessageReactionStore';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import {openReportMessageModal} from '@app/features/moderation/utils/ReportActionUtils';
import Permission from '@app/features/permissions/state/Permission';
import {
	AddReactionIcon,
	BookmarkIcon,
	CopyIdIcon,
	CopyLinkIcon,
	CopyMessageTextIcon,
	DebugMessageIcon,
	DeleteIcon,
	EditMessageIcon,
	ForwardIcon,
	MarkAsUnreadIcon,
	PinIcon,
	RemoveAllReactionsIcon,
	ReplyIcon,
	ReportMessageIcon,
	RetryIcon,
	SpeakMessageIcon,
	StopSpeakingIcon,
	SuppressEmbedsIcon,
	ViewReactionsIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {ThreadIcon} from '@app/features/ui/components/icons/ThreadIcon';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Channels from '@app/features/channel/state/Channels';
import Threads from '@app/features/channel/state/Threads';
import {KeybindHint} from '@app/features/ui/keybind_hint/KeybindHint';
import type {MenuGroupType, MenuItemType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import UserSettings from '@app/features/user/state/UserSettings';
import TtsUtils from '@app/features/voice/utils/VoiceTtsUtils';
import {MessageStates, MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useEffect, useMemo, useState} from 'react';

const MESSAGE_DEBUG_DESCRIPTOR = msg({
	message: 'Message debug',
	comment: 'Title of the developer-mode message debug modal opened from the message context menu.',
});
const VIEW_REACTIONS_DESCRIPTOR = msg({
	message: 'View reactions',
	comment: 'Message context menu item that opens the reactions list sheet for the message.',
});
const REMOVE_ALL_REACTIONS_DESCRIPTOR = msg({
	message: 'Remove all reactions',
	comment: 'Destructive message context menu item for moderators that clears every reaction on a message.',
});
const FORWARD_DESCRIPTOR = msg({
	message: 'Forward',
	comment: 'Message context menu item that opens the forward-to-channel picker.',
});
const CREATE_THREAD_DESCRIPTOR = msg({
	message: 'Create Thread',
	comment: 'Message context menu item that opens the create thread modal.',
});
const VIEW_THREAD_MENU_DESCRIPTOR = msg({
	message: 'View Thread',
	comment: 'Message context menu item that navigates to an existing thread.',
});
const UNSUPPRESS_EMBEDS_DESCRIPTOR = msg({
	message: 'Unsuppress embeds',
	comment: 'Message context menu item that re-shows previously hidden link previews and embeds on the message.',
});
const COPY_MESSAGE_DESCRIPTOR = msg({
	message: 'Copy message',
	comment: 'Message context menu item that copies the message text content to the clipboard.',
});
const STOP_SPEAKING_DESCRIPTOR = msg({
	message: 'Stop speaking',
	comment: 'Message context menu item that stops the text-to-speech playback in progress.',
});
const SPEAK_MESSAGE_DESCRIPTOR = msg({
	message: 'Speak message',
	comment: 'Message context menu item that reads the message aloud via text-to-speech.',
});
const DEBUG_MESSAGE_DESCRIPTOR = msg({
	message: 'Debug message',
	comment: 'Developer-mode message context menu item that opens an internal debug view for the message.',
});

interface MessageActionMenuOptions {
	onOpenEmojiPicker?: () => void;
	onOpenReactionsSheet?: () => void;
	onClose?: () => void;
	onDelete?: (bypassConfirm?: boolean) => void;
	sourceChannel?: MessagePermissions['channel'] | null;
	quickReactionCount?: number;
	submenuReactionCount?: number;
}

export const messageActionMenuItemIds = {
	addReaction: 'add-reaction',
	viewReactions: 'view_reactions',
	removeAllReactions: 'remove_all_reactions',
	reply: 'reply',
	forward: 'forward',
	createThread: 'create_thread',
	edit: 'edit',
	pinMessage: 'message_pin',
	bookmarkMessage: 'message_bookmark',
	suppressEmbeds: 'suppress_embeds',
	markUnread: 'message_mark_unread',
	speakMessage: 'message_speak',
	deleteMessage: 'message_delete',
	reportMessage: 'report_message',
	copyMessage: 'copy_message',
	copyMessageLink: 'message_copy_link',
	copyMessageId: 'message_copy_id',
	debugMessage: 'debug_message',
} as const;

export interface MessageActionMenuData {
	handlers: MessageActionHandlers;
	permissions: MessagePermissions | null;
	groups: Array<MenuGroupType>;
	quickReactionEmojis: Array<FlatEmoji>;
	submenuReactionEmojis: Array<FlatEmoji>;
	quickReactionRowVisible: boolean;
	isFailed: boolean;
	isSaved: boolean;
}

export const useMessageActionMenuData = (
	message: Message,
	options: MessageActionMenuOptions = {},
): MessageActionMenuData => {
	const {i18n} = useLingui();
	const {
		onOpenEmojiPicker,
		onOpenReactionsSheet,
		onClose,
		onDelete,
		sourceChannel,
		quickReactionCount = 5,
		submenuReactionCount = 16,
	} = options;
	const permissions = useMessagePermissions(message, sourceChannel);
	const handlers = useMemo(
		() => createMessageActionHandlers(message, {i18n, onClose, channel: permissions?.channel ?? sourceChannel}),
		[message, permissions?.channel, sourceChannel, onClose, i18n.locale],
	);
	const isSaved = useMemo(() => SavedMessages.isSaved(message.id), [message.id]);
	const channel = permissions?.channel ?? sourceChannel ?? null;
	const shouldBuildQuickReactions = permissions?.canAddReactions === true && message.state === MessageStates.SENT;
	const reactionEmojiLimit = Math.max(quickReactionCount, submenuReactionCount);
	const reactionEmojis = useQuickReactionEmojis(channel, reactionEmojiLimit, shouldBuildQuickReactions);
	const quickReactionEmojis = useMemo(
		() => reactionEmojis.slice(0, quickReactionCount),
		[reactionEmojis, quickReactionCount],
	);
	const submenuReactionEmojis = useMemo(
		() => reactionEmojis.slice(0, submenuReactionCount),
		[reactionEmojis, submenuReactionCount],
	);
	const developerMode = UserSettings.developerMode;
	const effectiveContent = useMemo(() => getEffectiveContent(message), [message]);
	const copyableMessageText = useMemo(() => getCopyableMessageText(message, i18n), [message, i18n.locale]);
	const canManageMessages = useMemo(
		() =>
			permissions != null &&
			!permissions.isDM &&
			Permission.can(Permissions.MANAGE_MESSAGES, {channelId: message.channelId}),
		[permissions, message.channelId],
	);
	const [isSpeaking, setIsSpeaking] = useState(TtsUtils.isSpeaking());
	const [voiceReady, setVoiceReady] = useState(TtsUtils.hasVoices());
	const supportsInteractiveActions = !isClientSystemMessage(message) && message.type !== MessageTypes.THREAD_STARTER_MESSAGE;
	useEffect(() => {
		if (!TtsUtils.isSupported()) {
			return;
		}
		const interval = setInterval(() => {
			setIsSpeaking(TtsUtils.isSpeaking());
			setVoiceReady(TtsUtils.hasVoices());
		}, 100);
		return () => clearInterval(interval);
	}, []);
	const handleSpeakMessage = useCallback(() => {
		requestSpeakMessage(message);
	}, [message]);
	const handleReportMessage = useCallback(() => {
		if (!canReportMessage(message)) {
			return;
		}
		if (onClose) {
			ModalCommands.runAfterBottomSheetClose(onClose, () => openReportMessageModal(message));
			return;
		}
		openReportMessageModal(message);
	}, [message, onClose]);
	const handleCreateThread = useCallback(() => {
		const open = () => {
			void import('@app/features/channel/state/ThreadCreation').then(({default: ThreadCreation}) =>
				ThreadCreation.open({
					channelId: message.channelId,
					sourceMessageId: message.id,
					sourceMessagePreview: message.content ? message.content.slice(0, 120) : undefined,
					sourceMessageAuthor: message.author.username,
				}),
			);
		};
		if (onClose) {
			ModalCommands.runAfterBottomSheetClose(onClose, open);
			return;
		}
		open();
	}, [message, onClose]);
	const handleViewThread = useCallback(() => {
		if (!message.threadId) return;
		const ch = Channels.getChannel(message.channelId);
		const navigate = () => {
			if (ch?.guildId) {
				if (!Threads.isJoined(message.threadId!)) {
					void import('@app/features/channel/commands/ThreadCommands').then(({join}) =>
						join(message.channelId, message.threadId!),
					);
				}
				NavigationCommands.selectThread(ch.guildId, message.channelId, message.threadId!);
			}
		};
		if (onClose) {
			ModalCommands.runAfterBottomSheetClose(onClose, navigate);
			return;
		}
		navigate();
	}, [message.threadId, message.channelId, onClose]);
	const handleDebugMessage = useCallback(() => {
		const messageDebugModal = modal(() => (
			<MessageDebugModal
				title={i18n._(MESSAGE_DEBUG_DESCRIPTOR)}
				message={message}
				data-flx="channel.message-action-menu.handle-debug-message.message-debug-modal"
			/>
		));
		if (onClose) {
			ModalCommands.pushWithKeyAfterBottomSheetClose(onClose, messageDebugModal, `message-debug-${message.id}`);
			return;
		}
		ModalCommands.pushWithKey(messageDebugModal, `message-debug-${message.id}`);
	}, [message, onClose, i18n]);
	const reactions = useMessageReactionsSnapshot(message.id);
	const groups = useMemo(() => {
		const reactionActions: Array<MenuItemType> = [];
		const interactionActions: Array<MenuItemType> = [];
		const managementActions: Array<MenuItemType> = [];
		const utilityActions: Array<MenuItemType> = [];
		if (message.type === MessageTypes.THREAD_STARTER_MESSAGE) {
			return [];
		}
		if (message.state === MessageStates.SENT) {
			if (permissions?.canAddReactions && onOpenEmojiPicker) {
				reactionActions.push({
					id: messageActionMenuItemIds.addReaction,
					icon: <AddReactionIcon size={20} data-flx="channel.message-action-menu.groups.add-reaction-icon" />,
					label: i18n._(ADD_REACTION_DESCRIPTOR),
					onClick: onOpenEmojiPicker,
					shortcut: <KeybindHint action="message_react" data-flx="channel.message-action-menu.groups.keybind-hint" />,
				});
			}
			if (reactions.length > 0 && onOpenReactionsSheet) {
				reactionActions.push({
					id: messageActionMenuItemIds.viewReactions,
					icon: <ViewReactionsIcon size={20} data-flx="channel.message-action-menu.groups.view-reactions-icon" />,
					label: i18n._(VIEW_REACTIONS_DESCRIPTOR),
					onClick: onOpenReactionsSheet,
				});
			}
			if (canManageMessages && reactions.length > 0) {
				reactionActions.push({
					id: messageActionMenuItemIds.removeAllReactions,
					icon: (
						<RemoveAllReactionsIcon size={20} data-flx="channel.message-action-menu.groups.remove-all-reactions-icon" />
					),
					label: i18n._(REMOVE_ALL_REACTIONS_DESCRIPTOR),
					onClick: handlers.handleRemoveAllReactions,
					danger: true,
				});
			}
			interactionActions.push({
				id: messageActionMenuItemIds.markUnread,
				icon: <MarkAsUnreadIcon size={20} data-flx="channel.message-action-menu.groups.mark-as-unread-icon" />,
				label: i18n._(MARK_AS_UNREAD_DESCRIPTOR),
				onClick: handlers.handleMarkAsUnread,
				shortcut: (
					<KeybindHint action="message_mark_unread" data-flx="channel.message-action-menu.groups.keybind-hint--2" />
				),
			});
			if (message.isUserMessage() && supportsInteractiveActions && permissions?.canSendMessages) {
				interactionActions.push({
					id: messageActionMenuItemIds.reply,
					icon: <ReplyIcon size={20} data-flx="channel.message-action-menu.groups.reply-icon" />,
					label: i18n._(REPLY_DESCRIPTOR),
					onClick: handlers.handleReply,
					shortcut: (
						<KeybindHint action="message_reply" data-flx="channel.message-action-menu.groups.keybind-hint--3" />
					),
				});
			}
			if (message.isUserMessage() && supportsInteractiveActions && permissions?.canForwardMessage) {
				interactionActions.push({
					id: messageActionMenuItemIds.forward,
					icon: <ForwardIcon size={20} data-flx="channel.message-action-menu.groups.forward-icon" />,
					label: i18n._(FORWARD_DESCRIPTOR),
					onClick: handlers.handleForward,
					shortcut: (
						<KeybindHint action="message_forward" data-flx="channel.message-action-menu.groups.keybind-hint--4" />
					),
				});
			}
			if (message.isUserMessage() && supportsInteractiveActions && !message.threadId && permissions?.canCreateThread) {
				interactionActions.push({
					id: messageActionMenuItemIds.createThread,
					icon: <ThreadIcon size={20} data-flx="channel.message-action-menu.groups.create-thread-icon" />,
					label: i18n._(CREATE_THREAD_DESCRIPTOR),
					onClick: handleCreateThread,
				});
			}
			if (message.isUserMessage() && supportsInteractiveActions && !!message.threadId) {
				interactionActions.push({
					id: messageActionMenuItemIds.createThread,
					icon: <ThreadIcon size={20} data-flx="channel.message-action-menu.groups.view-thread-icon" />,
					label: i18n._(VIEW_THREAD_MENU_DESCRIPTOR),
					onClick: handleViewThread,
				});
			}
			if (message.isCurrentUserAuthor() && message.isUserMessage() && !message.messageSnapshots) {
				interactionActions.push({
					id: messageActionMenuItemIds.edit,
					icon: <EditMessageIcon size={20} data-flx="channel.message-action-menu.groups.edit-message-icon" />,
					label: i18n._(EDIT_MESSAGE_DESCRIPTOR),
					onClick: handlers.handleEditMessage,
					shortcut: <KeybindHint action="message_edit" data-flx="channel.message-action-menu.groups.keybind-hint--5" />,
				});
			}
			if (message.isUserMessage() && permissions?.canPinMessage) {
				managementActions.push({
					id: messageActionMenuItemIds.pinMessage,
					icon: <PinIcon size={20} data-flx="channel.message-action-menu.groups.pin-icon" />,
					label: message.pinned ? i18n._(UNPIN_MESSAGE_DESCRIPTOR) : i18n._(PIN_MESSAGE_DESCRIPTOR),
					onClick: handlers.handlePinMessage,
					shortcut: <KeybindHint action="message_pin" data-flx="channel.message-action-menu.groups.keybind-hint--6" />,
				});
			}
			if (message.isUserMessage() && supportsInteractiveActions) {
				managementActions.push({
					id: messageActionMenuItemIds.bookmarkMessage,
					icon: <BookmarkIcon size={20} filled={isSaved} data-flx="channel.message-action-menu.groups.bookmark-icon" />,
					label: isSaved ? i18n._(REMOVE_BOOKMARK_DESCRIPTOR) : i18n._(BOOKMARK_MESSAGE_DESCRIPTOR),
					onClick: handlers.handleSaveMessage(isSaved),
					shortcut: (
						<KeybindHint action="message_bookmark" data-flx="channel.message-action-menu.groups.keybind-hint--7" />
					),
				});
			}
			if (permissions?.shouldRenderSuppressEmbeds) {
				managementActions.push({
					id: messageActionMenuItemIds.suppressEmbeds,
					icon: <SuppressEmbedsIcon size={20} data-flx="channel.message-action-menu.groups.suppress-embeds-icon" />,
					label: isEmbedsSuppressed(message)
						? i18n._(UNSUPPRESS_EMBEDS_DESCRIPTOR)
						: i18n._(SUPPRESS_EMBEDS_DESCRIPTOR),
					onClick: handlers.handleToggleSuppressEmbeds,
					shortcut: (
						<KeybindHint action="message_toggle_embeds" data-flx="channel.message-action-menu.groups.keybind-hint--8" />
					),
				});
			}
			if (permissions?.canDeleteMessage && onDelete) {
				managementActions.push({
					id: messageActionMenuItemIds.deleteMessage,
					icon: <DeleteIcon size={20} data-flx="channel.message-action-menu.groups.delete-icon" />,
					label: i18n._(DELETE_MESSAGE_DESCRIPTOR),
					onClick: (event: {shiftKey?: boolean}) => {
						const deleteMessage = () => onDelete(event?.shiftKey === true);
						if (onClose) {
							ModalCommands.runAfterBottomSheetClose(onClose, deleteMessage);
							return;
						}
						deleteMessage();
					},
					danger: true,
					shortcut: (
						<KeybindHint action="message_delete" data-flx="channel.message-action-menu.groups.keybind-hint--9" />
					),
				});
			}
			if (supportsInteractiveActions) {
				utilityActions.push({
					id: messageActionMenuItemIds.copyMessageLink,
					icon: <CopyLinkIcon size={20} data-flx="channel.message-action-menu.groups.copy-link-icon" />,
					label: i18n._(COPY_MESSAGE_LINK_DESCRIPTOR),
					onClick: handlers.handleCopyMessageLink,
					shortcut: (
						<KeybindHint action="message_copy_link" data-flx="channel.message-action-menu.groups.keybind-hint--10" />
					),
				});
			}
			if (copyableMessageText) {
				utilityActions.push({
					id: messageActionMenuItemIds.copyMessage,
					icon: <CopyMessageTextIcon size={20} data-flx="channel.message-action-menu.groups.copy-message-text-icon" />,
					label: i18n._(COPY_MESSAGE_DESCRIPTOR),
					onClick: handlers.handleCopyMessage,
					shortcut: (
						<KeybindHint action="message_copy_text" data-flx="channel.message-action-menu.groups.keybind-hint--11" />
					),
				});
			}
			if (TtsUtils.isSupported() && voiceReady && effectiveContent.trim()) {
				utilityActions.push({
					id: messageActionMenuItemIds.speakMessage,
					icon: isSpeaking ? (
						<StopSpeakingIcon size={20} data-flx="channel.message-action-menu.groups.stop-speaking-icon" />
					) : (
						<SpeakMessageIcon size={20} data-flx="channel.message-action-menu.groups.speak-message-icon" />
					),
					label: isSpeaking ? i18n._(STOP_SPEAKING_DESCRIPTOR) : i18n._(SPEAK_MESSAGE_DESCRIPTOR),
					onClick: handleSpeakMessage,
					closeOnSelect: false,
					shortcut: (
						<KeybindHint action="message_speak" data-flx="channel.message-action-menu.groups.keybind-hint--12" />
					),
				});
			}
			utilityActions.push({
				id: messageActionMenuItemIds.copyMessageId,
				icon: <CopyIdIcon size={20} data-flx="channel.message-action-menu.groups.copy-id-icon" />,
				label: i18n._(COPY_MESSAGE_ID_DESCRIPTOR),
				onClick: handlers.handleCopyMessageId,
				shortcut: (
					<KeybindHint action="message_copy_id" data-flx="channel.message-action-menu.groups.keybind-hint--13" />
				),
			});
			if (developerMode) {
				utilityActions.push({
					id: messageActionMenuItemIds.debugMessage,
					icon: <DebugMessageIcon size={20} data-flx="channel.message-action-menu.groups.debug-message-icon" />,
					label: i18n._(DEBUG_MESSAGE_DESCRIPTOR),
					onClick: handleDebugMessage,
				});
			}
		} else if (message.state === MessageStates.FAILED) {
			interactionActions.push({
				icon: <RetryIcon size={20} data-flx="channel.message-action-menu.groups.retry-icon" />,
				label: i18n._(TRY_AGAIN_DESCRIPTOR),
				onClick: handlers.handleRetryMessage,
			});
			managementActions.push({
				id: messageActionMenuItemIds.deleteMessage,
				icon: <DeleteIcon size={20} data-flx="channel.message-action-menu.groups.delete-icon--2" />,
				label: i18n._(DELETE_MESSAGE_DESCRIPTOR),
				onClick: handlers.handleFailedMessageDelete,
				danger: true,
			});
		}
		const groups: Array<MenuGroupType> = [
			{items: reactionActions},
			{items: interactionActions},
			{items: managementActions},
			{items: utilityActions},
		];
		if (canReportMessage(message)) {
			groups.push({
				items: [
					{
						id: messageActionMenuItemIds.reportMessage,
						icon: <ReportMessageIcon size={20} data-flx="channel.message-action-menu.groups.report-message-icon" />,
						label: i18n._(REPORT_MESSAGE_DESCRIPTOR),
						onClick: handleReportMessage,
						danger: true,
					},
				],
			});
		}
		return groups;
	}, [
		message,
		handlers,
		isSaved,
		onClose,
		onDelete,
		onOpenEmojiPicker,
		onOpenReactionsSheet,
		reactions.length,
		permissions,
		developerMode,
		canManageMessages,
		supportsInteractiveActions,
		isSpeaking,
		voiceReady,
		effectiveContent,
		copyableMessageText,
		handleSpeakMessage,
		handleReportMessage,
		handleDebugMessage,
		handleCreateThread,
		handleViewThread,
		i18n.locale,
	]);
	const quickReactionRowVisible =
		permissions?.canAddReactions === true && message.state === MessageStates.SENT && quickReactionEmojis.length > 0;
	return {
		handlers,
		permissions,
		groups,
		quickReactionEmojis,
		submenuReactionEmojis,
		quickReactionRowVisible,
		isFailed: message.state === MessageStates.FAILED,
		isSaved,
	};
};
