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

import * as ChannelPinActionCreators from '@app/actions/ChannelPinsActionCreators';
import * as MessageActionCreators from '@app/actions/MessageActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as ReactionActionCreators from '@app/actions/ReactionActionCreators';
import * as ReadStateActionCreators from '@app/actions/ReadStateActionCreators';
import * as SavedMessageActionCreators from '@app/actions/SavedMessageActionCreators';
import * as TextCopyActionCreators from '@app/actions/TextCopyActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {useMaybeMessageViewContext} from '@app/components/channel/MessageViewContext';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import {ForwardModal} from '@app/components/modals/ForwardModal';
import {CloudUpload} from '@app/lib/CloudUpload';
import {ComponentDispatch} from '@app/lib/ComponentDispatch';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import type {MessageRecord} from '@app/records/MessageRecord';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import ChannelStore from '@app/stores/ChannelStore';
import GuildMemberStore from '@app/stores/GuildMemberStore';
import GuildStore from '@app/stores/GuildStore';
import GuildVerificationStore from '@app/stores/GuildVerificationStore';
import MobileLayoutStore from '@app/stores/MobileLayoutStore';
import PermissionStore from '@app/stores/PermissionStore';
import RelationshipStore from '@app/stores/RelationshipStore';
import SavedMessagesStore from '@app/stores/SavedMessagesStore';
import UserStore from '@app/stores/UserStore';
import type {UnicodeEmoji} from '@app/types/EmojiTypes';
import {isSystemDmChannel} from '@app/utils/ChannelUtils';
import {buildMessageJumpLink} from '@app/utils/MessageLinkUtils';
import {type ReactionEmoji, toReactionEmoji} from '@app/utils/ReactionUtils';
import TtsUtils from '@app/utils/TtsUtils';
import {
	isMessageTypeDeletable,
	MessageFlags,
	MessageStates,
	MessageTypes,
	Permissions,
} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export function isEmbedsSuppressed(message: MessageRecord): boolean {
	return (message.flags & MessageFlags.SUPPRESS_EMBEDS) !== 0;
}

export function isClientSystemMessage(message: MessageRecord): boolean {
	return message.isClientSystemMessage();
}

export function canReportMessage(message: MessageRecord): boolean {
	if (message.state !== MessageStates.SENT) {
		return false;
	}
	if (message.isCurrentUserAuthor()) {
		return false;
	}
	if (message.author.system) {
		return false;
	}
	return message.type === MessageTypes.DEFAULT || message.type === MessageTypes.REPLY;
}

export function canDeleteAttachmentUtil(message: MessageRecord | undefined): boolean {
	if (!message?.isCurrentUserAuthor()) return false;
	const channel = ChannelStore.getChannel(message.channelId);
	const guild = channel?.guildId ? GuildStore.getGuild(channel.guildId) : null;
	const sendMessageDisabled = guild ? (guild.disabledOperations & GuildOperations.SEND_MESSAGE) !== 0 : false;
	return !sendMessageDisabled;
}

export function requestOpenReactionPicker(messageId: string): void {
	ComponentDispatch.dispatch('EMOJI_PICKER_OPEN', {messageId});
}

export function triggerAddReaction(message: MessageRecord): boolean {
	if (isClientSystemMessage(message)) {
		return false;
	}

	const messageElement = document.querySelector<HTMLElement>(`[data-message-id="${message.id}"]`);
	if (!messageElement) {
		ComponentDispatch.dispatch('EMOJI_PICKER_OPEN', {messageId: message.id});
		return false;
	}

	const addReactionButton = messageElement.querySelector<HTMLButtonElement>(
		'[data-action="message-add-reaction-button"]',
	);
	if (!addReactionButton) {
		ComponentDispatch.dispatch('EMOJI_PICKER_OPEN', {messageId: message.id});
		return false;
	}

	addReactionButton.click();
	return true;
}

export interface MessagePermissions {
	channel: ChannelRecord;
	isDM: boolean;
	canSendMessages: boolean;
	canAddReactions: boolean;
	canEditMessage: boolean;
	canDeleteMessage: boolean;
	canDeleteAttachment: boolean;
	canPinMessage: boolean;
	canSuppressEmbeds: boolean;
	shouldRenderSuppressEmbeds: boolean;
}

function getMessagePermissionsFromStores(message: MessageRecord): MessagePermissions | null {
	const channel = ChannelStore.getChannel(message.channelId);
	if (!channel) {
		return null;
	}
	const isDM = !channel.guildId;
	const isAuthorBlocked = RelationshipStore.isBlocked(message.author.id);
	const interactionsBlocked = isSystemDmChannel(channel);
	const isClientSystem = isClientSystemMessage(message);

	const passesVerification = isDM || GuildVerificationStore.canAccessGuild(channel.guildId || '');

	const guild = channel.guildId ? GuildStore.getGuild(channel.guildId) : null;
	const sendMessageDisabled = guild ? (guild.disabledOperations & GuildOperations.SEND_MESSAGE) !== 0 : false;
	const reactionsDisabled = guild ? (guild.disabledOperations & GuildOperations.REACTIONS) !== 0 : false;
	const messageTypeDeletable = isMessageTypeDeletable(message.type);
	const currentUserId = AuthenticationStore.currentUserId;
	const isCurrentUserTimedOut =
		guild && currentUserId ? GuildMemberStore.isUserTimedOut(guild.id, currentUserId) : false;

	const canSendMessages =
		!isClientSystem &&
		!interactionsBlocked &&
		(isDM ||
			(!sendMessageDisabled &&
				PermissionStore.can(Permissions.SEND_MESSAGES, {channelId: message.channelId}) &&
				passesVerification));
	const canAddReactions =
		!isClientSystem &&
		!interactionsBlocked &&
		!isAuthorBlocked &&
		(isDM ||
			(!reactionsDisabled &&
				PermissionStore.can(Permissions.ADD_REACTIONS, {channelId: message.channelId}) &&
				passesVerification &&
				!isCurrentUserTimedOut));

	const canEditMessage = !interactionsBlocked && !sendMessageDisabled && message.isCurrentUserAuthor();

	const canDeleteMessage =
		!interactionsBlocked &&
		messageTypeDeletable &&
		!sendMessageDisabled &&
		(message.isCurrentUserAuthor() ||
			(isDM ? false : PermissionStore.can(Permissions.MANAGE_MESSAGES, {channelId: message.channelId})));

	const canDeleteAttachment = !interactionsBlocked && !sendMessageDisabled && message.isCurrentUserAuthor();

	const canPinMessage =
		!interactionsBlocked &&
		!sendMessageDisabled &&
		(isDM ? true : PermissionStore.can(Permissions.PIN_MESSAGES, {channelId: message.channelId}));
	const canSuppressEmbeds =
		!interactionsBlocked &&
		!sendMessageDisabled &&
		message.isUserMessage() &&
		(message.isCurrentUserAuthor() || (!isDM && canDeleteMessage));

	const shouldRenderSuppressEmbeds =
		message.isUserMessage() && canSuppressEmbeds && (isEmbedsSuppressed(message) || message.embeds.length > 0);

	return {
		channel,
		isDM,
		canSendMessages,
		canAddReactions,
		canEditMessage,
		canDeleteMessage,
		canDeleteAttachment,
		canPinMessage,
		canSuppressEmbeds,
		shouldRenderSuppressEmbeds,
	};
}

export function useMessagePermissions(message: MessageRecord): MessagePermissions | null {
	const context = useMaybeMessageViewContext();

	if (context?.previewPermissions) {
		return {
			channel: context.channel,
			...context.previewPermissions,
		};
	}

	return getMessagePermissionsFromStores(message);
}

interface MessageActionHandlersOptions {
	i18n: I18n;
	onClose?: () => void;
}

export interface MessageActionHandlers {
	handleEmojiSelect: (emoji: UnicodeEmoji | ReactionEmoji) => void;
	handleCopyMessageId: () => void;
	handleCopyMessage: () => void;
	handleCopyMessageLink: () => void;
	handleSaveMessage: (isSaved: boolean) => (event?: React.MouseEvent | React.KeyboardEvent) => void;
	handleToggleSuppressEmbeds: () => void;
	handleReply: (event?: React.MouseEvent | React.KeyboardEvent) => void;
	handlePinMessage: (event?: React.MouseEvent | React.KeyboardEvent) => void;
	handleEditMessage: () => void;
	handleRetryMessage: () => void;
	handleFailedMessageDelete: () => void;
	handleForward: () => void;
	handleRemoveAllReactions: () => void;
	handleMarkAsUnread: () => void;
}

export function createMessageActionHandlers(
	message: MessageRecord,
	options: MessageActionHandlersOptions,
): MessageActionHandlers {
	const {i18n, onClose} = options;

	const handleEmojiSelect = (emoji: UnicodeEmoji | ReactionEmoji) => {
		if (isClientSystemMessage(message)) {
			return;
		}
		ReactionActionCreators.addReaction(i18n, message.channelId, message.id, toReactionEmoji(emoji));
	};

	const handleCopyMessageId = () => {
		requestCopyMessageId(message, i18n);
		onClose?.();
	};

	const handleCopyMessage = () => {
		if (message.content) {
			TextCopyActionCreators.copy(i18n, message.content);
			onClose?.();
		}
	};

	const handleCopyMessageLink = () => {
		requestCopyMessageLink(message, i18n);
		onClose?.();
	};

	const handleSaveMessage = (isSaved: boolean) => (event?: React.MouseEvent | React.KeyboardEvent) => {
		if (isClientSystemMessage(message)) {
			onClose?.();
			return;
		}

		if (isSaved) {
			SavedMessageActionCreators.remove(i18n, message.id);
		} else {
			SavedMessageActionCreators.create(i18n, message.channelId, message.id).then(() => {
				if (!event?.shiftKey) {
					ComponentDispatch.dispatch('SAVED_MESSAGES_OPEN');
				}
			});
		}
		onClose?.();
	};

	const handleToggleSuppressEmbeds = () => {
		requestToggleSuppressEmbeds(message, i18n);
		onClose?.();
	};

	const handleReply = (event?: React.MouseEvent | React.KeyboardEvent) => {
		const channel = ChannelStore.getChannel(message.channelId)!;
		requestMessageReply(message, {
			mention: !event?.shiftKey && !message.isCurrentUserAuthor() && channel.guildId != null,
		});
		onClose?.();
	};

	const handlePinMessage = (event?: React.MouseEvent | React.KeyboardEvent) => {
		onClose?.();
		requestMessagePin(message, i18n, {shiftKey: Boolean(event?.shiftKey)});
	};

	const handleEditMessage = () => {
		startMessageEdit(message);
		onClose?.();
	};

	const handleRetryMessage = () => {
		if (!message.nonce) {
			return;
		}

		const messageUpload = CloudUpload.getMessageUpload(message.nonce);
		const hasAttachments = messageUpload !== null;
		const optimisticMessage: Message = {
			...message.toJSON(),
			state: MessageStates.SENDING,
			edited_timestamp: undefined,
			attachments: message.attachments ? [...message.attachments] : undefined,
			reactions: [],
		};
		MessageActionCreators.retryLocal(message.channelId, message.id);
		MessageActionCreators.createOptimistic(message.channelId, optimisticMessage);

		MessageActionCreators.send(message.channelId, {
			content: message.content,
			nonce: message.nonce,
			hasAttachments,
			allowedMentions: message._allowedMentions,
			messageReference: message.messageReference,
			flags: message.flags,
			favoriteMemeId: message._favoriteMemeId,
			stickers: [...(message.stickers ?? [])],
		});
		onClose?.();
	};

	const handleFailedMessageDelete = () => {
		MessageActionCreators.deleteLocal(message.channelId, message.id);
		onClose?.();
	};

	const handleForward = () => {
		onClose?.();
		requestMessageForward(message);
	};

	const handleRemoveAllReactions = () => {
		ReactionActionCreators.removeAllReactions(i18n, message.channelId, message.id);
		onClose?.();
	};

	const handleMarkAsUnread = () => {
		requestMarkMessageUnread(message);
		onClose?.();
	};

	return {
		handleEmojiSelect,
		handleCopyMessageId,
		handleCopyMessage,
		handleCopyMessageLink,
		handleSaveMessage,
		handleToggleSuppressEmbeds,
		handleReply,
		handlePinMessage,
		handleEditMessage,
		handleRetryMessage,
		handleFailedMessageDelete,
		handleForward,
		handleRemoveAllReactions,
		handleMarkAsUnread,
	};
}

export function startMessageEdit(message: MessageRecord): void {
	if (message.messageSnapshots) {
		return;
	}
	if (MobileLayoutStore.isEnabled()) {
		MessageActionCreators.startEditMobile(message.channelId, message.id);
	} else {
		MessageActionCreators.startEdit(message.channelId, message.id, message.content);
	}
}

export function requestDeleteMessage(message: MessageRecord, i18n: I18n, bypassConfirm = false): void {
	if (bypassConfirm) {
		MessageActionCreators.remove(message.channelId, message.id);
		return;
	}

	MessageActionCreators.showDeleteConfirmation(i18n, {message});
}

export function requestMessagePin(message: MessageRecord, i18n: I18n, options: {shiftKey?: boolean} = {}): void {
	if (message.pinned) {
		if (options.shiftKey) {
			ChannelPinActionCreators.unpin(message.channelId, message.id);
			return;
		}
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(msg`Unpin Message`)}
					description={i18n._(msg`Do you want to send this pin back to the future?`)}
					message={message}
					primaryText={i18n._(msg`Unpin it`)}
					onPrimary={() => ChannelPinActionCreators.unpin(message.channelId, message.id)}
				/>
			)),
		);
		return;
	}
	if (options.shiftKey) {
		ChannelPinActionCreators.pin(message.channelId, message.id);
		return;
	}
	ModalActionCreators.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(msg`Pin it. Pin it good.`)}
				description={i18n._(msg`Pin this message to the channel for all to see. Unless ... you're chicken.`)}
				message={message}
				primaryText={i18n._(msg`Pin it real good`)}
				primaryVariant="primary"
				onPrimary={() => ChannelPinActionCreators.pin(message.channelId, message.id)}
			/>
		)),
	);
}

export function requestMessageReply(message: MessageRecord, options?: {mention?: boolean}): void {
	if (isClientSystemMessage(message)) {
		return;
	}

	const channel = ChannelStore.getChannel(message.channelId);
	if (!channel) return;
	const shouldMention = options?.mention ?? (!message.isCurrentUserAuthor() && Boolean(channel.guildId));
	MessageActionCreators.startReply(message.channelId, message.id, shouldMention);
}

export function requestMessageForward(message: MessageRecord): void {
	if (isClientSystemMessage(message)) {
		return;
	}

	const currentUser = UserStore.currentUser;
	if (!currentUser) {
		return;
	}

	ModalActionCreators.push(modal(() => <ForwardModal message={message} user={currentUser} />));
}

export function requestCopyMessageText(message: MessageRecord, i18n: I18n): void {
	if (!message.content) return;
	void TextCopyActionCreators.copy(i18n, message.content);
}

export function requestMarkMessageUnread(message: MessageRecord): void {
	ReadStateActionCreators.markAsUnread(message.channelId, message.id);
}

export function requestCopyMessageLink(message: MessageRecord, i18n: I18n): void {
	if (isClientSystemMessage(message)) {
		return;
	}

	const channel = ChannelStore.getChannel(message.channelId);
	if (!channel) return;
	const jumpLink = buildMessageJumpLink({
		guildId: channel.guildId,
		channelId: message.channelId,
		messageId: message.id,
	});
	void TextCopyActionCreators.copy(i18n, jumpLink);
}

export function requestCopyMessageId(message: MessageRecord, i18n: I18n): void {
	void TextCopyActionCreators.copy(i18n, message.id);
}

export function requestToggleBookmark(message: MessageRecord, i18n: I18n): void {
	if (isClientSystemMessage(message)) {
		return;
	}

	if (SavedMessagesStore.isSaved(message.id)) {
		SavedMessageActionCreators.remove(i18n, message.id);
		return;
	}
	void SavedMessageActionCreators.create(i18n, message.channelId, message.id);
}

export function requestToggleSuppressEmbeds(message: MessageRecord, i18n: I18n): void {
	if (isEmbedsSuppressed(message)) {
		MessageActionCreators.edit(
			message.channelId,
			message.id,
			undefined,
			message.flags & ~MessageFlags.SUPPRESS_EMBEDS,
		).then(() => {
			ToastActionCreators.createToast({
				type: 'success',
				children: i18n._(msg`Embeds unsuppressed`),
			});
		});
		return;
	}

	MessageActionCreators.edit(
		message.channelId,
		message.id,
		undefined,
		message.flags | MessageFlags.SUPPRESS_EMBEDS,
	).then(() => {
		ToastActionCreators.createToast({
			type: 'success',
			children: i18n._(msg`Embeds suppressed`),
		});
	});
}

export function requestSpeakMessage(message: MessageRecord): void {
	TtsUtils.speakMessage(message.content);
}
