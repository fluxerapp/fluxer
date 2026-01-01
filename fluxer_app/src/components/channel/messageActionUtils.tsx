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
import * as ChannelPinActionCreators from '~/actions/ChannelPinsActionCreators';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ReactionActionCreators from '~/actions/ReactionActionCreators';
import * as ReadStateActionCreators from '~/actions/ReadStateActionCreators';
import * as SavedMessageActionCreators from '~/actions/SavedMessageActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {GuildOperations, isMessageTypeDeletable, MessageFlags, Permissions} from '~/Constants';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {ForwardModal} from '~/components/modals/ForwardModal';
import {CloudUpload} from '~/lib/CloudUpload';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import type {MessageRecord} from '~/records/MessageRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import GuildVerificationStore from '~/stores/GuildVerificationStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PermissionStore from '~/stores/PermissionStore';
import RelationshipStore from '~/stores/RelationshipStore';
import {buildMessageJumpLink} from '~/utils/messageLinkUtils';
import {type ReactionEmoji, toReactionEmoji, type UnicodeEmoji} from '~/utils/ReactionUtils';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

export function isEmbedsSuppressed(message: MessageRecord): boolean {
	return (message.flags & MessageFlags.SUPPRESS_EMBEDS) !== 0;
}

export function canDeleteAttachmentUtil(message: MessageRecord | undefined): boolean {
	if (!message?.isCurrentUserAuthor()) return false;
	const channel = ChannelStore.getChannel(message.channelId);
	const guild = channel?.guildId ? GuildStore.getGuild(channel.guildId) : null;
	const sendMessageDisabled = guild ? (guild.disabledOperations & GuildOperations.SEND_MESSAGE) !== 0 : false;
	return !sendMessageDisabled;
}

export function triggerAddReaction(messageId: string): boolean {
	const messageElement = document.querySelector<HTMLElement>(`[data-message-id="${messageId}"]`);
	if (!messageElement) {
		ComponentDispatch.dispatch('EMOJI_PICKER_OPEN', {messageId});
		return false;
	}

	const addReactionButton = messageElement.querySelector<HTMLButtonElement>(
		'[data-action="message-add-reaction-button"]',
	);
	if (!addReactionButton) {
		ComponentDispatch.dispatch('EMOJI_PICKER_OPEN', {messageId});
		return false;
	}

	addReactionButton.click();
	return true;
}

export function useMessagePermissions(message: MessageRecord) {
	const channel = ChannelStore.getChannel(message.channelId)!;
	const isDM = !channel.guildId;
	const isAuthorBlocked = RelationshipStore.isBlocked(message.author.id);

	const passesVerification = isDM || GuildVerificationStore.canAccessGuild(channel.guildId || '');

	const guild = channel.guildId ? GuildStore.getGuild(channel.guildId) : null;
	const sendMessageDisabled = guild ? (guild.disabledOperations & GuildOperations.SEND_MESSAGE) !== 0 : false;
	const reactionsDisabled = guild ? (guild.disabledOperations & GuildOperations.REACTIONS) !== 0 : false;
	const messageTypeDeletable = isMessageTypeDeletable(message.type);
	const currentUserId = AuthenticationStore.currentUserId;
	const isCurrentUserTimedOut =
		guild && currentUserId ? GuildMemberStore.isUserTimedOut(guild.id, currentUserId) : false;

	const canSendMessages =
		isDM ||
		(!sendMessageDisabled &&
			PermissionStore.can(Permissions.SEND_MESSAGES, {channelId: message.channelId}) &&
			passesVerification);
	const canAddReactions =
		!isAuthorBlocked &&
		(isDM ||
			(!reactionsDisabled &&
				PermissionStore.can(Permissions.ADD_REACTIONS, {channelId: message.channelId}) &&
				passesVerification &&
				!isCurrentUserTimedOut));

	const canEditMessage = !sendMessageDisabled && message.isCurrentUserAuthor();

	const canDeleteMessage =
		messageTypeDeletable &&
		!sendMessageDisabled &&
		(message.isCurrentUserAuthor() ||
			(isDM ? false : PermissionStore.can(Permissions.MANAGE_MESSAGES, {channelId: message.channelId})));

	const canDeleteAttachment = !sendMessageDisabled && message.isCurrentUserAuthor();

	const canPinMessage =
		!sendMessageDisabled &&
		(isDM ? true : PermissionStore.can(Permissions.PIN_MESSAGES, {channelId: message.channelId}));
	const canSuppressEmbeds =
		!sendMessageDisabled && message.isUserMessage() && (message.isCurrentUserAuthor() || (!isDM && canDeleteMessage));

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

export function createMessageActionHandlers(message: MessageRecord, options?: {onClose?: () => void}) {
	const {t, i18n} = useLingui();
	const onClose = options?.onClose;

	const handleEmojiSelect = (emoji: UnicodeEmoji | ReactionEmoji) => {
		ReactionActionCreators.addReaction(i18n, message.channelId, message.id, toReactionEmoji(emoji));
	};

	const handleCopyMessageId = () => {
		TextCopyActionCreators.copy(i18n, message.id);
		onClose?.();
	};

	const handleCopyMessage = () => {
		if (message.content) {
			TextCopyActionCreators.copy(i18n, message.content);
			onClose?.();
		}
	};

	const handleCopyMessageLink = () => {
		const channel = ChannelStore.getChannel(message.channelId)!;
		const jumpLink = buildMessageJumpLink({
			guildId: channel.guildId,
			channelId: message.channelId,
			messageId: message.id,
		});
		TextCopyActionCreators.copy(i18n, jumpLink);
		onClose?.();
	};

	const handleSaveMessage = (isSaved: boolean) => (event?: React.MouseEvent | React.KeyboardEvent) => {
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
		if (isEmbedsSuppressed(message)) {
			MessageActionCreators.edit(
				message.channelId,
				message.id,
				undefined,
				message.flags & ~MessageFlags.SUPPRESS_EMBEDS,
			).then(() => {
				ToastActionCreators.createToast({
					type: 'success',
					children: t`Embeds unsuppressed`,
				});
			});
		} else {
			MessageActionCreators.edit(
				message.channelId,
				message.id,
				undefined,
				message.flags | MessageFlags.SUPPRESS_EMBEDS,
			).then(() => {
				ToastActionCreators.createToast({
					type: 'success',
					children: t`Embeds suppressed`,
				});
			});
		}
		onClose?.();
	};

	const handleReply = (event?: React.MouseEvent | React.KeyboardEvent) => {
		const channel = ChannelStore.getChannel(message.channelId)!;
		MessageActionCreators.startReply(
			message.channelId,
			message.id,
			!event?.shiftKey && !message.isCurrentUserAuthor() && channel.guildId != null,
		);
		onClose?.();
	};

	const handlePinMessage = (event?: React.MouseEvent | React.KeyboardEvent) => {
		const isPinned = message.pinned;
		onClose?.();

		if (isPinned) {
			if (event?.shiftKey) {
				ChannelPinActionCreators.unpin(message.channelId, message.id);
			} else {
				ModalActionCreators.push(
					modal(() => (
						<ConfirmModal
							title={t`Unpin Message`}
							description={t`Do you want to send this pin back to the future?`}
							message={message}
							primaryText={t`Unpin it`}
							onPrimary={() => ChannelPinActionCreators.unpin(message.channelId, message.id)}
						/>
					)),
				);
			}
		} else if (event?.shiftKey) {
			ChannelPinActionCreators.pin(message.channelId, message.id);
		} else {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Pin it. Pin it good.`}
						description={t`Pin this message to the channel for all to see. Unless ... you're chicken.`}
						message={message}
						primaryText={t`Pin it real good`}
						primaryVariant="primary"
						onPrimary={() => ChannelPinActionCreators.pin(message.channelId, message.id)}
					/>
				)),
			);
		}
	};

	const handleEditMessage = () => {
		if (message.messageSnapshots) {
			return;
		}
		const isMobile = MobileLayoutStore.enabled;
		if (isMobile) {
			MessageActionCreators.startEditMobile(message.channelId, message.id);
		} else {
			MessageActionCreators.startEdit(message.channelId, message.id, message.content);
		}
		onClose?.();
	};

	const handleRetryMessage = () => {
		if (!message.nonce) {
			return;
		}

		const messageUpload = CloudUpload.getMessageUpload(message.nonce);
		const hasAttachments = messageUpload !== null;
		const newNonce = SnowflakeUtils.fromTimestamp(Date.now());

		if (hasAttachments) {
			CloudUpload.moveMessageUpload(message.nonce, newNonce);
		}

		MessageActionCreators.deleteLocal(message.channelId, message.id);

		MessageActionCreators.send(message.channelId, {
			content: message.content,
			nonce: newNonce,
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
		ModalActionCreators.push(modal(() => <ForwardModal message={message} />));
	};

	const handleRemoveAllReactions = () => {
		ReactionActionCreators.removeAllReactions(i18n, message.channelId, message.id);
		onClose?.();
	};

	const handleMarkAsUnread = () => {
		ReadStateActionCreators.markAsUnread(message.channelId, message.id);
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
