// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import Authentication from '@app/features/auth/state/Authentication';
import * as ChannelPinCommands from '@app/features/channel/commands/ChannelPinsCommands';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {isSystemDmChannel} from '@app/features/channel/utils/ChannelUtils';
import type {UnicodeEmoji} from '@app/features/emoji/types/EmojiTypes';
import Guilds from '@app/features/guild/state/Guilds';
import GuildVerification from '@app/features/guild/state/GuildVerification';
import {UNPIN_MESSAGE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import * as ReactionCommands from '@app/features/messaging/commands/ReactionCommands';
import * as SavedMessageCommands from '@app/features/messaging/commands/SavedMessageCommands';
import {ForwardModal, type ForwardModalSuccess} from '@app/features/messaging/components/modals/ForwardModal';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import {buildRawMessageContentCopyText} from '@app/features/messaging/utils/MessageCopyTextUtils';
import {buildMessageJumpLink} from '@app/features/messaging/utils/MessageLinkUtils';
import {retryFailedMessage} from '@app/features/messaging/utils/MessageRetryUtils';
import {type ReactionEmoji, toReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import {getDefaultReplyMention} from '@app/features/notification/utils/MentionReplyPreferenceUtils';
import Permission from '@app/features/permissions/state/Permission';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import Relationships from '@app/features/relationship/state/Relationships';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Users from '@app/features/user/state/Users';
import TtsUtils from '@app/features/voice/utils/VoiceTtsUtils';
import {
	isMessageTypeDeletable,
	MessageFlags,
	MessageStates,
	MessageTypes,
	Permissions,
} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const ARE_YOU_SURE_YOU_WANT_TO_UNPIN_THIS_DESCRIPTOR = msg({
	message: 'Unpin this message?',
	comment: 'Body of the confirmation alert shown when unpinning a message.',
});
const UNPIN_DESCRIPTOR = msg({
	message: 'Unpin',
	comment: 'Confirm button label on the unpin message alert.',
});
const PIN_MESSAGE_DESCRIPTOR = msg({
	message: 'Pin message',
	comment: 'Title of the confirmation alert shown when pinning a message.',
});
const PIN_THIS_MESSAGE_TO_THE_CHANNEL_FOR_EVERYONE_DESCRIPTOR = msg({
	message: 'Pin this message to the channel for everyone to see.',
	comment: 'Body of the confirmation alert shown when pinning a message.',
});
const PIN_DESCRIPTOR = msg({
	message: 'Pin',
	comment: 'Confirm button label on the pin message alert.',
});
const REMOVE_ALL_REACTIONS_DESCRIPTOR = msg({
	message: 'Remove all reactions',
	comment: 'Title of the confirmation alert shown when clearing every reaction on a message.',
});
const ARE_YOU_SURE_YOU_WANT_TO_REMOVE_ALL_DESCRIPTOR = msg({
	message: 'Clear every reaction from this message?',
	comment: 'Body of the confirmation alert shown when clearing every reaction on a message.',
});
const REMOVE_ALL_DESCRIPTOR = msg({
	message: 'Remove all',
	comment: 'Confirm button label on the remove-all-reactions alert. Destructive.',
});
const EMBEDS_UNSUPPRESSED_DESCRIPTOR = msg({
	message: 'Embeds unsuppressed',
	comment: 'Toast confirmation after re-enabling link previews and embeds on a message.',
});
const EMBEDS_SUPPRESSED_DESCRIPTOR = msg({
	message: 'Embeds suppressed',
	comment: 'Toast confirmation after hiding link previews and embeds on a message.',
});

export function getEffectiveContent(message: Message): string {
	if (message.content) return message.content;
	if (message.messageSnapshots && message.messageSnapshots.length > 0) {
		return message.messageSnapshots[0].content ?? '';
	}
	return '';
}

export function getCopyableMessageText(message: Message, _i18n: I18n): string {
	return buildRawMessageContentCopyText(message);
}

export function isEmbedsSuppressed(message: Message): boolean {
	return (message.flags & MessageFlags.SUPPRESS_EMBEDS) !== 0;
}

export function isClientSystemMessage(message: Message): boolean {
	return message.isClientSystemMessage();
}

export function canReportMessage(message: Message): boolean {
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

export function canDeleteAttachmentUtil(message: Message | undefined): boolean {
	if (!message?.isCurrentUserAuthor()) return false;
	const channel = Channels.getChannel(message.channelId);
	const guild = channel?.guildId ? Guilds.getGuild(channel.guildId) : null;
	const sendMessageDisabled = guild ? (guild.disabledOperations & GuildOperations.SEND_MESSAGE) !== 0 : false;
	return !sendMessageDisabled;
}

export function requestOpenReactionPicker(messageId: string): void {
	ComponentBus.dispatch('EMOJI_PICKER_OPEN', {messageId});
}

function messageElementSelector(messageId: string): string {
	const escaped =
		typeof CSS === 'undefined' || typeof CSS.escape !== 'function'
			? messageId.replace(/["\\]/g, '\\$&')
			: CSS.escape(messageId);
	return `[data-message-id="${escaped}"]`;
}

export function triggerAddReaction(message: Message): boolean {
	if (isClientSystemMessage(message)) {
		return false;
	}
	const messageElement = document.querySelector<HTMLElement>(messageElementSelector(message.id));
	if (!messageElement) {
		ComponentBus.dispatch('EMOJI_PICKER_OPEN', {messageId: message.id});
		return false;
	}
	const addReactionButton = messageElement.querySelector<HTMLButtonElement>(
		'[data-action="message-add-reaction-button"]',
	);
	if (!addReactionButton) {
		ComponentBus.dispatch('EMOJI_PICKER_OPEN', {messageId: message.id});
		return false;
	}
	addReactionButton.click();
	return true;
}

export interface MessagePermissions {
	channel: Channel;
	isDM: boolean;
	canSendMessages: boolean;
	canAddReactions: boolean;
	canEditMessage: boolean;
	canDeleteMessage: boolean;
	canDeleteAttachment: boolean;
	canPinMessage: boolean;
	canForwardMessage: boolean;
	canSuppressEmbeds: boolean;
	shouldRenderSuppressEmbeds: boolean;
}

function canForwardMessageFromChannel(message: Message, channel: Channel, isDM: boolean): boolean {
	if (isClientSystemMessage(message)) {
		return false;
	}
	if (isDM) {
		return true;
	}
	if (!Permission.can(Permissions.VIEW_CHANNEL, channel)) {
		return false;
	}
	return true;
}

function getMessagePermissionsForChannel(message: Message, channel: Channel): MessagePermissions {
	const isDM = !channel.guildId;
	const isAuthorBlocked = Relationships.isBlocked(message.author.id);
	const interactionsBlocked = isSystemDmChannel(channel);
	const isClientSystem = isClientSystemMessage(message);
	const passesVerification = isDM || GuildVerification.canAccessGuild(channel.guildId || '');
	const guild = channel.guildId ? Guilds.getGuild(channel.guildId) : null;
	const sendMessageDisabled = guild ? (guild.disabledOperations & GuildOperations.SEND_MESSAGE) !== 0 : false;
	const reactionsDisabled = guild ? (guild.disabledOperations & GuildOperations.REACTIONS) !== 0 : false;
	const messageTypeDeletable = isMessageTypeDeletable(message.type);
	const currentUserId = Authentication.currentUserId;
	const isCurrentUserTimedOut = guild && currentUserId ? GuildMembers.isUserTimedOut(guild.id, currentUserId) : false;
	const canSendMessages =
		!isClientSystem &&
		!interactionsBlocked &&
		(isDM ||
			(!sendMessageDisabled &&
				Permission.can(Permissions.SEND_MESSAGES, {channelId: message.channelId}) &&
				passesVerification));
	const canAddReactions =
		!isClientSystem &&
		!interactionsBlocked &&
		!isAuthorBlocked &&
		(isDM ||
			(!reactionsDisabled &&
				Permission.can(Permissions.ADD_REACTIONS, {channelId: message.channelId}) &&
				passesVerification &&
				!isCurrentUserTimedOut));
	const canEditMessage = !interactionsBlocked && !sendMessageDisabled && message.isCurrentUserAuthor();
	const canDeleteMessage =
		!interactionsBlocked &&
		messageTypeDeletable &&
		!sendMessageDisabled &&
		(message.isCurrentUserAuthor() ||
			(isDM ? false : Permission.can(Permissions.MANAGE_MESSAGES, {channelId: message.channelId})));
	const canDeleteAttachment = !interactionsBlocked && !sendMessageDisabled && message.isCurrentUserAuthor();
	const canPinMessage =
		!interactionsBlocked &&
		!sendMessageDisabled &&
		(isDM ? true : Permission.can(Permissions.PIN_MESSAGES, {channelId: message.channelId}));
	const canForwardMessage =
		!interactionsBlocked && !sendMessageDisabled && canForwardMessageFromChannel(message, channel, isDM);
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
		canForwardMessage,
		canSuppressEmbeds,
		shouldRenderSuppressEmbeds,
	};
}

function getMessagePermissionsFromState(message: Message): MessagePermissions | null {
	const channel = Channels.getChannel(message.channelId);
	if (!channel) {
		return null;
	}
	return getMessagePermissionsForChannel(message, channel);
}

export function getMessagePermissions(message: Message, sourceChannel?: Channel | null): MessagePermissions | null {
	if (sourceChannel) {
		return getMessagePermissionsForChannel(message, sourceChannel);
	}
	return getMessagePermissionsFromState(message);
}

export function useMessagePermissions(message: Message, sourceChannel?: Channel | null): MessagePermissions | null {
	const context = useMaybeMessageViewContext();
	const channel = sourceChannel ?? context?.channel;
	if (context?.previewPermissions) {
		return {
			channel: channel ?? context.channel,
			...context.previewPermissions,
		};
	}
	if (channel) {
		return getMessagePermissionsForChannel(message, channel);
	}
	return getMessagePermissionsFromState(message);
}

interface MessageActionHandlersOptions {
	i18n: I18n;
	channel?: Channel | null;
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
	message: Message,
	options: MessageActionHandlersOptions,
): MessageActionHandlers {
	const {i18n, channel: sourceChannel, onClose} = options;
	const handleEmojiSelect = (emoji: UnicodeEmoji | ReactionEmoji) => {
		if (isClientSystemMessage(message)) {
			return;
		}
		const reactionEmoji = toReactionEmoji(emoji);
		if (message.getReaction(reactionEmoji)?.me) {
			ReactionCommands.removeReaction(i18n, message.channelId, message.id, reactionEmoji);
		} else {
			ReactionCommands.addReaction(i18n, message.channelId, message.id, reactionEmoji);
		}
	};
	const handleCopyMessageId = () => {
		requestCopyMessageId(message, i18n);
		onClose?.();
	};
	const handleCopyMessage = () => {
		const content = getCopyableMessageText(message, i18n);
		if (content) {
			TextCopyCommands.copy(i18n, content);
			onClose?.();
		}
	};
	const handleCopyMessageLink = () => {
		requestCopyMessageLink(message, i18n, sourceChannel);
		onClose?.();
	};
	const handleSaveMessage = (isSaved: boolean) => () => {
		if (isClientSystemMessage(message)) {
			onClose?.();
			return;
		}
		if (isSaved) {
			SavedMessageCommands.remove(i18n, message.id);
		} else {
			void SavedMessageCommands.create(i18n, message.channelId, message.id);
		}
		onClose?.();
	};
	const handleToggleSuppressEmbeds = () => {
		requestToggleSuppressEmbeds(message, i18n);
		onClose?.();
	};
	const handleReply = (event?: React.MouseEvent | React.KeyboardEvent) => {
		requestMessageReply(message, {
			mention: !event?.shiftKey && !message.isCurrentUserAuthor() && sourceChannel?.guildId != null,
			sourceChannel,
		});
		onClose?.();
	};
	const handlePinMessage = (event?: React.MouseEvent | React.KeyboardEvent) => {
		const pinMessage = () => requestMessagePin(message, i18n, {shiftKey: Boolean(event?.shiftKey)});
		if (onClose) {
			ModalCommands.runAfterBottomSheetClose(onClose, pinMessage);
			return;
		}
		pinMessage();
	};
	const handleEditMessage = () => {
		startMessageEdit(message);
		onClose?.();
	};
	const handleRetryMessage = () => {
		retryFailedMessage(message);
		onClose?.();
	};
	const handleFailedMessageDelete = () => {
		MessageCommands.deleteLocal(message.channelId, message.id);
		onClose?.();
	};
	const handleForward = () => {
		if (onClose) {
			ModalCommands.runAfterBottomSheetClose(onClose, () => requestMessageForward(message, sourceChannel));
			return;
		}
		requestMessageForward(message, sourceChannel);
	};
	const handleRemoveAllReactions = () => {
		if (onClose) {
			ModalCommands.runAfterBottomSheetClose(onClose, () => requestRemoveAllReactions(message, i18n));
			return;
		}
		requestRemoveAllReactions(message, i18n);
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

export function startMessageEdit(message: Message): void {
	if (message.messageSnapshots) {
		return;
	}
	if (MobileLayout.isEnabled()) {
		MessageCommands.startEditMobile(message.channelId, message.id);
	} else {
		MessageCommands.startEdit(message.channelId, message.id, message.content);
	}
}

export function requestDeleteMessage(message: Message, i18n: I18n, bypassConfirm = false): void {
	if (bypassConfirm) {
		MessageCommands.remove(message.channelId, message.id);
		return;
	}
	MessageCommands.showDeleteConfirmation(i18n, {message, showShiftBypassConfirmationTip: true});
}

export function requestMessagePin(message: Message, i18n: I18n, options: {shiftKey?: boolean} = {}): void {
	if (message.pinned) {
		if (options.shiftKey) {
			ChannelPinCommands.unpin(message.channelId, message.id);
			return;
		}
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(UNPIN_MESSAGE_DESCRIPTOR)}
					description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_UNPIN_THIS_DESCRIPTOR)}
					message={message}
					primaryText={i18n._(UNPIN_DESCRIPTOR)}
					onPrimary={() => ChannelPinCommands.unpin(message.channelId, message.id)}
					showShiftBypassConfirmationTip={true}
					data-flx="channel.message-action-utils.request-message-pin.confirm-modal"
				/>
			)),
		);
		return;
	}
	if (options.shiftKey) {
		ChannelPinCommands.pin(message.channelId, message.id);
		return;
	}
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(PIN_MESSAGE_DESCRIPTOR)}
				description={i18n._(PIN_THIS_MESSAGE_TO_THE_CHANNEL_FOR_EVERYONE_DESCRIPTOR)}
				message={message}
				primaryText={i18n._(PIN_DESCRIPTOR)}
				primaryVariant="primary"
				onPrimary={() => ChannelPinCommands.pin(message.channelId, message.id)}
				showShiftBypassConfirmationTip={true}
				data-flx="channel.message-action-utils.request-message-pin.confirm-modal--2"
			/>
		)),
	);
}

export function requestRemoveAllReactions(message: Message, i18n: I18n): void {
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(REMOVE_ALL_REACTIONS_DESCRIPTOR)}
				description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_REMOVE_ALL_DESCRIPTOR)}
				message={message}
				primaryText={i18n._(REMOVE_ALL_DESCRIPTOR)}
				primaryVariant="danger"
				onPrimary={() => ReactionCommands.removeAllReactions(i18n, message.channelId, message.id)}
				data-flx="channel.message-action-utils.request-remove-all-reactions.confirm-modal"
			/>
		)),
	);
}

interface RequestMessageReplyOptions {
	mention?: boolean;
	sourceChannel?: Channel | null;
}

export function requestMessageReply(message: Message, options?: RequestMessageReplyOptions): void {
	if (isClientSystemMessage(message)) {
		return;
	}
	const channel = options?.sourceChannel ?? Channels.getChannel(message.channelId);
	if (!channel) return;
	const startReply = (mentioning: boolean) => {
		MessageCommands.startReply(message.channelId, message.id, mentioning);
	};
	const shouldMention = getDefaultReplyMention({
		authorId: message.author.id,
		isOwnMessage: message.isCurrentUserAuthor(),
		guildId: channel.guildId,
		fallbackMention: options?.mention,
	});
	startReply(shouldMention);
}

interface RequestMessageForwardOptions {
	mediaSelection?: MessageCommands.ForwardMediaSelection;
	onForwardSuccess?: (result: ForwardModalSuccess) => void;
}

export function requestMessageForward(
	message: Message,
	sourceChannel?: Channel | null,
	options?: RequestMessageForwardOptions,
): void {
	if (isClientSystemMessage(message)) {
		return;
	}
	const currentUser = Users.currentUser;
	if (!currentUser) {
		return;
	}
	ModalCommands.push(
		modal(() => (
			<ForwardModal
				message={message}
				sourceChannel={sourceChannel}
				mediaSelection={options?.mediaSelection}
				onForwardSuccess={options?.onForwardSuccess}
				user={currentUser}
				data-flx="channel.message-action-utils.request-message-forward.forward-modal"
			/>
		)),
	);
}

export function requestCopyMessageText(message: Message, i18n: I18n): void {
	const content = getCopyableMessageText(message, i18n);
	if (!content) return;
	void TextCopyCommands.copy(i18n, content);
}

export function requestMarkMessageUnread(message: Message): void {
	ReadStateCommands.markAsUnread(message.channelId, message.id);
}

export function requestCopyMessageLink(message: Message, i18n: I18n, sourceChannel?: Channel | null): void {
	if (isClientSystemMessage(message)) {
		return;
	}
	const channel = sourceChannel ?? Channels.getChannel(message.channelId);
	if (!channel) return;
	const jumpLink = buildMessageJumpLink({
		guildId: channel.guildId,
		channelId: message.channelId,
		messageId: message.id,
	});
	void TextCopyCommands.copy(i18n, jumpLink);
}

export function requestCopyMessageId(message: Message, i18n: I18n): void {
	void TextCopyCommands.copy(i18n, message.id);
}

export function requestToggleBookmark(message: Message, i18n: I18n): void {
	if (isClientSystemMessage(message)) {
		return;
	}
	if (SavedMessages.isSaved(message.id)) {
		SavedMessageCommands.remove(i18n, message.id);
		return;
	}
	void SavedMessageCommands.create(i18n, message.channelId, message.id);
}

export function requestToggleSuppressEmbeds(message: Message, i18n: I18n): void {
	if (isEmbedsSuppressed(message)) {
		MessageCommands.edit(message.channelId, message.id, undefined, message.flags & ~MessageFlags.SUPPRESS_EMBEDS).then(
			() => {
				ToastCommands.createToast({
					type: 'success',
					children: i18n._(EMBEDS_UNSUPPRESSED_DESCRIPTOR),
				});
			},
		);
		return;
	}
	MessageCommands.edit(message.channelId, message.id, undefined, message.flags | MessageFlags.SUPPRESS_EMBEDS).then(
		() => {
			ToastCommands.createToast({
				type: 'success',
				children: i18n._(EMBEDS_SUPPRESSED_DESCRIPTOR),
			});
		},
	);
}

export function requestSpeakMessage(message: Message): void {
	TtsUtils.speakMessage(getEffectiveContent(message));
}
