// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import * as DraftCommands from '@app/features/messaging/commands/DraftCommands';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {Message} from '@app/features/messaging/models/MessagingMessage';
import * as MessageSubmitUtils from '@app/features/messaging/utils/MessageSubmitUtils';
import {formatUploadingAttachmentSummary} from '@app/features/messaging/utils/UploadingAttachmentLabelUtils';
import Permission from '@app/features/permissions/state/Permission';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import * as SlowmodeCommands from '@app/features/slowmode/commands/SlowmodeCommands';
import {SlowmodeRateLimitedModal} from '@app/features/slowmode/components/alerts/SlowmodeRateLimitedModal';
import Slowmode from '@app/features/slowmode/state/Slowmode';
import {TypingUtils} from '@app/features/typing/utils/TypingUtils';
import {modal, push as pushModal} from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';
import {MessageStates, MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {CHANNEL_RATE_LIMIT_PER_USER_MAX} from '@fluxer/constants/src/LimitConstants';
import type {
	AllowedMentions,
	MessageAttachment,
	MessageStickerItem,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {useLingui} from '@lingui/react/macro';
import {useCallback} from 'react';

interface UseMessageSubmissionOptions {
	channel: Channel | null;
	referencedMessage: Message | null;
	replyingMessage: {messageId: string; mentioning: boolean} | null;
	clearSegments?: () => void;
}

export type SendMessageFunction = (
	content: string,
	hasAttachments: boolean,
	stickersOrTts?: Array<MessageStickerItem> | boolean,
	favoriteMemeIdOrStickers?: string | Array<MessageStickerItem>,
	maybeFavoriteMemeId?: string,
) => boolean;

function isBlockedBySlowmode(channel: Channel): boolean {
	if (!channel.guildId) return false;
	const rateLimitPerUser = channel.rateLimitPerUser;
	if (
		rateLimitPerUser === undefined ||
		rateLimitPerUser === null ||
		!Number.isSafeInteger(rateLimitPerUser) ||
		rateLimitPerUser <= 0 ||
		rateLimitPerUser > CHANNEL_RATE_LIMIT_PER_USER_MAX
	) {
		return false;
	}
	if (Permission.can(Permissions.BYPASS_SLOWMODE, channel)) return false;
	const remainingMs = Slowmode.getSlowmodeRemaining(channel.id, rateLimitPerUser);
	if (remainingMs <= 0) return false;
	const retryAfter = Math.ceil(remainingMs / 1000);
	pushModal(
		modal(() => (
			<SlowmodeRateLimitedModal
				retryAfter={retryAfter}
				data-flx="messaging.use-message-submission.is-blocked-by-slowmode.slowmode-rate-limited-modal"
			/>
		)),
	);
	return true;
}

export const useMessageSubmission = ({channel, referencedMessage, replyingMessage}: UseMessageSubmissionOptions) => {
	const {i18n} = useLingui();
	const sendMessage = useCallback(
		(
			content: string,
			hasAttachments: boolean,
			stickersOrTts: Array<MessageStickerItem> | boolean = [],
			favoriteMemeIdOrStickers?: string | Array<MessageStickerItem>,
			maybeFavoriteMemeId?: string,
		) => {
			const isTtsCall = typeof stickersOrTts === 'boolean';
			const tts = isTtsCall ? stickersOrTts : undefined;
			const stickers = isTtsCall
				? Array.isArray(favoriteMemeIdOrStickers)
					? favoriteMemeIdOrStickers
					: []
				: stickersOrTts;
			const favoriteMemeId = isTtsCall
				? maybeFavoriteMemeId
				: typeof favoriteMemeIdOrStickers === 'string'
					? favoriteMemeIdOrStickers
					: undefined;
			const currentUser = Users.getCurrentUser();
			if (!channel || !currentUser) return false;
			if (isBlockedBySlowmode(channel)) return false;
			const nonce = SnowflakeUtils.fromTimestamp(Date.now());
			if (!MessageCommands.reserveSend(channel.id, nonce)) return false;
			const messageReference = MessageSubmitUtils.prepareMessageReference(channel.id, referencedMessage);
			TypingUtils.clear(channel.id);
			DraftCommands.deleteDraft(channel.id);
			MessageCommands.stopReply(channel.id);
			const uploadingAttachments = MessageSubmitUtils.createUploadingAttachments(
				MessageSubmitUtils.claimMessageAttachments(
					channel.id,
					nonce,
					content,
					messageReference,
					replyingMessage?.mentioning,
					favoriteMemeId,
				),
				{
					formatMultipleFileLabel: (count) => formatUploadingAttachmentSummary(i18n, count),
				},
			);
			const hasAttachmentsFinal = uploadingAttachments.length > 0 || hasAttachments;
			const allowedMentions: AllowedMentions = {replied_user: replyingMessage?.mentioning ?? true};
			const message = MessageSubmitUtils.createOptimisticMessage(
				{
					content,
					channelId: channel.id,
					nonce,
					currentUser,
					referencedMessage,
					replyMentioning: replyingMessage?.mentioning,
					stickers,
					favoriteMemeId,
				},
				uploadingAttachments,
			);
			MessageCommands.createOptimistic(channel.id, {
				...message.toJSON(),
				referenced_message: referencedMessage?.toJSON(),
			});
			SlowmodeCommands.prepareMessageSend(channel.id);
			void MessageCommands.send(channel.id, {
				content: message.content,
				nonce,
				hasAttachments: hasAttachmentsFinal,
				allowedMentions,
				messageReference,
				flags: message.flags,
				stickers,
				favoriteMemeId,
				tts,
			}).then((sentMessage) => {
				if (sentMessage) {
					SlowmodeCommands.recordMessageSend(channel.id);
				}
			});
			ComponentDispatch.dispatch('MESSAGE_SENT', {channelId: channel.id});
			return true;
		},
		[channel?.id, i18n, referencedMessage, replyingMessage],
	);
	const sendOptimisticMessage = useCallback(
		(
			messageData: {
				content: string;
				stickers?: Array<MessageStickerItem>;
				attachments?: Array<MessageAttachment>;
			},
			sendOptions: {
				hasAttachments: boolean;
				favoriteMemeId?: string;
			},
		) => {
			const currentUser = Users.getCurrentUser();
			if (!channel || !currentUser) return;
			if (isBlockedBySlowmode(channel)) return;
			const nonce = SnowflakeUtils.fromTimestamp(Date.now());
			if (!MessageCommands.reserveSend(channel.id, nonce)) return;
			TypingUtils.clear(channel.id);
			MessageCommands.stopReply(channel.id);
			const message = new Message({
				id: nonce,
				channel_id: channel.id,
				author: currentUser.toJSON(),
				type: referencedMessage ? MessageTypes.REPLY : MessageTypes.DEFAULT,
				flags: 0,
				pinned: false,
				mention_everyone: false,
				content: messageData.content,
				timestamp: new Date().toISOString(),
				mentions: [...(referencedMessage && replyingMessage?.mentioning ? [referencedMessage.author.toJSON()] : [])],
				message_reference: referencedMessage
					? {channel_id: channel.id, message_id: referencedMessage.id, type: 0}
					: undefined,
				state: MessageStates.SENDING,
				nonce,
				attachments: messageData.attachments || [],
				stickers: messageData.stickers || [],
				_allowedMentions: referencedMessage ? {replied_user: replyingMessage?.mentioning ?? true} : undefined,
			});
			MessageCommands.createOptimistic(channel.id, {
				...message.toJSON(),
				referenced_message: referencedMessage?.toJSON(),
			});
			SlowmodeCommands.prepareMessageSend(channel.id);
			const allowedMentions: AllowedMentions = {replied_user: replyingMessage?.mentioning ?? true};
			void MessageCommands.send(channel.id, {
				content: messageData.content,
				nonce,
				hasAttachments: sendOptions.hasAttachments,
				allowedMentions,
				messageReference: referencedMessage
					? {channel_id: channel.id, message_id: referencedMessage.id, type: 0}
					: undefined,
				flags: 0,
				stickers: messageData.stickers || [],
				favoriteMemeId: sendOptions.favoriteMemeId,
			}).then((sentMessage) => {
				if (sentMessage) {
					SlowmodeCommands.recordMessageSend(channel.id);
				}
			});
			ComponentDispatch.dispatch('MESSAGE_SENT', {channelId: channel.id});
		},
		[channel?.id, referencedMessage, replyingMessage],
	);
	return {sendMessage, sendOptimisticMessage};
};
