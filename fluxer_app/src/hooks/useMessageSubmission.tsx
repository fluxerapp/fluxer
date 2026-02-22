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

import * as DraftActionCreators from '@app/actions/DraftActionCreators';
import * as MessageActionCreators from '@app/actions/MessageActionCreators';
import * as SlowmodeActionCreators from '@app/actions/SlowmodeActionCreators';
import {ComponentDispatch} from '@app/lib/ComponentDispatch';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import {MessageRecord} from '@app/records/MessageRecord';
import UserStore from '@app/stores/UserStore';
import * as MessageSubmitUtils from '@app/utils/MessageSubmitUtils';
import {TypingUtils} from '@app/utils/TypingUtils';
import {MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {
	AllowedMentions,
	MessageAttachment,
	MessageStickerItem,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {useCallback} from 'react';

interface UseMessageSubmissionOptions {
	channel: ChannelRecord | null;
	referencedMessage: MessageRecord | null;
	replyingMessage: {messageId: string; mentioning: boolean} | null;
	clearSegments?: () => void;
}

export type SendMessageFunction = (
	content: string,
	hasAttachments: boolean,
	stickersOrTts?: Array<MessageStickerItem> | boolean,
	favoriteMemeIdOrStickers?: string | Array<MessageStickerItem>,
	maybeFavoriteMemeId?: string,
) => void;

export const useMessageSubmission = ({channel, referencedMessage, replyingMessage}: UseMessageSubmissionOptions) => {
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

			const currentUser = UserStore.getCurrentUser();
			if (!channel || !currentUser) return;
			const nonce = SnowflakeUtils.fromTimestamp(Date.now());
			const messageReference = MessageSubmitUtils.prepareMessageReference(channel.id, referencedMessage);

			TypingUtils.clear(channel.id);
			DraftActionCreators.deleteDraft(channel.id);
			MessageActionCreators.stopReply(channel.id);

			const uploadingAttachments = MessageSubmitUtils.createUploadingAttachments(
				MessageSubmitUtils.claimMessageAttachments(
					channel.id,
					nonce,
					content,
					messageReference,
					replyingMessage?.mentioning,
					favoriteMemeId,
				),
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

			MessageActionCreators.createOptimistic(channel.id, {
				...message.toJSON(),
				referenced_message: referencedMessage?.toJSON(),
			});

			SlowmodeActionCreators.recordMessageSend(channel.id);

			MessageActionCreators.send(channel.id, {
				content: message.content,
				nonce,
				hasAttachments: hasAttachmentsFinal,
				allowedMentions,
				messageReference,
				flags: message.flags,
				stickers,
				favoriteMemeId,
				tts,
			});

			ComponentDispatch.dispatch('MESSAGE_SENT');
		},
		[channel?.id, referencedMessage, replyingMessage],
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
			const currentUser = UserStore.getCurrentUser();
			if (!channel || !currentUser) return;
			const nonce = SnowflakeUtils.fromTimestamp(Date.now());

			TypingUtils.clear(channel.id);
			MessageActionCreators.stopReply(channel.id);

			const message = new MessageRecord({
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
			});

			MessageActionCreators.createOptimistic(channel.id, {
				...message.toJSON(),
				referenced_message: referencedMessage?.toJSON(),
			});

			SlowmodeActionCreators.recordMessageSend(channel.id);
			const allowedMentions: AllowedMentions = {replied_user: replyingMessage?.mentioning ?? true};

			MessageActionCreators.send(channel.id, {
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
			});
		},
		[channel?.id, referencedMessage, replyingMessage],
	);

	return {sendMessage, sendOptimisticMessage};
};
