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

import {makeAutoObservable} from 'mobx';
import AuthenticationStore from '~/stores/AuthenticationStore';
import MessageStore from '~/stores/MessageStore';

type MessageReplyState = Readonly<{
	messageId: string;
	mentioning: boolean;
}>;

class MessageReplyStore {
	replyingMessageIds: Record<string, MessageReplyState> = {};
	highlightMessageId: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	isReplying(channelId: string, messageId: string): boolean {
		return this.replyingMessageIds[channelId]?.messageId === messageId;
	}

	isHighlight(messageId: string): boolean {
		return this.highlightMessageId === messageId;
	}

	startReply(channelId: string, messageId: string, mentioning: boolean): void {
		const message = MessageStore.getMessage(channelId, messageId);
		if (!message) {
			return;
		}

		const shouldMention =
			message.author.id === AuthenticationStore.currentUserId || message.webhookId ? false : mentioning;

		this.replyingMessageIds = {
			...this.replyingMessageIds,
			[channelId]: {messageId, mentioning: shouldMention},
		};
	}

	setMentioning(channelId: string, mentioning: boolean): void {
		const currentReply = this.replyingMessageIds[channelId];
		if (!currentReply) {
			return;
		}

		this.replyingMessageIds = {
			...this.replyingMessageIds,
			[channelId]: {
				...currentReply,
				mentioning,
			},
		};
	}

	stopReply(channelId: string): void {
		const {[channelId]: _, ...remainingReplies} = this.replyingMessageIds;
		this.replyingMessageIds = remainingReplies;
	}

	highlightMessage(messageId: string): void {
		this.highlightMessageId = messageId;
	}

	clearHighlight(): void {
		this.highlightMessageId = null;
	}

	getReplyingMessage(channelId: string): MessageReplyState | null {
		return this.replyingMessageIds[channelId] ?? null;
	}
}

export default new MessageReplyStore();
