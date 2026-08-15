// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {makeAutoObservable} from 'mobx';

export interface MessageReplyState {
	messageId: string;
	mentioning: boolean;
	snapshot: Message;
}

class MessageReply {
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
		const message = Messages.getMessage(channelId, messageId);
		if (!message) {
			return;
		}
		const shouldMention = message.author.id === Authentication.currentUserId || message.webhookId ? false : mentioning;
		const current = this.replyingMessageIds[channelId];
		if (current && current.messageId === messageId && current.mentioning === shouldMention) {
			current.snapshot = message;
			return;
		}
		this.replyingMessageIds[channelId] = {messageId, mentioning: shouldMention, snapshot: message};
	}

	setMentioning(channelId: string, mentioning: boolean): void {
		const currentReply = this.replyingMessageIds[channelId];
		if (!currentReply || currentReply.mentioning === mentioning) {
			return;
		}
		currentReply.mentioning = mentioning;
	}

	stopReply(channelId: string): void {
		if (!(channelId in this.replyingMessageIds)) {
			return;
		}
		delete this.replyingMessageIds[channelId];
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

	getReferencedMessage(channelId: string): Message | null {
		const state = this.replyingMessageIds[channelId];
		if (!state) {
			return null;
		}
		return Messages.getMessage(channelId, state.messageId) ?? state.snapshot;
	}
}

export default new MessageReply();
