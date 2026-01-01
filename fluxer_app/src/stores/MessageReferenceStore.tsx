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
import {Endpoints} from '~/Endpoints';
import http, {HttpError} from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import {type Message, MessageRecord} from '~/records/MessageRecord';
import MessageStore from '~/stores/MessageStore';

const logger = new Logger('MessageReferenceStore');

export const MessageReferenceState = {
	LOADED: 'LOADED',
	NOT_LOADED: 'NOT_LOADED',
	DELETED: 'DELETED',
} as const;
export type MessageReferenceState = (typeof MessageReferenceState)[keyof typeof MessageReferenceState];

class MessageReferenceStore {
	deletedMessageIds = new Set<string>();
	cachedMessages = new Map<string, MessageRecord>();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	private getKey(channelId: string, messageId: string): string {
		return `${channelId}:${messageId}`;
	}

	handleMessageCreate(message: Message, _optimistic: boolean): void {
		if (message.referenced_message) {
			const refChannelId = message.message_reference?.channel_id ?? message.channel_id;
			const key = this.getKey(refChannelId, message.referenced_message.id);
			if (!this.cachedMessages.has(key) && !MessageStore.getMessage(refChannelId, message.referenced_message.id)) {
				const referencedMessageRecord = new MessageRecord(message.referenced_message);
				this.cachedMessages.set(key, referencedMessageRecord);
			}
		}
	}

	handleMessageDelete(channelId: string, messageId: string): void {
		const key = this.getKey(channelId, messageId);
		this.deletedMessageIds.add(key);
		this.cachedMessages.delete(key);
	}

	handleMessageDeleteBulk(channelId: string, messageIds: Array<string>): void {
		for (const messageId of messageIds) {
			const key = this.getKey(channelId, messageId);
			this.deletedMessageIds.add(key);
			this.cachedMessages.delete(key);
		}
	}

	handleMessagesFetchSuccess(channelId: string, messages: Array<Message>): void {
		for (const message of messages) {
			if (message.referenced_message) {
				const refChannelId = message.message_reference?.channel_id ?? channelId;
				const key = this.getKey(refChannelId, message.referenced_message.id);
				if (!this.cachedMessages.has(key) && !MessageStore.getMessage(refChannelId, message.referenced_message.id)) {
					const referencedMessageRecord = new MessageRecord(message.referenced_message);
					this.cachedMessages.set(key, referencedMessageRecord);
				}
			}
		}

		const potentiallyMissingMessageIds = messages
			.filter((message) => message.message_reference && !message.referenced_message)
			.map((message) => ({
				channelId: message.message_reference!.channel_id ?? channelId,
				messageId: message.message_reference!.message_id,
			}))
			.filter(
				({channelId: refChannelId, messageId}) =>
					!MessageStore.getMessage(refChannelId, messageId) &&
					!this.deletedMessageIds.has(this.getKey(refChannelId, messageId)) &&
					!this.cachedMessages.has(this.getKey(refChannelId, messageId)),
			);

		if (potentiallyMissingMessageIds.length > 0) {
			this.fetchMissingMessages(potentiallyMissingMessageIds);
		}

		this.cleanupCachedMessages(channelId, messages);
	}

	handleChannelDelete(channelId: string): void {
		this.cleanupChannelMessages(channelId);
	}

	handleConnectionOpen(): void {
		this.deletedMessageIds.clear();
		this.cachedMessages.clear();
	}

	private fetchMissingMessages(refs: Array<{channelId: string; messageId: string}>): void {
		Promise.allSettled(
			refs.map(({channelId, messageId}) =>
				http
					.get<Message>({
						url: Endpoints.CHANNEL_MESSAGE(channelId, messageId),
					})
					.then((response) => {
						if (response.body) {
							this.handleMessageFetchSuccess(channelId, messageId, response.body);
						}
					})
					.catch((error) => this.handleMessageFetchError(channelId, messageId, error)),
			),
		);
	}

	private handleMessageFetchSuccess(channelId: string, messageId: string, message: Message): void {
		const messageRecord = new MessageRecord(message);
		const key = this.getKey(channelId, messageId);

		this.cachedMessages.set(key, messageRecord);

		if (MessageStore.getMessage(channelId, messageId)) {
			this.cachedMessages.delete(key);
			this.deletedMessageIds.delete(key);
		}
	}

	private handleMessageFetchError(channelId: string, messageId: string, error: unknown): void {
		const key = this.getKey(channelId, messageId);

		if (error instanceof HttpError && error.status === 404) {
			this.deletedMessageIds.add(key);
			this.cachedMessages.delete(key);
		} else {
			logger.error(`Failed to fetch message ${messageId}`, error);
		}
	}

	private cleanupCachedMessages(channelId: string, messages: Array<Message>): void {
		for (const message of messages) {
			const messageId = message.message_reference?.message_id;
			if (!messageId) continue;

			const key = this.getKey(channelId, messageId);
			if (MessageStore.getMessage(channelId, messageId)) {
				this.cachedMessages.delete(key);
				this.deletedMessageIds.delete(key);
			}
		}
	}

	private cleanupChannelMessages(channelId: string): void {
		const channelPrefix = `${channelId}:`;

		for (const key of Array.from(this.deletedMessageIds)) {
			if (key.startsWith(channelPrefix)) {
				this.deletedMessageIds.delete(key);
			}
		}

		for (const key of Array.from(this.cachedMessages.keys())) {
			if (key.startsWith(channelPrefix)) {
				this.cachedMessages.delete(key);
			}
		}
	}

	getMessage(channelId: string, messageId: string): MessageRecord | null {
		const key = this.getKey(channelId, messageId);

		if (this.deletedMessageIds.has(key)) {
			return null;
		}

		return MessageStore.getMessage(channelId, messageId) || this.cachedMessages.get(key) || null;
	}

	getMessageReference(
		channelId: string,
		messageId: string,
	): {
		message: MessageRecord | null;
		state: MessageReferenceState;
	} {
		const key = this.getKey(channelId, messageId);

		if (this.deletedMessageIds.has(key)) {
			return {
				message: null,
				state: MessageReferenceState.DELETED,
			};
		}

		const message = MessageStore.getMessage(channelId, messageId);
		if (message) {
			return {
				message,
				state: MessageReferenceState.LOADED,
			};
		}

		const cachedMessage = this.cachedMessages.get(key);
		if (cachedMessage) {
			return {
				message: cachedMessage,
				state: MessageReferenceState.LOADED,
			};
		}

		return {
			message: null,
			state: MessageReferenceState.NOT_LOADED,
		};
	}
}

export default new MessageReferenceStore();
