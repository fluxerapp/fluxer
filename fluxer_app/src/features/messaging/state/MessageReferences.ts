// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import Channels from '@app/features/channel/state/Channels';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import {Message as MessageRecord} from '@app/features/messaging/models/MessagingMessage';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('MessageReferences');
export const MessageReferenceState = {
	LOADED: 'LOADED',
	NOT_LOADED: 'NOT_LOADED',
	DELETED: 'DELETED',
} as const;

export type MessageReferenceState = ValueOf<typeof MessageReferenceState>;
type MessageInput = WireMessage | MessageRecord;

const toWireMessage = (message: MessageInput): WireMessage =>
	message instanceof MessageRecord ? message.toJSON() : message;

class MessageReferences {
	deletedMessageIds = new Set<string>();
	cachedMessages = new Map<string, MessageRecord>();
	private referenceVersions = new Map<string, number>();
	private referenceCount = new Map<string, Set<string>>();
	private referencingMessages = new Map<
		string,
		{
			channelId: string;
			messageId: string;
		}
	>();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	private getKey(channelId: string, messageId: string): string {
		return `${channelId}:${messageId}`;
	}

	private bumpReferenceVersion(refChannelId: string, refMessageId: string): void {
		const key = this.getKey(refChannelId, refMessageId);
		this.referenceVersions.set(key, (this.referenceVersions.get(key) ?? 0) + 1);
	}

	private readReferenceVersion(refChannelId: string, refMessageId: string): number {
		return this.referenceVersions.get(this.getKey(refChannelId, refMessageId)) ?? 0;
	}

	private setCachedMessage(refChannelId: string, refMessageId: string, message: MessageInput): boolean {
		const key = this.getKey(refChannelId, refMessageId);
		const nextMessage = new MessageRecord(toWireMessage(message), {missingReactions: 'preserve'});
		const currentMessage = this.cachedMessages.get(key);
		if (currentMessage?.equals(nextMessage)) {
			return false;
		}
		this.cachedMessages.set(key, nextMessage);
		this.bumpReferenceVersion(refChannelId, refMessageId);
		return true;
	}

	private updateCachedMessage(refChannelId: string, refMessageId: string, updates: Partial<WireMessage>): boolean {
		const key = this.getKey(refChannelId, refMessageId);
		const currentMessage = this.cachedMessages.get(key);
		if (!currentMessage) {
			return false;
		}
		const nextMessage = currentMessage.withUpdates(updates);
		if (currentMessage.equals(nextMessage)) {
			return false;
		}
		this.cachedMessages.set(key, nextMessage);
		return true;
	}

	private handleReferencedMessageUpdate(message: WireMessage): void {
		const key = this.getKey(message.channel_id, message.id);
		const isTrackedReference = this.referenceCount.has(key) || this.cachedMessages.has(key);
		if (!isTrackedReference) {
			return;
		}
		this.updateCachedMessage(message.channel_id, message.id, message);
		this.bumpReferenceVersion(message.channel_id, message.id);
	}

	private addReference(refChannelId: string, refMessageId: string, referencingMessageId: string): void {
		const key = this.getKey(refChannelId, refMessageId);
		let refs = this.referenceCount.get(key);
		if (!refs) {
			refs = new Set<string>();
			this.referenceCount.set(key, refs);
		}
		refs.add(referencingMessageId);
		this.referencingMessages.set(referencingMessageId, {channelId: refChannelId, messageId: refMessageId});
	}

	private removeReference(refChannelId: string, refMessageId: string, referencingMessageId: string): void {
		const key = this.getKey(refChannelId, refMessageId);
		const refs = this.referenceCount.get(key);
		if (refs) {
			refs.delete(referencingMessageId);
			if (refs.size === 0) {
				this.referenceCount.delete(key);
				this.cachedMessages.delete(key);
				this.referenceVersions.delete(key);
			}
		}
		this.referencingMessages.delete(referencingMessageId);
	}

	handleMessageCreate(message: WireMessage, _optimistic: boolean): void {
		if (message.referenced_message) {
			const refChannelId = message.message_reference?.channel_id ?? message.channel_id;
			const refMessageId = message.referenced_message.id;
			this.setCachedMessage(refChannelId, refMessageId, message.referenced_message);
			this.addReference(refChannelId, refMessageId, message.id);
		}
	}

	handleMessageDelete(channelId: string, messageId: string): void {
		const key = this.getKey(channelId, messageId);
		this.deletedMessageIds.add(key);
		this.cachedMessages.delete(key);
		this.referenceVersions.delete(key);
		this.referenceCount.delete(key);
		const referencedBy = this.referencingMessages.get(messageId);
		if (referencedBy) {
			this.removeReference(referencedBy.channelId, referencedBy.messageId, messageId);
		}
	}

	handleMessageDeleteBulk(channelId: string, messageIds: Array<string>): void {
		for (const messageId of messageIds) {
			const key = this.getKey(channelId, messageId);
			this.deletedMessageIds.add(key);
			this.cachedMessages.delete(key);
			this.referenceVersions.delete(key);
			this.referenceCount.delete(key);
			const referencedBy = this.referencingMessages.get(messageId);
			if (referencedBy) {
				this.removeReference(referencedBy.channelId, referencedBy.messageId, messageId);
			}
		}
	}

	handleMessagesFetchSuccess(channelId: string, messages: Array<WireMessage>): void {
		for (const message of messages) {
			if (message.referenced_message) {
				const refChannelId = message.message_reference?.channel_id ?? channelId;
				const refMessageId = message.referenced_message.id;
				this.setCachedMessage(refChannelId, refMessageId, message.referenced_message);
				this.addReference(refChannelId, refMessageId, message.id);
			}
		}
		const potentiallyMissingMessageIds = messages
			.filter((message) => message.message_reference && !message.referenced_message)
			.map((message) => ({
				channelId: message.message_reference!.channel_id ?? channelId,
				messageId: message.message_reference!.message_id,
				referencingMessageId: message.id,
			}))
			.filter(
				({channelId: refChannelId, messageId}) =>
					!Messages.getMessage(refChannelId, messageId) &&
					!this.deletedMessageIds.has(this.getKey(refChannelId, messageId)) &&
					!this.cachedMessages.has(this.getKey(refChannelId, messageId)),
			);
		for (const {channelId: refChannelId, messageId, referencingMessageId} of potentiallyMissingMessageIds) {
			this.addReference(refChannelId, messageId, referencingMessageId);
		}
		if (potentiallyMissingMessageIds.length > 0) {
			this.fetchMissingMessages(potentiallyMissingMessageIds.map(({channelId, messageId}) => ({channelId, messageId})));
		}
	}

	handleChannelDelete(channelId: string): void {
		this.cleanupChannelMessages(channelId);
	}

	handleGatewayReady(): void {
		this.deletedMessageIds.clear();
		this.cachedMessages.clear();
		this.referenceVersions.clear();
		this.referenceCount.clear();
		this.referencingMessages.clear();
	}

	handleMessageUpdate(message: WireMessage): void {
		this.handleReferencedMessageUpdate(message);
		if (!('message_reference' in message) && !('referenced_message' in message)) {
			return;
		}
		const previousRef = this.referencingMessages.get(message.id);
		const newRefChannelId = message.message_reference?.channel_id ?? message.channel_id;
		const newRefMessageId = message.referenced_message?.id ?? message.message_reference?.message_id;
		if (previousRef) {
			const previousKey = this.getKey(previousRef.channelId, previousRef.messageId);
			const newKey = newRefMessageId ? this.getKey(newRefChannelId, newRefMessageId) : null;
			if (previousKey !== newKey) {
				this.removeReference(previousRef.channelId, previousRef.messageId, message.id);
			}
		}
		if (newRefMessageId) {
			if (message.referenced_message) {
				this.setCachedMessage(newRefChannelId, newRefMessageId, message.referenced_message);
			}
			this.addReference(newRefChannelId, newRefMessageId, message.id);
		}
	}

	private fetchMissingMessages(
		refs: Array<{
			channelId: string;
			messageId: string;
		}>,
	): void {
		const allowedRefs = refs.filter(({channelId}) => {
			const channel = Channels.getChannel(channelId);
			if (!channel) {
				return false;
			}
			if (channel.isPrivate()) {
				return true;
			}
			return !GuildMatureContentAgree.shouldShowGate({channelId: channel.id, guildId: channel.guildId ?? null});
		});
		if (allowedRefs.length === 0) {
			return;
		}
		Promise.allSettled(
			allowedRefs.map(({channelId, messageId}) =>
				http
					.get<WireMessage>(Endpoints.CHANNEL_MESSAGE(channelId, messageId))
					.then((response) => {
						if (response.body) {
							this.handleMessageFetchSuccess(channelId, messageId, response.body);
						}
					})
					.catch((error) => this.handleMessageFetchError(channelId, messageId, error)),
			),
		);
	}

	private handleMessageFetchSuccess(channelId: string, messageId: string, message: WireMessage): void {
		const messageRecord = new MessageRecord(message);
		const key = this.getKey(channelId, messageId);
		this.cachedMessages.set(key, messageRecord);
		this.bumpReferenceVersion(channelId, messageId);
	}

	private handleMessageFetchError(channelId: string, messageId: string, error: unknown): void {
		const key = this.getKey(channelId, messageId);
		if (error instanceof HttpError && error.status === 404) {
			this.deletedMessageIds.add(key);
			this.cachedMessages.delete(key);
			this.referenceVersions.delete(key);
		} else {
			logger.error(`Failed to fetch message ${messageId}`, error);
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
		for (const key of Array.from(this.referenceVersions.keys())) {
			if (key.startsWith(channelPrefix)) {
				this.referenceVersions.delete(key);
			}
		}
		for (const key of Array.from(this.referenceCount.keys())) {
			if (key.startsWith(channelPrefix)) {
				this.referenceCount.delete(key);
			}
		}
		for (const [messageId, ref] of Array.from(this.referencingMessages.entries())) {
			if (ref.channelId === channelId) {
				this.referencingMessages.delete(messageId);
			}
		}
	}

	getMessage(channelId: string, messageId: string): MessageRecord | null {
		const key = this.getKey(channelId, messageId);
		this.readReferenceVersion(channelId, messageId);
		if (this.deletedMessageIds.has(key)) {
			return null;
		}
		return Messages.getMessage(channelId, messageId) || this.cachedMessages.get(key) || null;
	}

	getMessageReference(
		channelId: string,
		messageId: string,
	): {
		message: MessageRecord | null;
		state: MessageReferenceState;
	} {
		const key = this.getKey(channelId, messageId);
		this.readReferenceVersion(channelId, messageId);
		if (this.deletedMessageIds.has(key)) {
			return {
				message: null,
				state: MessageReferenceState.DELETED,
			};
		}
		const message = Messages.getMessage(channelId, messageId);
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

export default new MessageReferences();
